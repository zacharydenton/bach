#!/usr/bin/env python3
"""patchboard.py — live patch auditioning for otb performances, in the browser.

Loops a compiled PerformanceIR through per-channel Surge XT instances and
serves a web UI (default http://<host>:8766) where each *part*
(contrapuntal voice) has a category-grouped patch selector with prev/next
surfing, a gain fader and a mute — switch timbres while the fugue plays
and hear the casting in context. The casting readout emits the exact
--patch-ch arguments for audition.py / render_showcase.sh.

**Audio goes to the browser.** The engine renders wall-clock-paced blocks
and broadcasts raw PCM (f32le stereo) over chunked HTTP at /pcm; the page
pulls it with fetch streaming into an AudioWorklet ring buffer. Phone on
the tailnet, laptop, several listeners at once — anything with a modern
browser is a monitor, ~200 ms behind live. No local audio device is
touched unless --local is passed (sounddevice/PortAudio: JACK/PipeWire on
Linux, CoreAudio on macOS).

    PYTHONPATH=<surgepy dir> .venv-audition/bin/python tools/patchboard.py \
        perf.json --scl w3.scl [--port 8766] [--local]

Patch loads happen in the render thread and may click — an auditioning
tool, not a performance instrument. Deps: surgepy, numpy; sounddevice
only for --local. License: GPL-2.0-or-later.
"""

import argparse
import json
import os
import queue
import sys
import threading
import time
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

import numpy as np
import surgepy

BLOCK = 32
CHUNK_FRAMES = 4096  # ~85 ms at 48k: the broadcast unit


def patch_dirs():
    cands = [
        "/usr/share/surge-xt/patches_factory",
        "/usr/share/surge-xt/patches_3rdparty",
        os.path.expanduser(
            "~/Library/Application Support/Surge XT/patches_factory"),
        "/Library/Application Support/Surge XT/patches_factory",
    ]
    return [d for d in cands if os.path.isdir(d)]


def scan_patches():
    cats = {}
    for root in patch_dirs():
        for cat in sorted(os.listdir(root)):
            catdir = os.path.join(root, cat)
            if not os.path.isdir(catdir) or cat in ("Tutorials", "Templates"):
                continue
            for f in sorted(os.listdir(catdir)):
                if f.endswith(".fxp"):
                    cats.setdefault(cat, []).append(
                        (f[:-4], os.path.join(catdir, f)))
    return cats


class Engine:
    def __init__(self, perf, sr, scl):
        self.sr = sr
        self.parts = []
        self.instances = {}
        self.events = {}
        self.pos = {}
        self.sample = 0
        self.playing = True
        self.pending = queue.Queue()
        self.subscribers = set()  # queues of PCM byte chunks
        self.sublock = threading.Lock()

        end = 0.0
        for vi, tr in enumerate(perf["tracks"]):
            chans = sorted({n["ch"] for n in tr})
            self.parts.append(
                {"name": f"voice {vi}", "channels": chans,
                 "gain": 1.0, "mute": False, "patch": "(init)"})
            for n in tr:
                ch = n["ch"]
                on = int(n["onS"] * sr)
                off = max(on + 1, int((n["onS"] + n["durS"]) * sr))
                end = max(end, n["onS"] + n["durS"])
                evs = self.events.setdefault(ch, [])
                evs.append((on, 0, n["bend"], 0))
                evs.append((on, 1, n["pitch"], n["vel"]))
                evs.append((off, 2, n["pitch"], 0))
        for ch, evs in self.events.items():
            evs.sort(key=lambda e: (e[0], e[1]))
            self.pos[ch] = 0
            s = surgepy.createSurge(sr)
            if scl:
                s.loadSCLFile(scl)
            self.instances[ch] = s
        self.loop_len = int((end + 2.0) * sr)
        self.part_by_ch = {
            ch: p for p in self.parts for ch in p["channels"]}

    def request_patch(self, part_idx, path):
        self.pending.put((part_idx, path))

    def _apply_pending(self):
        try:
            while True:
                pi, path = self.pending.get_nowait()
                part = self.parts[pi]
                for ch in part["channels"]:
                    s = self.instances[ch]
                    s.allNotesOff()
                    if path != "(init)":
                        s.loadPatch(path)
                part["patch"] = (os.path.basename(path)[:-4]
                                 if path != "(init)" else "(init)")
        except queue.Empty:
            pass

    def render(self, frames):
        """Mix `frames` samples (multiple of BLOCK) of the looping piece."""
        out = np.zeros((2, frames), dtype=np.float32)
        if not self.playing:
            return out
        self._apply_pending()
        done = 0
        while done < frames:
            if self.sample >= self.loop_len:
                self.sample = 0
                for ch, s in self.instances.items():
                    s.allNotesOff()
                    self.pos[ch] = 0
            for ch, s in self.instances.items():
                evs, i = self.events[ch], self.pos[ch]
                while i < len(evs) and evs[i][0] <= self.sample:
                    _, kind, a, b = evs[i]
                    if kind == 0:
                        s.pitchBend(0, a - 8192)
                    elif kind == 1:
                        s.playNote(0, a, b, 0)
                    else:
                        s.releaseNote(0, a, 0)
                    i += 1
                self.pos[ch] = i
                part = self.part_by_ch[ch]
                s.process()
                if not part["mute"]:
                    out[:, done:done + BLOCK] += s.getOutput() * part["gain"]
            self.sample += BLOCK
            done += BLOCK
        return out

    def subscribe(self):
        q = queue.Queue(maxsize=16)
        with self.sublock:
            self.subscribers.add(q)
        return q

    def unsubscribe(self, q):
        with self.sublock:
            self.subscribers.discard(q)

    def broadcaster(self):
        """Wall-clock-paced render loop feeding every /pcm listener."""
        period = CHUNK_FRAMES / self.sr
        deadline = time.monotonic()
        while True:
            mix = self.render(CHUNK_FRAMES)
            data = mix.T.reshape(-1).astype("<f4").tobytes()  # interleaved
            with self.sublock:
                subs = list(self.subscribers)
            for q in subs:
                try:
                    q.put_nowait(data)
                except queue.Full:  # listener fell behind: drop, stay live
                    try:
                        q.get_nowait()
                        q.put_nowait(data)
                    except queue.Empty:
                        pass
            deadline += period
            delay = deadline - time.monotonic()
            if delay > 0:
                time.sleep(delay)
            else:  # render fell behind: resync rather than spiral
                deadline = time.monotonic()

    def state(self):
        return {
            "playing": self.playing,
            "position": self.sample / self.sr,
            "length": self.loop_len / self.sr,
            "sr": self.sr,
            "listeners": len(self.subscribers),
            "parts": [
                {"name": p["name"], "channels": p["channels"],
                 "gain": p["gain"], "mute": p["mute"], "patch": p["patch"]}
                for p in self.parts
            ],
        }


PAGE = """<!doctype html><meta charset=utf-8>
<meta name=viewport content="width=device-width,initial-scale=1">
<title>otb patchboard</title>
<style>
body{font:15px/1.5 system-ui;background:#111;color:#ddd;max-width:46rem;
     margin:1.5rem auto;padding:0 1rem}
.part{border:1px solid #333;border-radius:8px;padding:.7rem;margin:.6rem 0}
.row{display:flex;gap:.5rem;align-items:center;flex-wrap:wrap}
select{flex:1;min-width:12rem;background:#222;color:#ddd;border:1px solid #444;
       padding:.3rem;border-radius:4px}
button{background:#2a2a2a;color:#ddd;border:1px solid #444;border-radius:4px;
       padding:.3rem .7rem;cursor:pointer}
button.on{background:#264;border-color:#4a8}
button.big{font-size:1.1rem;padding:.5rem 1.2rem}
input[type=range]{width:8rem}
h1{font-size:1.2rem} .patch{color:#9c9} .pos{color:#777;font-size:.85rem}
#cast{width:100%;background:#181818;color:#cc9;border:1px solid #333;
      font:12px monospace;padding:.5rem;white-space:pre-wrap}
</style>
<h1>otb patchboard</h1>
<div class=row>
 <button id=listen class=big onclick=listen()>▶ listen</button>
 <button id=play onclick=toggle()>pause</button>
 <span class=pos id=pos></span>
</div>
<div id=parts></div>
<h3>casting (paste into render_showcase / audition.py)</h3>
<div id=cast></div>
<script>
let CATS={}, STATE=null, audioOn=false;

const WORKLET = `
class Player extends AudioWorkletProcessor {
  constructor(){ super(); this.buf=[]; this.frame=0; this.chunk=null;
    this.port.onmessage = e => { if (this.buf.length < 64) this.buf.push(e.data); };
  }
  process(_, outputs){
    const out = outputs[0]; let need = out[0].length, wrote = 0;
    while (wrote < need){
      if (!this.chunk){
        if (!this.buf.length) break;
        this.chunk = this.buf.shift(); this.frame = 0;
      }
      const frames = this.chunk.length/2 - this.frame;
      const n = Math.min(frames, need - wrote);
      for (let i=0;i<n;i++){
        out[0][wrote+i] = this.chunk[(this.frame+i)*2];
        out[1][wrote+i] = this.chunk[(this.frame+i)*2+1];
      }
      wrote += n; this.frame += n;
      if (this.frame*2 >= this.chunk.length) this.chunk = null;
    }
    return true;
  }
}
registerProcessor('player', Player);`;

async function listen(){
  if (audioOn) return;
  audioOn = true;
  document.getElementById('listen').classList.add('on');
  document.getElementById('listen').textContent = '● live';
  const ctx = new AudioContext({sampleRate: STATE ? STATE.sr : 48000});
  const url = URL.createObjectURL(new Blob([WORKLET],{type:'text/javascript'}));
  await ctx.audioWorklet.addModule(url);
  const node = new AudioWorkletNode(ctx, 'player', {outputChannelCount:[2]});
  node.connect(ctx.destination);
  const resp = await fetch('/pcm');
  const reader = resp.body.getReader();
  let pend = new Uint8Array(0);
  while (true){
    const {done, value} = await reader.read();
    if (done) break;
    let all = new Uint8Array(pend.length + value.length);
    all.set(pend); all.set(value, pend.length);
    const usable = all.length - (all.length % 8); // whole stereo f32 frames
    if (usable){
      const floats = new Float32Array(all.buffer.slice(0, usable));
      node.port.postMessage(floats, [floats.buffer]);
    }
    pend = all.slice(usable);
  }
}

async function init(){
  CATS = await (await fetch('/patches')).json();
  await refresh(); setInterval(refresh, 1000);
}
function opts(){
  let h = '<option value="(init)">(init patch)</option>';
  for (const [cat, ps] of Object.entries(CATS)){
    h += `<optgroup label="${cat}">`;
    for (const [name, path] of ps)
      h += `<option value="${path.replace(/"/g,'&quot;')}">${name}</option>`;
    h += '</optgroup>';
  }
  return h;
}
async function refresh(){
  STATE = await (await fetch('/state')).json();
  document.getElementById('play').textContent = STATE.playing?'pause':'play';
  document.getElementById('pos').textContent =
    STATE.position.toFixed(1)+' / '+STATE.length.toFixed(1)+'s · '+
    STATE.listeners+' listening';
  const div = document.getElementById('parts');
  if (div.childElementCount != STATE.parts.length){
    div.innerHTML = STATE.parts.map((p,i)=>`
      <div class=part><div class=row>
        <b>${p.name}</b> <span class=patch id=pn${i}>${p.patch}</span>
      </div><div class=row>
        <button onclick=step(${i},-1)>◀</button>
        <select id=sel${i} onchange=setPatch(${i},this.value)>${opts()}</select>
        <button onclick=step(${i},1)>▶</button>
        <button id=mute${i} onclick=mute(${i})>mute</button>
        <input type=range min=0 max=1.5 step=0.05 value=1
          onchange=gain(${i},this.value)>
      </div></div>`).join('');
  }
  STATE.parts.forEach((p,i)=>{
    document.getElementById('pn'+i).textContent = p.patch;
    document.getElementById('mute'+i).className = p.mute?'on':'';
  });
  document.getElementById('cast').textContent = STATE.parts.map((p,i)=>
    p.patch=='(init)'?'':p.channels.map(c=>
      `--patch-ch "${c}:${sel(i)}"`).join(' ')).filter(x=>x).join(' \\\\\\n');
}
function sel(i){ return document.getElementById('sel'+i).value; }
async function post(url, body){
  await fetch(url,{method:'POST',body:JSON.stringify(body)}); refresh();
}
function setPatch(i,path){ post('/patch',{part:i,path}); }
function step(i,d){
  const s = document.getElementById('sel'+i);
  s.selectedIndex = Math.max(0, Math.min(s.length-1, s.selectedIndex+d));
  setPatch(i, s.value);
}
function mute(i){ post('/mute',{part:i}); }
function gain(i,v){ post('/gain',{part:i,gain:parseFloat(v)}); }
function toggle(){ post('/toggle',{}); }
init();
</script>"""


def serve(engine, cats, port):
    class H(BaseHTTPRequestHandler):
        protocol_version = "HTTP/1.1"

        def log_message(self, *a):
            pass

        def _json(self, obj, code=200):
            b = json.dumps(obj).encode()
            self.send_response(code)
            self.send_header("Content-Type", "application/json")
            self.send_header("Content-Length", str(len(b)))
            self.end_headers()
            self.wfile.write(b)

        def do_GET(self):
            if self.path == "/":
                b = PAGE.encode()
                self.send_response(200)
                self.send_header("Content-Type", "text/html; charset=utf-8")
                self.send_header("Content-Length", str(len(b)))
                self.end_headers()
                self.wfile.write(b)
            elif self.path == "/state":
                self._json(engine.state())
            elif self.path == "/patches":
                self._json(cats)
            elif self.path == "/pcm":
                self.send_response(200)
                self.send_header("Content-Type", "application/octet-stream")
                self.send_header("Cache-Control", "no-store")
                self.send_header("Transfer-Encoding", "chunked")
                self.end_headers()
                q = engine.subscribe()
                try:
                    while True:
                        data = q.get()
                        self.wfile.write(
                            f"{len(data):X}\r\n".encode() + data + b"\r\n")
                        self.wfile.flush()
                except (BrokenPipeError, ConnectionResetError, OSError):
                    pass
                finally:
                    engine.unsubscribe(q)
            else:
                self._json({"err": "not found"}, 404)

        def do_POST(self):
            n = int(self.headers.get("Content-Length", 0))
            body = json.loads(self.rfile.read(n) or b"{}")
            if self.path == "/patch":
                engine.request_patch(body["part"], body["path"])
            elif self.path == "/mute":
                p = engine.parts[body["part"]]
                p["mute"] = not p["mute"]
            elif self.path == "/gain":
                engine.parts[body["part"]]["gain"] = float(body["gain"])
            elif self.path == "/toggle":
                engine.playing = not engine.playing
            self._json({"ok": True})

    srv = ThreadingHTTPServer(("0.0.0.0", port), H)
    threading.Thread(target=srv.serve_forever, daemon=True).start()
    return srv


def main():
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("perf")
    ap.add_argument("--scl")
    ap.add_argument("--sr", type=int, default=48000)
    ap.add_argument("--port", type=int, default=8766)
    ap.add_argument("--local", action="store_true",
                    help="also play on the local audio device (sounddevice)")
    args = ap.parse_args()

    with open(args.perf) as f:
        perf = json.load(f)
    cats = scan_patches()
    if not cats:
        sys.exit("no Surge patch library found (patches_factory)")
    engine = Engine(perf, args.sr, args.scl)
    serve(engine, cats, args.port)
    print(f"patchboard: http://0.0.0.0:{args.port}/  "
          f"({len(engine.instances)} lanes, {len(engine.parts)} parts, "
          f"loop {engine.loop_len/args.sr:.0f}s) — press ▶ listen in the page",
          flush=True)

    if args.local:
        import sounddevice as sd
        q = engine.subscribe()

        def cb(outdata, frames, _t, _s):
            try:
                data = q.get_nowait()
                arr = np.frombuffer(data, dtype="<f4").reshape(-1, 2)
                outdata[:len(arr)] = arr[:frames]
                if len(arr) < frames:
                    outdata[len(arr):] = 0
            except queue.Empty:
                outdata[:] = 0

        stream = sd.OutputStream(samplerate=args.sr, channels=2,
                                 dtype="float32", blocksize=CHUNK_FRAMES,
                                 callback=cb)
        stream.start()

    engine.broadcaster()  # runs forever, wall-clock paced


if __name__ == "__main__":
    main()
