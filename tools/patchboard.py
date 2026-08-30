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
pulls it with fetch streaming into an AudioWorklet ring buffer. A bounded
remote path transcodes to Ogg Opus through ffmpeg. Phone on the tailnet,
laptop, several listeners at once — anything with a modern browser is a
monitor. No local audio device is touched unless --local is passed
(sounddevice/PortAudio: JACK/PipeWire on Linux, CoreAudio on macOS).

    PYTHONPATH=<surgepy dir> .venv-audition/bin/python tools/patchboard.py \
        perf.json --scl w3.scl [--port 8766] [--host 0.0.0.0] [--local]

Patch loads happen in the render thread and may click — an auditioning
tool, not a performance instrument. Deps: surgepy, numpy; ffmpeg for remote
Opus; sounddevice only for --local. License: GPL-2.0-or-later.
"""

import argparse
import json
import os
import collections
import queue
import shutil
import subprocess
import sys
import threading
import time
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

try:
    import numpy as np
    import surgepy
except ImportError:  # the HTTP layer is testable without the synth
    np = None
    surgepy = None

BLOCK = 32
CHUNK_FRAMES = 4096  # ~85 ms at 48k: the broadcast unit
MAX_OPUS_CLIENTS = 4


def patch_dirs():
    cands = [
        os.environ.get("SURGE_PATCHES", ""),  # explicit override wins
        "/usr/share/surge-xt/patches_factory",
        "/usr/share/surge-xt/patches_3rdparty",
        os.path.expanduser(
            "~/Library/Application Support/Surge XT/patches_factory"),
        "/Library/Application Support/Surge XT/patches_factory",
        # a surge-src checkout works without installing Surge at all
        os.path.expanduser("~/code/surge-src/resources/data/patches_factory"),
    ]
    return [d for d in cands if d and os.path.isdir(d)]


def scan_patches():
    # a system install and a surge-src checkout carry the same factory
    # bank; first tree wins per (category, name) so nothing lists twice
    cats = {}
    seen = set()
    for root in patch_dirs():
        for cat in sorted(os.listdir(root)):
            catdir = os.path.join(root, cat)
            if not os.path.isdir(catdir) or cat in ("Tutorials", "Templates"):
                continue
            for f in sorted(os.listdir(catdir)):
                if f.endswith(".fxp") and (cat, f) not in seen:
                    seen.add((cat, f))
                    cats.setdefault(cat, []).append(
                        (f[:-4], os.path.join(catdir, f)))
    return cats


class Engine:
    """Plays a *playlist* of PerformanceIRs in order, like an album side.

    Surge instances are created once for the widest piece and persist
    across pieces; gains, mutes and loaded patches live per *channel*, so
    a casting you dial in survives piece changes unless a piece brings
    its own config/casting/<piece>.json.
    """

    def __init__(self, playlist, sr, scl, casting_dir=None,
                 calibration=None):
        self.calibration = calibration or {}
        self.sr = sr
        self.playlist = playlist  # [(name, perf)]
        self.casting_dir = casting_dir
        self._default_cast_done = False
        self.scl_active = bool(scl)
        self.playing = True
        self.pending = queue.Queue()  # (part_idx, path)
        self.jump = None  # requested piece index
        self.subscribers = set()
        self.sublock = threading.Lock()
        # rolling history: new /opus listeners get this as a burst so
        # their buffer fills at network speed instead of 1 s per second
        self.history = collections.deque(
            maxlen=int(5 * sr / CHUNK_FRAMES) + 1)

        maxch = max(n["ch"] for _, p in playlist
                    for tr in p["tracks"] for n in tr) + 1
        self.instances, self.bufs = {}, {}
        self.ch_gain = {ch: 0.25 for ch in range(maxch)}  # summing headroom
        self.ch_mute = {ch: False for ch in range(maxch)}
        self.ch_patch = {ch: "(init)" for ch in range(maxch)}
        self.ch_patch_path = {ch: "(init)" for ch in range(maxch)}
        for ch in range(maxch):
            s = surgepy.createSurge(sr)
            if scl:
                s.loadSCLFile(scl)
            self.instances[ch] = s
            self.bufs[ch] = s.createMultiBlock(CHUNK_FRAMES // BLOCK)

        self.piece_i = -1
        self.load_piece(0)

    def load_piece(self, idx):
        self.piece_i = idx % len(self.playlist)
        name, perf = self.playlist[self.piece_i]
        self.piece_name = name
        self.sample = 0
        self.events, self.pos = {}, {}
        self.parts = []

        # kern orders spines bass-first: name parts by register from the
        # notes, so nobody casts a lead onto the bass again
        means = [sum(n["pitch"] for n in tr) / max(1, len(tr))
                 for tr in perf["tracks"]]
        order = sorted(range(len(means)), key=lambda i: means[i])
        regnames = {1: ["solo"], 2: ["bass", "soprano"],
                    3: ["bass", "alto", "soprano"],
                    4: ["bass", "tenor", "alto", "soprano"],
                    5: ["bass", "tenor", "alto", "mezzo", "soprano"]}
        names = regnames.get(len(means))
        labels = {vi: (names[rank] if names else f"voice {vi}")
                  for rank, vi in enumerate(order)}

        # the executable edition's subtitle track: every note's rule
        # citations, indexed by sounding time
        self.why_index = sorted(
            (n["onS"], n["onS"] + n["durS"], n["ch"], n["whys"])
            for tr in perf["tracks"] for n in tr if n.get("whys"))

        for vi, tr in enumerate(perf["tracks"]):
            chans = sorted({n["ch"] for n in tr})
            self.parts.append(
                {"name": f"{labels[vi]} (voice {vi})", "channels": chans})

        # resolve THIS piece's effective patches BEFORE building events:
        # calibration compensation must see the patches that will play,
        # not whatever was loaded when the previous piece ended
        eff = self._resolve_casting(name)
        for ch, path in eff.items():
            if path != self.ch_patch_path.get(ch) and os.path.isfile(path):
                self.request_patch_ch(ch, path)

        end = 0.0
        for vi, tr in enumerate(perf["tracks"]):
            for n in tr:
                ch = n["ch"]
                on = int(n["onS"] * self.sr)
                # timbre-aware articulation: subtract part of the
                # channel's measured release tail (calibrate_patch.py)
                # so breaths stay audible on pads
                m = self.calibration.get(eff.get(ch, ""), None)
                comp = min(0.5 * m["releaseS"], 0.3) if m else 0.0
                durS = max(n["durS"] * 0.5, n["durS"] - comp)
                off = max(on + 1, int((n["onS"] + durS) * self.sr))
                end = max(end, n["onS"] + n["durS"])
                evs = self.events.setdefault(ch, [])
                if not self.scl_active:
                    # with native .scl tuning, bends would tune twice
                    evs.append((on, 0, n["bend"], 0))
                evs.append((on, 1, n["pitch"], n["vel"]))
                evs.append((off, 2, n["pitch"], 0))
        for ch, evs in self.events.items():
            evs.sort(key=lambda e: (e[0], {2: 0, 0: 1, 1: 2}[e[1]]))
            self.pos[ch] = 0
        self.tempo_evs = sorted(
            (int(t.get("onS", 0) * self.sr), t["bpm"])
            for t in perf.get("tempoMap", []) if "onS" in t)
        self.tempo_i = 0
        # BLOCK-aligned so chunk spans never straddle the wrap mid-block
        self.loop_len = -(-int((end + 2.0) * self.sr) // BLOCK) * BLOCK
        for s in self.instances.values():
            s.allNotesOff()


    def _resolve_casting(self, name):
        """Effective patch per channel for a piece about to load.

        Current patches survive (auditioning continuity); on the very
        first load casting/default.json seeds the standing rig; a piece
        with its own casting file overrides, its per-part entries
        expanded to every channel of the part.
        """
        eff = dict(self.ch_patch_path)
        if not self.casting_dir:
            return eff
        if not self._default_cast_done:
            self._default_cast_done = True
            dflt = os.path.join(self.casting_dir, "default.json")
            if os.path.isfile(dflt):
                with open(dflt) as f:
                    casting = json.load(f)
                for ch, path in casting.items():
                    if (ch.isdigit() and int(ch) in self.instances
                            and os.path.isfile(path)):
                        eff[int(ch)] = path
        cand = os.path.join(self.casting_dir, name + ".json")
        if os.path.isfile(cand):
            with open(cand) as f:
                casting = json.load(f)
            for part in self.parts:
                path = casting.get(str(part["channels"][0]))
                if path and os.path.isfile(path):
                    for ch in part["channels"]:
                        eff[ch] = path
        return eff

    def request_patch(self, part_idx, path):
        self.pending.put(("part", part_idx, path))

    def request_patch_ch(self, ch, path):
        self.pending.put(("ch", ch, path))

    def _apply_pending(self):
        try:
            while True:
                kind, key, path = self.pending.get_nowait()
                if kind == "ch":
                    chans = [key] if key in self.instances else []
                elif key >= len(self.parts):
                    continue
                else:
                    chans = self.parts[key]["channels"]
                for ch in chans:
                    s = self.instances[ch]
                    s.allNotesOff()
                    if path != "(init)":
                        s.loadPatch(path)
                    self.ch_patch[ch] = (os.path.basename(path)[:-4]
                                         if path != "(init)" else "(init)")
                    self.ch_patch_path[ch] = path
        except queue.Empty:
            pass

    def render(self, frames):
        """Mix `frames` samples (multiple of BLOCK) of the looping piece.

        Batched via createMultiBlock/processMultiBlock — the same approach
        as audition.py (and the bcrsim rigs): per chunk, each instance gets
        a handful of processMultiBlock calls split at event boundaries,
        instead of a Python->C++ crossing per 32-sample block.
        """
        out = np.zeros((2, frames), dtype=np.float32)
        if not self.playing:
            return out
        self._apply_pending()
        done = 0
        while done < frames:
            if self.jump is not None:
                self.load_piece(self.jump)
                self.jump = None
                self._apply_pending()
            if self.sample >= self.loop_len:  # piece over: next in the album
                self.load_piece(self.piece_i + 1)
                self._apply_pending()
            while (self.tempo_i < len(self.tempo_evs)
                   and self.tempo_evs[self.tempo_i][0] <= self.sample):
                bpm = self.tempo_evs[self.tempo_i][1]
                for s in self.instances.values():
                    if hasattr(s, "setTempo"):
                        s.setTempo(bpm)
                self.tempo_i += 1
            span = min(frames - done, self.loop_len - self.sample)
            # stop at the next tempo change so it lands on its block, not
            # at the start of the next broadcast chunk
            if self.tempo_i < len(self.tempo_evs):
                nxt = self.tempo_evs[self.tempo_i][0] - self.sample
                if 0 < nxt < span:
                    span = -(-nxt // BLOCK) * BLOCK
            span -= span % BLOCK
            nblocks = span // BLOCK
            for ch in self.events:
                s = self.instances[ch]
                buf = self.bufs[ch]
                evs, i = self.events[ch], self.pos[ch]
                cursor = 0  # blocks rendered so far within this span
                while cursor < nblocks:
                    # next event inside the span, quantised to its block
                    if i < len(evs) and evs[i][0] < self.sample + span:
                        evb = min(nblocks,
                                  max(cursor, (evs[i][0] - self.sample) // BLOCK))
                    else:
                        evb = nblocks
                    if evb > cursor:
                        s.processMultiBlock(buf, cursor, evb - cursor)
                        cursor = evb
                    while (i < len(evs)
                           and (evs[i][0] - self.sample) // BLOCK <= cursor
                           and evs[i][0] < self.sample + span):
                        _, kind, a, b = evs[i]
                        if kind == 0:
                            s.pitchBend(0, a - 8192)
                        elif kind == 1:
                            s.playNote(0, a, b, 0)
                        else:
                            s.releaseNote(0, a, 0)
                        i += 1
                self.pos[ch] = i
                if not self.ch_mute[ch]:
                    out[:, done:done + span] += \
                        buf.reshape(2, -1)[:, :span] * self.ch_gain[ch]
            self.sample += span
            done += span
        return out

    def subscribe(self, prefill=False):
        q = queue.Queue(maxsize=256)
        with self.sublock:
            if prefill:
                for d in self.history:
                    q.put_nowait(d)
                # exact timeline anchor for this listener: the engine
                # position NOW, and how much banked history precedes it
                # in the stream — the subtitle pane needs both. (A
                # simultaneous second listener would overwrite this;
                # the ms-scale race is accepted for a one-user board.)
                self.last_anchor = {
                    "pieceIndex": self.piece_i,
                    "position": self.sample / self.sr,
                    "historyS": len(self.history) * CHUNK_FRAMES / self.sr,
                    "length": self.loop_len / self.sr,
                }
            self.subscribers.add(q)
        return q

    def unsubscribe(self, q):
        with self.sublock:
            self.subscribers.discard(q)

    def broadcaster(self):
        """Wall-clock-paced render loop feeding every /pcm listener."""
        period = CHUNK_FRAMES / self.sr
        deadline = time.monotonic()
        limiter_gain = 1.0
        applied = 1.0
        while True:
            mix = self.render(CHUNK_FRAMES)
            # peak meter (pre-limiter) + a simple riding limiter: browsers
            # hard-clip anything past 0 dBFS, which sounds like ring mod.
            peak = float(np.max(np.abs(mix))) if mix.size else 0.0
            self.peak = max(peak, getattr(self, "peak", 0.0) * 0.94)
            target = min(1.0, 0.89 / peak) if peak > 0.89 else 1.0
            # fast attack, slow release
            limiter_gain = min(target, limiter_gain * 1.02)
            g = min(limiter_gain, target)
            if g < 1.0 or applied < 1.0:
                # ramp across the chunk: a per-chunk gain STEP is itself an
                # audible discontinuity when peaks hover near the ceiling
                mix = mix * np.linspace(applied, g, mix.shape[1],
                                        dtype=np.float32)[None, :]
            applied = g
            data = mix.T.reshape(-1).astype("<f4").tobytes()  # interleaved
            with self.sublock:
                self.history.append(data)
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

    def whys_at(self, t):
        """Rule citations for everything sounding at loop-time t."""
        out = []
        for on, off, ch, ws in self.why_index:
            if on > t:
                break
            if t < off:
                for w in ws:
                    out.append({"ch": ch, "why": w})
        return out[:16]

    def state(self):
        return {
            "playing": self.playing,
            "position": self.sample / self.sr,
            "length": self.loop_len / self.sr,
            "sr": self.sr,
            "peak": round(getattr(self, "peak", 0.0), 3),
            "listeners": len(self.subscribers),
            "piece": self.piece_name,
            "pieceIndex": self.piece_i,
            "pieceCount": len(self.playlist),
            "parts": [
                {"name": p["name"], "channels": p["channels"],
                 "gain": self.ch_gain[p["channels"][0]],
                 "mute": self.ch_mute[p["channels"][0]],
                 "patch": self.ch_patch[p["channels"][0]],
                 "patchPath": self.ch_patch_path[p["channels"][0]]}
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
 <button id=listenr class=big onclick=listenRemote()>▶ listen</button>
 <button id=listen onclick=listen()>live (LAN)</button>
 <button id=play onclick=toggle()>pause</button>
 <span class=pos id=pos></span>
 <span class=pos id=astat></span>
</div>
<div class=row style="margin-top:.4rem">
 <button onclick=pieceStep(-1)>⏮</button>
 <select id=piecesel onchange=setPiece(this.selectedIndex)
   style="flex:1;min-width:10rem"></select>
 <button onclick=pieceStep(1)>⏭</button>
</div>
<div id=parts></div>
<div id=whys style="min-height:5.5em;margin:8px 0;padding:6px 8px;
  font-size:12px;line-height:1.45;opacity:.85;border-left:3px solid #888;
  font-family:monospace;white-space:pre-wrap"></div>
<h3>casting (paste into render_showcase / audition.py)</h3>
<div id=cast></div>
<script>
let CATS={}, STATE=null, audioOn=false;

const WORKLET = `
class Player extends AudioWorkletProcessor {
  constructor(opts){
    super();
    this.ratio = opts.processorOptions.ratio; // streamRate / ctxRate
    this.chunks = []; this.read = 0.0; this.total = 0; this.base = 0;
    this.underruns = 0; this.dropped = 0;
    this.port.onmessage = e => {
      if (this.chunks.length >= 96){ // overflow: drop oldest, stay coherent
        this.base += this.chunks[0].length/2;
        this.chunks.shift(); this.dropped++;
        if (this.read < this.base) this.read = this.base;
      }
      this.chunks.push(e.data); this.total += e.data.length/2;
    };
    this.stat = 0;
  }
  sampleAt(i){ // absolute frame index across the chunk FIFO
    let rel = i - this.base;
    for (const c of this.chunks){
      const n = c.length/2;
      if (rel < n) return [c[rel*2], c[rel*2+1]];
      rel -= n;
    }
    return null;
  }
  gc(){
    while (this.chunks.length && this.read - this.base >= this.chunks[0].length/2){
      this.base += this.chunks[0].length/2; this.chunks.shift();
    }
  }
  process(_, outputs){
    const out = outputs[0], need = out[0].length;
    for (let i=0;i<need;i++){
      const i0 = Math.floor(this.read), frac = this.read - i0;
      const a = this.sampleAt(i0), b = this.sampleAt(i0+1);
      if (!a){ out[0][i]=0; out[1][i]=0; this.underruns++; continue; }
      const bb = b || a;
      out[0][i] = a[0]+(bb[0]-a[0])*frac;
      out[1][i] = a[1]+(bb[1]-a[1])*frac;
      this.read += this.ratio;
    }
    this.gc();
    if (++this.stat >= 300){ this.stat = 0;
      this.port.postMessage({underruns:this.underruns, dropped:this.dropped,
        buffered: this.total - Math.floor(this.read - this.base) -
                  (this.base ? 0 : 0)});
    }
    return true;
  }
}
registerProcessor('player', Player);`;

const START_S = 1.5; // browsers cap paused-media readahead (~2.3 s
// observed), so gating playback on a big pre-buffer DEADLOCKS. Start
// early instead: while PLAYING the browser buffers aggressively, and the
// server's 5 s history burst builds the real cushion during playback.
function listenRemote(){
  // Opus 256k over ffmpeg (~32 kB/s instead of 384): transparent for
  // music, and the <audio> element buffers. Start once START_S seconds
  // are banked, then let the history burst build a larger cushion.
  if (audioOn) return;
  audioOn = true;
  const a = new Audio('opus');
  a.preload = 'auto';
  // the server records an exact anchor (engine position + true banked
  // history) when it assembles this listener's burst; fetch it so the
  // why-subtitles track OUR ears, not the engine's now
  OPUS = { el: a, anchor: null };
  fetch('anchor').then(r=>r.json()).then(x=>{ if (!x.err) OPUS.anchor = x; });
  const b = document.getElementById('listenr');
  b.classList.add('on');
  const stat = s => { b.textContent = s; };
  stat('buffering…');
  let started = false;
  const ahead = () => {
    try { return a.buffered.length ? a.buffered.end(a.buffered.length-1) - a.currentTime : 0; }
    catch(e){ return 0; }
  };
  setInterval(() => {
    const buf = ahead();
    if (!started || a.paused){
      if (buf >= START_S){ a.play(); started = true; }
      stat('buffering ' + buf.toFixed(1) + '/' + START_S + 's');
    } else {
      stat('● listening (' + buf.toFixed(1) + 's banked)');
    }
  }, 500);
  a.addEventListener('waiting', () => stat('stall — refilling'));
}

async function listen(){
  if (audioOn) return;
  audioOn = true;
  document.getElementById('listen').classList.add('on');
  const ctx = new AudioContext({sampleRate: STATE ? STATE.sr : 48000});
  const srStream = STATE ? STATE.sr : 48000;
  const ratio = srStream / ctx.sampleRate;
  document.getElementById('listen').textContent =
    '● live @'+ctx.sampleRate+(ratio!=1?' (resampling '+srStream+'→)':'');
  const url = URL.createObjectURL(new Blob([WORKLET],{type:'text/javascript'}));
  await ctx.audioWorklet.addModule(url);
  const node = new AudioWorkletNode(ctx, 'player',
    {outputChannelCount:[2], processorOptions:{ratio}});
  node.port.onmessage = e => {
    document.getElementById('astat').textContent =
      'underruns '+e.data.underruns+' · drops '+e.data.dropped;
  };
  node.connect(ctx.destination);
  const resp = await fetch('pcm');
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

let PIECES=[];
async function init(){
  CATS = await (await fetch('patches')).json();
  PIECES = await (await fetch('pieces')).json();
  const ps = document.getElementById('piecesel');
  ps.innerHTML = PIECES.map((n,i)=>`<option>${n}</option>`).join('');
  await refresh(); setInterval(refresh, 1000);
  setInterval(refreshWhys, 400); // the interpretation, explaining itself
}
function setPiece(i){ post('piece',{index:i}); }
let lastWhys = '';
let OPUS = null;
async function refreshWhys(){
  if (!STATE || !STATE.playing) return;
  // estimate the current position between 1 Hz state polls
  let at = STATE.position + (Date.now() - (STATE._t||Date.now()))/1000;
  if (OPUS && OPUS.anchor && !OPUS.el.paused && OPUS.el.currentTime > 0){
    const an = OPUS.anchor;
    at = an.position - an.historyS + OPUS.el.currentTime;
    // transitions are gapless: when our ears cross the anchored piece's
    // end, bake its length into the anchor and advance the piece index
    // (length of intermediate pieces is only known once the engine's
    // state agrees, so subtitles stay hidden until the indices match)
    while (at >= an.length && an.pieceIndex < (STATE.pieceIndex|0)){
      an.position -= an.length;
      an.pieceIndex += 1;
      an.length = (an.pieceIndex === (STATE.pieceIndex|0))
        ? STATE.length : an.length;
      at = an.position - an.historyS + OPUS.el.currentTime;
    }
    if (an.pieceIndex !== (STATE.pieceIndex|0)){
      const d = document.getElementById('whys');
      if (d.textContent) { d.textContent = ''; lastWhys = ''; }
      return;
    }
    if (STATE.length > 0) at = Math.max(0, Math.min(at, STATE.length));
  }
  const ws = await (await fetch('whys?at='+at.toFixed(2))).json();
  const seen = new Set();
  const lines = [];
  for (const w of ws){
    const key = w.why.split(':')[0] + w.ch;
    if (seen.has(key)) continue;
    seen.add(key);
    lines.push('ch'+w.ch+'  '+w.why);
    if (lines.length >= 7) break;
  }
  const txt = lines.join('\n');
  if (txt !== lastWhys){
    lastWhys = txt;
    document.getElementById('whys').textContent = txt;
  }
}
function pieceStep(d){
  if (!STATE) return;
  setPiece((STATE.pieceIndex + d + PIECES.length) % PIECES.length);
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
  STATE = await (await fetch('state')).json();
  STATE._t = Date.now();
  document.getElementById('play').textContent = STATE.playing?'pause':'play';
  document.getElementById('pos').textContent =
    STATE.piece+' ('+(STATE.pieceIndex+1)+'/'+STATE.pieceCount+') · '+
    STATE.position.toFixed(1)+' / '+STATE.length.toFixed(1)+'s · '+
    STATE.listeners+' listening · peak '+STATE.peak+
    (STATE.peak>0.89?' ⚠CLIP-LIMITED':'');
  const ps = document.getElementById('piecesel');
  if (ps.selectedIndex != STATE.pieceIndex) ps.selectedIndex = STATE.pieceIndex;
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
        <input type=range min=0 max=1.5 step=0.05 value=${p.gain}
          onchange=gain(${i},this.value)>
      </div></div>`).join('');
  }
  STATE.parts.forEach((p,i)=>{
    document.getElementById('pn'+i).textContent = p.patch;
    document.getElementById('mute'+i).className = p.mute?'on':'';
    // keep the select showing what's actually loaded (piece changes and
    // casting preloads happen server-side), but not while it has focus
    const s = document.getElementById('sel'+i);
    if (s && document.activeElement !== s && s.value !== p.patchPath)
      s.value = p.patchPath;
  });
  document.getElementById('cast').textContent = STATE.parts.map((p,i)=>
    p.patch=='(init)'?'':p.channels.map(c=>
      `--patch-ch "${c}:${sel(i)}"`).join(' ')).filter(x=>x).join(' \\\\\\n');
}
function sel(i){ return document.getElementById('sel'+i).value; }
async function post(url, body){
  await fetch(url,{method:'POST',body:JSON.stringify(body)}); refresh();
}
function setPatch(i,path){ post('patch',{part:i,path}); }
function step(i,d){
  const s = document.getElementById('sel'+i);
  s.selectedIndex = Math.max(0, Math.min(s.length-1, s.selectedIndex+d));
  setPatch(i, s.value);
}
function mute(i){ post('mute',{part:i}); }
function gain(i,v){ post('gain',{part:i,gain:parseFloat(v)}); }
function toggle(){ post('toggle',{}); }
init();
</script>"""


MAX_BODY = 4096  # a control POST is a few dozen bytes


def serve(engine, cats, host, port):
    # Only patches the scanner found may be loaded: Surge's native loader
    # gets a path, and the browser must not be able to point it anywhere.
    allowed = {path for entries in cats.values() for _, path in entries}
    allowed.add("(init)")
    ffmpeg_path = shutil.which("ffmpeg")
    opus_slots = threading.BoundedSemaphore(MAX_OPUS_CLIENTS)

    class H(BaseHTTPRequestHandler):
        protocol_version = "HTTP/1.1"

        def log_message(self, *a):
            pass

        def _same_origin(self):
            """Reject cross-site requests (a web page CSRF-ing localhost).

            Browsers always send Origin on POST; require it to match the
            Host we were reached at. Non-browser clients (curl) send no
            Origin and are let through — they can already reach the port.
            """
            origin = self.headers.get("Origin")
            if origin is None:
                return True
            host_hdr = self.headers.get("Host", "")
            return origin.rstrip("/") in (
                f"http://{host_hdr}", f"https://{host_hdr}")

        def _stream_allowed(self):
            return (self._same_origin()
                    and self.headers.get("Sec-Fetch-Site") != "cross-site")

        def _body(self):
            n = int(self.headers.get("Content-Length", 0))
            if n < 0 or n > MAX_BODY:
                raise ValueError("body too large")
            body = json.loads(self.rfile.read(n) or b"{}")
            if not isinstance(body, dict):
                raise ValueError("body must be an object")
            return body

        def _part(self, body):
            pi = body.get("part")
            if not isinstance(pi, int) or not 0 <= pi < len(engine.parts):
                raise ValueError("bad part index")
            return pi

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
            elif self.path == "/anchor":
                self._json(getattr(engine, "last_anchor", None)
                           or {"err": "no stream"})
            elif self.path.startswith("/whys"):
                try:
                    at = float((self.path.split("=", 1) + ["0"])[1])
                except ValueError:
                    at = 0.0
                self._json(engine.whys_at(at))
            elif self.path == "/patches":
                self._json(cats)
            elif self.path == "/pieces":
                self._json([n for n, _ in engine.playlist])
            elif self.path == "/opus":
                # remote path: Opus at 256 kbps through ffmpeg, streamed
                # as Ogg — the <audio> element's own jitter buffering
                # absorbs WAN stalls that starve the raw PCM path
                if not self._stream_allowed():
                    self._json({"err": "cross-origin stream refused"}, 403)
                    return
                if ffmpeg_path is None:
                    self._json({"err": "remote audio needs ffmpeg"}, 503)
                    return
                if not opus_slots.acquire(blocking=False):
                    self._json({"err": "remote listener limit reached"}, 503)
                    return

                proc = None
                q = None
                feeder = None
                stop = threading.Event()
                try:
                    try:
                        proc = subprocess.Popen(
                            [ffmpeg_path, "-loglevel", "quiet",
                             "-f", "f32le", "-ar", str(engine.sr),
                             "-ac", "2", "-i", "pipe:0", "-c:a",
                             "libopus", "-b:a", "256k", "-application",
                             "audio", "-frame_duration", "60", "-f", "ogg",
                             "-page_duration", "100000",
                             "-flush_packets", "1", "pipe:1"],
                            stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                            stderr=subprocess.DEVNULL)
                    except OSError:
                        self._json({"err": "ffmpeg failed to start"}, 503)
                        return

                    q = engine.subscribe(prefill=True)
                    self.send_response(200)
                    self.send_header("Content-Type", "audio/ogg")
                    self.send_header("Cache-Control", "no-store")
                    self.send_header("Transfer-Encoding", "chunked")
                    self.end_headers()

                    def feed():
                        try:
                            while not stop.is_set():
                                try:
                                    data = q.get(timeout=0.25)
                                except queue.Empty:
                                    continue
                                if stop.is_set():
                                    break
                                proc.stdin.write(data)
                                proc.stdin.flush()
                        except (BrokenPipeError, OSError, ValueError):
                            pass

                    feeder = threading.Thread(
                        target=feed, name="opus-feed", daemon=True)
                    feeder.start()
                    fd = proc.stdout.fileno()
                    while True:
                        # os.read returns whatever is available — a blocking
                        # read(4096) would sit ~190 ms waiting to fill at
                        # opus bitrates, adding lump latency on top of pages
                        data = os.read(fd, 65536)
                        if not data:
                            break
                        self.wfile.write(
                            f"{len(data):X}\r\n".encode() + data + b"\r\n")
                        self.wfile.flush()
                except (BrokenPipeError, ConnectionResetError, OSError):
                    pass
                finally:
                    self.close_connection = True
                    stop.set()
                    try:
                        if q is not None:
                            engine.unsubscribe(q)
                        if proc is not None:
                            if proc.poll() is None:
                                proc.terminate()
                            try:
                                proc.wait(timeout=1)
                            except subprocess.TimeoutExpired:
                                proc.kill()
                                proc.wait()
                        if feeder is not None:
                            feeder.join(timeout=1)
                        if proc is not None:
                            for stream in (proc.stdin, proc.stdout):
                                if stream is not None:
                                    try:
                                        stream.close()
                                    except (BrokenPipeError, OSError, ValueError):
                                        pass
                    finally:
                        opus_slots.release()
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
            if not self._same_origin():
                self._json({"err": "cross-origin request refused"}, 403)
                return
            try:
                body = self._body()
                if self.path == "/patch":
                    pi = self._part(body)
                    path = body.get("path")
                    if path not in allowed:
                        raise ValueError("patch not in library")
                    engine.request_patch(pi, path)
                elif self.path == "/mute":
                    chans = engine.parts[self._part(body)]["channels"]
                    v = not engine.ch_mute[chans[0]]
                    for ch in chans:
                        engine.ch_mute[ch] = v
                elif self.path == "/gain":
                    g = body.get("gain")
                    if not isinstance(g, (int, float)) or not 0 <= g <= 4:
                        raise ValueError("gain must be in [0, 4]")
                    for ch in engine.parts[self._part(body)]["channels"]:
                        engine.ch_gain[ch] = float(g)
                elif self.path == "/piece":
                    idx = body.get("index")
                    if (not isinstance(idx, int)
                            or not 0 <= idx < len(engine.playlist)):
                        raise ValueError("bad piece index")
                    engine.jump = idx
                elif self.path == "/toggle":
                    engine.playing = not engine.playing
                else:
                    self._json({"err": "not found"}, 404)
                    return
            except (ValueError, TypeError) as e:
                self._json({"err": str(e)}, 400)
                return
            self._json({"ok": True})

    srv = ThreadingHTTPServer((host, port), H)
    threading.Thread(target=srv.serve_forever, daemon=True).start()
    return srv


def main():
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("perf", nargs="+",
                    help="PerformanceIR JSON file(s) or a directory of them; "
                         "several = an album played in order")
    ap.add_argument("--scl")
    ap.add_argument("--sr", type=int, default=48000)
    ap.add_argument("--port", type=int, default=8766)
    ap.add_argument("--host", default="127.0.0.1",
                    help="bind address (default loopback; 0.0.0.0 exposes "
                         "the unauthenticated control page to the LAN)")
    ap.add_argument("--local", action="store_true",
                    help="also play on the local audio device (sounddevice)")
    ap.add_argument("--calibration", metavar="CAL.json",
                    default=None,
                    help="patch-envelope calibration; note ends are "
                         "compensated for each channel's release tail "
                         "(applied at piece load)")
    ap.add_argument("--casting", metavar="DIR",
                    help="directory of <piece>.json channel->patch maps "
                         "(default: config/casting/)")
    args = ap.parse_args()
    if np is None or surgepy is None:
        sys.exit("patchboard needs numpy and surgepy (see README: macOS)")

    # expand dirs, order like the book: prelude then fugue per key number
    paths = []
    for p in args.perf:
        if os.path.isdir(p):
            paths.extend(os.path.join(p, f) for f in os.listdir(p)
                         if f.endswith(".json"))
        else:
            paths.append(p)

    def album_key(path):
        b = os.path.splitext(os.path.basename(path))[0]
        if len(b) == 7 and b.startswith("wtc") and b[4] in "pf":
            return (0, int(b[3]), int(b[5:7]), 0 if b[4] == "p" else 1, "")
        return (1, 0, 0, 0, b)

    paths.sort(key=album_key)
    playlist = []
    for p in paths:
        with open(p) as f:
            perf = json.load(f)
        name = perf.get("piece") or os.path.splitext(os.path.basename(p))[0]
        playlist.append((name, perf))
    if not playlist:
        sys.exit("no performances found")

    cats = scan_patches()
    if not cats:
        sys.exit("no Surge patch library found (patches_factory)")
    casting_dir = args.casting or os.path.join(
        os.path.dirname(os.path.abspath(__file__)), "..", "config", "casting")
    calibration = None
    if args.calibration and os.path.isfile(args.calibration):
        with open(args.calibration) as f:
            calibration = json.load(f)
    engine = Engine(playlist, args.sr, args.scl, casting_dir=casting_dir,
                    calibration=calibration)
    print(f"album: {len(playlist)} pieces, starting {playlist[0][0]}",
          flush=True)
    if shutil.which("ffmpeg") is None:
        print("WARN ffmpeg not found: remote Opus listening is disabled",
              file=sys.stderr, flush=True)

    serve(engine, cats, args.host, args.port)
    print(f"patchboard: http://{args.host}:{args.port}/  "
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
