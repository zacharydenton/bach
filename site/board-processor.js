/*
 * The static patchboard's AudioWorkletProcessor: one Surge wasm module,
 * one scene per LANE — each SurgeWasm instance carries up to two lanes
 * OF THE SAME SLOT as scene A / scene B in MIDI channel-split mode
 * (bridge loadScenePatches, same preset on both scenes). Every lane
 * keeps its own voice pool, so mono presets stay mono per lane and
 * overlapping same-pitch notes in different lanes cannot release each
 * other — while the instance's ordinary output is its preset's FULL
 * native signal path (scenes, inserts, sends, globals; most factory
 * presets keep their space in send/global slots). Slot gain and mute
 * apply per instance at the mix. Lane -> (instance, scene) comes with
 * the score (routing.js lanePlacement).
 *
 * Scheduling matches tools/patchboard.py render(): the walk is in 32-frame
 * engine blocks and every event lands at the boundary of the block that
 * contains it (≤1 ms quantization). Pause simply stops walking — synth
 * state freezes and resume continues mid-note, like the live engine.
 * Seeks chase the score: notes that should already be sounding at the
 * target are re-struck (routing.js heldAt).
 */

// Must come first: installs globals the Emscripten runtime reads at load time.
import "./engine/worklet-shim.js";
import createSurgeModule from "./engine/surge-worklet.js";
import { heldAt } from "./routing.js";

const BLOCK = 32;
const N_SLOTS = 4;

class Board extends AudioWorkletProcessor {
  constructor(options) {
    super();
    const { wasmBinary, nInstances } = options.processorOptions;

    this.ready = false;
    this.queued = [];
    this.playing = false;
    this.ended = false;
    this.cursor = 0;
    this.ev = null;
    this.evI = 0;
    this.tempoI = 0;
    this.gain = new Array(N_SLOTS).fill(0.25); // per SLOT (live default)
    this.mute = new Array(N_SLOTS).fill(false);
    this.laneInst = []; // per lane: its instance
    this.laneScene = []; // per lane: scene A or B on that instance
    this.instSlot = []; // per instance: the slot whose gain/mute it wears
    this.views = []; // per instance: {l, r} into its output scratch
    // riding limiter state (patchboard._limit, rates rescaled from its
    // 4096-frame chunk to the 128-frame quantum: 0.94^(128/4096),
    // 1.02^(128/4096))
    this.peak = 0;
    this.limGain = 1;
    this.limApplied = 1;
    this.posDiv = 0;
    this.renderMs = 0;
    this.renderN = 0;

    this.port.onmessage = (e) => {
      if (this.ready) this.handle(e.data);
      else this.queued.push(e.data);
    };

    createSurgeModule({ wasmBinary, locateFile: (p) => p })
      .then((mod) => {
        this.mod = mod;
        this.synths = [];
        this.ptrs = [];
        for (let i = 0; i < nInstances; i++) {
          const s = new mod.SurgeWasm(sampleRate, 128);
          if (!s.ok()) throw new Error(s.initError());
          this.synths.push(s);
          this.ptrs.push(s.outputPtr());
        }
        this.maxFrames = this.synths[0].maxFrames();
        this.ready = true;
        for (const m of this.queued) this.handle(m);
        this.queued = [];
        this.port.postMessage({ type: "ready", sampleRate, n: this.synths.length });
      })
      .catch((err) =>
        this.port.postMessage({ type: "error", message: String(err) }),
      );
  }

  allOff() {
    for (const s of this.synths) s.allNotesOff();
  }

  handle(msg) {
    switch (msg.type) {
      case "score":
        this.allOff();
        this.ev = msg;
        this.laneInst = msg.laneInst;
        this.laneScene = msg.laneScene;
        this.instSlot = msg.instSlot;
        this.cursor = 0;
        this.evI = 0;
        this.tempoI = 0;
        this.ended = false;
        break;
      case "patches": {
        // both voices of one instance, as a pair — the scene merge is a
        // whole-patch operation (see the bridge)
        const s = this.synths[msg.inst];
        if (s) {
          s.allNotesOff();
          if (!s.loadScenePatches(msg.bytesA, msg.nameA || "",
                                  msg.bytesB, msg.nameB || ""))
            this.port.postMessage({
              type: "error",
              message: `scene patch load failed on instance ${msg.inst}`,
            });
        }
        break;
      }
      case "scl": {
        let err = "";
        for (const s of this.synths) err = s.loadSCLString(msg.text) || err;
        this.port.postMessage({ type: "scl", err });
        break;
      }
      case "play":
        this.playing = true;
        break;
      case "pause":
        this.playing = false;
        break;
      case "seek":
        this.seek(msg.frame);
        break;
      case "gain":
        if (msg.ch < N_SLOTS) this.gain[msg.ch] = msg.v;
        break;
      case "mute":
        if (msg.ch < N_SLOTS) this.mute[msg.ch] = msg.v;
        break;
    }
  }

  seek(frame) {
    if (!this.ev) return;
    this.allOff();
    frame = Math.max(0, Math.min(frame - (frame % BLOCK), this.ev.loopFrames));
    this.cursor = frame;
    // re-arm the event walk; notes already sounding across the seek point
    // are not re-struck (same as the live board's piece jump)
    const f = this.ev.frames;
    let lo = 0;
    let hi = f.length;
    while (lo < hi) {
      const mid = (lo + hi) >> 1;
      if (f[mid] < frame) lo = mid + 1;
      else hi = mid;
    }
    this.evI = lo;
    const tf = this.ev.tempoFrames;
    let ti = 0;
    while (ti < tf.length && tf[ti] <= frame) ti++;
    this.tempoI = ti;
    if (ti > 0) {
      const bpm = this.ev.tempoBpm[ti - 1];
      for (const s of this.synths) s.setTempo(bpm);
    }
    // chase: re-strike what should already be sounding at the target
    // (their note-offs all lie at or beyond evI, so they end naturally)
    for (const h of heldAt(this.ev, frame)) {
      const s = this.synths[this.laneInst[h.lane]];
      if (s) s.noteOn(this.laneScene[h.lane], h.key, h.vel);
    }
    this.ended = false;
    this.postPos();
  }

  refreshViews() {
    const heap = this.mod.HEAPF32;
    for (let i = 0; i < this.synths.length; i++) {
      if (!this.views[i] || this.views[i].l.buffer !== heap.buffer) {
        const base = this.ptrs[i]; // planar [L * maxFrames][R * maxFrames]
        this.views[i] = {
          l: new Float32Array(heap.buffer, base, BLOCK),
          r: new Float32Array(heap.buffer, base + this.maxFrames * 4, BLOCK),
        };
      }
    }
  }

  postPos() {
    this.port.postMessage({
      type: "pos",
      frame: this.cursor,
      playing: this.playing && !this.ended,
      peak: this.peak,
      riding: this.limApplied < 1,
      // wall ms spent rendering per second of audio, Date-coarse but honest
      load: this.renderN
        ? this.renderMs / ((this.renderN * 128) / sampleRate) / 1000
        : 0,
    });
  }

  process(_inputs, outputs) {
    const out = outputs[0];
    if (!out || !out.length) return true;
    const L = out[0];
    const R = out.length > 1 ? out[1] : out[0];
    L.fill(0);
    if (R !== L) R.fill(0);

    if (this.ready && this.playing && this.ev && !this.ended) {
      const t0 = Date.now();
      const { frames, kinds, chans, a, b, tempoFrames, tempoBpm } = this.ev;

      for (let done = 0; done < L.length; done += BLOCK) {
        const blockEnd = this.cursor + BLOCK;

        while (this.evI < frames.length && frames[this.evI] < blockEnd) {
          const i = this.evI++;
          const lane = chans[i];
          const s = this.synths[this.laneInst[lane]];
          const scene = this.laneScene[lane]; // MIDI channel selects the scene
          if (!s) continue;
          if (kinds[i] === 1) s.noteOn(scene, a[i], b[i]);
          else if (kinds[i] === 2) s.noteOff(scene, a[i], 0);
          else s.pitchBend(scene, a[i] - 8192);
        }
        while (
          this.tempoI < tempoFrames.length &&
          tempoFrames[this.tempoI] < blockEnd
        ) {
          const bpm = tempoBpm[this.tempoI++];
          for (const s of this.synths) s.setTempo(bpm);
        }

        // muted instances still render (their voices keep evolving, like
        // the live board, whose mute is applied at the mix) — just not sum.
        // Only the instances this piece placed lanes on render at all.
        const active = this.instSlot.length;
        for (let i = 0; i < active; i++) this.synths[i].render(BLOCK);
        this.refreshViews();
        for (let inst = 0; inst < active; inst++) {
          const slot = this.instSlot[inst];
          if (this.mute[slot]) continue;
          const g = this.gain[slot];
          if (g <= 0) continue;
          const { l, r } = this.views[inst];
          for (let i = 0; i < BLOCK; i++) {
            L[done + i] += l[i] * g;
            R[done + i] += r[i] * g;
          }
        }

        this.cursor = blockEnd;
        if (this.cursor >= this.ev.loopFrames) {
          this.ended = true;
          this.port.postMessage({ type: "ended" });
          break;
        }
      }

      this.limit(L, R);
      this.renderMs += Date.now() - t0;
      this.renderN++;
    }

    // ~4 Hz position reports (375 quanta/s at 48 k)
    if (++this.posDiv >= 90) {
      this.posDiv = 0;
      this.postPos();
      this.renderMs = 0;
      this.renderN = 0;
    }
    return true;
  }

  // Peak meter + riding limiter (patchboard._limit): browsers hard-clip past
  // 0 dBFS, which sounds like ring mod. Instant attack; the release ramps
  // across the quantum so gain steps never land as discontinuities.
  limit(L, R) {
    let peak = 0;
    for (let i = 0; i < L.length; i++) {
      const l = Math.abs(L[i]);
      const r = Math.abs(R[i]);
      if (l > peak) peak = l;
      if (r > peak) peak = r;
    }
    this.peak = Math.max(peak, this.peak * 0.998); // 0.94^(128/4096)
    const target = peak > 0.89 ? Math.min(1, 0.89 / peak) : 1;
    this.limGain = Math.min(target, this.limGain * 1.00062); // 1.02^(128/4096)
    const g = Math.min(this.limGain, target);
    let applied = this.limApplied;
    if (g < applied) applied = g; // attack must not ramp
    if (g < 1 || applied < 1) {
      const n = L.length;
      for (let i = 0; i < n; i++) {
        const w = applied + ((g - applied) * i) / n;
        L[i] *= w;
        R[i] *= w;
      }
    }
    this.limApplied = g;
  }
}

registerProcessor("board", Board);
