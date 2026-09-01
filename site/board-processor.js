/*
 * The static patchboard's AudioWorkletProcessor: one Surge wasm module,
 * TWO SurgeWasm instances carrying the four voices — each instance holds
 * two voices as scene A / scene B in MIDI channel-split mode (bridge
 * loadScenePatches), and renderScenes exposes each scene's post-insert-FX
 * stereo separately, so per-voice gain and mute still happen here at the
 * mix, exactly as when every voice had its own instance.
 *
 * Slot s (0=bass 1=tenor 2=alto 3=soprano) lives on instance s>>1 as
 * scene s&1; scene routing is by MIDI channel (0 -> A, 1+ -> B).
 *
 * Scheduling matches tools/patchboard.py render(): the walk is in 32-frame
 * engine blocks and every event lands at the boundary of the block that
 * contains it (≤1 ms quantization). Pause simply stops walking — synth
 * state freezes and resume continues mid-note, like the live engine.
 */

// Must come first: installs globals the Emscripten runtime reads at load time.
import "./engine/worklet-shim.js";
import createSurgeModule from "./engine/surge-worklet.js";

const BLOCK = 32;
const N_SLOTS = 4;

class Board extends AudioWorkletProcessor {
  constructor(options) {
    super();
    const { wasmBinary } = options.processorOptions;

    this.ready = false;
    this.queued = [];
    this.playing = false;
    this.ended = false;
    this.cursor = 0;
    this.ev = null;
    this.evI = 0;
    this.tempoI = 0;
    this.gain = new Array(N_SLOTS).fill(0.25); // summing headroom (live default)
    this.mute = new Array(N_SLOTS).fill(false);
    this.views = []; // per slot: {l, r} into its instance's scene lanes
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
        this.scenePtrs = [];
        for (let i = 0; i < N_SLOTS / 2; i++) {
          const s = new mod.SurgeWasm(sampleRate, 128);
          if (!s.ok()) throw new Error(s.initError());
          this.synths.push(s);
          this.scenePtrs.push(s.sceneOutputPtr());
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
    this.ended = false;
    this.postPos();
  }

  refreshViews() {
    const heap = this.mod.HEAPF32;
    for (let slot = 0; slot < N_SLOTS; slot++) {
      if (!this.views[slot] || this.views[slot].l.buffer !== heap.buffer) {
        const base = this.scenePtrs[slot >> 1];
        const lane = (slot & 1) * 2; // [A_L][A_R][B_L][B_R]
        this.views[slot] = {
          l: new Float32Array(heap.buffer, base + lane * this.maxFrames * 4, BLOCK),
          r: new Float32Array(heap.buffer, base + (lane + 1) * this.maxFrames * 4, BLOCK),
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
          const s = this.synths[chans[i] >> 1];
          const scene = chans[i] & 1; // MIDI channel selects the scene
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

        // muted voices still render (their scenes keep evolving, like the
        // live board, whose mute is applied at the mix) — they just don't sum
        for (const s of this.synths) s.renderScenes(BLOCK);
        this.refreshViews();
        for (let slot = 0; slot < N_SLOTS; slot++) {
          if (this.mute[slot]) continue;
          const g = this.gain[slot];
          if (g <= 0) continue;
          const { l, r } = this.views[slot];
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
