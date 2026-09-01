/*
 * Pure logic for the static patchboard. The board is a fixed four-voice
 * rig — bass, tenor, alto, soprano — and every piece's channels are
 * distributed among those four slots by register. The slots persist
 * across pieces (they ARE the register roles the live board carried), so
 * patches, gains and mutes survive piece changes by construction.
 * Everything here is data-in/data-out — no DOM, no audio — and runs
 * under `node --test` (routing.test.js) as well as in the browser.
 */

export const SLOTS = ["bass", "tenor", "alto", "soprano"];

// Rank a piece's channels bass-first by mean pitch (kern orders spines
// bass-first, but the notes are the ground truth) and spread the ranks
// across the four slots. A single-channel piece takes the TOP slot —
// the solo carries the lead, the live board's rule.
//
// Each ranked channel is also a LANE: lanes keep their own synth voice
// pool underneath the four slots (lane r = instance r>>1, scene r&1 in
// the worklet), because lanes that merely share a UI slot must not share
// polyphony — a mono preset would collapse a three-lane slot to one
// sounding note, and overlapping same-pitch notes would release each
// other's voices.
export function slotMap(perf) {
  const sum = {};
  const cnt = {};
  for (const tr of perf.tracks) {
    for (const n of tr) {
      sum[n.ch] = (sum[n.ch] || 0) + n.pitch;
      cnt[n.ch] = (cnt[n.ch] || 0) + 1;
    }
  }
  const chans = Object.keys(sum).map(Number).sort(
    (a, b) => sum[a] / cnt[a] - sum[b] / cnt[b] || a - b);
  const n = chans.length;
  const chToSlot = {};
  const chToLane = {};
  const slotChannels = [[], [], [], []];
  const laneSlots = [];
  chans.forEach((ch, r) => {
    const s = n === 1 ? 3 : Math.round((r * 3) / (n - 1));
    chToSlot[ch] = s;
    chToLane[ch] = r;
    laneSlots.push(s);
    slotChannels[s].push(ch);
  });
  return { chToSlot, chToLane, laneSlots, slotChannels };
}

// Effective patch per slot for a piece about to load: current slot
// patches survive (the rig is standing); on the very first load the
// "default" casting seeds it; a piece with its own casting overrides.
// Castings are keyed by source channel (bass-first, as cast on the live
// board) — a slot wears the entry of its bass-most cast channel.
export function resolveSlots(castings, name, slotChannels, current,
                             castState) {
  const eff = [...current];
  const apply = (cast) => {
    if (!cast) return;
    slotChannels.forEach((chs, s) => {
      for (const ch of chs) {
        const url = cast[String(ch)];
        if (url) {
          eff[s] = url;
          break;
        }
      }
    });
  };
  if (!castState.defaultDone) {
    castState.defaultDone = true;
    apply(castings["default"]);
  }
  apply(castings[name]);
  return eff;
}

// Calibration entry for a patch, by url or by bank tail (Category/Name.fxp) —
// measurements travel with the patch, not the filesystem (patchboard._cal_for).
export function calFor(calibration, url) {
  if (!url || url === "(init)") return null;
  if (calibration[url]) return calibration[url];
  const tail = url.replace(/\\/g, "/").split("/").slice(-2).join("/");
  for (const [k, v] of Object.entries(calibration)) {
    if (k.replace(/\\/g, "/").endsWith("/" + tail)) return v;
  }
  return null;
}

export const BLOCK = 32;

// The event list the worklet plays: patchboard.load_piece's build, with
// each note routed to its LANE (per-lane synth state; the slot only
// chooses patch/gain/mute) and packed into transferable typed arrays.
// kind 0=bend 1=on 2=off; same-frame order off < bend < on. `sclActive`
// drops per-note bends (native SCL tuning applies the temperament; bends
// would tune twice).
export function buildEvents(perf, sr, chToSlot, chToLane, slotUrls,
                            calibration, sclActive) {
  const evs = [];
  let end = 0;
  for (const tr of perf.tracks) {
    for (const n of tr) {
      const lane = chToLane[n.ch] ?? 0;
      const slot = chToSlot[n.ch] ?? 3;
      const on = Math.floor(n.onS * sr);
      // timbre-aware articulation: subtract part of the slot's measured
      // release tail so breaths stay audible on pads
      const m = calFor(calibration, slotUrls[slot] ?? "");
      const comp = m ? Math.min(0.5 * (m.releaseS || 0), 0.3) : 0;
      const durS = Math.max(n.durS * 0.5, n.durS - comp);
      const off = Math.max(on + 1, Math.floor((n.onS + durS) * sr));
      end = Math.max(end, n.onS + n.durS);
      if (!sclActive) evs.push({ frame: on, kind: 0, ch: lane, a: n.bend, b: 0 });
      evs.push({ frame: on, kind: 1, ch: lane, a: n.pitch, b: n.vel });
      evs.push({ frame: off, kind: 2, ch: lane, a: n.pitch, b: 0 });
    }
  }
  const KO = { 2: 0, 0: 1, 1: 2 };
  evs.sort((x, y) => x.frame - y.frame || KO[x.kind] - KO[y.kind]);

  const tempo = (perf.tempoMap || [])
    .filter((t) => "onS" in t)
    .map((t) => ({ frame: Math.floor(t.onS * sr), bpm: t.bpm }))
    .sort((a, b) => a.frame - b.frame);

  // BLOCK-aligned, with the same +2 s ring-out as the live board; endS
  // extends past the last note-off when a held silence closes the piece
  const endS = Math.max(end, perf.endS || 0);
  const loopFrames = Math.ceil(Math.floor((endS + 2.0) * sr) / BLOCK) * BLOCK;

  return {
    frames: Int32Array.from(evs, (e) => e.frame),
    kinds: Int8Array.from(evs, (e) => e.kind),
    chans: Int8Array.from(evs, (e) => e.ch),
    a: Int16Array.from(evs, (e) => e.a),
    b: Int16Array.from(evs, (e) => e.b),
    tempoFrames: Int32Array.from(tempo, (t) => t.frame),
    tempoBpm: Float32Array.from(tempo, (t) => t.bpm),
    loopFrames,
  };
}

// Place lanes onto synth instances, pairing ONLY within a slot: an
// instance's two scenes always wear the same preset, so its full native
// output — scenes, insert FX, send FX, global FX — is that preset's
// true signal path (477 of 627 factory patches keep reverb/delay in
// send or global slots, which a per-scene tap would silently drop).
// Slot gain/mute therefore apply per instance at the mix.
export function lanePlacement(laneSlots) {
  const laneInst = new Array(laneSlots.length);
  const laneScene = new Array(laneSlots.length);
  const instSlot = [];
  for (let s = 0; s < SLOTS.length; s++) {
    const lanes = [];
    laneSlots.forEach((sl, l) => {
      if (sl === s) lanes.push(l);
    });
    for (let i = 0; i < lanes.length; i += 2) {
      const inst = instSlot.length;
      instSlot.push(s);
      laneInst[lanes[i]] = inst;
      laneScene[lanes[i]] = 0;
      if (i + 1 < lanes.length) {
        laneInst[lanes[i + 1]] = inst;
        laneScene[lanes[i + 1]] = 1;
      }
    }
  }
  return { laneInst, laneScene, instSlot };
}

// Notes sounding across `frame`: walk the event stream up to the seek
// point tracking held (lane, key) pairs, so a seek can re-strike what
// should already be ringing instead of landing in silence until the
// next attack. Envelopes restart — a re-struck note is an honest
// approximation, not a reconstruction.
export function heldAt(score, frame) {
  const { frames, kinds, chans, a, b } = score;
  const held = new Map();
  for (let i = 0; i < frames.length && frames[i] < frame; i++) {
    const key = chans[i] * 128 + a[i];
    if (kinds[i] === 1) held.set(key, { lane: chans[i], key: a[i], vel: b[i] });
    else if (kinds[i] === 2) held.delete(key);
  }
  return [...held.values()];
}

// The executable edition's subtitle track: every note's rule citations,
// indexed by sounding time (patchboard.why_index / whys_at).
export function whyIndex(perf) {
  const idx = [];
  for (const tr of perf.tracks) {
    for (const n of tr) {
      if (n.whys && n.whys.length) idx.push([n.onS, n.onS + n.durS, n.ch, n.whys]);
    }
  }
  idx.sort((x, y) => x[0] - y[0] || x[1] - y[1] || x[2] - y[2]);
  return idx;
}

export function whysAt(idx, t) {
  const out = [];
  for (const [on, off, ch, ws] of idx) {
    if (on > t) break;
    if (t < off) for (const w of ws) out.push({ ch, why: w });
  }
  return out.slice(0, 16);
}

// wtc1p01 -> "Prelude I in C major", BWV 846. WTC order pairs each major with
// its parallel minor; No. 8 is the famous enharmonic split (E-flat minor
// prelude against D-sharp minor fugue in Book I; both D-sharp minor in Book 2).
export const KEYS = ["C major", "C minor", "C♯ major", "C♯ minor",
  "D major", "D minor", "E♭ major", "D♯ minor", "E major", "E minor",
  "F major", "F minor", "F♯ major", "F♯ minor", "G major", "G minor",
  "A♭ major", "G♯ minor", "A major", "A minor", "B♭ major",
  "B♭ minor", "B major", "B minor"];
export const ROMAN = ["I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX",
  "X", "XI", "XII", "XIII", "XIV", "XV", "XVI", "XVII", "XVIII", "XIX", "XX",
  "XXI", "XXII", "XXIII", "XXIV"];

export function pieceTitle(slug) {
  const m = /^wtc([12])([pf])(\d\d)$/.exec(slug);
  if (!m) return { main: slug, bwv: "" };
  const book = +m[1];
  const kind = m[2] === "p" ? "Prelude" : "Fugue";
  const n = +m[3];
  let key = KEYS[n - 1];
  if (n === 8 && book === 1 && kind === "Prelude") key = "E♭ minor";
  return {
    main: `${kind} ${ROMAN[n - 1]} in ${key}`,
    bwv: `BWV ${(book === 1 ? 845 : 869) + n}`,
  };
}
