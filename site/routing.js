/*
 * Pure logic for the static patchboard, ported line-for-line from the live
 * board (tools/patchboard.py) so the two agree note-for-note. Everything here
 * is data-in/data-out — no DOM, no audio — and runs under `node --test`
 * (routing.test.js) as well as in the browser.
 */

// kern orders spines bass-first: name parts by register from the notes,
// so nobody casts a lead onto the bass again (patchboard.load_piece).
export const REGNAMES = {
  1: ["solo"],
  2: ["bass", "soprano"],
  3: ["bass", "alto", "soprano"],
  4: ["bass", "tenor", "alto", "soprano"],
  5: ["bass", "tenor", "alto", "mezzo", "soprano"],
};

export function partsOf(perf) {
  const tracks = perf.tracks;
  const means = tracks.map(
    (tr) => tr.reduce((s, n) => s + n.pitch, 0) / Math.max(1, tr.length),
  );
  const order = means.map((_, i) => i).sort((a, b) => means[a] - means[b]);
  const names = REGNAMES[means.length];
  const labels = {};
  order.forEach((vi, rank) => {
    labels[vi] = names ? names[rank] : `voice ${vi}`;
  });
  const parts = tracks.map((tr, vi) => ({
    name: `${labels[vi]} (voice ${vi})`,
    channels: [...new Set(tr.map((n) => n.ch))].sort((a, b) => a - b),
  }));
  return { parts, order };
}

// ROLE-STABLE routing: the outgoing piece's settings, snapshotted in register
// order (bass..soprano), map onto the new parts by register rank — equal
// ranks match, differing voice counts interpolate, and a single-part piece
// inherits the TOP role (the solo carries the lead). Gains/mutes apply
// directly; patches become the base the casting resolution starts from.
export function carryRoles(prevRoles, parts, order, chState) {
  const rankParts = order.map((vi) => parts[vi]);
  let roleBase = null;
  if (prevRoles && prevRoles.length) {
    const m = prevRoles.length;
    const n = rankParts.length;
    roleBase = { ...chState.patchPath };
    rankParts.forEach((part, r) => {
      let src;
      if (n === 1) src = prevRoles[m - 1];
      else if (m === 1) src = prevRoles[0];
      else src = prevRoles[Math.round((r * (m - 1)) / (n - 1))];
      for (const ch of part.channels) {
        chState.gain[ch] = src.gain;
        chState.mute[ch] = src.mute;
        roleBase[ch] = src.patch;
      }
    });
  }
  return { roleBase, rankChannels: rankParts.map((p) => p.channels[0]) };
}

// Effective patch per channel for a piece about to load: current patches
// survive (auditioning continuity); on the very first load the "default"
// casting seeds the standing rig; a piece with its own casting overrides,
// its per-part entries expanded to every channel of the part.
// `castings` is the baked data/casting.json: {name: {ch: url}}.
export function resolveCasting(castings, name, parts, base, chPatchPath,
                               castState, nInstances) {
  const eff = { ...(base ?? chPatchPath) };
  if (!castState.defaultDone) {
    castState.defaultDone = true;
    const dflt = castings["default"];
    if (dflt) {
      for (const [ch, url] of Object.entries(dflt)) {
        if (+ch < nInstances) eff[+ch] = url;
      }
    }
  }
  const cast = castings[name];
  if (cast) {
    for (const part of parts) {
      const url = cast[String(part.channels[0])];
      if (url) for (const ch of part.channels) eff[ch] = url;
    }
  }
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

// The event list the worklet plays: patchboard.load_piece's build, packed
// into transferable typed arrays. kind 0=bend 1=on 2=off; same-frame order
// off < bend < on. `sclActive` drops per-note bends (native SCL tuning
// applies the temperament; bends would tune twice).
export function buildEvents(perf, sr, eff, calibration, sclActive) {
  const evs = [];
  let end = 0;
  for (const tr of perf.tracks) {
    for (const n of tr) {
      const on = Math.floor(n.onS * sr);
      // timbre-aware articulation: subtract part of the channel's measured
      // release tail so breaths stay audible on pads
      const m = calFor(calibration, eff[n.ch] ?? "");
      const comp = m ? Math.min(0.5 * (m.releaseS || 0), 0.3) : 0;
      const durS = Math.max(n.durS * 0.5, n.durS - comp);
      const off = Math.max(on + 1, Math.floor((n.onS + durS) * sr));
      end = Math.max(end, n.onS + n.durS);
      if (!sclActive) evs.push({ frame: on, kind: 0, ch: n.ch, a: n.bend, b: 0 });
      evs.push({ frame: on, kind: 1, ch: n.ch, a: n.pitch, b: n.vel });
      evs.push({ frame: off, kind: 2, ch: n.ch, a: n.pitch, b: 0 });
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
