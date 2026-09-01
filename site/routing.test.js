/*
 * node --test site/routing.test.js
 *
 * Mirrors tools/test_patchboard.py for the logic the static board ports:
 * register naming, role-stable routing, casting resolution, calibration
 * compensation, event building, the why index, and title engraving.
 */
import test from "node:test";
import assert from "node:assert/strict";
import {
  partsOf, carryRoles, resolveCasting, calFor, buildEvents,
  whyIndex, whysAt, pieceTitle, BLOCK,
} from "./routing.js";

const note = (ch, onS, durS, extra = {}) => ({
  onS, durS, ch, pitch: 60, vel: 100, bend: 8192, whys: [], ...extra,
});

const SR = 48000;

// composes partsOf/carryRoles/resolveCasting the way app.js loadPiece does
function switchTo(perf, name, state) {
  const { parts, order } = partsOf(perf);
  for (const part of parts) {
    for (const ch of part.channels) {
      state.ch.gain[ch] ??= 0.25;
      state.ch.mute[ch] ??= false;
      state.ch.patchPath[ch] ??= "(init)";
    }
  }
  const prevRoles = state.rankChannels
    ? state.rankChannels.map((ch) => ({
        patch: state.ch.patchPath[ch] ?? "(init)",
        gain: state.ch.gain[ch] ?? 0.25,
        mute: state.ch.mute[ch] ?? false,
      }))
    : null;
  const carried = carryRoles(prevRoles, parts, order, state.ch);
  state.rankChannels = carried.rankChannels;
  const eff = resolveCasting(state.castings, name, parts, carried.roleBase,
    state.ch.patchPath, state.castState, 16);
  for (const [ch, url] of Object.entries(eff)) state.ch.patchPath[+ch] = url;
  return { parts, eff };
}

const freshState = (castings = {}) => ({
  ch: { gain: {}, mute: {}, patchPath: {}, patchName: {} },
  castState: { defaultDone: false },
  rankChannels: null,
  castings,
});

test("register naming is bass-first from mean pitch", () => {
  const fugue = {
    tracks: [[note(0, 0, 1, { pitch: 40 })], [note(1, 0, 1, { pitch: 80 })],
      [note(2, 0, 1, { pitch: 60 })]],
  };
  const { parts, order } = partsOf(fugue);
  assert.equal(parts[0].name, "bass (voice 0)");
  assert.equal(parts[1].name, "soprano (voice 1)");
  assert.equal(parts[2].name, "alto (voice 2)");
  assert.deepEqual(order, [0, 2, 1]);
});

test("voice routing follows register roles across piece changes", () => {
  // piece A: one 3-lane solo part; piece B: three parts partitioned
  // differently. Patches/gains follow the register ROLE, and the solo
  // inherits the TOP role (test_patchboard.py's case, verbatim).
  const solo = { tracks: [[0, 1, 2].map((c) => note(c, 0, 1, { pitch: 60 }))] };
  const fugue = {
    tracks: [[note(0, 0, 1, { pitch: 40 })], [note(1, 0, 1, { pitch: 60 })],
      [note(2, 0, 1, { pitch: 80 })]],
  };
  const st = freshState();
  switchTo(fugue, "fugue", st);
  st.ch.patchPath[0] = "data/patches/A/a.fxp";
  st.ch.patchPath[1] = "data/patches/B/b.fxp";
  st.ch.patchPath[2] = "data/patches/C/c.fxp";
  st.ch.gain[2] = 0.7; // soprano turned down

  // fugue -> solo: the single part wears the TOP role
  const { parts } = switchTo(solo, "solo", st);
  assert.equal(parts.length, 1);
  for (const ch of parts[0].channels) {
    assert.equal(st.ch.patchPath[ch], "data/patches/C/c.fxp");
    assert.equal(st.ch.gain[ch], 0.7);
  }

  // solo -> fugue: uniform inheritance, no partial smear
  const back = switchTo(fugue, "fugue", st);
  const worn = new Set(back.parts.map((p) => st.ch.patchPath[p.channels[0]]));
  assert.deepEqual(worn, new Set(["data/patches/C/c.fxp"]));
});

test("casting: default seeds once, piece file casts whole parts", () => {
  const castings = {
    default: { 0: "data/patches/Leads/Deep.fxp" },
    t: { 0: "data/patches/Pads/Warm.fxp" },
  };
  // one part on two channels: the piece file names only the part's
  // first channel yet must cast the whole part
  const twoLane = { tracks: [[note(0, 0, 1), note(1, 0, 1)]] };
  const st = freshState(castings);
  switchTo(twoLane, "t", st);
  assert.equal(st.ch.patchPath[0], "data/patches/Pads/Warm.fxp");
  assert.equal(st.ch.patchPath[1], "data/patches/Pads/Warm.fxp");

  // default.json alone (no piece file) seeds the first load only
  const st2 = freshState(castings);
  switchTo({ tracks: [[note(0, 0, 1)]] }, "other", st2);
  assert.equal(st2.ch.patchPath[0], "data/patches/Leads/Deep.fxp");
  st2.ch.patchPath[0] = "(init)";
  switchTo({ tracks: [[note(0, 0, 1)]] }, "other", st2);
  assert.equal(st2.ch.patchPath[0], "(init)"); // not re-seeded
});

test("calibration matches by bank tail and shaves half the release", () => {
  const cal = { "/mac/paths/Leads/Good.fxp": { attackS: 0.01, releaseS: 0.4 } };
  const eff = { 0: "data/patches/Leads/Good.fxp" };
  const perf = { tracks: [[note(0, 0.0, 1.0)]] };
  const ev = buildEvents(perf, SR, eff, cal, true);
  const offs = [...ev.frames].filter((_, i) => ev.kinds[i] === 2);
  assert.equal(offs[0], Math.floor(0.8 * SR)); // 1.0 - 0.4/2
  // a hand-edited entry without releaseS is a no-op, not a crash
  const ev2 = buildEvents(perf, SR, eff,
    { "/mac/paths/Leads/Good.fxp": { attackS: 0.01 } }, true);
  assert.equal([...ev2.frames].filter((_, i) => ev2.kinds[i] === 2)[0], SR);
  assert.equal(calFor({}, "(init)"), null);
});

test("events: same-frame order off<bend<on; scl drops bends", () => {
  const perf = {
    tracks: [[note(0, 0.0, 0.5, { pitch: 60 }),
      note(0, 0.5, 0.5, { pitch: 62, bend: 8000 })]],
  };
  const ev = buildEvents(perf, SR, {}, {}, false);
  const at = Math.floor(0.5 * SR);
  const idx = [...ev.frames].map((f, i) => [f, ev.kinds[i]])
    .filter(([f]) => f === at).map(([, k]) => k);
  assert.deepEqual(idx, [2, 0, 1]); // off, then bend, then on
  const scl = buildEvents(perf, SR, {}, {}, true);
  assert.ok(![...scl.kinds].includes(0));
});

test("loop length runs out endS and is BLOCK-aligned", () => {
  const perf = { tracks: [[note(0, 0.0, 1.0)]], endS: 5.0 };
  const ev = buildEvents(perf, SR, {}, {}, true);
  assert.equal(ev.loopFrames % BLOCK, 0);
  assert.ok(ev.loopFrames >= 7.0 * SR); // endS + 2 s ring-out
});

test("whysAt returns sounding rules, capped at 16", () => {
  const perf = {
    tracks: [[
      note(0, 0.0, 1.0, { whys: ["breath: gap  [Quantz]"] }),
      note(0, 2.0, 1.0, { whys: ["rit: slow"] }),
    ]],
  };
  const idx = whyIndex(perf);
  assert.deepEqual(whysAt(idx, 0.5),
    [{ ch: 0, why: "breath: gap  [Quantz]" }]);
  assert.deepEqual(whysAt(idx, 1.5), []);
  const many = {
    tracks: [[note(0, 0.0, 1.0,
      { whys: Array.from({ length: 20 }, (_, i) => `r${i}: d`) })]],
  };
  assert.equal(whysAt(whyIndex(many), 0.5).length, 16);
});

test("titles: engraved names, BWV, the No. 8 enharmonic split", () => {
  assert.deepEqual(pieceTitle("wtc1p01"),
    { main: "Prelude I in C major", bwv: "BWV 846" });
  assert.equal(pieceTitle("wtc1p08").main, "Prelude VIII in E♭ minor");
  assert.equal(pieceTitle("wtc1f08").main, "Fugue VIII in D♯ minor");
  assert.deepEqual(pieceTitle("wtc2f24"),
    { main: "Fugue XXIV in B minor", bwv: "BWV 893" });
  assert.deepEqual(pieceTitle("xtra-ground-folia"),
    { main: "xtra-ground-folia", bwv: "" });
});
