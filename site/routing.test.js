/*
 * node --test site/routing.test.js
 *
 * The static board's logic: register-ranked distribution of a piece's
 * channels over the fixed four-voice rig, casting resolution onto slots,
 * calibration compensation, event building, the why index, and title
 * engraving. Where behavior is shared with the live board
 * (tools/test_patchboard.py), the cases mirror it.
 */
import test from "node:test";
import assert from "node:assert/strict";
import {
  SLOTS, slotMap, lanePlacement, resolveSlots, calFor, buildEvents,
  heldAt, whyIndex, whysAt, pieceTitle, BLOCK,
} from "./routing.js";

const note = (ch, onS, durS, extra = {}) => ({
  onS, durS, ch, pitch: 60, vel: 100, bend: 8192, whys: [], ...extra,
});

const SR = 48000;

test("four slots, bass-first by mean pitch", () => {
  assert.deepEqual(SLOTS, ["bass", "tenor", "alto", "soprano"]);
  // channel numbers deliberately disagree with register: pitch decides
  const fugue = {
    tracks: [[note(0, 0, 1, { pitch: 80 })], [note(1, 0, 1, { pitch: 40 })],
      [note(2, 0, 1, { pitch: 55 })], [note(3, 0, 1, { pitch: 70 })]],
  };
  const { chToSlot } = slotMap(fugue);
  assert.deepEqual(chToSlot, { 1: 0, 2: 1, 3: 2, 0: 3 });
});

test("more channels than slots spread contiguously by rank", () => {
  // six lanes (the live wtc1f01 shape): ends get their own slot, the
  // middle pairs share
  const perf = {
    tracks: [[0, 1, 2, 3, 4, 5].map((c) => note(c, 0, 1, { pitch: 40 + c * 8 }))],
  };
  const { chToSlot, chToLane, laneSlots, slotChannels } = slotMap(perf);
  assert.deepEqual(slotChannels, [[0], [1, 2], [3, 4], [5]]);
  assert.equal(chToSlot[2], 1);
  // lanes are the register ranks: each keeps its own synth voice pool
  assert.deepEqual(chToLane, { 0: 0, 1: 1, 2: 2, 3: 3, 4: 4, 5: 5 });
  assert.deepEqual(laneSlots, [0, 1, 1, 2, 2, 3]);
});

test("fewer channels than slots leave slots tacet; a solo takes the top", () => {
  const duo = { tracks: [[note(0, 0, 1, { pitch: 40 })], [note(1, 0, 1, { pitch: 80 })]] };
  assert.deepEqual(slotMap(duo).slotChannels, [[0], [], [], [1]]);
  const solo = { tracks: [[note(7, 0, 1)]] };
  assert.deepEqual(slotMap(solo).slotChannels, [[], [], [], [7]]);
});

test("lanes pair only within their slot; an instance wears one preset", () => {
  // the wtc1f01 shape: [0],[1,2],[3,4],[5] — one instance per slot,
  // middle slots use both scenes
  const p = lanePlacement([0, 1, 1, 2, 2, 3]);
  assert.deepEqual(p.instSlot, [0, 1, 2, 3]);
  assert.deepEqual(p.laneInst, [0, 1, 1, 2, 2, 3]);
  assert.deepEqual(p.laneScene, [0, 0, 1, 0, 1, 0]);

  // an odd slot population overflows onto a second instance of the SAME
  // slot — never onto a neighbour (that would put two presets on one
  // patch, and cost the second one its send/global FX)
  const q = lanePlacement([0, 0, 0, 3]);
  assert.deepEqual(q.instSlot, [0, 0, 3]);
  assert.deepEqual(q.laneInst, [0, 0, 1, 2]);
  assert.deepEqual(q.laneScene, [0, 1, 0, 0]);
});

test("casting: default seeds once, piece casting lands on register slots", () => {
  const castings = {
    default: { 0: "data/patches/Leads/Deep.fxp" },
    wtc1f01: {
      0: "data/patches/A/a.fxp", 1: "data/patches/B/b.fxp",
      2: "data/patches/B/b2.fxp", 3: "data/patches/C/c.fxp",
      4: "data/patches/C/c2.fxp", 5: "data/patches/D/d.fxp",
    },
  };
  const six = [[0], [1, 2], [3, 4], [5]];
  const castState = { defaultDone: false };
  let rig = ["(init)", "(init)", "(init)", "(init)"];

  // first load: default seeds the bass, everything else stands
  rig = resolveSlots(castings, "other", six, rig, castState);
  assert.deepEqual(rig, ["data/patches/Leads/Deep.fxp",
    "(init)", "(init)", "(init)"]);

  // the piece's own casting wins, one entry per slot via its bass-most
  // channel — and the standing rig would otherwise persist untouched
  rig = resolveSlots(castings, "wtc1f01", six, rig, castState);
  assert.deepEqual(rig, ["data/patches/A/a.fxp", "data/patches/B/b.fxp",
    "data/patches/C/c.fxp", "data/patches/D/d.fxp"]);

  // no casting for the next piece: the rig persists (roles are the slots)
  const after = resolveSlots(castings, "another", [[0], [1], [2], [3]],
    rig, castState);
  assert.deepEqual(after, rig);

  // default does not re-seed
  const again = resolveSlots(castings, "other",
    [[0], [1], [2], [3]], ["(init)", "(init)", "(init)", "(init)"], castState);
  assert.deepEqual(again, ["(init)", "(init)", "(init)", "(init)"]);
});

test("slot-name casting keys win over channel keys and any ranking", () => {
  // the standing rig speaks the rig's own vocabulary: whatever channels
  // a piece has and however they rank, bass is bass
  const castings = {
    default: {
      bass: "data/patches/Leads/DNA Sequencer.fxp",
      tenor: "data/patches/Polysynths/Poly Lala.fxp",
      alto: "data/patches/Winds/Dreamy Flute.fxp",
      soprano: "data/patches/Leads/Lera.fxp",
      0: "data/patches/Keys/EP 2.fxp", // channel keys lose to names
    },
  };
  // a prelude-like ranking where channel 0 is NOT the bass
  const rig = resolveSlots(castings, "wtc1p01", [[1], [2], [4, 0], [3]],
    ["(init)", "(init)", "(init)", "(init)"], { defaultDone: false });
  assert.deepEqual(rig, [
    "data/patches/Leads/DNA Sequencer.fxp",
    "data/patches/Polysynths/Poly Lala.fxp",
    "data/patches/Winds/Dreamy Flute.fxp",
    "data/patches/Leads/Lera.fxp",
  ]);
});

test("casting falls through to a later channel in the slot", () => {
  // the slot's bass-most channel is uncast; its other channel is
  const castings = { t: { 2: "data/patches/Pads/Warm.fxp" } };
  const rig = resolveSlots(castings, "t", [[0], [1, 2], [], []],
    ["(init)", "(init)", "(init)", "(init)"], { defaultDone: true });
  assert.equal(rig[1], "data/patches/Pads/Warm.fxp");
});

test("calibration matches by bank tail and shaves half the release", () => {
  const cal = { "/mac/paths/Leads/Good.fxp": { attackS: 0.01, releaseS: 0.4 } };
  const slotUrls = ["data/patches/Leads/Good.fxp", "(init)", "(init)", "(init)"];
  const perf = { tracks: [[note(0, 0.0, 1.0)]] };
  const ev = buildEvents(perf, SR, { 0: 0 }, { 0: 0 }, slotUrls, cal, true);
  const offs = [...ev.frames].filter((_, i) => ev.kinds[i] === 2);
  assert.equal(offs[0], Math.floor(0.8 * SR)); // 1.0 - 0.4/2
  // a hand-edited entry without releaseS is a no-op, not a crash
  const ev2 = buildEvents(perf, SR, { 0: 0 }, { 0: 0 }, slotUrls,
    { "/mac/paths/Leads/Good.fxp": { attackS: 0.01 } }, true);
  assert.equal([...ev2.frames].filter((_, i) => ev2.kinds[i] === 2)[0], SR);
  assert.equal(calFor({}, "(init)"), null);
});

test("events land on LANES; same-frame order off<bend<on; scl drops bends", () => {
  const perf = {
    tracks: [[note(5, 0.0, 0.5, { pitch: 60 }),
      note(5, 0.5, 0.5, { pitch: 62, bend: 8000 })]],
  };
  const ev = buildEvents(perf, SR, { 5: 2 }, { 5: 4 }, [], {}, false);
  assert.ok([...ev.chans].every((c) => c === 4)); // the lane, not the slot
  const at = Math.floor(0.5 * SR);
  const idx = [...ev.frames].map((f, i) => [f, ev.kinds[i]])
    .filter(([f]) => f === at).map(([, k]) => k);
  assert.deepEqual(idx, [2, 0, 1]); // off, then bend, then on
  const scl = buildEvents(perf, SR, { 5: 2 }, { 5: 4 }, [], {}, true);
  assert.ok(![...scl.kinds].includes(0));
});

test("loop length runs out endS and is BLOCK-aligned", () => {
  const perf = { tracks: [[note(0, 0.0, 1.0)]], endS: 5.0 };
  const ev = buildEvents(perf, SR, { 0: 0 }, { 0: 0 }, [], {}, true);
  assert.equal(ev.loopFrames % BLOCK, 0);
  assert.ok(ev.loopFrames >= 7.0 * SR); // endS + 2 s ring-out
});

test("heldAt finds the notes sounding across a seek point", () => {
  // lane 0: a long pedal under two short notes on lane 1; seek into the
  // second short note — the pedal AND that note should be re-struck
  const perf = {
    tracks: [
      [note(0, 0.0, 4.0, { pitch: 36, vel: 90 })],
      [note(1, 0.0, 1.0, { pitch: 60 }), note(1, 2.0, 1.0, { pitch: 62 })],
    ],
  };
  const ev = buildEvents(perf, SR, { 0: 0, 1: 3 }, { 0: 0, 1: 1 }, [], {}, true);
  const held = heldAt(ev, Math.floor(2.5 * SR))
    .sort((x, y) => x.lane - y.lane);
  assert.deepEqual(held, [
    { lane: 0, key: 36, vel: 90 },
    { lane: 1, key: 62, vel: 100 },
  ]);
  // between the two short notes only the pedal sounds
  assert.deepEqual(heldAt(ev, Math.floor(1.5 * SR)),
    [{ lane: 0, key: 36, vel: 90 }]);
  // at frame 0 nothing has been struck yet
  assert.deepEqual(heldAt(ev, 0), []);
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
