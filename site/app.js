/*
 * The static patchboard's main thread: fetches the baked album, owns the
 * four-voice rig — bass, tenor, alto, soprano, one Surge instance each,
 * shown at all times — and drives the worklet (board-processor.js) with
 * pre-built event lists. Each piece's channels are distributed among the
 * four slots by register (routing.js slotMap); the rig persists across
 * pieces, so a casting you dial in stands until a piece's own casting
 * file overrides it. The progress rail seeks, and the ledger reads while
 * paused — seek anywhere and study the moment.
 */
import {
  SLOTS, slotMap, lanePlacement, resolveSlots, buildEvents,
  whyIndex, whysAt, pieceTitle,
} from "./routing.js";

// async guards: fetches resolve in any order, so every load carries a
// generation and only the latest generation is allowed to take effect.
// reqIdx tracks the piece most recently REQUESTED (synchronously), so
// rapid stepping advances from the request, not the committed piece.
let pieceGen = 0;
let reqIdx = null;
let warmedUp = false;
const slotRev = [0, 0, 0, 0]; // bumped on every user retarget of a slot
let startIdx = 0; // where the board opens: START_PIECE, resolved from the manifest
const START_PIECE = "wtc1p05";
const instGen = {};
const lastShipped = {};

let MANIFEST, CASTINGS, CAL, CATS;
let ctx, node, SR;
let INIT_BYTES; // Init Saw — what "(init)" loads as a scene partner
let PLAYING = false;
let PIECE = null; // {idx, name, chToSlot, slotChannels, whyIdx, loopFrames}
let POS = { frame: 0, t: Date.now(), playing: false };
let STAT = { peak: 0, riding: false, load: 0 };
const slotPatch = ["(init)", "(init)", "(init)", "(init)"];
const slotGain = [0.25, 0.25, 0.25, 0.25]; // summing headroom (live default)
const slotMute = [false, false, false, false];
const castState = { defaultDone: false };
const patchCache = new Map();

const $ = (id) => document.getElementById(id);
const esc = (s) => String(s).replace(/[&<>"]/g,
  (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" }[c]));
const encUrl = (u) => u.split("/").map(encodeURIComponent).join("/");
const patchDisplay = (url) => url === "(init)"
  ? "(init)" : url.split("/").pop().replace(/\.fxp$/, "");
const bankTail = (url) => url.split("/").slice(-2).join("/");
const fmt = (s) => {
  s = Math.max(0, s | 0);
  return `${(s / 60) | 0}:${String(s % 60).padStart(2, "0")}`;
};

async function init() {
  [MANIFEST, CASTINGS, CAL, CATS] = await Promise.all([
    "data/manifest.json", "data/casting.json",
    "data/calibration.json", "data/patches.json",
  ].map((u) => fetch(u).then((r) => r.json())));
  $("piecesel").innerHTML = MANIFEST.pieces
    .map((p) => {
      const t = pieceTitle(p.name);
      const label = t.bwv
        ? `${t.main} · ${t.bwv}` : t.main;
      return `<option>${esc(label)}</option>`;
    }).join("");
  startIdx = Math.max(0,
    MANIFEST.pieces.findIndex((p) => p.name === START_PIECE));
  $("piecesel").selectedIndex = startIdx;
  const t = pieceTitle(MANIFEST.pieces[startIdx].name);
  $("title").textContent = t.main;
  $("idx").textContent =
    `${t.bwv ? t.bwv + " · " : ""}${startIdx + 1} / ${MANIFEST.pieces.length}`;
  $("stats").textContent =
    `${MANIFEST.pieces.length} pieces baked · four voices on ` +
    `${MANIFEST.nInstances} Surge synths` +
    ` will render in this tab — press Start`;
  renderRig();

  $("start").addEventListener("click", start);
  $("play").addEventListener("click", playToggle);
  $("prev").addEventListener("click", () => pieceStep(-1));
  $("next").addEventListener("click", () => pieceStep(1));
  $("piecesel").addEventListener("change", (e) => setPiece(e.target.selectedIndex));
  $("rail").addEventListener("click", (e) => {
    const r = e.currentTarget.getBoundingClientRect();
    seekFrac((e.clientX - r.left) / r.width);
  });
  $("rail").addEventListener("keydown", (e) => {
    const d = { ArrowLeft: -5, ArrowRight: 5 }[e.key];
    if (d && PIECE) {
      e.preventDefault();
      seekFrac((playhead() + d) / (PIECE.loopFrames / SR));
    }
  });
  setInterval(refreshWhys, 400);
  requestAnimationFrame(tick);
}

async function start() {
  $("start").disabled = true;
  $("stats").textContent = "loading engine…";
  try {
    // playback-sized buffers: this is a player, not an instrument — the
    // larger latency absorbs cold-start and GC hiccups that would
    // otherwise crackle
    ctx = new AudioContext({ latencyHint: "playback" });
    await ctx.resume();
    SR = ctx.sampleRate;
    const [wasmBinary, initBytes] = await Promise.all([
      fetch("engine/surge-worklet.wasm").then((r) => r.arrayBuffer()),
      fetch("data/init.fxp").then((r) => r.arrayBuffer()),
    ]);
    INIT_BYTES = initBytes;
    await ctx.audioWorklet.addModule("board-processor.js");
    node = new AudioWorkletNode(ctx, "board", {
      numberOfInputs: 0,
      numberOfOutputs: 1,
      outputChannelCount: [2],
      // one scene per lane, lanes paired within their slot; the bake
      // computed the widest piece's need (bake_site.piece_instances)
      processorOptions: { wasmBinary, nInstances: MANIFEST.nInstances },
    });
    const ready = new Promise((res, rej) => {
      node.port.onmessage = (e) => {
        if (e.data.type === "ready") res(e.data);
        else if (e.data.type === "error") rej(new Error(e.data.message));
        else onMsg(e.data);
      };
    });
    node.connect(ctx.destination);
    await ready;
    node.port.onmessage = (e) => onMsg(e.data);

    const scl = await (await fetch(MANIFEST.scl)).text();
    node.port.postMessage({ type: "scl", text: scl });

    $("start").hidden = true;
    $("play").hidden = false;
    PLAYING = true;
    await loadPiece(startIdx);
  } catch (err) {
    $("stats").textContent = "engine failed: " + err.message;
    $("start").disabled = false;
    throw err;
  }
}

function onMsg(d) {
  if (d.type === "pos") {
    POS = { frame: d.frame, t: Date.now(), playing: d.playing };
    STAT = { peak: d.peak, riding: d.riding, load: d.load };
    if (d.levels) updateMeters(d.levels);
    renderStats();
  } else if (d.type === "ended") {
    loadPiece((PIECE.idx + 1) % MANIFEST.pieces.length);
  } else if (d.type === "warmed") {
    console.log(`engine warmed in ${d.ms} ms`);
  } else if (d.type === "scl") {
    if (d.err) $("stats").textContent = "SCL error: " + d.err;
  } else if (d.type === "error") {
    $("stats").textContent = "engine error: " + d.message;
  }
}

async function loadPiece(idx) {
  const gen = ++pieceGen;
  reqIdx = idx;
  const p = MANIFEST.pieces[idx];
  node.port.postMessage({ type: "pause" });
  const perf = await (await fetch(encUrl(p.url))).json();
  if (gen !== pieceGen) return; // a newer navigation superseded this one
  const { chToSlot, chToLane, laneSlots, slotChannels } = slotMap(perf);
  const placement = lanePlacement(laneSlots);

  // STAGE the effective rig: a load that ends up superseded must leave
  // no trace in the standing rig or the once-only default-cast latch —
  // an uncasted successor would otherwise inherit the loser's casting
  const staged = { defaultDone: castState.defaultDone };
  const stageRevs = [...slotRev]; // which slots the user touches after this
  const eff = resolveSlots(CASTINGS, p.name, slotChannels, slotPatch,
    staged);

  // every instance this piece places lanes on — piece changes reassign
  // lanes to instances, so this is not just "whose patch changed"
  await Promise.all(placement.instSlot.map((slot, inst) =>
    shipInstance(inst, eff[slot])));
  if (gen !== pieceGen) return;

  // this request won: settle the FINAL rig — the staged casting, except
  // where the user retargeted a slot after staging (their edit may have
  // shipped against the outgoing piece's instances). Every instance is
  // shipped its final patch and AWAITED before the score posts, and the
  // settle RETRIES until no slot revision moved during the wait —
  // playback must not begin on a patch whose replacement is still in
  // flight, and the score's calibration must match what actually plays.
  // (shipInstance dedups, so a stable pass is all no-ops.)
  let finalEff;
  for (;;) {
    const revs = [...slotRev];
    finalEff = SLOTS.map((_, s) =>
      slotRev[s] === stageRevs[s] ? eff[s] : slotPatch[s]);
    await Promise.all(placement.instSlot.map((slot, inst) =>
      shipInstance(inst, finalEff[slot])));
    if (gen !== pieceGen) return;
    if (SLOTS.every((_, s) => slotRev[s] === revs[s])) break;
  }

  // ...and only now commit, so a load that loses at any check has left
  // the standing rig untouched
  castState.defaultDone = staged.defaultDone;
  for (let s = 0; s < SLOTS.length; s++) {
    if (slotRev[s] === stageRevs[s]) slotPatch[s] = finalEff[s];
  }

  // calibration compensation sees the patches that will actually play
  const score = buildEvents(perf, SR, chToSlot, chToLane, finalEff, CAL, true);
  PIECE = {
    idx, name: p.name, chToSlot, laneSlots, slotChannels,
    instSlot: placement.instSlot,
    whyIdx: whyIndex(perf), loopFrames: score.loopFrames,
  };
  const laneInst = Int8Array.from(placement.laneInst);
  const laneScene = Int8Array.from(placement.laneScene);
  const instSlot = Int8Array.from(placement.instSlot);
  node.port.postMessage(
    { type: "score", ...score, laneInst, laneScene, instSlot }, [
      score.frames.buffer, score.kinds.buffer, score.chans.buffer,
      score.a.buffer, score.b.buffer,
      score.tempoFrames.buffer, score.tempoBpm.buffer,
      laneInst.buffer, laneScene.buffer, instSlot.buffer,
    ]);
  for (let s = 0; s < SLOTS.length; s++) {
    node.port.postMessage({ type: "gain", ch: s, v: slotGain[s] });
    node.port.postMessage({ type: "mute", ch: s, v: slotMute[s] });
  }
  POS = { frame: 0, t: Date.now(), playing: PLAYING };
  renderHeader();
  refreshRig();
  if (!warmedUp) {
    // queued between the patches and "play": port messages are FIFO, so
    // the engine warms through the loaded presets while still silent
    warmedUp = true;
    node.port.postMessage({ type: "warmup" });
  }
  if (PLAYING) node.port.postMessage({ type: "play" });
}

async function patchBytes(url) {
  if (url === "(init)") return INIT_BYTES; // a real Init Saw, so (init)
  let bytes = patchCache.get(url); //         is loadable, not just "booted"
  if (!bytes) {
    bytes = await (await fetch(encUrl(url))).arrayBuffer();
    patchCache.set(url, bytes);
  }
  return bytes;
}

// An instance's two scenes wear its slot's preset — same patch twice,
// so the instance's native output (sends and globals included) is the
// preset's true signal path. Guarded by a per-instance generation so a
// slow fetch can never overwrite a newer selection; unchanged patches
// are skipped so piece changes only reload what actually changed.
async function shipInstance(inst, url) {
  if (!node) return;
  // bump the generation BEFORE the cache-hit check: reverting to the
  // already-loaded patch must also cancel a slower load still in flight,
  // or the stale load would land after the revert and contradict the UI
  const gen = (instGen[inst] = (instGen[inst] || 0) + 1);
  if (lastShipped[inst] === url) return;
  const bytes = await patchBytes(url);
  if (gen !== instGen[inst]) return; // a newer patch is on its way
  const copyA = new Uint8Array(bytes.slice(0)); // transfer must not eat the cache
  const copyB = new Uint8Array(bytes.slice(0));
  node.port.postMessage(
    { type: "patches", inst, bytesA: copyA, nameA: patchDisplay(url),
      bytesB: copyB, nameB: patchDisplay(url) },
    [copyA.buffer, copyB.buffer],
  );
  lastShipped[inst] = url;
}

// ---- transport -------------------------------------------------------------

function playToggle() {
  if (!node || !PIECE) return;
  PLAYING = !PLAYING;
  if (!PLAYING) POS = { frame: playhead() * SR, t: Date.now(), playing: false };
  node.port.postMessage({ type: PLAYING ? "play" : "pause" });
  $("play").textContent = PLAYING ? "Pause" : "Play";
}

function setPiece(i) {
  if (node && PIECE && i !== (reqIdx ?? PIECE.idx)) loadPiece(i);
}

function pieceStep(d) {
  if (!PIECE) return;
  const base = reqIdx ?? PIECE.idx;
  loadPiece((base + d + MANIFEST.pieces.length) % MANIFEST.pieces.length);
}

function seekFrac(f) {
  if (!node || !PIECE) return;
  const frame = Math.round(
    Math.max(0, Math.min(1, f)) * PIECE.loopFrames);
  POS = { frame, t: Date.now(), playing: PLAYING };
  node.port.postMessage({ type: "seek", frame });
}

// where the playhead is, in seconds of the current piece
function playhead() {
  if (!PIECE) return 0;
  let at = POS.frame / SR;
  if (PLAYING && POS.playing) at += (Date.now() - POS.t) / 1000;
  return Math.min(at, PIECE.loopFrames / SR);
}

// ---- rendering -------------------------------------------------------------

function renderHeader() {
  const t = pieceTitle(PIECE.name);
  $("title").textContent = t.main;
  $("idx").textContent =
    `${t.bwv ? t.bwv + " · " : ""}${PIECE.idx + 1} / ${MANIFEST.pieces.length}`;
  if ($("piecesel").selectedIndex !== PIECE.idx)
    $("piecesel").selectedIndex = PIECE.idx;
  $("play").textContent = PLAYING ? "Pause" : "Play";
}

function opts() {
  let h = '<option value="(init)">(init patch)</option>';
  for (const [cat, ps] of Object.entries(CATS)) {
    h += `<optgroup label="${esc(cat)}">`;
    for (const p of ps)
      h += `<option value="${esc(p.url)}">${esc(p.name)}</option>`;
    h += "</optgroup>";
  }
  return h;
}

// The four voices, shown at all times. Built once; pieces only change
// which slots are sounding (tacet) and what the casting put on them.
function renderRig() {
  const div = $("parts");
  div.innerHTML = SLOTS.map((name, s) => `
    <div class=part id=part${s} style="--vc:var(--v${s})">
     <div class=head>
      <b>${name}</b>
      <span class=patch id=pn${s}></span>
      <span class=tacet id=tc${s}></span>
     </div>
     <div class=row>
      <button data-step=-1 data-slot=${s} aria-label="previous patch">◀</button>
      <select id=sel${s} data-slot=${s} style="flex:1">${opts()}</select>
      <button data-step=1 data-slot=${s} aria-label="next patch">▶</button>
      <button id=mute${s} data-mute=${s}>Mute</button>
      <span class=fader><span class=meter id=meter${s}></span>
       <input type=range min=0 max=1.5 step=0.05 id=gain${s}
         data-gain=${s} aria-label="gain"></span>
     </div>
    </div>`).join("");
  div.querySelectorAll("[data-step]").forEach((b) =>
    b.addEventListener("click", () =>
      stepPatch(+b.dataset.slot, +b.dataset.step)));
  div.querySelectorAll("select[data-slot]").forEach((sel) =>
    sel.addEventListener("change", () => setPatch(+sel.dataset.slot, sel.value)));
  div.querySelectorAll("[data-mute]").forEach((b) =>
    b.addEventListener("click", () => muteToggle(+b.dataset.mute)));
  div.querySelectorAll("[data-gain]").forEach((r) =>
    r.addEventListener("input", () =>
      setGain(+r.dataset.gain, parseFloat(r.value))));
  refreshRig();
}

function refreshRig() {
  SLOTS.forEach((_, s) => {
    $(`pn${s}`).textContent = patchDisplay(slotPatch[s]);
    const sel = $(`sel${s}`);
    if (document.activeElement !== sel && sel.value !== slotPatch[s])
      sel.value = slotPatch[s];
    $(`mute${s}`).className = slotMute[s] ? "on" : "";
    $(`part${s}`).classList.toggle("muted", slotMute[s]);
    $(`gain${s}`).value = slotGain[s];
    $(`tc${s}`).textContent =
      PIECE && !PIECE.slotChannels[s].length ? "tacet" : "";
  });
}

async function setPatch(s, url) {
  slotPatch[s] = url;
  slotRev[s]++;
  if (PIECE) {
    // every instance wearing this slot
    const insts = [];
    PIECE.instSlot.forEach((slot, inst) => {
      if (slot === s) insts.push(inst);
    });
    await Promise.all(insts.map((i) => shipInstance(i, url)));
  }
  refreshRig();
}

function stepPatch(s, d) {
  const sel = $(`sel${s}`);
  sel.selectedIndex = Math.max(0, Math.min(sel.length - 1, sel.selectedIndex + d));
  setPatch(s, sel.value);
}

function muteToggle(s) {
  slotMute[s] = !slotMute[s];
  if (node) node.port.postMessage({ type: "mute", ch: s, v: slotMute[s] });
  refreshRig();
}

function setGain(s, v) {
  slotGain[s] = v;
  if (node) node.port.postMessage({ type: "gain", ch: s, v });
}

function renderStats() {
  const bits = [
    `peak ${STAT.peak.toFixed(3)}`,
    `engine load ${(STAT.load * 100).toFixed(0)}%`,
    `${SLOTS.length} voices · ${MANIFEST.nInstances} synths`,
  ];
  $("stats").innerHTML = bits.join(" · ") +
    (STAT.riding ? ' · <span class=clip>limiter riding</span>' : "");
}

// smooth progress: rAF between worklet position reports
function tick() {
  if (PIECE) {
    const len = PIECE.loopFrames / SR;
    const at = playhead();
    const p = Math.min(1, at / (len || 1));
    $("fill").style.width = (p * 100).toFixed(2) + "%";
    $("rail").setAttribute("aria-valuenow", Math.round(p * 100));
    $("tnow").textContent = fmt(at);
    $("tlen").textContent = fmt(len);
  }
  requestAnimationFrame(tick);
}

// The ledger: an append-only log. A rule logs one line when it ENTERS
// (rising edge over the why index), holds its place, and scrolls away
// like history — nothing flashes out. Backward seeks and piece changes
// rebuild the log up to the playhead, so the list is a pure function
// of (piece, time). Autoscroll sticks only while the reader is at the
// bottom.
const WHY_RE = /^([^:]+): (.*?)(?:\s*\[(.*)\])?$/;
const slotOf = (ch) => (PIECE && PIECE.chToSlot[ch]) ?? 3;
const ledger = { piece: null, pos: 0, lastT: -1, activeUntil: new Map() };
const REENTER_GAP = 1.5; // s of silence before a rule logs again

// Fast attack, slow release, perceptual (pow .4) width — a signal
// meter behind the fader, not a copy of it.
const METER = [0, 0, 0, 0];
function updateMeters(levels) {
  for (let s = 0; s < 4; s++) {
    METER[s] = Math.max(levels[s] || 0, METER[s] * 0.72);
    const el = $(`meter${s}`);
    if (el) el.style.width =
      (Math.min(1, METER[s]) ** 0.4 * 100).toFixed(1) + "%";
  }
}

function ledgerReset() {
  ledger.piece = PIECE && PIECE.name;
  ledger.pos = 0;
  ledger.lastT = -1;
  ledger.activeUntil.clear();
  $("whys").textContent = "";
}

function fmtT(t) {
  const m = Math.floor(t / 60), s = Math.floor(t % 60);
  return `${m}:${String(s).padStart(2, "0")}`;
}

function refreshWhys() {
  if (!PIECE) return;
  const box = $("whys");
  const t = playhead();
  if (ledger.piece !== PIECE.name || t < ledger.lastT - 0.3) ledgerReset();
  ledger.lastT = t;
  const stick =
    box.scrollTop + box.clientHeight >= box.scrollHeight - 30;
  const idx = PIECE.whyIdx;
  let appended = false;
  while (ledger.pos < idx.length && idx[ledger.pos][0] <= t) {
    const [on, off, ch, ws] = idx[ledger.pos++];
    for (const why of ws) {
      const m = WHY_RE.exec(why) || [null, why, "", ""];
      const s = slotOf(ch);
      const key = s + "|" + m[1];
      const until = ledger.activeUntil.get(key) ?? -Infinity;
      if (on > until + REENTER_GAP) {
        const el = document.createElement("div");
        el.className = "why in";
        el.style.setProperty("--vc", `var(--v${s})`);
        el.innerHTML = `<span class=at>${fmtT(on)}</span>`
          + `<span class=chip>${esc(SLOTS[s])}</span>`
          + `<span><b>${esc(m[1])}</b> <span class=delta>${esc(m[2] || "")}</span>`
          + (m[3] ? ` <span class=cite>${esc(m[3])}</span>` : "") + "</span>";
        box.appendChild(el);
        appended = true;
        while (box.children.length > 400) box.firstChild.remove();
      }
      ledger.activeUntil.set(key, Math.max(until, off));
    }
  }
  if (appended && stick) box.scrollTop = box.scrollHeight;
}

init();