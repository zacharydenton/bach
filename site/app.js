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
  SLOTS, slotMap, resolveSlots, buildEvents,
  whyIndex, whysAt, pieceTitle,
} from "./routing.js";

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
    .map((p) => `<option>${esc(p.name)}</option>`).join("");
  const t = pieceTitle(MANIFEST.pieces[0].name);
  $("title").textContent = t.main;
  $("idx").textContent = `${t.bwv ? t.bwv + " · " : ""}1 / ${MANIFEST.pieces.length}`;
  $("stats").textContent =
    `${MANIFEST.pieces.length} pieces baked · four voices on two Surge` +
    ` synths will render in this tab — press Start`;
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
    ctx = new AudioContext();
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
      processorOptions: { wasmBinary },
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
    await loadPiece(0);
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
    renderStats();
  } else if (d.type === "ended") {
    loadPiece((PIECE.idx + 1) % MANIFEST.pieces.length);
  } else if (d.type === "scl") {
    if (d.err) $("stats").textContent = "SCL error: " + d.err;
  } else if (d.type === "error") {
    $("stats").textContent = "engine error: " + d.message;
  }
}

async function loadPiece(idx) {
  const p = MANIFEST.pieces[idx];
  node.port.postMessage({ type: "pause" });
  const perf = await (await fetch(encUrl(p.url))).json();
  const { chToSlot, slotChannels } = slotMap(perf);

  const eff = resolveSlots(CASTINGS, p.name, slotChannels, slotPatch,
    castState);
  const dirty = new Set();
  for (let s = 0; s < SLOTS.length; s++) {
    if (eff[s] !== slotPatch[s]) {
      slotPatch[s] = eff[s];
      dirty.add(s >> 1);
    }
  }
  for (const inst of dirty) await shipInstance(inst);

  // calibration compensation sees the patches that will play
  const score = buildEvents(perf, SR, chToSlot, eff, CAL, true);
  PIECE = {
    idx, name: p.name, chToSlot, slotChannels,
    whyIdx: whyIndex(perf), loopFrames: score.loopFrames,
  };
  node.port.postMessage({ type: "score", ...score }, [
    score.frames.buffer, score.kinds.buffer, score.chans.buffer,
    score.a.buffer, score.b.buffer,
    score.tempoFrames.buffer, score.tempoBpm.buffer,
  ]);
  for (let s = 0; s < SLOTS.length; s++) {
    node.port.postMessage({ type: "gain", ch: s, v: slotGain[s] });
    node.port.postMessage({ type: "mute", ch: s, v: slotMute[s] });
  }
  POS = { frame: 0, t: Date.now(), playing: PLAYING };
  renderHeader();
  refreshRig();
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

// The scene merge is a whole-patch operation, so an instance's two voices
// always ship together: slot 2i as scene A, slot 2i+1 as scene B.
async function shipInstance(inst) {
  if (!node) return;
  const urlA = slotPatch[inst * 2];
  const urlB = slotPatch[inst * 2 + 1];
  const [bytesA, bytesB] = await Promise.all([patchBytes(urlA), patchBytes(urlB)]);
  const copyA = new Uint8Array(bytesA.slice(0)); // transfer must not eat the cache
  const copyB = new Uint8Array(bytesB.slice(0));
  node.port.postMessage(
    { type: "patches", inst, bytesA: copyA, nameA: patchDisplay(urlA),
      bytesB: copyB, nameB: patchDisplay(urlB) },
    [copyA.buffer, copyB.buffer],
  );
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
  if (node && PIECE && i !== PIECE.idx) loadPiece(i);
}

function pieceStep(d) {
  if (!PIECE) return;
  loadPiece((PIECE.idx + d + MANIFEST.pieces.length) % MANIFEST.pieces.length);
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
      <input type=range min=0 max=1.5 step=0.05 id=gain${s}
        data-gain=${s} aria-label="gain">
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
  $("cast").textContent = !PIECE ? "" : SLOTS.map((_, s) => {
    if (slotPatch[s] === "(init)") return "";
    return PIECE.slotChannels[s]
      .map((c) => `--patch-ch "${c}:${bankTail(slotPatch[s])}"`).join(" ");
  }).filter(Boolean).join(" \\\n");
}

async function setPatch(s, url) {
  slotPatch[s] = url;
  await shipInstance(s >> 1);
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
    `${SLOTS.length} voices · 2 synths`,
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

// The ledger: keyed per-line DOM. A rule that keeps applying HOLDS STILL;
// only entering lines rise in and departing lines fade out. Chips wear the
// slot the note sounds on; rules are deduped per slot.
const WHY_RE = /^([^:]+): (.*?)(?:\s*\[(.*)\])?$/;
const slotOf = (ch) => (PIECE && PIECE.chToSlot[ch]) ?? 3;
function refreshWhys() {
  if (!PIECE) return;
  const box = $("whys");
  const ws = whysAt(PIECE.whyIdx, playhead());
  const seen = new Map();
  for (const w of ws) {
    const m = WHY_RE.exec(w.why) || [null, w.why, "", ""];
    const s = slotOf(w.ch);
    const key = s + "|" + m[1];
    if (!seen.has(key))
      seen.set(key, { slot: s, rule: m[1], delta: m[2] || "", cite: m[3] || "" });
  }
  const want = [...seen.entries()].sort(
    (a, b) => a[1].slot - b[1].slot
      || a[1].rule.localeCompare(b[1].rule)).slice(0, 6);
  const wantKeys = new Set(want.map(([k]) => k));
  for (const el of [...box.children]) {
    if (!wantKeys.has(el.dataset.key) && !el.classList.contains("out")) {
      el.classList.add("out");
      setTimeout(() => el.remove(), 260);
    }
  }
  const have = new Set([...box.children]
    .filter((el) => !el.classList.contains("out")).map((el) => el.dataset.key));
  for (const [key, w] of want) {
    if (have.has(key)) continue;
    const el = document.createElement("div");
    el.className = "why in";
    el.dataset.key = key;
    el.style.setProperty("--vc", `var(--v${w.slot})`);
    el.innerHTML = `<span class=chip>${esc(SLOTS[w.slot])}</span>`
      + `<span><b>${esc(w.rule)}</b> <span class=delta>${esc(w.delta)}</span>`
      + (w.cite ? ` <span class=cite>${esc(w.cite)}</span>` : "") + "</span>";
    box.appendChild(el);
  }
}

init();
