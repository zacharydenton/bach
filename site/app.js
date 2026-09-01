/*
 * The static patchboard's main thread: fetches the baked album, owns the
 * patch/gain/mute state and the role-stable routing, and drives the worklet
 * (board-processor.js) with pre-built event lists. The UI is the live
 * board's page (tools/patchboard.py PAGE) minus everything server-born —
 * and the progress rail seeks, which the streamed board never could.
 */
import {
  partsOf, carryRoles, resolveCasting, buildEvents,
  whyIndex, whysAt, pieceTitle,
} from "./routing.js";

let MANIFEST, CASTINGS, CAL, CATS;
let ctx, node, SR;
let PLAYING = false;
let PIECE = null; // {idx, name, parts, whyIdx, loopFrames}
let POS = { frame: 0, t: Date.now(), playing: false };
let STAT = { peak: 0, riding: false, load: 0 };
const chState = { gain: {}, mute: {}, patchPath: {}, patchName: {} };
const castState = { defaultDone: false };
let rankChannels = null;
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
    `${MANIFEST.pieces.length} pieces baked · ${MANIFEST.nInstances} Surge` +
    ` instances will render in this tab — press Start`;

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
    const wasmBinary = await (await fetch("engine/surge-worklet.wasm")).arrayBuffer();
    await ctx.audioWorklet.addModule("board-processor.js");
    node = new AudioWorkletNode(ctx, "board", {
      numberOfInputs: 0,
      numberOfOutputs: 1,
      outputChannelCount: [2],
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
  const { parts, order } = partsOf(perf);
  for (const part of parts) {
    for (const ch of part.channels) {
      chState.gain[ch] ??= 0.25; // summing headroom (live board default)
      chState.mute[ch] ??= false;
      chState.patchPath[ch] ??= "(init)";
    }
  }

  // role-stable routing: snapshot outgoing roles BEFORE the map changes
  const prevRoles = rankChannels
    ? rankChannels.map((ch) => ({
        patch: chState.patchPath[ch] ?? "(init)",
        gain: chState.gain[ch] ?? 0.25,
        mute: chState.mute[ch] ?? false,
      }))
    : null;
  const carried = carryRoles(prevRoles, parts, order, chState);
  rankChannels = carried.rankChannels;
  const eff = resolveCasting(CASTINGS, p.name, parts, carried.roleBase,
    chState.patchPath, castState, MANIFEST.nInstances);
  for (const [chS, url] of Object.entries(eff)) {
    if (url !== chState.patchPath[+chS]) await applyPatch(+chS, url);
    else chState.patchPath[+chS] = url;
  }

  // calibration compensation sees the patches that will play
  const score = buildEvents(perf, SR, eff, CAL, true);
  PIECE = {
    idx, name: p.name, parts,
    whyIdx: whyIndex(perf), loopFrames: score.loopFrames,
  };
  node.port.postMessage({ type: "score", ...score }, [
    score.frames.buffer, score.kinds.buffer, score.chans.buffer,
    score.a.buffer, score.b.buffer,
    score.tempoFrames.buffer, score.tempoBpm.buffer,
  ]);
  for (const part of parts) {
    for (const ch of part.channels) {
      node.port.postMessage({ type: "gain", ch, v: chState.gain[ch] });
      node.port.postMessage({ type: "mute", ch, v: !!chState.mute[ch] });
    }
  }
  POS = { frame: 0, t: Date.now(), playing: PLAYING };
  renderHeader();
  renderParts();
  if (PLAYING) node.port.postMessage({ type: "play" });
}

async function applyPatch(ch, url) {
  chState.patchPath[ch] = url;
  chState.patchName[ch] = patchDisplay(url);
  // "(init)" is the booted state; once a patch is loaded there is nothing
  // to reload, so it only means "leave this instance alone" (as live)
  if (url === "(init)" || !node) return;
  let bytes = patchCache.get(url);
  if (!bytes) {
    bytes = await (await fetch(encUrl(url))).arrayBuffer();
    patchCache.set(url, bytes);
  }
  const copy = new Uint8Array(bytes.slice(0)); // transfer must not eat the cache
  node.port.postMessage(
    { type: "patch", ch, bytes: copy, name: chState.patchName[ch] },
    [copy.buffer],
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

function renderParts() {
  const div = $("parts");
  div.innerHTML = PIECE.parts.map((p, i) => `
    <div class=part id=part${i} style="--vc:var(--v${i % 6})">
     <div class=head>
      <b id=nm${i}>${esc(p.name)}</b>
      <span class=patch id=pn${i}></span>
     </div>
     <div class=row>
      <button data-step=-1 data-part=${i} aria-label="previous patch">◀</button>
      <select id=sel${i} data-part=${i} style="flex:1">${opts()}</select>
      <button data-step=1 data-part=${i} aria-label="next patch">▶</button>
      <button id=mute${i} data-mute=${i}>Mute</button>
      <input type=range min=0 max=1.5 step=0.05 id=gain${i}
        data-gain=${i} aria-label="gain">
     </div>
    </div>`).join("");
  div.querySelectorAll("[data-step]").forEach((b) =>
    b.addEventListener("click", () =>
      stepPatch(+b.dataset.part, +b.dataset.step)));
  div.querySelectorAll("select[data-part]").forEach((s) =>
    s.addEventListener("change", () => setPatch(+s.dataset.part, s.value)));
  div.querySelectorAll("[data-mute]").forEach((b) =>
    b.addEventListener("click", () => muteToggle(+b.dataset.mute)));
  div.querySelectorAll("[data-gain]").forEach((r) =>
    r.addEventListener("input", () =>
      setGain(+r.dataset.gain, parseFloat(r.value))));
  refreshPartRows();
}

function refreshPartRows() {
  PIECE.parts.forEach((p, i) => {
    const ch0 = p.channels[0];
    const path = chState.patchPath[ch0] ?? "(init)";
    $(`pn${i}`).textContent = patchDisplay(path);
    const sel = $(`sel${i}`);
    if (document.activeElement !== sel && sel.value !== path) sel.value = path;
    const muted = !!chState.mute[ch0];
    $(`mute${i}`).className = muted ? "on" : "";
    $(`part${i}`).classList.toggle("muted", muted);
    $(`gain${i}`).value = chState.gain[ch0] ?? 0.25;
  });
  $("cast").textContent = PIECE.parts.map((p) => {
    const path = chState.patchPath[p.channels[0]] ?? "(init)";
    if (path === "(init)") return "";
    return p.channels.map((c) => `--patch-ch "${c}:${bankTail(path)}"`).join(" ");
  }).filter(Boolean).join(" \\\n");
}

async function setPatch(i, url) {
  for (const ch of PIECE.parts[i].channels) await applyPatch(ch, url);
  refreshPartRows();
}

function stepPatch(i, d) {
  const s = $(`sel${i}`);
  s.selectedIndex = Math.max(0, Math.min(s.length - 1, s.selectedIndex + d));
  setPatch(i, s.value);
}

function muteToggle(i) {
  const v = !chState.mute[PIECE.parts[i].channels[0]];
  for (const ch of PIECE.parts[i].channels) {
    chState.mute[ch] = v;
    node.port.postMessage({ type: "mute", ch, v });
  }
  refreshPartRows();
}

function setGain(i, v) {
  for (const ch of PIECE.parts[i].channels) {
    chState.gain[ch] = v;
    node.port.postMessage({ type: "gain", ch, v });
  }
}

function renderStats() {
  const bits = [
    `peak ${STAT.peak.toFixed(3)}`,
    `engine load ${(STAT.load * 100).toFixed(0)}%`,
    `${MANIFEST.nInstances} synths`,
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
// only entering lines rise in and departing lines fade out. Unlike the live
// board this also works paused — seek anywhere and study the moment.
const WHY_RE = /^([^:]+): (.*?)(?:\s*\[(.*)\])?$/;
function partIndexOf(ch) {
  if (!PIECE) return 0;
  for (let i = 0; i < PIECE.parts.length; i++)
    if (PIECE.parts[i].channels.includes(ch)) return i;
  return 0;
}
function chipName(ch) {
  const i = partIndexOf(ch);
  return PIECE ? PIECE.parts[i].name.split(" ")[0] : "ch" + ch;
}
function refreshWhys() {
  if (!PIECE) return;
  const box = $("whys");
  const ws = whysAt(PIECE.whyIdx, playhead());
  const seen = new Map();
  for (const w of ws) {
    const m = WHY_RE.exec(w.why) || [null, w.why, "", ""];
    const key = w.ch + "|" + m[1];
    if (!seen.has(key))
      seen.set(key, { ch: w.ch, rule: m[1], delta: m[2] || "", cite: m[3] || "" });
  }
  const want = [...seen.entries()].sort(
    (a, b) => a[1].ch - b[1].ch || a[1].rule.localeCompare(b[1].rule)).slice(0, 6);
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
    el.style.setProperty("--vc", `var(--v${partIndexOf(w.ch) % 6})`);
    el.innerHTML = `<span class=chip>${esc(chipName(w.ch))}</span>`
      + `<span><b>${esc(w.rule)}</b> <span class=delta>${esc(w.delta)}</span>`
      + (w.cite ? ` <span class=cite>${esc(w.cite)}</span>` : "") + "</span>";
    box.appendChild(el);
  }
}

init();
