# The board with no server

A self-contained static build of the patchboard: the Surge XT engine
compiled to WebAssembly renders the album **in the listener's browser**,
from the same PerformanceIR JSON the interpreter emits. No stream, no
ffmpeg, no surgepy — a dumb file host is the whole deployment.

What ships here (committed):

- `index.html`, `app.css`, `app.js` — the board UI, ported from
  the retired live patchboard's PAGE and recast as a **fixed four-voice
  rig**: bass, tenor, alto, soprano, shown at all times, each with its
  own preset, mute and gain. Every piece's channels are distributed
  among the four by register rank (a slot with nothing to play reads
  *tacet*), and because the slots persist, the live board's role-stable
  routing falls out by construction — a preset you pick stands until a
  piece's own casting file overrides it. The progress rail **seeks**,
  which the streamed board never could, and the ledger works while
  paused — seek anywhere and study the moment.
- `board-processor.js` — an AudioWorkletProcessor hosting one scene
  per LANE: each `SurgeWasm` instance carries up to two score lanes
  OF THE SAME SLOT as scene A / scene B in MIDI channel-split mode
  (the fork's `loadScenePatches`, same preset on both scenes), so
  every lane keeps its own voice pool — a mono preset stays mono per
  lane, and overlapping same-pitch notes in different lanes cannot
  release each other — while the instance's ordinary output is its
  preset's FULL native signal path: scenes, insert FX, send FX,
  global FX, master volume. That last part is why presets sound like
  Surge: 477 of 627 factory patches keep reverb/delay in send or
  global slots, and an earlier per-scene tap dropped all of it
  (measured: the paired path renders EP 2 at 102% of a native
  instance's RMS). Slot gain and mute apply per instance at the
  worklet mix; native SCL tuning frees the MIDI channels for scene
  routing. Events walk in 32-frame engine blocks with the live
  board's riding limiter; seeks re-strike the notes that should
  already be sounding. "(init)" is a real `Init Saw.fxp` (baked as
  `data/init.fxp`), so it genuinely loads. Wavetables need no asset
  shipping: Surge embeds them in the `.fxp` itself. Known gap: the
  wasm build has no Lua, so formula-modulator motion (≈2 factory
  patches) is absent.
- `routing.js` + `routing.test.js` — the pure logic (slot
  distribution, casting-to-slot resolution, calibration, event build);
  run with `node --test site/routing.test.js`.

What the bake produces (gitignored — regenerate, don't commit):

- `engine/` — `surge-worklet.{js,wasm}` + `worklet-shim.js` from the
  surge fork (`~/code/surge`, branch `wasm-headless-audioworklet`,
  which carries `loadSCLString` for the Werckmeister III tuning).
- `data/` — the album IRs, `w3.scl`, the factory patch bank,
  `patches.json`, `casting.json`, `calibration.json`, `manifest.json`.

Bake and serve:

```
python3 tools/bake_site.py                # builds wasm + regenerates IRs
python3 tools/bake_site.py --skip-wasm --perf-dir ~/.local/share/otb/perf
python3 -m http.server -d site 8877      # or any static host
```

All URLs are relative, so the site works from a subdirectory, GitHub
Pages, or `tailscale serve` alike. Deployed on the tailnet since
2026-09-01, replacing the (since retired) live board's proxy:

```
sudo tailscale serve --bg --set-path /bach /home/zach/code/otb/site
```

The only server requirement is the correct `application/wasm` MIME
type (python's http.server and GitHub Pages both comply). AudioWorklets
need a secure context: https or localhost.

Verified end-to-end with Playwright over the tailnet mount (the default
rig seeds; wtc1f01's casting lands one patch per slot; a hand-picked
preset survives piece changes; the two-voice fugue shows tenor and alto
tacet; a soloed voice is audible and a full mute is silent; a mid-piece
seek re-strikes the notes that should already be sounding; pause/resume
and the ledger all behave; console clean). The fork's node regression
pins the scene separation itself: Bass 1 on A, EP 2 on B — a channel-0
note lands only in A's lanes.
