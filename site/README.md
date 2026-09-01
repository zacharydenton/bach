# The board with no server

A self-contained static build of the patchboard: the Surge XT engine
compiled to WebAssembly renders the album **in the listener's browser**,
from the same PerformanceIR JSON the live board plays. No stream, no
ffmpeg, no surgepy — a dumb file host is the whole deployment.

What ships here (committed):

- `index.html`, `app.css`, `app.js` — the board UI, ported from
  `tools/patchboard.py`'s PAGE and recast as a **fixed four-voice
  rig**: bass, tenor, alto, soprano, shown at all times, each with its
  own preset, mute and gain. Every piece's channels are distributed
  among the four by register rank (a slot with nothing to play reads
  *tacet*), and because the slots persist, the live board's role-stable
  routing falls out by construction — a preset you pick stands until a
  piece's own casting file overrides it. The progress rail **seeks**,
  which the streamed board never could, and the ledger works while
  paused — seek anywhere and study the moment.
- `board-processor.js` — an AudioWorkletProcessor hosting just TWO
  `SurgeWasm` instances: each carries two voices as scene A / scene B
  in MIDI channel-split mode. The fork's `loadScenePatches` merges two
  factory presets into one patch (the scene clipboard carries params,
  modulation, MSEGs, wavetables and the scene's insert FX; send/global
  FX come from the scene-A patch alone), and `renderScenes` taps each
  scene's post-insert-FX stereo separately, so per-voice gain and mute
  still happen at the worklet mix. Several score lanes share a voice's
  synth; native SCL tuning is what makes all of this safe — no
  per-note bends to fight over. Events walk in 32-frame engine blocks
  with the live board's riding limiter on the mix. "(init)" is a real
  `Init Saw.fxp` (baked as `data/init.fxp`), so it genuinely loads.
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
2026-09-01, replacing the live board's proxy:

```
sudo tailscale serve --bg --set-path /bach /home/zach/code/otb/site
``` The only server requirement is the
correct `application/wasm` MIME type (python's http.server and GitHub
Pages both comply). AudioWorklets need a secure context: https or
localhost.

Verified end-to-end with Playwright over the tailnet mount (two wasm
synths at ~4–6% engine load; the default rig seeds; wtc1f01's casting
lands one patch per slot; a hand-picked preset survives piece changes;
the two-voice fugue shows tenor and alto tacet; a soloed scene-B voice
is audible and a full mute is silent; seek, pause/resume and the
ledger all behave; console clean). The fork's node regression pins the
scene separation itself: Bass 1 on A, EP 2 on B — a channel-0 note
lands only in A's lanes.
