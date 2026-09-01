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
- `board-processor.js` — an AudioWorkletProcessor hosting four
  `SurgeWasm` instances (several score lanes share a voice's synth;
  native SCL tuning is what makes that safe — no per-note bends to
  fight over), walking the pre-built event list in 32-frame engine
  blocks with the live board's riding limiter on the mix.
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

Verified end-to-end with Playwright over the tailnet mount (four wasm
synths at ~6–8% engine load; the default rig seeds; wtc1f01's casting
lands one patch per slot; a hand-picked preset survives piece changes;
the two-voice fugue shows tenor and alto tacet; seek, pause/resume and
the ledger all behave; console clean).
