# The board with no server

A self-contained static build of the patchboard: the Surge XT engine
compiled to WebAssembly renders the album **in the listener's browser**,
from the same PerformanceIR JSON the live board plays. No stream, no
ffmpeg, no surgepy — a dumb file host is the whole deployment.

What ships here (committed):

- `index.html`, `app.css`, `app.js` — the board UI, ported from
  `tools/patchboard.py`'s PAGE. Same register-role routing, casting
  resolution, calibration compensation and why-ledger; the progress
  rail additionally **seeks**, which the streamed board never could,
  and the ledger works while paused — seek anywhere and study the
  moment.
- `board-processor.js` — an AudioWorkletProcessor hosting one
  `SurgeWasm` instance per MIDI channel (the live board's
  one-surgepy-instance-per-channel design), walking the pre-built
  event list in 32-frame engine blocks with the live board's riding
  limiter on the mix.
- `routing.js` + `routing.test.js` — the pure logic, mirrored from
  `tools/test_patchboard.py`'s cases; run with
  `node --test site/routing.test.js`.

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
Pages, or `tailscale serve` alike. The only server requirement is the
correct `application/wasm` MIME type (python's http.server and GitHub
Pages both comply). AudioWorklets need a secure context: https or
localhost.

Verified end-to-end with Playwright (12 wasm synth instances at ~14%
engine load; default rig seeds; wtc1f01's casting file overrides the
role carry; seek, pause/resume and the ledger all behave; console
clean).
