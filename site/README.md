# The patchboard

A self-contained static build of the player: the Surge XT engine
compiled to WebAssembly renders the album in the browser, from the same
PerformanceIR JSON the compiler emits. Any static file host is the
whole deployment.

## Layout

Committed:

- `index.html`, `app.css`, `app.js` — the UI: a fixed four-voice rig
  (bass, tenor, alto, soprano), each with its own preset, mute, gain
  and level meter. A slot with nothing to play in the current piece
  reads *tacet*; preset choices persist across pieces unless a piece's
  casting file overrides them.
- `board-processor.js` — the AudioWorkletProcessor driving the
  SurgeWasm instances.
- `routing.js`, `routing.test.js` — the pure routing/event logic.
  Run tests with `node --test site/routing.test.js`.

Baked (gitignored — regenerate, don't commit):

- `engine/` — `surge-worklet.{js,wasm}` + `worklet-shim.js`, built
  from the surge fork.
- `data/` — album IRs, `w3.scl` tuning, the factory patch bank,
  `patches.json`, `casting.json`, `calibration.json`, `manifest.json`.

## Bake and serve

```sh
stack exec otb -- bake-site               # builds wasm + regenerates IRs
stack exec otb -- bake-site --skip-wasm   # data only, reuse the engine
python3 -m http.server -d site 8877       # or any static host
```

Requirements:

- The surge fork at `~/code/surge`, branch `wasm-headless-audioworklet`
  (override with `--surge-dir`); emscripten on PATH or at
  `--emscripten-bin`.
- The server must send `application/wasm` for `.wasm` (python's
  http.server and GitHub Pages both do).
- AudioWorklets need a secure context: https or localhost.

All URLs are relative, so the site works from a subdirectory (e.g.
`tailscale serve --bg --set-path /bach .../otb/site`).

GitHub Pages: `site/deploy-gh-pages.sh` pushes the baked contents as a
single-commit orphan `gh-pages` branch —
https://zacharydenton.github.io/bach/ picks it up in a minute or two.
Bake first; deploy as often as you like, history stays one commit.

## Architecture notes

Each SurgeWasm instance hosts up to two score lanes of the same slot as
scene A / scene B in MIDI channel-split mode, so every lane keeps its
own voice pool while the instance outputs its preset's full native
signal path (scenes, insert/send/global FX, master) — which is why
factory presets sound like they do in Surge. Tuning is native SCL
(Werckmeister III), leaving MIDI channels free for scene routing. Seeks
re-strike notes that should already be sounding. Known gap: the wasm
build has no Lua, so formula-modulator motion (~2 factory patches) is
absent.
