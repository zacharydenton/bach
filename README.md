# otb

**An interpretation compiler: public-domain scores in, performances out.**

`otb` compiles `**kern` (Humdrum) encodings of Bach's keyboard works into
performed MIDI — articulation, ornament realization, agogics, dynamics and
temperament applied as versioned, diffable rules. All interpretive decisions
live in `config/default.toml`; every number in it regenerates from HEAD.

## Build

Use stack (it brings its own GHC; some distro GHCs — e.g. Arch's
dynamic-only build — cannot compile this):

```sh
stack build
git clone --depth 1 https://github.com/humdrum-tools/bach-wtc corpus/bach-wtc
git clone --depth 1 https://github.com/craigsapp/bach-370-chorales corpus/bach-chorales
git clone --depth 1 https://github.com/craigsapp/bach-musical-offering corpus/bach-musical-offering
stack test        # the corpus sweep needs the clone; OTB_NO_CORPUS=1 runs units only
stack run -- corpus/bach-wtc/kern/wtc1p01.krn -o bwv846.mid
```

Useful commands (`stack exec otb -- --help` lists everything):

```sh
otb compile SCORE.krn -o out.mid [--emit-json ir.json] [--emit-scl w3.scl]
otb explain SCORE.krn --bar 12       # why each note sounds the way it does
otb album corpus/bach-wtc/kern OUT/  # the whole corpus, in parallel
```

## The patchboard (a static site)

`site/` is a self-contained player: the Surge XT engine compiled to
WebAssembly renders the collections (the WTC, the 371 chorales, the
Musical Offering) in
the browser. Bake and serve:

```sh
stack exec otb -- bake-site           # builds wasm + regenerates the album
python3 -m http.server -d site 8877   # or any static host
```

The wasm build needs the surge fork (`~/code/surge`, branch
`wasm-headless-audioworklet`; override with `--surge-dir`). Details and
requirements: `site/README.md`.

## Offline rendering (WAV)

`tools/audition.py` renders a PerformanceIR to WAV through surgepy;
`tools/render_showcase.sh` and `tools/calibrate_patch.py` build on it.
They need a surgepy build:

```sh
git clone https://github.com/surge-synthesizer/surge && cd surge
git submodule update --init --recursive
# REQUIRED: stock surgepy has no setTempo — without this patch every
# tempo-synced LFO and delay drifts against the piece
git apply /path/to/otb/tools/surgepy-patches/0001-expose-tempo.patch
uv venv --python 3.11 ~/.venv-audition && uv pip install --python ~/.venv-audition/bin/python numpy
cmake -S . -B build -DSURGE_BUILD_PYTHON_BINDINGS=ON -DCMAKE_BUILD_TYPE=Release \
      -DCMAKE_POLICY_VERSION_MINIMUM=3.5 \
      -DPython_EXECUTABLE=~/.venv-audition/bin/python
cmake --build build --target surgepy -j
```

Patch library discovery: installed Surge XT locations are searched
automatically; a bare checkout works too, or set
`SURGE_PATCHES=/path/to/patches_factory`.

## The research loop

The fitting rigs are built into the compiler:

```sh
otb eval corpus/bach-wtc/kern              # beat-tempo scoreboard vs humans
otb eval corpus/bach-wtc/kern --velocity   # note-velocity scoreboard
otb fit corpus/bach-wtc/kern --dry-run     # per-piece fits (--apply writes them)
otb landscape corpus/bach-wtc/kern         # multi-start knob landscape
otb maestro-fetch                          # MAESTRO v3 archive + WTC catalog
otb maestro-align --validate               # aligner vs ASAP ground truth
otb maestro-align                          # emit corpus/maestro-wtc
otb bridge-dump SCORE.krn PERF.match       # score<->performance note pairs
```

Human data: `corpus/asap` (a clone of the ASAP dataset) plus
`corpus/maestro-wtc`, which `otb maestro-align` derives from MAESTRO v3.
`otb fit --apply` writes per-piece sections into `config/default.toml`
with `# PIECE-FIT` provenance; hand-authored keys always win and are
never touched.

Landscape runs checkpoint per start (resumable), refuse to run under an
unidentifiable build, and write a registered manifest — hashes,
arguments, train/test membership, results — into `experiments/`.
`--zero-floor`, `--diversity-bonus`, `--floor-only` and `--emit-elite`
select experimental conditions; see `experiments/*.notes.md` for the
findings to date.

## License

GPL-2.0-or-later.
