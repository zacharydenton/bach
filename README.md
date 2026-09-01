# otb — One-Take Bach

**An interpretation compiler: public-domain scores in, performances out.**

Compiles `**kern` (Humdrum) encodings of Bach's keyboard works into performed
MIDI — articulation, ornament realization, agogics, dynamics and temperament
applied as versioned, diffable rules — for a six-voice analog ensemble
(Elektron Analog Four ×4, Behringer Model D, Bass Station II) or Surge XT.
The performance is recorded in **one simultaneous multitrack take**: the
machine never misses a note; the human conducts.

Wendy Carlos spent ~1,100 hours multitracking Switched-On Bach one monophonic
line at a time. This project's thesis is that the modern equivalent of her
method is a compiler, and the take is the only layer allowed to be
unreproducible.

```
 .krn ──[scratch lexer + spine machine]──► Score ──[Player over Euterpea's
        stems/beams stripped, ties merged,          Music algebra: the taste]──►
        every accidental explicit (kern law)
      ──► Performance ──► format-1 SMF ──► Surge XT / the hardware rig
```

## Why Haskell

Nothing here is computationally intensive — a fugue is a few thousand notes —
so the type system is the entire language criterion:

- **Units as newtypes** (`WholeNotes`, `Seconds`, `Ticks`): mixing notated and
  performed time fails to compile.
- **Instrument capabilities as classes** (planned M6): the Model D takes no
  velocity and no CC — a velocity lane aimed at it is a compile error.
- **Counterpoint as a parser oracle** (planned M1): Bach doesn't write
  parallel fifths; if the parse contains them, the parser is wrong. The mezzo
  idea, inverted.
- **EuterpeaLite** supplies the `Music` algebra (`:+:`/`:=:`) — Euterpea with
  the PortMidi/realtime half removed; this compiler emits files and never
  opens a MIDI port. The SMF writer is ours: temperament (M3) needs per-note
  pitch bend interleaved per channel, which Euterpea's export cannot do.

## Build

Arch's system GHC ships dynamic-only artifacts and cannot build this; use
stack (it brings its own GHC — see the comments in `stack.yaml`):

```sh
stack build
git clone --depth 1 https://github.com/humdrum-tools/bach-wtc corpus/bach-wtc
stack test   # the corpus sweep is part of the suite; OTB_NO_CORPUS=1 runs units only
stack run -- corpus/bach-wtc/kern/wtc1p01.krn -o bwv846.mid
```

Reference for A/B: `../bcrsim/switched_on_bach_bwv846.wav` — the same prelude,
programmed into an emulated BCR2000 by OSC gestures and sequenced by real
firmware. The compiler exists to replace that 107-second gesture session with
a build step.

## The patchboard (a static site)

The board lives in `site/` and renders the whole album with the Surge
engine compiled to WebAssembly — all audio synthesized client-side, any
dumb static host is the whole deployment. `python3 tools/bake_site.py`
fills it; `site/README.md` has the details. (An earlier server-rendered
patchboard streamed Opus from surgepy; it was retired 2026-09-01 once
the static board reached parity and beyond — seeking, per-lane
polyphony, the full native FX path.)

## Offline rendering (audition.py)

`tools/audition.py` renders a PerformanceIR to WAV through surgepy, and
`tools/render_showcase.sh` / `tools/calibrate_patch.py` build on it. They
need a surgepy build (no platform branches — works on macOS too):

```sh
# once: xcode-select --install ; brew install cmake ninja ; uv python install 3.11
git clone https://github.com/surge-synthesizer/surge && cd surge
git submodule update --init --recursive
# REQUIRED: stock surgepy has no setTempo — without this patch every
# tempo-synced LFO and delay drifts against the piece (audition warns,
# but warns is all it can do)
git apply /path/to/otb/tools/surgepy-patches/0001-expose-tempo.patch
uv venv --python 3.11 ~/.venv-audition && uv pip install --python ~/.venv-audition/bin/python numpy
cmake -S . -B build -DSURGE_BUILD_PYTHON_BINDINGS=ON -DCMAKE_BUILD_TYPE=Release \
      -DCMAKE_POLICY_VERSION_MINIMUM=3.5 \
      -DPython_EXECUTABLE=~/.venv-audition/bin/python
cmake --build build --target surgepy -j
```

Patch library discovery: installed Surge XT locations are searched
automatically; a bare checkout works too, or set
`SURGE_PATCHES=/path/to/patches_factory`. Vendored pybind11 caps the
venv at Python 3.11.

## Status

M0–M5 land (spine to sound, articulation, temperament, agogics, ornaments
including grace notes, dynamics, the expressive layer) plus the M6a/b
audition path. Still open: M6 proper (instrument capability classes, the
hardware rig). The design document lives outside this repo.

License: GPL-2.0-or-later.
