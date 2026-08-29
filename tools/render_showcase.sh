#!/usr/bin/env bash
# Showcase renders through Surge XT with a Switched-On-Bach-style casting.
#
# Voice casting follows the Carlos discipline — the ear must be able to
# follow every line by timbre alone:
#   soprano  bright singing lead    alto  hollow reed (the clarinet square)
#   tenor    warm and hollow        bass  Moog-weight
#
# Channel map comes from otb's lane allocation (voice 0 first), printed by
# the JSON; per-piece below.
#
# Usage: tools/render_showcase.sh OUTDIR
# Env:   SURGEPY_DIR (dir containing surgepy*.so), VENV (python env)
# License: GPL-2.0-or-later.
set -euo pipefail

OUT=${1:?usage: render_showcase.sh OUTDIR}
SURGEPY_DIR=${SURGEPY_DIR:-$(dirname "$(find ~/code/surge-src/ignore/bpy -name 'surgepy*.so' | head -1)")}
VENV=${VENV:-.venv-audition}
FP=/usr/share/surge-xt/patches_factory
PY="$VENV/bin/python"

stack run -- corpus/bach-wtc/kern/wtc1p01.krn -o "$OUT/wtc1p01.mid" \
  --emit-json "$OUT/wtc1p01.json" --emit-scl "$OUT/w3.scl" | grep -v '^Stack' || true
stack run -- corpus/bach-wtc/kern/wtc1f01.krn -o "$OUT/wtc1f01.mid" \
  --emit-json "$OUT/wtc1f01.json" | grep -v '^Stack' || true

# Prelude: one texture, all five lanes on the same warm pluck.
PYTHONPATH=$SURGEPY_DIR $PY tools/audition.py "$OUT/wtc1p01.json" \
  --scl "$OUT/w3.scl" -o "$OUT/wtc1p01_surge_cast.wav" \
  --patch "$FP/Plucks/Nice Pluck 2.fxp"

# Fugue: the canonical voicing lives in config/casting/wtc1f01.json —
# the same file the patchboard preloads. One source, two consumers.
CAST_ARGS=$("$PY" -c "
import json
c = json.load(open('config/casting/wtc1f01.json'))
print(' '.join(f'--patch-ch \"{k}:{v}\"' for k, v in sorted(c.items()) if k.isdigit()))")
eval PYTHONPATH=\"$SURGEPY_DIR\" \"$PY\" tools/audition.py \"$OUT/wtc1f01.json\" \
  --scl \"$OUT/w3.scl\" -o \"$OUT/wtc1f01_surge_cast.wav\" $CAST_ARGS
