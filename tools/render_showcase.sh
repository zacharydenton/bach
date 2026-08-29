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

# Fugue: four voices, four timbres. KERN ORDERS SPINES BASS-FIRST:
#   voice 0 (BASS) ch0    | voice 1 (tenor) ch1
#   voice 2 (alto) ch2,3  | voice 3 (SOPRANO) ch4,5
PYTHONPATH=$SURGEPY_DIR $PY tools/audition.py "$OUT/wtc1f01.json" \
  --scl "$OUT/w3.scl" -o "$OUT/wtc1f01_surge_cast.wav" \
  --patch-ch "0:$FP/Basses/Bass 1.fxp" \
  --patch-ch "1:$FP/Leads/Smoothy Hollow.fxp" \
  --patch-ch "2:$FP/Winds/Clarinet.fxp" \
  --patch-ch "3:$FP/Winds/Clarinet.fxp" \
  --patch-ch "4:$FP/Leads/Classic Lead 1.fxp" \
  --patch-ch "5:$FP/Leads/Classic Lead 1.fxp"
