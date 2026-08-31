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
# resolve OUTDIR against the caller's cwd before moving to the repo root
case "$OUT" in /*) ;; *) OUT=$PWD/$OUT ;; esac
ROOT=$(cd "$(dirname "$0")/.." && pwd)
cd "$ROOT"
SURGEPY_DIR=${SURGEPY_DIR:-$HOME/.local/share/otb/surgepy}
VENV=${VENV:-.venv-audition}
FP=/usr/share/surge-xt/patches_factory
PY="$VENV/bin/python"

mkdir -p "$OUT"
# One process, all cores: otb album emits every .mid/.json plus w3.scl.
stack run -- album corpus/bach-wtc/kern "$OUT"

# Prelude: one texture, all five lanes on the same warm pluck.
PYTHONPATH=$SURGEPY_DIR $PY tools/audition.py "$OUT/wtc1p01.json" \
  --scl "$OUT/w3.scl" -o "$OUT/wtc1p01_surge_cast.wav" \
  --patch "$FP/Plucks/Nice Pluck 2.fxp"

# Fugue: the canonical voicing lives in config/casting/wtc1f01.json —
# the same file the patchboard preloads. One source, two consumers.
# A plain $(...) rather than a process substitution: a failure inside
# < <(...) is invisible to set -e and the fugue would silently render
# on the init patch.
CASTING=config/casting/wtc1f01.json
[ -r "$CASTING" ] || { echo "missing casting file: $ROOT/$CASTING" >&2; exit 1; }
CAST_LINES=$("$PY" -c "
import json
c = json.load(open('$CASTING'))
for k, v in sorted(c.items()):
    if k.isdigit():
        print(f'{k}:{v}')")
[ -n "$CAST_LINES" ] || { echo "no channel entries in $CASTING" >&2; exit 1; }
CAST_ARGS=()
while IFS= read -r line; do
  CAST_ARGS+=(--patch-ch "$line")
done <<< "$CAST_LINES"
PYTHONPATH=$SURGEPY_DIR $PY tools/audition.py "$OUT/wtc1f01.json" \
  --scl "$OUT/w3.scl" -o "$OUT/wtc1f01_surge_cast.wav" "${CAST_ARGS[@]}"
