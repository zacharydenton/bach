#!/usr/bin/env bash
# Regenerate the Inventions & Sinfonias kern from MuseData.
#
# Source: https://bitbucket.org/musedata/bach (rasmuss/inventio,
# rasmuss/sinfonie — S. Rasmussen's encodings, CCARH). Converter:
# musedata2hum from https://github.com/craigsapp/humlib.
# The converted kern is COMMITTED here (unlike the cloned corpora)
# because the source is not a kern repo; this script is the provenance.
#
# Usage: regenerate.sh /path/to/musedata-bach /path/to/musedata2hum
set -euo pipefail
SRC=${1:?musedata bach checkout}; M2H=${2:?musedata2hum binary}
OUT=$(cd "$(dirname "$0")/kern" && pwd)

# musedata2hum's grace-note handling is unfinished and leaks debug
# prints into stdout (sometimes glued to the first record). Scrub
# them; the affected movements (BWV 776, 797) convert with a few
# grace-adjacent tokens nulled — a documented fidelity caveat, not
# silence.
scrub() {
  sed -e 's/^.*\(!!!COM\)/\1/' -e 's/^.*\(!!!OTL\)/\1/' \
    | grep -vE '^(PROCESS GRACE NOTE HERE|GRID STAFF:|Warning,)'
}

convert() { # prefer the score-membership file; else concat the parts
  local rel=$1 slug=$2 bwv=$3
  local st="$SRC/$rel/stage2" mdir tmp
  if [ -d "$st/01" ]; then mdir="$st/01"; else mdir="$st"; fi
  local out="$OUT/$slug.krn"
  if [ -f "$mdir/s01" ]; then
    "$M2H" "$mdir/s01" 2>/dev/null | scrub > "$out"
  elif [ -f "$mdir/01" ] && grep -q "^score:" "$mdir/01"; then
    "$M2H" "$mdir/01" 2>/dev/null | scrub > "$out"
  else
    tmp=$(mktemp)
    cat "$mdir"/0[0-9] > "$tmp"
    "$M2H" -g data "$tmp" 2>/dev/null | scrub > "$out"
    rm -f "$tmp"
  fi
  printf '!!!SCT: BWV %s\n' "$bwv" >> "$out"
}

for d in "$SRC"/rasmuss/inventio/*/; do
  id=$(basename "$d"); bwv=${id#0}
  convert "rasmuss/inventio/$id" "inv$id" "$bwv"
done
for d in "$SRC"/rasmuss/sinfonie/*/; do
  id=$(basename "$d"); bwv=${id#0}
  convert "rasmuss/sinfonie/$id" "sinf$id" "$bwv"
done
ls "$OUT" | wc -l
