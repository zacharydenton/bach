#!/usr/bin/env bash
# Deploy the baked board to GitHub Pages.
#
# Publishes the CONTENTS of site/ (including the gitignored data/ and
# engine/ build products) as a single-commit orphan gh-pages branch —
# history stays one commit deep no matter how often you deploy.
#
# Usage: bake first, then  site/deploy-gh-pages.sh
set -euo pipefail

ROOT=$(cd "$(dirname "$0")/.." && pwd)
SITE="$ROOT/site"

[ -f "$SITE/data/manifest.json" ] || {
  echo "no bake in site/data — run: stack exec otb -- bake-site" >&2; exit 1; }
[ -f "$SITE/engine/surge-worklet.wasm" ] || {
  echo "no engine in site/engine — run a bake without --skip-wasm" >&2; exit 1; }

TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

cp -r "$SITE"/. "$TMP"/
rm -f "$TMP"/deploy-gh-pages.sh "$TMP"/README.md
touch "$TMP/.nojekyll"   # patch paths contain spaces; keep Jekyll out of it

git -C "$TMP" init -q -b gh-pages
git -C "$TMP" add -A
git -C "$TMP" commit -qm "bake $(git -C "$ROOT" rev-parse --short HEAD) ($(date +%F))"
git -C "$TMP" push -f "$(git -C "$ROOT" remote get-url origin)" gh-pages

echo "deployed: https://$(git -C "$ROOT" remote get-url origin \
  | sed -E 's#.*[:/]([^/]+)/([^/.]+)(\.git)?$#\1.github.io/\2#')/"
