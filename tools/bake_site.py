#!/usr/bin/env python3
"""Bake the static patchboard: site/ becomes a self-contained board.

The live patchboard (tools/patchboard.py) renders on the server; the
static board renders in the listener's browser via the surge fork's
WebAssembly build (~/code/surge, branch wasm-headless-audioworklet).
Everything the server used to know is baked into site/data/:

  data/perf/<piece>.json   the PerformanceIRs, regenerated from HEAD
                           (otb album) unless --perf-dir copies a render
  data/w3.scl              the temperament (client loads it per synth)
  data/patches/...         the Surge factory bank, category/name layout
  data/patches.json        {category: [{name, url}]}
  data/casting.json        config/casting/*.json with paths rewritten
                           to site-relative patch urls
  data/calibration.json    config/calibration.json rekeyed to bank
                           tails (Category/Name.fxp)
  data/manifest.json       ordered album + per-piece endS/maxCh +
                           global nInstances

site/engine/ receives the wasm build artifacts (surge-worklet.js,
surge-worklet.wasm, worklet-shim.js). Neither data/ nor engine/ is
committed; this script regenerates both.

  python3 tools/bake_site.py             # everything
  python3 tools/bake_site.py --skip-wasm --perf-dir ~/.local/share/otb/perf
"""
import argparse
import json
import os
import shutil
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
sys.path.insert(0, HERE)
from patchboard import patch_dirs, scan_patches  # noqa: E402

WASM_FILES = ["surge-worklet.js", "surge-worklet.wasm", "worklet-shim.js"]

# The emcmake configure from the fork's src/surge-wasm/README.md.
WASM_CMAKE_FLAGS = [
    "-DCMAKE_BUILD_TYPE=Release",
    "-DSURGE_BUILD_32BIT_LINUX=TRUE",
    "-DSURGE_SKIP_JUCE_FOR_RACK=TRUE",
    "-DSURGE_SKIP_LUA=TRUE",
    "-DSURGE_SKIP_ODDSOUND_MTS=TRUE",
    "-DSURGE_BUILD_XT=OFF",
    "-DSURGE_BUILD_FX=OFF",
    "-DSURGE_BUILD_TESTRUNNER=OFF",
    "-DSURGE_SKIP_WERROR=TRUE",
    "-DENABLE_LTO=OFF",
    "-DSURGE_BUILD_WASM=ON",
]


def album_key(path):
    # mirrors patchboard.py main(): prelude before fugue per key number,
    # non-WTC extras alphabetically at the end
    b = os.path.splitext(os.path.basename(path))[0]
    if len(b) == 7 and b.startswith("wtc") and b[4] in "pf":
        return (0, int(b[3]), int(b[5:7]), 0 if b[4] == "p" else 1, "")
    return (1, 0, 0, 0, b)


def patch_url(abspath):
    """Site-relative url for a bank patch: data/patches/Category/Name.fxp."""
    tail = abspath.replace("\\", "/").split("/")[-2:]
    return "data/patches/" + "/".join(tail)


def bake_wasm(surge_dir, engine_dir, emscripten_bin):
    src = os.path.join(surge_dir, "src", "surge-wasm")
    bridge = os.path.join(src, "surge-worklet.cpp")
    if not os.path.isfile(bridge):
        sys.exit(f"no wasm bridge at {bridge} — is {surge_dir} the fork "
                 "on branch wasm-headless-audioworklet?")
    with open(bridge) as f:
        if "loadSCLString" not in f.read():
            sys.exit("surge-worklet.cpp lacks loadSCLString — the static "
                     "board needs SCL tuning; update the fork branch")
    # the submodule needs its emscripten guards (see the fork README)
    sub = os.path.join(surge_dir, "libs", "sst", "sst-plugininfra")
    clean = subprocess.run(
        ["git", "-C", sub, "diff", "--quiet"]).returncode == 0
    if clean:
        sys.exit("sst-plugininfra submodule is unpatched; run:\n"
                 f"  git -C {sub} apply "
                 "../../../src/surge-wasm/sst-plugininfra-emscripten.patch")

    build = os.path.join(surge_dir, "build-wasm")
    out = os.path.join(build, "src", "surge-wasm")
    wasm = os.path.join(out, "surge-worklet.wasm")
    env = dict(os.environ)
    if emscripten_bin:
        env["PATH"] = emscripten_bin + os.pathsep + env["PATH"]
    stale = (not os.path.isfile(wasm)
             or os.path.getmtime(wasm) < os.path.getmtime(bridge))
    if stale:
        if not os.path.isdir(build):
            subprocess.run(["emcmake", "cmake", "-B", build, "-S", surge_dir]
                           + WASM_CMAKE_FLAGS, env=env, check=True)
        subprocess.run(
            ["cmake", "--build", build, "-j", str(os.cpu_count() or 4),
             "--target", "surge-worklet"], env=env, check=True)

    os.makedirs(engine_dir, exist_ok=True)
    for f in WASM_FILES:
        shutil.copy2(os.path.join(out, f), os.path.join(engine_dir, f))
    print(f"engine: {len(WASM_FILES)} files "
          f"({os.path.getsize(wasm) // 1024} KB wasm)")


def bake_perf(perf_dir, data_dir):
    # build into a clean staging directory and swap it in, so a re-bake
    # never inherits performances the source no longer produces
    out = os.path.join(data_dir, "perf")
    stage = out + ".staging"
    if os.path.isdir(stage):
        shutil.rmtree(stage)
    os.makedirs(stage)
    if perf_dir:
        n = 0
        for f in sorted(os.listdir(perf_dir)):
            if f.endswith(".json") or f.endswith(".scl"):
                shutil.copy2(os.path.join(perf_dir, f),
                             os.path.join(stage, f))
                n += 1
        print(f"perf: copied {n} files from {perf_dir}")
    else:
        # house rule: the committed board regenerates from HEAD
        subprocess.run(
            ["stack", "exec", "otb", "--", "album",
             "corpus/bach-wtc/kern", stage], cwd=ROOT, check=True)
        for f in os.listdir(stage):  # the album's .mid siblings: not served
            if f.endswith(".mid"):
                os.remove(os.path.join(stage, f))
        print("perf: regenerated via otb album")
    scl = os.path.join(stage, "w3.scl")
    if not os.path.isfile(scl):
        shutil.rmtree(stage)
        sys.exit(f"no w3.scl landed in {stage}")
    if os.path.isdir(out):
        shutil.rmtree(out)
    os.replace(stage, out)
    shutil.copy2(os.path.join(out, "w3.scl"),
                 os.path.join(data_dir, "w3.scl"))


def bake_patches(data_dir):
    cats = scan_patches()
    if not cats:
        sys.exit("no Surge patch library found (patches_factory)")
    listing = {}
    n = 0
    for cat, entries in sorted(cats.items()):
        outdir = os.path.join(data_dir, "patches", cat)
        os.makedirs(outdir, exist_ok=True)
        listing[cat] = []
        for name, path in entries:
            shutil.copy2(path, os.path.join(outdir, name + ".fxp"))
            listing[cat].append({"name": name, "url": patch_url(path)})
            n += 1
    with open(os.path.join(data_dir, "patches.json"), "w") as f:
        json.dump(listing, f)
    # the client's "(init)" is a real Init Saw: with two voices per synth
    # (scene A/B), every scene needs actual patch bytes to merge from
    for root in patch_dirs():
        init = os.path.join(root, "Templates", "Init Saw.fxp")
        if os.path.isfile(init):
            shutil.copy2(init, os.path.join(data_dir, "init.fxp"))
            break
    else:
        sys.exit("no Templates/Init Saw.fxp in any patch library")
    print(f"patches: {n} across {len(listing)} categories (+ init.fxp)")
    return cats


def bake_casting(data_dir, casting_dir, calibration_file):
    """Rewrite casting + calibration from filesystem paths to bank urls.

    Casting entries resolve like the live board's resolve_patch: the
    Category/Name.fxp tail is the identity, the machine path is not.
    """
    castings = {}
    if os.path.isdir(casting_dir):
        for f in sorted(os.listdir(casting_dir)):
            if not f.endswith(".json"):
                continue
            with open(os.path.join(casting_dir, f)) as fh:
                raw = json.load(fh)
            entry = {ch: patch_url(p) for ch, p in raw.items()
                     if ch.isdigit()}
            if entry:
                castings[f[:-5]] = entry
    with open(os.path.join(data_dir, "casting.json"), "w") as f:
        json.dump(castings, f, indent=1)

    cal = {}
    if calibration_file and os.path.isfile(calibration_file):
        with open(calibration_file) as fh:
            raw = json.load(fh)
        cal = {patch_url(p): m for p, m in raw.items()}
    with open(os.path.join(data_dir, "calibration.json"), "w") as f:
        json.dump(cal, f, indent=1)
    print(f"casting: {len(castings)} castings, {len(cal)} calibrations")


def bake_manifest(data_dir):
    perf_dir = os.path.join(data_dir, "perf")
    paths = sorted(
        (os.path.join(perf_dir, f) for f in os.listdir(perf_dir)
         if f.endswith(".json")), key=album_key)
    pieces = []
    max_ch = 0
    for p in paths:
        with open(p) as f:
            perf = json.load(f)
        name = perf.get("piece") or os.path.splitext(os.path.basename(p))[0]
        end = max((n["onS"] + n["durS"]
                   for tr in perf["tracks"] for n in tr), default=0.0)
        end = max(end, perf.get("endS", 0.0))
        chs = [n["ch"] for tr in perf["tracks"] for n in tr]
        piece_max = max(chs, default=-1) + 1
        max_ch = max(max_ch, piece_max)
        pieces.append({
            "name": name,
            "url": "data/perf/" + os.path.basename(p),
            "endS": round(end, 3),
            "maxCh": piece_max,
        })
    manifest = {"pieces": pieces, "nInstances": max_ch, "scl": "data/w3.scl"}
    with open(os.path.join(data_dir, "manifest.json"), "w") as f:
        json.dump(manifest, f, indent=1)
    print(f"manifest: {len(pieces)} pieces, {max_ch} synth instances")
    return manifest


def main():
    ap = argparse.ArgumentParser(
        description="bake the static patchboard into site/")
    ap.add_argument("--site", default=os.path.join(ROOT, "site"))
    ap.add_argument("--surge-dir",
                    default=os.environ.get(
                        "SURGE_DIR", os.path.expanduser("~/code/surge")))
    ap.add_argument("--emscripten-bin", default="/usr/lib/emscripten",
                    help="prepended to PATH for emcmake (empty to skip)")
    ap.add_argument("--perf-dir", default=None,
                    help="copy IRs from here instead of running otb album")
    ap.add_argument("--casting-dir",
                    default=os.path.join(ROOT, "config", "casting"))
    ap.add_argument("--calibration",
                    default=os.path.join(ROOT, "config", "calibration.json"))
    ap.add_argument("--skip-wasm", action="store_true")
    ap.add_argument("--skip-perf", action="store_true")
    ap.add_argument("--skip-patches", action="store_true")
    args = ap.parse_args()

    data_dir = os.path.join(args.site, "data")
    os.makedirs(data_dir, exist_ok=True)

    if not args.skip_wasm:
        bake_wasm(args.surge_dir, os.path.join(args.site, "engine"),
                  args.emscripten_bin)
    if not args.skip_perf:
        bake_perf(args.perf_dir, data_dir)
    if not args.skip_patches:
        bake_patches(data_dir)
    bake_casting(data_dir, args.casting_dir, args.calibration)
    bake_manifest(data_dir)
    print(f"baked: {args.site}")


if __name__ == "__main__":
    main()
