#!/usr/bin/env python3
"""Measure a patch's envelope so articulation survives the timbre.

Every interpretation system assumes the piano: a 60% gate is supposed
to be heard as a 40% silence. But on a patch with a slow attack and a
long release the silence disappears into the tail — the compiler's
articulation decisions, all those Quantz citations, sound legato mush.
Because we own the renderer, we can measure instead of assume: render
a probe note through the actual patch with surgepy, read the envelope,
and record how much sounding time the patch itself adds.

  attackS   time from note-on to 90% of peak RMS
  releaseS  time from note-off until the envelope falls 20 dB

Consumers subtract (a clamped fraction of) releaseS from note ends on
that channel, so the *perceived* articulation matches the intended
one. One calibration file, two consumers (audition.py --calibrate,
the baked board's data/calibration.json via tools/bake_site.py).

  PYTHONPATH=$SURGEPY_DIR python tools/calibrate_patch.py \
      config/casting/default.json -o config/calibration.json

License: GPL-2.0-or-later.
"""

import argparse
import json
import math
import os
import sys


def measure(surgepy, path, sr=48000):
    s = surgepy.createSurge(sr)
    s.loadPatch(path)
    block = s.getBlockSize()
    hold_blocks = int(1.0 * sr / block)
    tail_blocks = int(2.0 * sr / block)
    buf = s.createMultiBlock(hold_blocks + tail_blocks)
    s.playNote(0, 60, 100, 0)
    s.processMultiBlock(buf, 0, hold_blocks)
    s.releaseNote(0, 60, 0)
    s.processMultiBlock(buf, hold_blocks, tail_blocks)
    mono = (buf[0] + buf[1]) / 2.0
    win = max(1, int(0.005 * sr))
    n = len(mono) // win
    env = [math.sqrt(float((mono[i * win:(i + 1) * win] ** 2).mean()))
           for i in range(n)]
    peak = max(env) or 1e-9
    hold_w = hold_blocks * block // win
    attack = next((i for i, e in enumerate(env) if e >= 0.9 * peak), 0)
    release_peak = max(env[hold_w - 4:hold_w]) if hold_w >= 4 else peak
    rel = next((i for i, e in enumerate(env[hold_w:])
                if e < release_peak * 0.1), len(env) - hold_w)
    return {"attackS": round(attack * win / sr, 4),
            "releaseS": round(rel * win / sr, 4)}


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("casting", help="casting json (channel -> patch) "
                                    "or a single .fxp path")
    ap.add_argument("-o", "--output", default="config/calibration.json")
    ap.add_argument("--sr", type=int, default=48000)
    args = ap.parse_args()

    import surgepy  # needs PYTHONPATH to the built module

    if args.casting.endswith(".fxp"):
        patches = {args.casting}
    else:
        with open(args.casting) as f:
            cast = json.load(f)
        patches = {v for k, v in cast.items()
                   if k != "_" and isinstance(v, str)}

    cal = {}
    if os.path.isfile(args.output):
        with open(args.output) as f:
            cal = json.load(f)
    for p in sorted(patches):
        if not os.path.isfile(p):
            print(f"skip (missing): {p}", file=sys.stderr)
            continue
        m = measure(surgepy, p, args.sr)
        cal[p] = m
        print(f"{os.path.basename(p):<28} attack {m['attackS']*1000:6.1f} ms"
              f"   release {m['releaseS']*1000:7.1f} ms")
    with open(args.output, "w") as f:
        json.dump(cal, f, indent=2, sort_keys=True)
    print(f"-> {args.output} ({len(cal)} patches)")


if __name__ == "__main__":
    main()
