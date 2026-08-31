#!/usr/bin/env python3
"""audition.py — render otb PerformanceIR through Surge XT, offline.

    otb SCORE.krn -o x.mid --emit-json perf.json --emit-scl w3.scl
    .venv-audition/bin/python tools/audition.py perf.json -o out.wav \
        [--scl w3.scl] [--patch path.fxp] [--patch-ch 3:other.fxp] [--sr 48000]

One Surge instance per MIDI channel (a channel is a monophonic lane, so
each may carry its own patch). Events are delivered between
processMultiBlock calls, i.e. quantised to Surge's block (32 samples,
under a millisecond at 48 kHz — finer than the ms-scale rules, but not
sample-accurate). Instances are summed and written as 16-bit WAV.

Temperament: prefer --scl (Surge's native microtuning — the ground-truth
path); without it the JSON's per-note pitch-bend values are sent instead,
which is exactly what the hardware will receive. Rendering the same
performance both ways and comparing is the tuning oracle.

Deps: surgepy (built from ~/code/surge; stable copy in ~/.local/share/otb/surgepy), numpy. Everything else stdlib.
License: GPL-2.0-or-later.
"""

import argparse
import json
import math
import struct
import sys
import wave

import numpy as np
import surgepy

BLOCK = 32  # surge block size; queried per instance below


def load_perf(path):
    with open(path) as f:
        return json.load(f)


def comp_for(cal, patch_path):
    """Note-end compensation (seconds) for a patch's release tail.

    Half the measured release, capped at 300 ms: enough that the
    intended silence survives the tail, never so much that the note
    itself disappears (a second clamp at half the note guards that).
    """
    if not cal or not patch_path:
        return 0.0
    m = cal.get(patch_path)
    if not m:
        # measurements travel with the patch, not the filesystem: fall
        # back to the Category/Name.fxp tail, same rule as the
        # patchboard's _cal_for
        tail = "/".join(patch_path.replace("\\", "/").split("/")[-2:])
        for k, v in cal.items():
            if k.replace("\\", "/").endswith("/" + tail):
                m = v
                break
    if not m:
        return 0.0
    return min(0.5 * m.get("releaseS", 0.0), 0.3)


def render(perf, sr, scl=None, patches=None, tail=3.0, bend_range=2.0,
           cal=None):
    tracks = perf["tracks"]
    events = []  # (samples, kind, ch, a, b) 0=bend 1=on 2=off 3=tempo(all)
    channels = set()
    for tr in tracks:
        for n in tr:
            ch = n["ch"]
            channels.add(ch)
            on = int(n["onS"] * sr)
            # timbre-aware articulation: subtract the patch's measured
            # release so perceived silence matches the written gate
            p = patches or {}
            comp = comp_for(cal, p.get(ch, p.get(None)))
            durS = max(n["durS"] * 0.5, n["durS"] - comp)
            off = int((n["onS"] + durS) * sr)
            if not scl:
                # bend carries the temperament ONLY when Surge isn't
                # natively tuned — with .scl loaded, sending bends too
                # would apply the temperament twice
                events.append((on, 0, ch, n["bend"], 0))
            events.append((on, 1, ch, n["pitch"], n["vel"]))
            events.append((max(off, on + 1), 2, ch, n["pitch"], 0))
    # tempo map -> setTempo on every instance, so tempo-synced LFOs and
    # delays track the piece (and its ritardando) instead of a fixed 120
    tempo_evs = [(int(t.get("onS", 0) * sr), 3, None, t["bpm"], 0)
                 for t in perf.get("tempoMap", []) if "onS" in t]
    events.extend(tempo_evs)
    # same-tick order: tempo < off < bend < on — off-before-on prevents a
    # just-started note being released by its predecessor's note-off
    prio = {3: 0, 2: 1, 0: 2, 1: 3}
    events.sort(key=lambda e: (e[0], prio[e[1]]))
    if not events:
        sys.exit("empty performance")

    total = events[-1][0] + int(tail * sr)
    synths = {}
    for ch in sorted(channels):
        s = surgepy.createSurge(sr)
        if scl:
            s.loadSCLFile(scl)
        patch = (patches or {}).get(ch, (patches or {}).get(None))
        if patch:
            s.loadPatch(patch)
        synths[ch] = s

    block = synths[min(channels)].getBlockSize()
    nblocks = total // block + 1
    mix = np.zeros((2, nblocks * block), dtype=np.float32)

    for ch, s in synths.items():
        if hasattr(s, "setTempo") and perf.get("tempoMap"):
            s.setTempo(perf["tempoMap"][0]["bpm"])
        buf = s.createMultiBlock(nblocks)
        pos = 0  # in blocks
        chev = [e for e in events if e[2] == ch or e[1] == 3]
        for i, (smp, kind, _, a, b) in enumerate(chev):
            evblock = min(smp // block, nblocks)
            if evblock > pos:
                s.processMultiBlock(buf, pos, evblock - pos)
                pos = evblock
            if kind == 0:
                # surgepy pitchBend takes the signed 14-bit range
                s.pitchBend(0, a - 8192)
            elif kind == 1:
                s.playNote(0, a, b, 0)
            elif kind == 3:
                if hasattr(s, "setTempo"):
                    s.setTempo(a)
            else:
                s.releaseNote(0, a, 0)
        if pos < nblocks:
            s.processMultiBlock(buf, pos, nblocks - pos)
        mix += buf.reshape(2, -1)[:, : mix.shape[1]]

    peak = float(np.max(np.abs(mix))) or 1.0
    if peak > 0.891:  # normalise only if we'd clip past -1 dBFS
        mix *= 0.891 / peak
    return mix


def write_wav(path, mix, sr):
    data = (np.clip(mix, -1, 1) * 32767).astype("<i2")
    interleaved = np.empty(data.shape[1] * 2, dtype="<i2")
    interleaved[0::2] = data[0]
    interleaved[1::2] = data[1]
    with wave.open(path, "wb") as w:
        w.setnchannels(2)
        w.setsampwidth(2)
        w.setframerate(sr)
        w.writeframes(interleaved.tobytes())


def main():
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("perf", help="PerformanceIR JSON from otb --emit-json")
    ap.add_argument("-o", "--output", default="out.wav")
    ap.add_argument("--scl", help=".scl from otb --emit-scl: native microtuning")
    ap.add_argument("--patch", help="default .fxp patch for every voice")
    ap.add_argument("--patch-ch", action="append", default=[],
                    metavar="CH:FILE.fxp", help="per-channel patch override")
    ap.add_argument("--sr", type=int, default=48000)
    ap.add_argument("--calibrate", metavar="CAL.json",
                    help="patch-envelope calibration (tools/"
                         "calibrate_patch.py): note ends compensated "
                         "for each channel's release tail")
    args = ap.parse_args()

    patches = {}
    if args.patch:
        patches[None] = args.patch
    for spec in args.patch_ch:
        ch, _, path = spec.partition(":")
        patches[int(ch)] = path

    cal = None
    if args.calibrate:
        with open(args.calibrate) as f:
            cal = json.load(f)
    perf = load_perf(args.perf)
    mix = render(perf, args.sr, scl=args.scl, patches=patches,
        cal=cal)
    write_wav(args.output, mix, args.sr)
    n = sum(len(t) for t in perf["tracks"])
    print(f"{n} notes | {mix.shape[1]/args.sr:.1f}s | "
          f"{'scl-tuned' if args.scl else 'bend-tuned'} | -> {args.output}")


if __name__ == "__main__":
    main()
