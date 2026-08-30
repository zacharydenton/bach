#!/usr/bin/env python3
"""Score following: the compiler learns to listen — duo prototype.

Online time warping (Dixon 2005, the MATCH/Antescofo lineage) over
chroma features: a reference render of the piece (score time known
exactly, because we made it) is followed against a live performance
arriving frame by frame. The follower emits, per live frame, the
estimated score position and a local tempo ratio — precisely the
conducting signal a future patchboard mode needs to play the A4 parts
*around* a human at the Model D. The take stops being playback plus
hands and becomes chamber music with a machine that has read Quantz.

This prototype proves the tracking core honestly: in --sim mode both
"reference" and "live" are otb renders of the same piece under
different interpretations, so the true correspondence is known from
the two tempo maps, and the report is a real error number, not vibes.

  tools/follow.py --reference a.wav --live b.wav \
      [--truth ref_ir.json live_ir.json]

Frames are 50 ms; the follower is causal — it never sees a live frame
before emitting the previous estimate.

STATUS: prototype. Tracking is verified in simulation (renders with
known tempo maps) only; no live audio input, no engine integration
yet. The ~100 ms medians are a floor for the algorithm, not a claim
about stage conditions.

License: GPL-2.0-or-later.
"""

import argparse
import json
import math
import struct
import sys
import wave

import numpy as np

HOP_S = 0.05
FFT = 4096


def load_wav_mono(path):
    w = wave.open(path)
    sr = w.getframerate()
    raw = w.readframes(w.getnframes())
    width = w.getsampwidth()
    if width == 2:
        x = np.frombuffer(raw, dtype="<i2").astype(np.float32) / 32768.0
    elif width == 4:
        x = np.frombuffer(raw, dtype="<i4").astype(np.float32) / 2**31
    elif width == 3:
        b = np.frombuffer(raw, dtype=np.uint8).reshape(-1, 3)
        x = ((b[:, 0].astype(np.int32))
             | (b[:, 1].astype(np.int32) << 8)
             | (b[:, 2].astype(np.int32) << 16))
        x = (x - ((x >> 23) & 1) * (1 << 24)).astype(np.float32) / 2**23
    elif width == 1:
        x = (np.frombuffer(raw, dtype=np.uint8).astype(np.float32)
             - 128.0) / 128.0
    else:
        sys.exit(f"{path}: unsupported sample width {width}")
    ch = w.getnchannels()
    if ch > 1:
        x = x.reshape(-1, ch).mean(axis=1)
    return x, sr


def chroma(x, sr):
    """Frames x 12, L1-normalised; 50 ms hop, 4096-point FFT."""
    hop = int(HOP_S * sr)
    nframes = max(0, (len(x) - FFT) // hop)
    freqs = np.fft.rfftfreq(FFT, 1 / sr)
    # map bins 60 Hz..5 kHz onto pitch classes
    mask = (freqs > 60) & (freqs < 5000)
    pcs = np.zeros(len(freqs), dtype=int)
    pcs[mask] = np.round(
        12 * np.log2(freqs[mask] / 440.0) + 69).astype(int) % 12
    win = np.hanning(FFT).astype(np.float32)
    out = np.zeros((nframes, 12), dtype=np.float32)
    for i in range(nframes):
        seg = x[i * hop:i * hop + FFT] * win
        mag = np.abs(np.fft.rfft(seg))
        mag[~mask] = 0
        for pc in range(12):
            out[i, pc] = mag[pcs == pc].sum()
        nrm = np.linalg.norm(out[i])
        if nrm > 0:
            out[i] /= nrm
    return out


class OnlineFollower:
    """Causal chroma follower: banded incremental DTW.

    For each live frame, costs are relaxed over a window of reference
    frames around the current position; the path may stand still or
    advance up to `max_step` frames — the classic slope constraint.
    """

    def __init__(self, ref, band_s=4.0, max_step=3):
        self.ref = ref
        self.band = int(band_s / HOP_S)
        self.max_step = max_step
        self.d = np.full(len(ref), np.inf, dtype=np.float64)
        self.d[0] = 0.0
        self.pos = 0
        self.history = []

    def feed(self, frame):
        lo = max(0, self.pos - self.band // 4)
        hi = min(len(self.ref), self.pos + self.band)
        cost = 1.0 - self.ref[lo:hi] @ frame  # cosine distance
        nd = np.full_like(self.d, np.inf)
        # step biases: standing still is taxed (repetitive figuration
        # otherwise lets the path lag for free), the diagonal is free,
        # long jumps pay a little
        for k in range(lo, hi):
            best = self.d[k] + 0.08  # stand still
            for s in range(1, self.max_step + 1):
                if k - s >= 0:
                    c = self.d[k - s] + (0.02 * (s - 1))
                    if c < best:
                        best = c
            nd[k] = best + cost[k - lo]
        self.d = nd
        # leaky normalisation keeps the numbers bounded over minutes
        m = np.min(self.d[lo:hi])
        if math.isfinite(m):
            self.d[lo:hi] -= m
        raw = lo + int(np.argmin(self.d[lo:hi]))
        self.history.append(raw)
        # median of the recent raw estimates: half-bar flickers cancel
        recent = sorted(self.history[-5:])
        self.pos = recent[len(recent) // 2]
        return self.pos

    def tempo_ratio(self, over_s=2.0):
        """Reference seconds consumed per live second, recent window."""
        n = int(over_s / HOP_S)
        if len(self.history) < n + 1:
            return 1.0
        adv = self.history[-1] - self.history[-1 - n]
        return max(0.25, min(4.0, adv / n))


def integrate(tempo_map, wn):
    t = 0.0
    for (w0, b0), (w1, _) in zip(tempo_map, tempo_map[1:]):
        if wn <= w0:
            break
        t += (min(wn, w1) - w0) * 240.0 / b0
        if wn <= w1:
            return t
    wl, bl = tempo_map[-1]
    if wn > wl:
        t += (wn - wl) * 240.0 / bl
    return t


def invert_map(tempo_map, end_wn, seconds):
    """Score position (wn) at a wall-clock time, by bisection."""
    lo, hi = 0.0, end_wn
    for _ in range(40):
        mid = (lo + hi) / 2
        if integrate(tempo_map, mid) < seconds:
            lo = mid
        else:
            hi = mid
    return (lo + hi) / 2


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--reference", required=True)
    ap.add_argument("--live", required=True)
    ap.add_argument("--truth", nargs=2, metavar=("REF_IR", "LIVE_IR"),
                    help="IR jsons for both renders: report true "
                         "tracking error from the two tempo maps")
    args = ap.parse_args()

    xr, sr1 = load_wav_mono(args.reference)
    xl, sr2 = load_wav_mono(args.live)
    if sr1 != sr2:
        sys.exit("sample rates differ")
    ref = chroma(xr, sr1)
    live = chroma(xl, sr2)
    fol = OnlineFollower(ref)

    est = []
    for j in range(len(live)):
        k = fol.feed(live[j])
        est.append((j * HOP_S, k * HOP_S, fol.tempo_ratio()))
    print(f"followed {len(live)} live frames over "
          f"{len(ref)} reference frames")

    if args.truth:
        with open(args.truth[0]) as f:
            ir_ref = json.load(f)
        with open(args.truth[1]) as f:
            ir_live = json.load(f)
        tm_ref = [(t["wn"], t["bpm"]) for t in ir_ref["tempoMap"]]
        tm_live = [(t["wn"], t["bpm"]) for t in ir_live["tempoMap"]]
        end_wn = max(n["onWn"] + n["durWn"]
                     for tr in ir_live["tracks"] for n in tr)
        errs = []
        for t_live, t_ref_est, _ in est:
            wn = invert_map(tm_live, end_wn, t_live)
            if wn >= end_wn - 0.5:
                break  # both renders end in silence; nothing to track
            t_ref_true = integrate(tm_ref, wn)
            errs.append(abs(t_ref_est - t_ref_true))
        errs.sort()
        mean = sum(errs) / len(errs)
        med = errs[len(errs) // 2]
        p95 = errs[int(len(errs) * 0.95)]
        print(f"tracking error vs ground truth over {len(errs)} frames:")
        print(f"  median {med*1000:.0f} ms | mean {mean*1000:.0f} ms | "
              f"95th pct {p95*1000:.0f} ms")
    else:
        for t, p, r in est[:: max(1, len(est) // 10)]:
            print(f"  live {t:6.2f}s -> score {p:6.2f}s  (tempo x{r:.2f})")


if __name__ == "__main__":
    main()
