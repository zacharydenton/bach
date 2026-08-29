#!/usr/bin/env python3
"""oracle.py — the tuning oracle: Surge native .scl vs the compiler's bends.

Renders a sustained C#4 three ways on an init patch (equal temperament,
pitch-bend of -400/8192 at range 2, and the Werckmeister .scl) and
asserts the bend and scl paths agree within tolerance of each other and
of theory (-9.775 c).

Passing means the MIDI artifact's bend lane and Surge's native
microtuning are the same temperament — so what the hardware receives is
what the oracle hears. Known, accepted divergence in full texture:
channel bend retunes the *release tail* of a lane's previous note; the
scl path does not. That is faithful to what hardware bend does.

    PYTHONPATH=<surgepy dir> .venv-audition/bin/python tools/oracle.py w3.scl

License: GPL-2.0-or-later.
"""

import math
import sys

import numpy as np
import surgepy

NB = 6000  # 4 s at block 32 / 48 kHz


def freq_of(setup):
    s = surgepy.createSurge(48000)
    setup(s)
    buf = s.createMultiBlock(NB)
    s.processMultiBlock(buf, 0, 100)
    s.playNote(0, 61, 100, 0)
    s.processMultiBlock(buf, 100, NB - 100)
    d = buf[0][96000:180000]
    win = np.hanning(len(d))
    sp = np.abs(np.fft.rfft(d * win))
    freqs = np.fft.rfftfreq(len(d), 1 / 48000)
    i = int(np.argmax(sp[50:])) + 50
    a, b, c = np.log(sp[i - 1]), np.log(sp[i]), np.log(sp[i + 1])
    return freqs[i] + 0.5 * (a - c) / (a - 2 * b + c) * (freqs[1] - freqs[0])


def main():
    scl = sys.argv[1] if len(sys.argv) > 1 else None
    if not scl:
        sys.exit("usage: oracle.py werckmeister3.scl")
    et = freq_of(lambda s: None)
    bent = freq_of(lambda s: s.pitchBend(0, -400))
    tuned = freq_of(lambda s: s.loadSCLFile(scl))
    cents = lambda f: 1200 * math.log2(f / et)
    print(f"ET   {et:9.3f} Hz {cents(et):+7.2f} c")
    print(f"bend {bent:9.3f} Hz {cents(bent):+7.2f} c (theory -9.77)")
    print(f"scl  {tuned:9.3f} Hz {cents(tuned):+7.2f} c (theory -9.77)")
    dpaths = abs(cents(bent) - cents(tuned))
    dtheory = max(abs(cents(bent) + 9.775), abs(cents(tuned) + 9.775))
    ok = dpaths < 0.5 and dtheory < 0.5
    print(f"paths agree within {dpaths:.2f} c; off theory {dtheory:.2f} c "
          f"-> {'ORACLE PASS' if ok else 'ORACLE FAIL'}")
    sys.exit(0 if ok else 1)


if __name__ == "__main__":
    main()
