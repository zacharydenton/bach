"""A minimal Standard MIDI File reader — stdlib only, tempo-aware.

The MAESTRO campaign needs performed note-ons with absolute seconds and
velocities out of format-0/1 SMF. The repo's tooling is deliberately
stdlib-pure (requirements-audition pins only numpy/sklearn for the
research rigs), so rather than adopt a MIDI dependency this reads the
few event kinds that matter and integrates the tempo map. Validated
against ASAP ground truth: parsing a performance .mid reproduces the
note count, onsets and velocities its Vienna .match records (see
test_maestro_align.py).

    notes = read_smf(path)   # [Note(pitch, vel, on_s, off_s, ch, track)]
"""
import struct
from dataclasses import dataclass


@dataclass
class Note:
    pitch: int
    vel: int
    on_s: float
    off_s: float
    ch: int
    track: int


def _varlen(data, i):
    v = 0
    while True:
        b = data[i]
        i += 1
        v = (v << 7) | (b & 0x7F)
        if not b & 0x80:
            return v, i


def _events(track, ti):
    """(abs_ticks, kind, payload) for note-on/off and tempo events."""
    i = 0
    ticks = 0
    status = 0
    out = []
    while i < len(track):
        dt, i = _varlen(track, i)
        ticks += dt
        b = track[i]
        if b >= 0x80:
            status = b
            i += 1
        # else: running status — reuse the previous status byte
        kind = status & 0xF0
        ch = status & 0x0F
        if kind in (0x80, 0x90):
            pitch, vel = track[i], track[i + 1]
            i += 2
            on = kind == 0x90 and vel > 0
            out.append((ticks, "on" if on else "off", (ch, pitch, vel)))
        elif kind in (0xA0, 0xB0, 0xE0):
            i += 2
        elif kind in (0xC0, 0xD0):
            i += 1
        elif status == 0xFF:
            meta = track[i]
            i += 1
            ln, i = _varlen(track, i)
            if meta == 0x51:
                us = int.from_bytes(track[i:i + 3], "big")
                out.append((ticks, "tempo", us))
            i += ln
        elif status in (0xF0, 0xF7):  # sysex
            ln, i = _varlen(track, i)
            i += ln
        else:
            raise ValueError(f"track {ti}: unhandled status {status:#x}")
    return out


def read_smf(path):
    """All notes of an SMF, with tempo-map-integrated absolute seconds."""
    data = open(path, "rb").read()
    if data[:4] != b"MThd":
        raise ValueError("not an SMF")
    _, fmt, ntrk, division = struct.unpack(">IHHH", data[4:14])
    if division & 0x8000:
        raise ValueError("SMPTE division unsupported")

    i = 14
    tracks = []
    for _ in range(ntrk):
        if data[i:i + 4] != b"MTrk":
            raise ValueError("bad track header")
        ln = struct.unpack(">I", data[i + 4:i + 8])[0]
        tracks.append(data[i + 8:i + 8 + ln])
        i += 8 + ln

    evs = []
    for ti, tr in enumerate(tracks):
        evs.extend((t, ti, kind, p) for t, kind, p in _events(tr, ti))
    evs.sort(key=lambda e: e[0])

    # walk once, integrating the tempo map (default 120 bpm = 500000 us/qn)
    notes = []
    open_notes = {}
    t_prev, s_prev, us = 0, 0.0, 500000
    for ticks, ti, kind, p in evs:
        s = s_prev + (ticks - t_prev) * us / 1e6 / division
        if kind == "tempo":
            t_prev, s_prev, us = ticks, s, p
        elif kind == "on":
            ch, pitch, vel = p
            open_notes.setdefault((ti, ch, pitch), []).append((s, vel))
        elif kind == "off":
            ch, pitch, _ = p
            stack = open_notes.get((ti, ch, pitch))
            if stack:
                on_s, vel = stack.pop(0)
                notes.append(Note(pitch, vel, on_s, s, ch, ti))
    # dangling note-ons close at the end of the file
    for (ti, ch, pitch), stack in open_notes.items():
        for on_s, vel in stack:
            notes.append(Note(pitch, vel, on_s, on_s, ch, ti))
    notes.sort(key=lambda n: (n.on_s, n.pitch))
    return notes
