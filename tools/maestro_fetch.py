#!/usr/bin/env python3
"""Fetch the MAESTRO v3 MIDI archive and catalog its WTC performances.

MAESTRO (piano-e-competition Disklavier captures) is ASAP's parent:
real performed MIDI with real velocities. ASAP aligned only part of it;
this campaign aligns the rest ourselves. This tool:

  1. downloads maestro-v3.0.0-midi.zip (~57 MB, checksum-verified) into
     corpus/maestro/ and extracts ONLY the WTC-referenced files;
  2. writes corpus/maestro/wtc-catalog.json — every MAESTRO WTC row,
     joined against corpus/asap/metadata.csv's maestro_midi_performance
     column so performances ASAP already aligned are flagged (their
     ground truth also validates our aligner);
  3. names the candidate otb pieces per file (a competition file may
     hold the prelude, the fugue, or both — segmentation decides later,
     in maestro_align.py).

    python3 tools/maestro_fetch.py            # fetch + catalog
    python3 tools/maestro_fetch.py --catalog  # catalog only (no network)
"""
import argparse
import csv
import hashlib
import json
import os
import re
import sys
import urllib.request
import zipfile

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
MAESTRO = os.path.join(ROOT, "corpus", "maestro")
ASAP_META = os.path.join(ROOT, "corpus", "asap", "metadata.csv")

URL = ("https://storage.googleapis.com/magentadata/datasets/maestro/"
       "v3.0.0/maestro-v3.0.0-midi.zip")
CSV_URL = ("https://storage.googleapis.com/magentadata/datasets/maestro/"
           "v3.0.0/maestro-v3.0.0.csv")
# sha256 of maestro-v3.0.0-midi.zip, recorded at first fetch 2026-09-01
# (Google's canonical distribution publishes no digest; this pins ours)
ZIP_SHA256 = "70470ee253295c8d2c71e6d9d4a815189e35c89624b76d22fce5a019d5dde12c"


def wtc_rows(meta_rows):
    out = []
    for r in meta_rows:
        title = r["canonical_title"]
        m = re.search(r"BWV (8[4-9]\d)", title)
        if not m:
            continue
        bwv = int(m.group(1))
        if not (846 <= bwv <= 893):
            continue
        out.append((bwv, r))
    return out


def piece_names(bwv):
    """otb piece slugs for a BWV number: (prelude, fugue)."""
    book, num = (1, bwv - 845) if bwv <= 869 else (2, bwv - 869)
    return (f"wtc{book}p{num:02d}", f"wtc{book}f{num:02d}")


# WTC ordering: number = chromatic position of the key, major first
KEY_NUM = {("C", "maj"): 1, ("C", "min"): 2, ("C-sharp", "maj"): 3,
           ("C-sharp", "min"): 4, ("D-flat", "maj"): 3,
           ("D", "maj"): 5, ("D", "min"): 6,
           ("E-flat", "maj"): 7, ("E-flat", "min"): 8,
           ("D-sharp", "min"): 8, ("E", "maj"): 9, ("E", "min"): 10,
           ("F", "maj"): 11, ("F", "min"): 12,
           ("F-sharp", "maj"): 13, ("F-sharp", "min"): 14,
           ("G", "maj"): 15, ("G", "min"): 16,
           ("A-flat", "maj"): 17, ("G-sharp", "min"): 18,
           ("A", "maj"): 19, ("A", "min"): 20,
           ("B-flat", "maj"): 21, ("B-flat", "min"): 22,
           ("B", "maj"): 23, ("B", "min"): 24}


def title_key_bwv(title):
    """BWV inferred from 'in <Key> <Major|Minor>' + 'WTC <I|II>' — the
    titles' explicit BWV numbers are sometimes WRONG (a 2014 file says
    'BWV 846' and 'WTC II' in one breath; ASAP proves it is 870), so
    the key+book reading is computed independently."""
    m = re.search(r"in ([A-G])(?:[- ](flat|sharp))? (Major|Minor)", title)
    book = 2 if re.search(r"WTC\s*II|Book\s*II", title) else (
        1 if re.search(r"WTC\s*I|Book\s*I", title) else None)
    if not m or book is None:
        return None
    key = m.group(1) + (f"-{m.group(2)}" if m.group(2) else "")
    num = KEY_NUM.get((key, "maj" if m.group(3) == "Major" else "min"))
    if num is None:
        return None
    return (845 if book == 1 else 869) + num


def folder_pieces(folder):
    """'Bach/Fugue/bwv_870' -> ['wtc2f01']."""
    m = re.match(r"Bach/(Prelude|Fugue)/bwv_(\d+)", folder)
    if not m:
        return []
    pre, fug = piece_names(int(m.group(2)))
    return [pre if m.group(1) == "Prelude" else fug]


def asap_used():
    """maestro paths ASAP already aligned -> {path: [asap folders]}."""
    used = {}
    if not os.path.isfile(ASAP_META):
        return used
    with open(ASAP_META) as f:
        for r in csv.DictReader(f):
            mp = (r.get("maestro_midi_performance") or "").strip()
            if mp:
                used.setdefault(mp.replace("{maestro}/", ""), []).append(
                    r["folder"])
    return used


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--catalog", action="store_true",
                    help="rebuild the catalog from cached files only")
    args = ap.parse_args()

    os.makedirs(MAESTRO, exist_ok=True)
    csv_path = os.path.join(MAESTRO, "maestro-v3.0.0.csv")
    zip_path = os.path.join(MAESTRO, "maestro-v3.0.0-midi.zip")

    if not args.catalog:
        if not os.path.isfile(csv_path):
            print("fetching metadata csv…")
            urllib.request.urlretrieve(CSV_URL, csv_path)
        if not os.path.isfile(zip_path):
            print("fetching MIDI archive (~57 MB)…")
            urllib.request.urlretrieve(URL, zip_path)
        digest = hashlib.sha256(open(zip_path, "rb").read()).hexdigest()
        if ZIP_SHA256 and digest != ZIP_SHA256:
            sys.exit(f"archive digest mismatch: {digest}")

    with open(csv_path) as f:
        rows = wtc_rows(list(csv.DictReader(f)))
    used = asap_used()

    zf = zipfile.ZipFile(zip_path)
    prefix = None
    for n in zf.namelist():
        if n.endswith(".csv"):
            prefix = n.rsplit("/", 1)[0] + "/" if "/" in n else ""
            break
    if prefix is None:
        prefix = "maestro-v3.0.0/"

    catalog = []
    extracted = 0
    for bwv, r in sorted(rows, key=lambda x: (x[0], x[1]["midi_filename"])):
        rel = r["midi_filename"]
        dest = os.path.join(MAESTRO, rel)
        if not os.path.isfile(dest):
            os.makedirs(os.path.dirname(dest), exist_ok=True)
            with zf.open(prefix + rel) as src, open(dest, "wb") as out:
                out.write(src.read())
            extracted += 1
        title = r["canonical_title"]
        folders = used.get(rel, [])
        # candidates from every signal we have; the aligner's match-rate
        # gate is the arbiter when they disagree
        bwvs = {bwv}
        kb = title_key_bwv(title)
        if kb:
            bwvs.add(kb)
        candidates = []
        for b in sorted(bwvs):
            pre, fug = piece_names(b)
            has_p = "Prelude" in title or "Fugue" not in title
            has_f = "Fugue" in title or "Prelude" not in title
            if has_p:
                candidates.append(pre)
            if has_f:
                candidates.append(fug)
        for f_ in folders:  # ASAP's mapping is definitive where present
            for p in folder_pieces(f_):
                if p not in candidates:
                    candidates.append(p)
        catalog.append({
            "midi": rel,
            "bwv": bwv,
            "title": title,
            "title_bwv_conflict": bool(kb and kb != bwv),
            "year": r["year"],
            "duration": float(r["duration"]),
            "candidates": candidates,
            "asap_folders": folders,
        })

    out = os.path.join(MAESTRO, "wtc-catalog.json")
    with open(out, "w") as f:
        json.dump(catalog, f, indent=1)
    in_asap = sum(1 for c in catalog if c["asap_folders"])
    print(f"catalog: {len(catalog)} WTC performances "
          f"({in_asap} already in ASAP — the aligner's validation set), "
          f"{extracted} newly extracted -> {out}")


if __name__ == "__main__":
    main()
