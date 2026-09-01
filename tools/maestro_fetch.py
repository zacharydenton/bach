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
        pre, fug = piece_names(bwv)
        title = r["canonical_title"]
        candidates = []
        if not re.search(r"\bFugue\b", title) or "Prelude" in title:
            candidates.append(pre)
        if not re.search(r"\bPrelude\b", title) or "Fugue" in title:
            candidates.append(fug)
        catalog.append({
            "midi": rel,
            "bwv": bwv,
            "title": title,
            "year": r["year"],
            "duration": float(r["duration"]),
            "candidates": candidates or [pre, fug],
            "asap_folders": used.get(rel, []),
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
