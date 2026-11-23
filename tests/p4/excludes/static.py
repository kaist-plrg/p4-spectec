#!/usr/bin/env python3
"""Summarize statistics for *.exclude files under the static/ directory."""

import os
from pathlib import Path

STATIC_DIR = Path(__file__).parent / "static"


def count_tests(exclude_file: Path) -> tuple[int, int]:
    """Return (positive, negative) test counts for a single .exclude file."""
    pos = neg = 0
    for line in exclude_file.read_text().splitlines():
        line = line.strip()
        if not line:
            continue
        if "p4_16_samples" in line:
            pos += 1
        elif "p4_16_errors" in line:
            neg += 1
    return pos, neg


def main():
    top_dirs = sorted(d for d in STATIC_DIR.iterdir() if d.is_dir())

    for top_dir in top_dirs:
        exclude_files = sorted(top_dir.rglob("*.exclude"))
        if not exclude_files:
            continue

        dir_pos = dir_neg = 0
        rels = [ef.relative_to(top_dir) for ef in exclude_files]
        col = max(len(str(r)) for r in rels)
        col = max(col, len("File"))
        sep = "=" * (col + 16)
        print(sep)
        print(f"  {top_dir.relative_to(STATIC_DIR)}/")
        print(sep)
        print(f"  {'File':<{col}}  {'Pos':>5}  {'Neg':>5}")
        print(f"  {'-'*col}  {'-----':>5}  {'-----':>5}")

        for ef, rel in zip(exclude_files, rels):
            pos, neg = count_tests(ef)
            dir_pos += pos
            dir_neg += neg
            print(f"  {str(rel):<{col}}  {pos:>5}  {neg:>5}")

        print(f"  {'-'*col}  {'-----':>5}  {'-----':>5}")
        print(f"  {'TOTAL':<{col}}  {dir_pos:>5}  {dir_neg:>5}")
        print()


if __name__ == "__main__":
    main()
