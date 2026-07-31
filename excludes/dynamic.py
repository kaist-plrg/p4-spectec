#!/usr/bin/env python3
"""Summarize statistics for *.exclude files under the dynamic/ directory."""

from pathlib import Path

DYNAMIC_DIR = Path(__file__).parent / "dynamic"


def count_tests(exclude_file: Path) -> tuple[int, int]:
    """Return (p4, stf) file counts for a single .exclude file."""
    p4 = stf = 0
    for line in exclude_file.read_text().splitlines():
        line = line.strip()
        if not line:
            continue
        if line.endswith(".p4"):
            p4 += 1
        elif line.endswith(".stf"):
            stf += 1
    return p4, stf


def main():
    top_dirs = sorted(d for d in DYNAMIC_DIR.iterdir() if d.is_dir())

    for top_dir in top_dirs:
        exclude_files = sorted(top_dir.rglob("*.exclude"))
        if not exclude_files:
            continue

        dir_p4 = dir_stf = 0
        rels = [ef.relative_to(top_dir) for ef in exclude_files]
        col = max(len(str(r)) for r in rels)
        col = max(col, len("File"))
        sep = "=" * (col + 16)
        print(sep)
        print(f"  {top_dir.relative_to(DYNAMIC_DIR)}/")
        print(sep)
        print(f"  {'File':<{col}}  {'.p4':>5}  {'.stf':>5}")
        print(f"  {'-'*col}  {'-----':>5}  {'-----':>5}")

        for ef, rel in zip(exclude_files, rels):
            p4, stf = count_tests(ef)
            dir_p4 += p4
            dir_stf += stf
            print(f"  {str(rel):<{col}}  {p4:>5}  {stf:>5}")

        print(f"  {'-'*col}  {'-----':>5}  {'-----':>5}")
        print(f"  {'TOTAL':<{col}}  {dir_p4:>5}  {dir_stf:>5}")
        print()


if __name__ == "__main__":
    main()
