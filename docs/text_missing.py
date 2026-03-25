#!/usr/bin/env python3
"""
Collect integers that appear right after // (with optional spaces),
e.g.  // 123    //456 note    //   789 - fix
"""

import sys
from pathlib import Path
import re
from typing import Set, Iterable

SCRIPT_DIR = Path(__file__).resolve().parent

def collect_numbers_from_paths(paths: Iterable[Path]) -> Set[int]:
    """Collect all unique integers after // in the given files"""
    pattern = re.compile(r'//\s*(\d+)')
    numbers: Set[int] = set()

    for filepath in paths:
        if not filepath.is_file() or not filepath.suffix.lower() == '.adoc':
            continue

        try:
            with filepath.open(encoding="utf-8") as f:
                for line in f:
                    for match in pattern.finditer(line):
                        num_str = match.group(1)
                        numbers.add(int(num_str))
        except Exception as e:
            print(f"Error reading {filepath}: {e}", file=sys.stderr)

    return numbers

def process_directory(directory: str | Path) -> Set[int]:
    root = Path(directory).resolve()

    if not root.is_dir():
        print(f"Error: Not a directory: {root}", file=sys.stderr)
        sys.exit(1)

    adoc_files = list(root.rglob("*.adoc")) + list(root.rglob("*.adoc"))

    return collect_numbers_from_paths(adoc_files)

def process_single_file(filepath: str | Path) -> Set[int]:
    path = Path(filepath).resolve()

    if not path.is_file():
        print(f"Error: Not a file: {path}", file=sys.stderr)
        sys.exit(1)

    if path.suffix.lower() != '.adoc':
        print(f"Warning: File does not have .adoc extension: {path}")

    return collect_numbers_from_paths([path])

def print_missing(numbers: Set[int], total: int) -> None:
    numbers_sorted = sorted(numbers)

    print(f"Missing {len(numbers)} out of {total} paragraphs {'(' + f'{len(numbers)/total:.2%}' + ')'}:")
    print("-" * 120)

    # Print in neat columns
    for i, num in enumerate(numbers_sorted, 1):
        print(f"{num:6d}", end="  " if i % 15 != 0 else "\n")
    if len(numbers_sorted) % 15 != 0:
        print()

def main():
    numbers_original = process_single_file(SCRIPT_DIR / "P4-16-spec.adoc")
    numbers_spliced = process_directory(SCRIPT_DIR / "sections-skeleton")
    numbers_missing = numbers_original - numbers_spliced
    print_missing(numbers_missing, len(numbers_original))

if __name__ == "__main__":
    main()
