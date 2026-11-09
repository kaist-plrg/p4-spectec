"""Prints statistics of a K specification.

Usage: python scripts/stat/spec-meta-k.py PATH...

Requires pyk (`pip install kframework`).
Each PATH is a .k file or a directory searched recursively for them.
Files are parsed one by one with pyk's outer parser, so `requires` are not followed.
"""

import sys
from pathlib import Path

from pyk.kast.outer_parser import OuterParser
from pyk.kast.outer_syntax import Rule, SyntaxSentence


def collect_files(paths: list[Path]) -> list[Path]:
    files = []
    for path in paths:
        if path.is_dir():
            files += sorted(file for file in path.rglob("*.k") if "kompiled" not in file.parts)
        elif path.is_file():
            files.append(path)
        else:
            sys.exit(f"cannot read {path}")
    return files


def count_lines(text: str) -> int:
    """Counts every line, including comments and blank lines."""
    return text.count("\n") + (1 if text and not text.endswith("\n") else 0)


def main() -> None:
    if len(sys.argv) < 2:
        sys.exit(__doc__.strip().splitlines()[2])
    files = collect_files([Path(arg) for arg in sys.argv[1:]])

    lines = syntax = rule = 0
    for file in files:
        text = file.read_text()
        lines += count_lines(text)
        for module in OuterParser(text, source=file).definition().modules:
            for sentence in module.sentences:
                syntax += isinstance(sentence, SyntaxSentence)
                rule += isinstance(sentence, Rule)

    rows = [("files", len(files)), ("LoC", lines), ("syntax", syntax), ("rule", rule)]
    for name, count in rows:
        print(f"{name:<14}{count:>8}")


if __name__ == "__main__":
    main()
