"""CI drift check: keep excludes/ rows in sync with the GitHub Projects board.

Pure core (`check`) is offline and unit-tested with injected data. The
`__main__` block wires real inputs (excludes via tracker_lib.walk_excludes,
board via a flat items JSON -- normally produced by dump_board.py's GraphQL
dump, `{"items": [...]}`) and is not tested.
"""
import argparse
import json
import re
import sys
from pathlib import Path

_UPSTREAM_URL_RE = re.compile(
    r"^https://github\.com/p4lang/(p4c|p4-spec)/issues/(\d+)$"
)
_UPSTREAM_TOKEN_RE = re.compile(r"^[cs]\d+$")


def _s(v):
    """Stringify for comparison, treating None/missing as ''."""
    return "" if v is None else str(v)


def _upstream_token(v):
    """Normalize an `Upstream issue` value to its `c<n>`/`s<n>` token.

    The board's `Upstream issue` field holds a full p4lang/p4c or
    p4lang/p4-spec issue URL; tracker_lib.walk_excludes yields the
    filename token (`c<n>`/`s<n>`) instead. Both sides must be
    normalized to the same token before comparing, or every seeded
    row spuriously mismatches. A `c<n>`/`s<n>` value is returned
    unchanged; anything else not matching (empty, a /pull/ URL,
    None) normalizes to '' via `_s`.
    """
    s = _s(v)
    if _UPSTREAM_TOKEN_RE.match(s):
        return s
    m = _UPSTREAM_URL_RE.match(s)
    if m:
        repo, number = m.group(1), m.group(2)
        prefix = "c" if repo == "p4c" else "s"
        return f"{prefix}{number}"
    return ""


def check(excludes_rows, board_items):
    """Compare excludes/ rows against board items.

    Order-independent: keyed by name via dicts, never positional.
    Returns a list of human-readable violation strings (empty = OK).
    """
    mismatch_items = list(board_items)
    by_name = {it.get("Name"): it for it in mismatch_items}

    violations = []
    for row in excludes_rows:
        name = row["name"]
        item = by_name.get(name)
        if item is None:
            violations.append(f"excludes '{name}' not on board")
            continue

        if _s(item.get("Category")) != _s(row["category"]):
            violations.append(
                f"'{name}' category mismatch: board={item.get('Category')!r} "
                f"excludes={row['category']!r}"
            )
        if _s(item.get("State")) != _s(row["status"]):
            violations.append(
                f"'{name}' status mismatch: board={item.get('State')!r} "
                f"excludes={row['status']!r}"
            )
        if _s(item.get("Repo issue")) != _s(row["repo"]):
            violations.append(
                f"'{name}' repo issue mismatch: board={item.get('Repo issue')!r} "
                f"excludes={row['repo']!r}"
            )
        if _upstream_token(item.get("Upstream issue")) != _upstream_token(row["upstream"]):
            violations.append(
                f"'{name}' upstream issue mismatch: board={item.get('Upstream issue')!r} "
                f"excludes={row['upstream']!r}"
            )
        if item.get("#Pos") != row["pos"] or item.get("#Neg") != row["neg"]:
            violations.append(
                f"'{name}' pos/neg count mismatch: "
                f"board=({item.get('#Pos')!r},{item.get('#Neg')!r}) "
                f"excludes=({row['pos']!r},{row['neg']!r})"
            )

    exclude_names = {row["name"] for row in excludes_rows}
    exempt_statuses = {"patched", "future", "out-of-scope"}
    for item in mismatch_items:
        name = item.get("Name")
        if not name or name in exclude_names:
            continue
        status = item.get("State")
        if _s(status) in exempt_statuses:
            continue
        violations.append(
            f"board item '{name}' has no matching .exclude (status={status!r})"
        )

    return violations


def _load_board_items(path):
    """Accept either dump_board.py's flat dump ({"items": [...]})
    or a bare list of the dicts `check` expects."""
    data = json.loads(Path(path).read_text())
    if isinstance(data, dict) and "items" in data:
        return data["items"]
    return data


def main():
    parser = argparse.ArgumentParser(
        description="CI drift check: verify excludes/ rows match the GitHub Projects board."
    )
    parser.add_argument("--excludes", default="excludes",
                         help="Path to the excludes/ directory (default: excludes)")
    parser.add_argument("--board", required=True,
                         help="Path to a JSON dump of board items "
                              "(dump_board.py's {\"items\": [...]}, or a bare list)")
    args = parser.parse_args()

    sys.path.insert(0, str(Path(__file__).resolve().parent))
    import tracker_lib

    excludes_rows = tracker_lib.walk_excludes(Path(args.excludes))
    board_items = _load_board_items(args.board)

    violations = check(excludes_rows, board_items)
    if violations:
        for v in violations:
            print(v)
        sys.exit(1)
    else:
        print("tracker check: OK")
        sys.exit(0)


if __name__ == "__main__":
    main()
