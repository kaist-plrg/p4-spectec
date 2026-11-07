"""Mirror the `duplicate` label onto the Soundness board's `Duplicate` field.

Each issue carrying the `duplicate` label gets `Duplicate = O`; every other
item gets the field cleared. The label is authoritative, so adding or
removing it is all a human does -- the board follows.

`plan_duplicate()` is the pure, unit-tested core; the `__main__` block reads
the board via GraphQL and issues the mutations through `sync_board`'s
helpers, so it needs a Projects read+write token.
"""
import argparse
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))
from sync_board import _field_meta, _gq, _set

LABEL = "duplicate"
FIELD = "Duplicate"
MARK = "O"


def plan_duplicate(items):
    """Pure diff. `items`: dicts with id, number, labels (names), duplicate
    (current field value or None). Returns [(id, number, target)] for every
    item whose field disagrees with its labels; target is MARK or None."""
    changes = []
    for it in items:
        target = MARK if LABEL in it["labels"] else None
        if (it.get("duplicate") or None) != target:
            changes.append((it["id"], it["number"], target))
    return changes


# ---- I/O (not unit-tested; needs a live board + write token) ----

QUERY = """
query($o:String!,$n:Int!,$c:String){
  organization(login:$o){
    projectV2(number:$n){
      items(first:100, after:$c){
        pageInfo{ hasNextPage endCursor }
        nodes{
          id
          content{ ... on Issue { number labels(first:50){ nodes{ name } } } }
          fieldValueByName(name:"%s"){ ... on ProjectV2ItemFieldSingleSelectValue { name } }
        }
      }
    }
  }
}
""" % FIELD


def _board_items(number, owner):
    out, cursor = [], None
    while True:
        variables = {"o": owner, "n": number}
        if cursor:
            variables["c"] = cursor
        items = _gq(QUERY, variables)["data"]["organization"]["projectV2"]["items"]
        for node in items["nodes"]:
            content = node.get("content") or {}
            if content.get("number") is None:
                continue
            out.append({
                "id": node["id"],
                "number": content["number"],
                "labels": {l["name"] for l in content["labels"]["nodes"]},
                "duplicate": (node.get("fieldValueByName") or {}).get("name"),
            })
        if not items["pageInfo"]["hasNextPage"]:
            return out
        cursor = items["pageInfo"]["endCursor"]


def main():
    ap = argparse.ArgumentParser(description="Mirror the duplicate label onto the Soundness board.")
    ap.add_argument("--project", type=int, required=True, help="Soundness project number")
    ap.add_argument("--owner", default="kaist-plrg")
    ap.add_argument("--dry-run", action="store_true")
    args = ap.parse_args()

    changes = plan_duplicate(_board_items(args.project, args.owner))
    for _, number, target in changes:
        print(f"#{number}: {FIELD} -> {target or '(clear)'}")
    if args.dry_run or not changes:
        print(f"{len(changes)} change(s){' (dry run)' if args.dry_run else ''}")
        return

    pid, fmeta = _field_meta(args.project, args.owner)
    for iid, _, target in changes:
        _set(pid, iid, fmeta, FIELD, target)
    print(f"applied {len(changes)} change(s)")


if __name__ == "__main__":
    main()
