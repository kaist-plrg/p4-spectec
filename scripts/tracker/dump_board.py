"""Dump a GitHub Projects v2 board to the flat item list `tracker_check.py` consumes.

Why GraphQL and not `gh project item-list --format json`: that command keys
each custom field by a version-dependent token, not a stable name. This
script queries the board directly via GraphQL, which pairs every field value
with its explicit field NAME -- deterministic and testable offline.

Pure core (`extract_items`) turns paginated GraphQL item pages into flat
{field name: value} dicts and is unit-tested against a fixture
(test_dump_board.py). The `__main__` block wires the real `gh api graphql`
calls (paginated) or, with `--from-file`, loads a saved response instead
(never calls `gh`) -- this is how the offline/CI-fixture path is exercised.
"""
import argparse
import json
import subprocess
from pathlib import Path

QUERY = """
query($org:String!,$num:Int!,$cursor:String){
  organization(login:$org){
    projectV2(number:$num){
      items(first:100, after:$cursor){
        pageInfo{ hasNextPage endCursor }
        nodes{
          content{ ... on Issue { number title } ... on DraftIssue { title } }
          fieldValues(first:40){ nodes{
            ... on ProjectV2ItemFieldTextValue        { text   field{ ... on ProjectV2FieldCommon { name } } }
            ... on ProjectV2ItemFieldNumberValue      { number field{ ... on ProjectV2FieldCommon { name } } }
            ... on ProjectV2ItemFieldSingleSelectValue { name   field{ ... on ProjectV2FieldCommon { name } } }
          }}
        }
      }
    }
  }
}
"""


def extract_items(pages):
    """Flatten paginated GraphQL `items` pages into `check()`-shaped dicts.

    `pages` is a list of `data.organization.projectV2.items` objects (one
    per page), each holding a `nodes` list of item nodes. For each item
    node: `Title` comes from `content.title` (Issue or DraftIssue);
    `_issue_number` comes from `content.number` (Issue only -- absent/None
    for a DraftIssue, which has no `number`); each `fieldValues.nodes` entry
    contributes `flat[field name] = value`, where value is whichever of
    text/name/number is present (an explicit None check, not `or`, so a
    NUMBER field legitimately holding 0 -- e.g. `#Neg`=0 -- is not lost).
    Field-value nodes with no `field` (the `{}` nodes GraphQL emits for
    retired/inaccessible fields) are skipped.
    """
    flat_items = []
    for page in pages:
        for node in page.get("nodes", []):
            flat = {}
            content = node.get("content") or {}
            if content.get("title") is not None:
                flat["Title"] = content["title"]
            flat["_issue_number"] = content.get("number")

            for field_value in node.get("fieldValues", {}).get("nodes", []):
                field = field_value.get("field")
                if not field:
                    continue
                name = field.get("name")
                if name is None:
                    continue
                for key in ("text", "name", "number"):
                    value = field_value.get(key)
                    if value is not None:
                        flat[name] = value
                        break

            flat_items.append(flat)
    return flat_items


def _run_graphql(org, number, cursor):
    args = [
        "gh", "api", "graphql",
        "-f", f"query={QUERY}",
        "-f", f"org={org}",
        "-F", f"num={number}",
    ]
    if cursor:
        args += ["-f", f"cursor={cursor}"]
    result = subprocess.run(args, check=True, capture_output=True, text=True)
    return json.loads(result.stdout)


def fetch_live_pages(org, number):
    """Page through the live board via `gh api graphql`, first:100 + cursor."""
    pages = []
    cursor = None
    while True:
        data = _run_graphql(org, number, cursor)
        items = data["data"]["organization"]["projectV2"]["items"]
        pages.append(items)
        page_info = items["pageInfo"]
        if not page_info.get("hasNextPage"):
            break
        cursor = page_info.get("endCursor")
    return pages


def load_offline_pages(path):
    """Load a single saved `gh api graphql` response as a one-page list."""
    data = json.loads(Path(path).read_text())
    items = data["data"]["organization"]["projectV2"]["items"]
    return [items]


def main():
    parser = argparse.ArgumentParser(
        description="Dump a GitHub Projects v2 board to a flat board-items JSON file."
    )
    parser.add_argument("project_number", type=int, help="Projects v2 board number")
    parser.add_argument("--owner", default="kaist-plrg",
                         help="Organization login owning the board (default: kaist-plrg)")
    parser.add_argument("--from-file", default=None,
                         help="Load a saved `gh api graphql` response instead of "
                              "calling `gh` (offline path, e.g. for CI fixtures)")
    parser.add_argument("--out", default="board.json",
                         help="Path to write the flat {\"items\": [...]} JSON (default: board.json)")
    args = parser.parse_args()

    if args.from_file:
        pages = load_offline_pages(args.from_file)
    else:
        pages = fetch_live_pages(args.owner, args.project_number)

    items = extract_items(pages)
    Path(args.out).write_text(json.dumps({"items": items}, indent=2) + "\n")
    print(f"wrote {len(items)} item(s) to {args.out}")


if __name__ == "__main__":
    main()
