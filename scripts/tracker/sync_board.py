"""Push excludes/ into the Mismatches board (the inverse of tracker_check).

`excludes/` is authoritative for a mismatch's derivable fields; this makes
the board mirror them so the drift check stays green. For every `.exclude`
it sets the board item's `Name`, `Category`, `State`, `Repo issue`,
`Upstream issue`, `#Pos`, `#Neg` from the file/dir/filename, renames an item
whose `.exclude` was renamed (matched by upstream or repo issue), and adds
an item for a new `.exclude`. Hand-authored columns (`Owner`, `Report`,
`Discussed`, `Notes`, `Patch PR`, `Upstream state`) are never touched.

`plan_sync()` is the pure, unit-tested core; the `__main__` block resolves
field ids and issues the GraphQL mutations, so it needs a Projects
read+write token. Board items with a Name matching no `.exclude` (and a
non-exempt State) are reported, not deleted -- removing an item is left to a
human.
"""
import argparse
import json
import re
import subprocess
import sys
import time
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))
import tracker_lib

_URL = re.compile(r"github\.com/p4lang/(p4c|p4-spec)/issues/(\d+)")
EXEMPT = {"patched", "future", "out-of-scope"}


def _upstream_token(v):
    if not v:
        return None
    m = re.match(r"^([cs])(\d+)$", v)
    if m:
        return v
    m = _URL.search(v)
    return (("c" if m.group(1) == "p4c" else "s") + m.group(2)) if m else None


def _upstream_url(token):
    if not token:
        return None
    repo = "p4c" if token[0] == "c" else "p4-spec"
    return f"https://github.com/p4lang/{repo}/issues/{token[1:]}"


def plan_sync(exrows, board_items):
    """Pure diff. `board_items`: dicts with id, name, category, state, repo,
    upstream (token or URL), pos, neg. Returns updates/adds/orphans."""
    by_name = {b.get("name"): b for b in board_items if b.get("name")}
    by_up = {_upstream_token(b.get("upstream")): b for b in board_items if _upstream_token(b.get("upstream"))}
    by_repo = {str(b.get("repo")): b for b in board_items if b.get("repo")}
    used, updates, adds = set(), [], []
    for r in exrows:
        b = by_name.get(r["name"]) \
            or (by_up.get(r["upstream"]) if r["upstream"] else None) \
            or (by_repo.get(r["repo"]) if r["repo"] else None)
        target = {
            "Name": r["name"],
            "Category": r["category"] or None,
            "State": r["status"] or None,
            "Repo issue": r["repo"] or None,
            "Upstream issue": _upstream_url(r["upstream"]),
            "#Pos": r["pos"],
            "#Neg": r["neg"],
        }
        if b is None:
            adds.append(target)
        else:
            used.add(b["id"])
            updates.append((b["id"], target))
    orphans = [b for b in board_items
               if b["id"] not in used and b.get("name")
               and (b.get("state") or "") not in EXEMPT]
    return {"updates": updates, "adds": adds, "orphans": orphans}


# ---- I/O (not unit-tested; needs a live board + write token) ----

def _gq(query, variables=None, retries=4):
    body = {"query": query}
    if variables:
        body["variables"] = variables
    for _ in range(retries):
        r = subprocess.run(["gh", "api", "graphql", "--input", "-"],
                           input=json.dumps(body), capture_output=True, text=True)
        try:
            d = json.loads(r.stdout)
            if d.get("data") is not None and "errors" not in d:
                return d
        except json.JSONDecodeError:
            pass
        time.sleep(1.5)
    raise RuntimeError((r.stdout or r.stderr)[:300])


def _field_meta(number, owner):
    q = ('query($o:String!,$n:Int!){organization(login:$o){projectV2(number:$n){id '
         'fields(first:60){nodes{__typename ... on ProjectV2FieldCommon{id name} '
         '... on ProjectV2SingleSelectField{id name options{id name}}}}}}}')
    p = _gq(q, {"o": owner, "n": number})["data"]["organization"]["projectV2"]
    fields = {f["name"]: {"id": f["id"], "opts": {o["name"]: o["id"] for o in f.get("options", [])}}
              for f in p["fields"]["nodes"] if f.get("name")}
    return p["id"], fields


def _board_items(number, owner):
    raw = subprocess.run(["gh", "project", "item-list", str(number), "--owner", owner,
                          "--format", "json", "--limit", "200"], capture_output=True, text=True)
    out = []
    for it in json.loads(raw.stdout)["items"]:
        out.append({"id": it["id"], "name": it.get("name"), "category": it.get("category"),
                    "state": it.get("state"), "repo": it.get("repo issue"),
                    "upstream": it.get("upstream issue")})
    return out


def _set(pid, iid, fmeta, fname, value):
    fid = fmeta[fname]["id"]
    if value is None or value == "":
        _gq('mutation($p:ID!,$i:ID!,$f:ID!){clearProjectV2ItemFieldValue(input:{projectId:$p,itemId:$i,fieldId:$f}){projectV2Item{id}}}',
            {"p": pid, "i": iid, "f": fid})
        return
    opts = fmeta[fname]["opts"]
    if opts:  # single-select
        oid = opts.get(value)
        if not oid:
            print(f"  WARNING: no option {value!r} for {fname}", file=sys.stderr)
            return
        inner = f'{{singleSelectOptionId:"{oid}"}}'
    elif fname in ("#Pos", "#Neg"):
        inner = f'{{number:{int(value)}}}'
    else:
        inner = f'{{text:{json.dumps(str(value))}}}'
    _gq('mutation($p:ID!,$i:ID!,$f:ID!){updateProjectV2ItemFieldValue(input:{projectId:$p,itemId:$i,fieldId:$f,value:'
        + inner + '}){projectV2Item{id}}}', {"p": pid, "i": iid, "f": fid})


def _apply_fields(pid, iid, fmeta, target):
    for fname, value in target.items():
        _set(pid, iid, fmeta, fname, value)


def _add_item(pid, fmeta, target, owner):
    repo = target["Repo issue"]
    if repo:
        nid = _gq('query($o:String!,$r:String!,$n:Int!){repository(owner:$o,name:$r){issue(number:$n){id}}}',
                  {"o": owner, "r": "p4-spectec", "n": int(repo)})["data"]["repository"]["issue"]["id"]
        iid = _gq('mutation($p:ID!,$c:ID!){addProjectV2ItemById(input:{projectId:$p,contentId:$c}){item{id}}}',
                  {"p": pid, "c": nid})["data"]["addProjectV2ItemById"]["item"]["id"]
    else:
        iid = _gq('mutation($p:ID!,$t:String!){addProjectV2DraftIssue(input:{projectId:$p,title:$t}){projectItem{id}}}',
                  {"p": pid, "t": target["Name"]})["data"]["addProjectV2DraftIssue"]["projectItem"]["id"]
    _apply_fields(pid, iid, fmeta, target)


def main():
    ap = argparse.ArgumentParser(description="Push excludes/ into the Mismatches board.")
    ap.add_argument("--project", type=int, required=True, help="Mismatches project number")
    ap.add_argument("--owner", default="kaist-plrg")
    ap.add_argument("--excludes", default="excludes")
    ap.add_argument("--dry-run", action="store_true")
    args = ap.parse_args()

    exrows = tracker_lib.walk_excludes(Path(args.excludes))
    items = _board_items(args.project, args.owner)
    plan = plan_sync(exrows, items)

    print(f"updates={len(plan['updates'])}  adds={len(plan['adds'])}  orphans={len(plan['orphans'])}")
    if plan["adds"]:
        print("would ADD:", ", ".join(t["Name"] for t in plan["adds"]))
    if plan["orphans"]:
        print("ORPHANS (board item, no .exclude — remove by hand):",
              ", ".join(str(b.get("name")) for b in plan["orphans"]))
    if args.dry_run:
        return

    pid, fmeta = _field_meta(args.project, args.owner)
    for iid, target in plan["updates"]:
        _apply_fields(pid, iid, fmeta, target)
    for target in plan["adds"]:
        _add_item(pid, fmeta, target, args.owner)
    print(f"applied {len(plan['updates'])} update(s), {len(plan['adds'])} add(s)")


if __name__ == "__main__":
    main()
