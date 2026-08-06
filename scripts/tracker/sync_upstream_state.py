"""Sync an 'Upstream state' field on the Mismatches board from each item's
'Upstream issue' URL.

The kaist-plrg P4-SpecTec issue stays the authoritative item content; this
just annotates each mismatch with the open/closed/merged state of its
related p4lang/p4c or p4lang/p4-spec issue/PR, read from the 'Upstream
issue' text field. Re-run any time to refresh (it's a periodic sync, not
live). Needs a gh token with Projects write on the org + public-repo read.

Usage:
  python3 sync_upstream_state.py --project 14 --owner kaist-plrg [--dry-run]

Creates the 'Upstream state' single-select field (open/closed/merged) if it
doesn't exist, then sets it on every item whose 'Upstream issue' URL
resolves.
"""
import argparse
import json
import re
import subprocess
import sys

URL = re.compile(r"github\.com/([^/]+)/([^/]+)/(?:issues|pull)/(\d+)")
STATE_Q = ('query($o:String!,$n:String!,$num:Int!){repository(owner:$o,name:$n)'
           '{issueOrPullRequest(number:$num){... on Issue{state} ... on PullRequest{state}}}}')


def gq(query, variables=None):
    body = {"query": query}
    if variables:
        body["variables"] = variables
    r = subprocess.run(["gh", "api", "graphql", "--input", "-"],
                       input=json.dumps(body), capture_output=True, text=True)
    if not r.stdout.strip().startswith("{"):
        raise RuntimeError(r.stderr or r.stdout)
    return json.loads(r.stdout)


def _project_id(owner, number):
    return gq('query($o:String!,$n:Int!){organization(login:$o){projectV2(number:$n){id}}}',
              {"o": owner, "n": number})["data"]["organization"]["projectV2"]["id"]


def _find_field(owner, number, name):
    fs = gq('query($o:String!,$n:Int!){organization(login:$o){projectV2(number:$n)'
            '{fields(first:50){nodes{... on ProjectV2SingleSelectField{id name options{id name}}}}}}}',
            {"o": owner, "n": number})["data"]["organization"]["projectV2"]["fields"]["nodes"]
    for f in fs:
        if f.get("name") == name:
            return f["id"], {o["name"]: o["id"] for o in f["options"]}
    return None, None


def _create_state_field(project_id):
    f = gq('mutation($p:ID!,$opts:[ProjectV2SingleSelectFieldOptionInput!]){'
           'createProjectV2Field(input:{projectId:$p,dataType:SINGLE_SELECT,name:"Upstream state",'
           'singleSelectOptions:$opts}){projectV2Field{... on ProjectV2SingleSelectField{id options{id name}}}}}',
           {"p": project_id, "opts": [{"name": "open", "color": "GREEN", "description": ""},
                                      {"name": "closed", "color": "PURPLE", "description": ""},
                                      {"name": "merged", "color": "PINK", "description": ""}]}
           )["data"]["createProjectV2Field"]["projectV2Field"]
    return f["id"], {o["name"]: o["id"] for o in f["options"]}


def _all_items(project_id):
    items, cursor = [], None
    while True:
        d = gq('query($p:ID!,$c:String){node(id:$p){... on ProjectV2{items(first:80,after:$c)'
               '{pageInfo{hasNextPage endCursor} nodes{id fieldValues(first:30){nodes'
               '{... on ProjectV2ItemFieldTextValue{text field{... on ProjectV2FieldCommon{name}}}}}}}}}}',
               {"p": project_id, "c": cursor})["data"]["node"]["items"]
        items += d["nodes"]
        if not d["pageInfo"]["hasNextPage"]:
            return items
        cursor = d["pageInfo"]["endCursor"]


def _upstream_url(item):
    for v in item["fieldValues"]["nodes"]:
        if v.get("field", {}).get("name") == "Upstream issue" and v.get("text"):
            return v["text"]
    return None


def main():
    ap = argparse.ArgumentParser(description="Sync 'Upstream state' from 'Upstream issue' URLs.")
    ap.add_argument("--project", type=int, required=True, help="Mismatches project number")
    ap.add_argument("--owner", default="kaist-plrg")
    ap.add_argument("--dry-run", action="store_true")
    args = ap.parse_args()

    pid = _project_id(args.owner, args.project)
    fid, opt = _find_field(args.owner, args.project, "Upstream state")
    if fid is None:
        if args.dry_run:
            print("would create 'Upstream state' field (open/closed/merged)")
            opt = {"open": "?", "closed": "?", "merged": "?"}
        else:
            fid, opt = _create_state_field(pid)

    done = skipped = 0
    counts = {}
    for it in _all_items(pid):
        url = _upstream_url(it)
        if not url:
            skipped += 1
            continue
        m = URL.search(url)
        if not m:
            skipped += 1
            continue
        o, n, num = m.groups()
        node = gq(STATE_Q, {"o": o, "n": n, "num": int(num)})["data"]["repository"]["issueOrPullRequest"]
        if not node:
            skipped += 1
            continue
        st = node["state"].lower()
        if st not in opt:
            skipped += 1
            continue
        counts[st] = counts.get(st, 0) + 1
        if args.dry_run:
            print(f"  {o}/{n}#{num} -> {st}")
        else:
            gq('mutation($p:ID!,$i:ID!,$f:ID!,$o:String!){updateProjectV2ItemFieldValue(input:'
               '{projectId:$p,itemId:$i,fieldId:$f,value:{singleSelectOptionId:$o}}){projectV2Item{id}}}',
               {"p": pid, "i": it["id"], "f": fid, "o": opt[st]})
        done += 1
    verb = "would set" if args.dry_run else "set"
    print(f"{verb} Upstream state on {done} item(s) ({skipped} without a resolvable upstream) | {counts}",
          file=sys.stderr if args.dry_run else sys.stdout)


if __name__ == "__main__":
    main()
