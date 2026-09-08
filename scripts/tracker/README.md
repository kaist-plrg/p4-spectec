# Tracker board maintenance

The kaist-plrg tracker spans **three** GitHub Projects — Mismatches,
Soundness, and Negative-testgen. This directory holds the ongoing
drift-check machinery that keeps two of them honest on every PR, plus the
upstream-state sync. (Creating and seeding the boards is a separate one-time
operator step, kept off-repo; this runbook is only about keeping them
correct afterward.)

## Scripts

| script | what it does |
|---|---|
| `tracker_lib.py` | Parse each `.exclude` filename + its directory into `{name, repo, upstream, layer, category, status, #Pos, #Neg}` — the excludes-side data the drift check compares against the board. |
| `dump_board.py` | Dump a GitHub Projects board via GraphQL into flat `{field: value}` items — the input to both checks (`--from-file` reads a saved response for offline runs). |
| `tracker_check.py` | The Mismatches drift check: compares `excludes/` against a dumped board **both** directions (an `.exclude` missing from the board; an orphaned board item; a disagreeing `Category`/`State`/`Repo issue`/`Upstream issue`/counts). |
| `label_check.py` | The Soundness check: verifies every `label:soundness` issue is an item on the project. |
| `sync_board.py` | Push `excludes/` onto the Mismatches board — set each item's derivable fields (`Name`/`Category`/`State`/`Repo issue`/`Upstream issue`/`#Pos`/`#Neg`) from the `.exclude`, rename a renamed one, add a new one. The writer side of the drift check; hand-authored columns untouched. |
| `sync_upstream_state.py` | Fill the Mismatches `Upstream state` field from each item's `Upstream issue` URL — the open/closed/merged state of the related p4lang/p4c or p4lang/p4-spec issue/PR. |

`test_*.py` and the `testdata/` fixtures exercise all of the above offline in the `unit` CI job.

## Board fields

**Mismatches** — each item is a kaist-plrg P4-SpecTec issue (or a draft, for
entries with no tracking issue), annotated with:

| field | meaning |
|---|---|
| `Name` | the `.exclude` filename tail — the key CI joins on |
| `Layer` | `static` (type-checking) or `dynamic` (simulation) |
| `Category` | `p4c` / `p4-spec` / `p4c-specific` / `target-specific` / `p4testgen` |
| `State` | triage status: `reported` → `confirmed` → `discussed` → `pending` → `patched`, plus `future` / `out-of-scope` / `implicit` |
| `Report` | reported upstream? `No` / `Yes-Us` / `Yes-Others` |
| `Repo issue` | the kaist-plrg issue number (also the item's linked content) |
| `Upstream issue` | link to the related p4lang/p4c or p4lang/p4-spec issue/PR |
| `Upstream state` | that upstream's `open` / `closed` / `merged` (synced daily) |
| `Patch PR` | link to the fix, once one exists |
| `#Pos` / `#Neg` | positive / negative test counts, recomputed from `excludes/` |
| `Arch` | `v1model` / `ebpf` / `psa` (dynamic-layer entries) |
| `Owner` | assignee (GitHub login) |
| `Discussed` | month it was discussed, e.g. `2026-04` |
| `Notes` | free text |

**Soundness** — native GitHub Project, so **no custom fields**: the metadata
is the issue itself — its title, its `soundness` (and any other) labels, and
its native open/closed status.

**Negative-testgen** — each item is a real p4lang/p4c issue (native title +
open/closed), annotated with:

| field | meaning |
|---|---|
| `Kind` | `compiler` (a crash) or `soundness` (fails to reject an ill-typed program) |
| `Case ID` | the entry's identifier (`CB1`…`CB14`, `SB1`…`SB15`) |
| `Campaign` | which fuzzing campaign(s) surfaced it (`C1`–`C6`) |
| `P4C issue` | link to the p4lang/p4c issue (redundant with the item; kept for reference) |

## Maintaining each board

**Mismatches** — just edit the `.exclude` files; the board follows. When you
add, rename, move, or remove a `.exclude`, its filename must encode
`i<repo>-[c|s]<n>-name` (repo issue number, upstream c/s issue number, then a
descriptive name — see `tracker_lib.parse_name`) and its directory sets
Category/State. **You do not touch the board by hand**: once the change lands
on `main`, `sync_board.py` (the `sync board from excludes/` workflow) pushes
the derivable fields onto the board — sets `Name`/`Category`/`State`/`Repo
issue`/`Upstream issue`/`#Pos`/`#Neg`, renames a renamed item, adds a new one
— leaving hand-authored columns alone.

The `drift` job is then the safety net: on every PR it diffs the board
against `excludes/` (both directions — a missing item, or an orphaned board
item, exempting patched/future/out-of-scope) and fails if they disagree, so a
sync that didn't run or a hand-tampered board is caught. The only manual case
`sync_board.py` won't auto-resolve is a **true orphan** (a board item whose
`.exclude` was deleted) — it reports it for you to remove.

**Soundness** — file the issue and add the `soundness` label. The project's
native auto-add pulls it in, and the `soundness-check` CI job verifies every
`label:soundness` issue is on the board. No board mutation script to run.

**Negative-testgen** — nothing to maintain. It's a frozen, curated set of
external p4lang/p4c bugs (references to the real issues, whose open/closed
state updates natively); seeded once, never re-run.

**Upstream state** (Mismatches) — each item's `Upstream state` field
(open/closed/merged) is refreshed from its `Upstream issue` URL by
`sync_upstream_state.py`, run daily by `sync-upstream-state.yml` or on
demand. It's a periodic sync, not live; the kaist P4-SpecTec issue stays the
authoritative item content.

## Running the checks locally

Needs [`gh`](https://cli.github.com/) (authenticated) and `python3` (3.9+).

```bash
# Dump the live Mismatches board, then run the drift check against it:
python3 scripts/tracker/dump_board.py <project_number> --owner kaist-plrg --out board.json
python3 scripts/tracker/tracker_check.py --board board.json

# Soundness: check every label:soundness issue is on project <N>:
python3 scripts/tracker/label_check.py --label soundness --project <N>

# Refresh the Mismatches "Upstream state" (needs a Projects read+write token;
# --dry-run previews without writing):
python3 scripts/tracker/sync_upstream_state.py --project <N> --owner kaist-plrg

# The scripts' own tests (offline, no secrets):
python3 -m pytest scripts/tracker/ -q
```
