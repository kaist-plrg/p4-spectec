# Tracker board maintenance

Scripts that keep the kaist-plrg tracker boards in sync with this repo.

- Three GitHub Projects: **Mismatches**, **Soundness**, **Negative-testgen**.
- CI checks Mismatches and Soundness on PRs; Negative-testgen is frozen.
- Creating and seeding the boards is a one-time step, not covered here.

## Scripts

- `tracker_lib.py`: parses an `.exclude` path into `name`, `repo`, `upstream`, `layer`, `category`, `status`, `#Pos`, `#Neg`.
- `dump_board.py`: dumps a board via GraphQL into flat `{field: value}` items. `--from-file` reads a saved response.
- `tracker_check.py`: Mismatches drift check between `excludes/` and the board.
- `label_check.py`: checks that every `label:soundness` issue is on the Soundness board.
- `sync_board.py`: writes the fields derived from `excludes/` onto the Mismatches board.
- `sync_upstream_state.py`: fills `Upstream state` from each item's `Upstream issue`.
- `test_*.py`, `testdata/`: offline tests, run by the `unit` CI job.

## Mismatches

Each item is a kaist-plrg issue, or a draft if there is no issue.

Fields:
- `Name`: `.exclude` filename tail; the key CI matches on.
- `Layer`: `static` or `dynamic`.
- `Category`: `p4c`, `p4-spec`, `p4c-specific`, `target-specific`, `p4testgen`.
- `State`: `reported` → `confirmed` → `discussed` → `pending` → `patched`, or `future`, `out-of-scope`, `implicit`.
- `Repo issue`, `Upstream issue`, `Upstream state` (`open`/`closed`/`merged`), `Patch PR`.
- `#Pos`, `#Neg`: test counts from `excludes/`.
- `Arch` (dynamic only), `Owner`, `Discussed` (e.g. `2026-04`), `Notes`.

Maintenance:
- Edit `.exclude` files only; don't edit derived fields on the board.
- Filename: `i<repo>-[c|s]<n>-<name>.exclude` (see `tracker_lib.parse_name`). The directory sets `Category` and `State`.
- On push to `main` and daily, `sync_board.py` updates `Name`, `Category`, `State`, `Repo issue`, `Upstream issue`, `#Pos`, `#Neg`, renames renamed items, and adds new ones. Hand-written fields are left alone.
- On PRs touching `excludes/` or `scripts/tracker/`, the `drift` job fails when:
  - an `.exclude` has no board item;
  - a board item has no `.exclude`, unless its `State` is `patched`, `future`, or `out-of-scope`, or its `Status` is `Done`;
  - a derived field disagrees.
- Deleting an `.exclude` leaves its item on the board. Set its `Status` to `Done` to keep it as a record, or delete the item.
- `Upstream state` is refreshed daily by `tracker-upstream-sync.yml`.

## Soundness

- No custom fields; the issue's title, labels, and open/closed state are the data.
- File the issue with the `soundness` label; the project auto-adds it.
- The `membership` CI job checks every `label:soundness` issue is on the board.

## Negative-testgen

- Frozen set of p4lang/p4c issues; nothing to maintain.
- Fields: `Kind` (`compiler` or `soundness`), `Case ID` (`CB1`–`CB14`, `SB1`–`SB15`), `Campaign` (`C1`–`C6`), `P4C issue`.

## Running locally

Needs an authenticated `gh` and `python3` (3.9+).

```bash
# Mismatches drift check
python3 scripts/tracker/dump_board.py <project> --owner kaist-plrg --out board.json
python3 scripts/tracker/tracker_check.py --board board.json

# Soundness membership
python3 scripts/tracker/label_check.py --label soundness --project <project> --owner kaist-plrg

# Refresh Upstream state (needs a Projects read/write token; --dry-run to preview)
python3 scripts/tracker/sync_upstream_state.py --project <project> --owner kaist-plrg

# Offline tests
python3 -m pytest scripts/tracker/ -q
```
