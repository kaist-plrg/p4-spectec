"""Generic native-project drift check: every `label:<L>` issue is on project <N>.

Backs both the Soundness and Negative-testgen boards (native GitHub Projects
with a built-in auto-add-by-label workflow): this check catches the case
where the workflow didn't fire (e.g. it was added/edited after the label,
or the workflow was briefly misconfigured) by comparing the labelled-issue
set against the board's item set directly, independent of the workflow.

Pure core (`check_label_subset`) is offline and unit-tested
(test_label_check.py). The `__main__` block wires the live inputs (`gh
issue list --label` for labelled issues, `dump_board.fetch_live_pages` +
`extract_items` for project items) or, with `--issues-file`/`--board-file`,
loads both offline instead (never calls `gh`) -- mirrors dump_board.py's
`--from-file` offline path.
"""
import argparse
import json
import subprocess
import sys
from pathlib import Path

import dump_board


def check_label_subset(labeled_numbers, project_numbers):
    """Every number in `labeled_numbers` must also be in `project_numbers`.

    Returns a list of human-readable violation strings, one per labelled
    issue missing from the project (empty list = OK). Order-independent
    (both args are sets); the returned list is sorted for determinism.
    """
    missing = labeled_numbers - project_numbers
    return [
        f"issue #{n} has label but is not on the project"
        for n in sorted(missing)
    ]


def _fetch_labeled_live(label, repo):
    result = subprocess.run(
        ["gh", "issue", "list", "--repo", repo, "--label", label,
         "--state", "all", "--limit", "500", "--json", "number",
         "--jq", ".[].number"],
        check=True, capture_output=True, text=True,
    )
    return {int(line) for line in result.stdout.splitlines() if line.strip()}


def _load_labeled_offline(path):
    """Load labelled issue numbers from a JSON file: a bare list of ints."""
    data = json.loads(Path(path).read_text())
    return {int(n) for n in data}


def _fetch_project_numbers_live(owner, project_number):
    pages = dump_board.fetch_live_pages(owner, project_number)
    items = dump_board.extract_items(pages)
    return {it["_issue_number"] for it in items if it.get("_issue_number") is not None}


def _load_project_numbers_offline(path):
    """Load project item issue numbers from a board-file.

    Accepts either a saved `gh api graphql` response (the shape
    `dump_board.load_offline_pages` reads, same as
    `testdata/native-project-sample.json`) or a bare list of ints
    (pre-extracted numbers) -- kept simple, mirroring dump_board's
    `--from-file` shape.
    """
    data = json.loads(Path(path).read_text())
    if isinstance(data, list):
        return {int(n) for n in data}
    pages = dump_board.load_offline_pages(path)
    items = dump_board.extract_items(pages)
    return {it["_issue_number"] for it in items if it.get("_issue_number") is not None}


def main():
    parser = argparse.ArgumentParser(
        description="Check that every label:<L> issue is an item on project <N>."
    )
    parser.add_argument("--label", required=True, help="Label to check (e.g. soundness)")
    parser.add_argument("--project", type=int, default=None,
                         help="Projects v2 board number (required for the live path)")
    parser.add_argument("--owner", default="kaist-plrg",
                         help="Organization login owning the board (default: kaist-plrg)")
    parser.add_argument("--repo", default="kaist-plrg/p4-spectec",
                         help="Repo to list labelled issues from "
                              "(default: kaist-plrg/p4-spectec)")
    parser.add_argument("--issues-file", default=None,
                         help="Offline: JSON list of labelled issue numbers, "
                              "instead of `gh issue list`")
    parser.add_argument("--board-file", default=None,
                         help="Offline: saved GraphQL board dump (or a bare list of "
                              "issue numbers), instead of dump_board.fetch_live_pages")
    args = parser.parse_args()

    if args.issues_file and args.board_file:
        labeled_numbers = _load_labeled_offline(args.issues_file)
        project_numbers = _load_project_numbers_offline(args.board_file)
    else:
        if args.project is None:
            parser.error("--project is required for the live path "
                          "(omit only together with --issues-file/--board-file)")
        labeled_numbers = _fetch_labeled_live(args.label, args.repo)
        project_numbers = _fetch_project_numbers_live(args.owner, args.project)

    violations = check_label_subset(labeled_numbers, project_numbers)
    if violations:
        for v in violations:
            print(v)
        sys.exit(1)
    else:
        print(f"label check '{args.label}': OK")
        sys.exit(0)


if __name__ == "__main__":
    main()
