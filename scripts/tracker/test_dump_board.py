import json
from pathlib import Path

from dump_board import extract_items
from tracker_check import check

FIXTURE = Path(__file__).parent / "testdata" / "board-graphql-sample.json"


def _load_pages():
    data = json.loads(FIXTURE.read_text())
    pages = [data]  # single page (pageInfo.hasNextPage == false)
    return [p["data"]["organization"]["projectV2"]["items"] for p in pages]


def test_extract_from_fixture():
    items = extract_items(_load_pages())
    assert len(items) == 2

    # the mismatch item is fully populated and consumable by check()
    m = next(it for it in items if it.get("Name") == "self-nesting-struct")
    assert m["Name"] and m["Category"] and m["State"] and m["Repo issue"]
    assert m["Title"] == "self-nesting-struct mismatch"
    assert m["#Pos"] == 0  # NUMBER field holding 0 must survive extraction
    assert m["#Neg"] == 1
    assert m["_issue_number"] == 67  # content.number, the Issue's own number

    # the leading empty fieldValue node ({}) did not crash extraction or
    # leave a junk key -- exactly the 7 named fields + Title + _issue_number survive
    assert set(m) == {"Title", "Name", "Category", "State", "Repo issue",
                       "Upstream issue", "#Pos", "#Neg", "_issue_number"}

    # the draft item has only Title (no fieldValues at all), and no issue
    # number (a DraftIssue has no `number` field in the GraphQL response)
    d = next(it for it in items if it.get("Title") == "future idea draft")
    assert d.get("Name") is None
    assert d.get("_issue_number") is None


def test_extract_satisfies_check():
    """The extracted mismatch item matches the real excludes row it mirrors
    (excludes/**/i67-c5240-self-nesting-struct.exclude, see test_tracker_lib.py),
    proving the dump shape is actually consumable by check().

    The fixture's `Upstream issue` is the real URL the board seeds
    (`https://github.com/p4lang/p4c/issues/5240`), NOT the `c5240` token
    tracker_lib.walk_excludes parses from the filename -- so this also
    proves `check()`'s `_upstream_token` normalization makes the two forms
    compare equal end-to-end (the gap that let Blocker 2 slip through).
    """
    items = extract_items(_load_pages())
    m = next(it for it in items if it.get("Name") == "self-nesting-struct")
    assert m["Upstream issue"] == "https://github.com/p4lang/p4c/issues/5240"

    row = {
        "name": "self-nesting-struct", "repo": "67", "upstream": "c5240",
        "category": "p4c", "status": "confirmed", "pos": 0, "neg": 1,
    }
    assert check([row], items) == []
