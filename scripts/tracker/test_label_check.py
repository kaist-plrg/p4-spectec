import json
from pathlib import Path

import dump_board
from label_check import check_label_subset

FIXTURE = Path(__file__).parent / "testdata" / "native-project-sample.json"


def test_check_label_subset_missing():
    msgs = check_label_subset({196, 197}, {196, 198})
    assert msgs == ["issue #197 has label but is not on the project"]


def test_check_label_subset_full_coverage():
    assert check_label_subset({196, 198}, {196, 198, 200}) == []


def test_check_label_subset_empty_labeled():
    assert check_label_subset(set(), {196, 198}) == []


def _project_numbers_from_fixture():
    pages = dump_board.load_offline_pages(FIXTURE)
    items = dump_board.extract_items(pages)
    return {it["_issue_number"] for it in items if it.get("_issue_number") is not None}


def test_extract_items_yields_issue_numbers_from_fixture():
    numbers = _project_numbers_from_fixture()
    assert numbers == {196, 198}


def test_cross_check_fixture_flags_exactly_the_missing_issue():
    project_numbers = _project_numbers_from_fixture()
    labeled_numbers = project_numbers | {197}  # superset: adds one not on the board
    msgs = check_label_subset(labeled_numbers, project_numbers)
    assert msgs == ["issue #197 has label but is not on the project"]
