from tracker_check import check, _upstream_token


def _row(**kw):
    base = {"name": "foo", "repo": "1", "upstream": "c9", "category": "p4c",
            "status": "confirmed", "pos": 1, "neg": 0}
    base.update(kw)
    return base


def _item(**kw):
    base = {"Name": "foo", "Category": "p4c", "State": "confirmed",
            "Repo issue": "1", "Upstream issue": "c9", "#Pos": 1, "#Neg": 0}
    base.update(kw)
    return base


def test_clean():
    assert check([_row()], [_item()]) == []


def test_missing_on_board():
    msgs = check([_row(name="bar")], [])
    assert any("bar" in m for m in msgs)


def test_category_drift():
    msgs = check([_row()], [_item(Category="p4-spec")])
    assert any("foo" in m for m in msgs)


def test_count_drift():
    msgs = check([_row(neg=1)], [_item()])          # excludes neg=1, board #Neg=0
    assert any("foo" in m for m in msgs)


def test_upstream_token_url_p4c():
    assert _upstream_token("https://github.com/p4lang/p4c/issues/5042") == "c5042"


def test_upstream_token_url_p4_spec():
    assert _upstream_token("https://github.com/p4lang/p4-spec/issues/1373") == "s1373"


def test_upstream_token_passthrough():
    assert _upstream_token("c5042") == "c5042"
    assert _upstream_token("s1373") == "s1373"


def test_upstream_token_empty_and_none():
    assert _upstream_token(None) == ""
    assert _upstream_token("") == ""


def test_upstream_token_pull_url_not_matched():
    assert _upstream_token("https://github.com/p4lang/p4c/pull/5042") == ""


def test_upstream_seeded_url_matches_excludes_token():
    # board seeded with the real URL, excludes row parsed from the
    # filename token (tracker_lib.parse_name) -- must compare equal.
    msgs = check(
        [_row(upstream="c5042")],
        [_item(**{"Upstream issue": "https://github.com/p4lang/p4c/issues/5042"})],
    )
    assert msgs == []


def test_upstream_genuine_mismatch_still_flagged():
    msgs = check(
        [_row(upstream="c5042")],
        [_item(**{"Upstream issue": "https://github.com/p4lang/p4c/issues/9999"})],
    )
    assert any("foo" in m and "upstream" in m for m in msgs)


def test_orphan_board_item_flagged():
    # board has an item with no matching exclude and a non-exempt status
    board = [_item(Name="ghost", State="confirmed")]
    msgs = check([], board)                 # no excludes at all
    assert any("ghost" in m and "no matching .exclude" in m for m in msgs)


def test_orphan_exempt_status_ok():
    # a patched clarification legitimately has no exclude -> not flagged
    board = [_item(Name="clarify-x", State="patched")]
    assert check([], board) == []


def test_orphan_draft_no_name_ok():
    # a draft item (no Name) is not an orphan
    board = [_item(Name="", State="confirmed")]
    assert check([], board) == []


def test_no_orphan_when_matched():
    row = _row(name="foo")
    board = [_item(Name="foo")]
    # foo is on both sides -> no orphan violation (and no missing-on-board violation)
    assert check([row], board) == []
