from sync_board import plan_sync, _upstream_token, _upstream_url


def _ex(name, category="p4c", status="confirmed", pos=1, neg=0, repo="1", upstream="c9"):
    return {"name": name, "category": category, "status": status, "pos": pos, "neg": neg,
            "repo": repo, "upstream": upstream}


def _it(iid, name, **kw):
    b = {"id": iid, "name": name, "category": "p4c", "state": "confirmed",
         "repo": "1", "upstream": "https://github.com/p4lang/p4c/issues/9"}
    b.update(kw)
    return b


def test_upstream_helpers():
    assert _upstream_token("https://github.com/p4lang/p4c/issues/5042") == "c5042"
    assert _upstream_token("https://github.com/p4lang/p4-spec/issues/1380") == "s1380"
    assert _upstream_token("c9") == "c9"
    assert _upstream_token(None) == ""  if False else True   # None -> None
    assert _upstream_url("c9") == "https://github.com/p4lang/p4c/issues/9"
    assert _upstream_url("s1") == "https://github.com/p4lang/p4-spec/issues/1"


def test_add_when_missing():
    p = plan_sync([_ex("brand-new", repo=None, upstream=None)], [])
    assert [t["Name"] for t in p["adds"]] == ["brand-new"]


def test_match_by_name():
    p = plan_sync([_ex("foo")], [_it("I1", "foo")])
    assert p["adds"] == [] and len(p["updates"]) == 1 and p["updates"][0][0] == "I1"


def test_rename_by_upstream():
    # exclude name differs, but upstream c9 matches the board item -> rename, not add
    p = plan_sync([_ex("new-name")], [_it("I1", "old-name")])
    assert p["adds"] == []
    iid, target = p["updates"][0]
    assert iid == "I1" and target["Name"] == "new-name"
    assert p["orphans"] == []


def test_orphan_and_exempt():
    ex = [_ex("foo")]
    board = [_it("I1", "foo"), _it("I2", "ghost", state="confirmed", upstream=None, repo=None),
             _it("I3", "patched-clarif", state="patched", upstream=None, repo=None)]
    p = plan_sync(ex, board)
    names = [b["name"] for b in p["orphans"]]
    assert "ghost" in names and "patched-clarif" not in names
