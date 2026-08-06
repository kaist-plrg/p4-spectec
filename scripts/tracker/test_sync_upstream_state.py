from sync_upstream_state import URL


def test_issue_url():
    assert URL.search("https://github.com/p4lang/p4c/issues/5042").groups() == ("p4lang", "p4c", "5042")


def test_pull_url():
    assert URL.search("https://github.com/p4lang/p4-spec/pull/1360").groups() == ("p4lang", "p4-spec", "1360")


def test_no_match():
    assert URL.search("not a url") is None
