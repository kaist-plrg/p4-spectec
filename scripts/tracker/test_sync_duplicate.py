from sync_duplicate import plan_duplicate


def _it(iid, number, labels=(), duplicate=None):
    return {"id": iid, "number": number, "labels": set(labels), "duplicate": duplicate}


def test_mark_labelled_item():
    assert plan_duplicate([_it("I1", 1, ["duplicate", "soundness"])]) == [("I1", 1, "O")]


def test_clear_unlabelled_item():
    assert plan_duplicate([_it("I1", 1, ["soundness"], duplicate="O")]) == [("I1", 1, None)]


def test_no_change_when_in_sync():
    items = [_it("I1", 1, ["duplicate"], duplicate="O"), _it("I2", 2, ["soundness"])]
    assert plan_duplicate(items) == []


def test_only_disagreeing_items_change():
    items = [
        _it("I1", 1, ["duplicate"], duplicate="O"),
        _it("I2", 2, ["duplicate"]),
        _it("I3", 3, [], duplicate="O"),
        _it("I4", 4, []),
    ]
    assert plan_duplicate(items) == [("I2", 2, "O"), ("I3", 3, None)]
