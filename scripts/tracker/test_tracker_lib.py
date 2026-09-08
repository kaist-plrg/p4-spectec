from pathlib import Path

from tracker_lib import parse_name, walk_excludes


def test_parse_full():
    assert parse_name("i67-c5240-self-nesting-struct") == {
        "repo": "67", "upstream": "c5240", "name": "self-nesting-struct"}


def test_parse_repo_only():
    assert parse_name("i66-namespace") == {"repo": "66", "upstream": None, "name": "namespace"}


def test_parse_upstream_only():
    assert parse_name("c5524-bmv2-subparser-copy-out") == {
        "repo": None, "upstream": "c5524", "name": "bmv2-subparser-copy-out"}


def test_parse_bare():
    assert parse_name("annotation") == {"repo": None, "upstream": None, "name": "annotation"}


def test_walk_real_excludes():
    rows = walk_excludes(Path(__file__).parents[2] / "excludes")
    assert len(rows) == 59
    byname = {r["name"]: r for r in rows}
    sns = byname["self-nesting-struct"]
    assert sns["repo"] == "67" and sns["upstream"] == "c5240"
    assert sns["layer"] == "static" and sns["category"] == "p4c" and sns["status"] == "confirmed"
    assert sns["pos"] == 0 and sns["neg"] == 1
