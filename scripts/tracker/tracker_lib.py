import re
from pathlib import Path


def parse_name(stem):
    repo = upstream = None
    m = re.match(r"i(\d+)-(.*)", stem)
    if m: repo, stem = m.group(1), m.group(2)
    m = re.match(r"([cs]\d+)-(.*)", stem)
    if m: upstream, stem = m.group(1), m.group(2)
    return {"repo": repo, "upstream": upstream, "name": stem}


def walk_excludes(root):
    rows = []
    for f in sorted(Path(root).rglob("*.exclude")):
        rel = f.relative_to(root).parts
        layer = rel[0]
        category = rel[1] if len(rel) > 2 else None
        status = rel[2] if len(rel) > 3 else None
        pos = neg = 0
        for line in f.read_text().splitlines():
            line = line.strip()
            if not line or line.startswith("#"): continue
            if "p4_16_samples" in line: pos += 1
            elif "p4_16_errors" in line: neg += 1
        rows.append({**parse_name(f.stem), "layer": layer, "category": category,
                     "status": status, "pos": pos, "neg": neg, "path": str(f)})
    return rows
