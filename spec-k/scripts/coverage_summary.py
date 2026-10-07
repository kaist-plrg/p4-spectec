#!/usr/bin/env python3
"""Merge and summarize the coverage logs of spectec-boot krun -cover.

usage: coverage_summary.py <log or directory>... [-o merged.log] [--missed FILE ...]

Each log lists the spec, each definition under a ";; <file>:<lines>" header,
with its instructions marked + (executed) or - (not). Logs of runs with the
same spec have the same lines, so an instruction counts as executed when some
log marks it +. The summary counts executed instructions per spec file; -o
writes the merged log, and --missed prints the instructions no run executed
in the given spec files, under their definition headers.
"""
import argparse
import collections
import glob
import os
import re
import sys

MARK = re.compile(r"([+-])( +\d+\. .*)")


def read(path):
    with open(path) as fh:
        lines = fh.read().split("\n")
    return lines[1:]  # the first line is the coverage of that run alone


def merge(paths):
    merged = None
    for path in paths:
        lines = read(path)
        if merged is None:
            merged = lines
            continue
        if len(lines) != len(merged):
            sys.exit("%s is not a log of the same spec" % path)
        for i, line in enumerate(lines):
            if line.startswith("+") and merged[i].startswith("-"):
                merged[i] = "+" + merged[i][1:]
    return merged


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("logs", nargs="+", help="coverage logs, or directories searched for *.log")
    ap.add_argument("-o", "--output", help="write the merged log")
    ap.add_argument("--missed", nargs="*", default=[], help="print missed instructions of these spec files")
    args = ap.parse_args()
    paths = []
    for p in args.logs:
        paths += sorted(glob.glob(os.path.join(p, "**", "*.log"), recursive=True)) if os.path.isdir(p) else [p]
    if not paths:
        sys.exit("no logs")
    lines = merge(paths)

    counts = collections.defaultdict(lambda: [0, 0])  # file -> [executed, total]
    missed = collections.defaultdict(list)
    current = header = None
    for line in lines:
        m = re.match(r";; (\S+?):(\d+)", line)
        if m:
            # paths relative to the worktree, e.g. spec-k/2-match.watsup
            current = re.sub(r"^.*/(?=spec-k/|spec-meta/)", "", m.group(1))
            header = line.strip()
            continue
        m = MARK.match(line)
        if m and current:
            counts[current][1] += 1
            if m.group(1) == "+":
                counts[current][0] += 1
            elif current in args.missed:
                missed[current].append((header, line.rstrip()))
    total = [sum(c[0] for c in counts.values()), sum(c[1] for c in counts.values())]
    if args.output:
        with open(args.output, "w") as fh:
            fh.write(";; Instruction coverage: %d/%d (%.2f%%), %d runs\n"
                     % (total[0], total[1], 100.0 * total[0] / total[1], len(paths)) + "\n".join(lines))

    print("%d runs" % len(paths))
    print("| file | executed | instructions | % |")
    print("|---|---|---|---|")
    for f, (h, t) in sorted(counts.items()):
        print("| %s | %d | %d | %.1f |" % (f, h, t, 100.0 * h / t if t else 0))
    print("| total | %d | %d | %.1f |" % (total[0], total[1], 100.0 * total[0] / total[1] if total[1] else 0))
    for f in args.missed:
        print("\n## missed in %s" % f)
        last = None
        for header, line in missed[f]:
            if header != last:
                print(header)
                last = header
            print(line)


if __name__ == "__main__":
    main()
