#!/usr/bin/env python3
"""Find the first step where the K spec and krun disagree, for debugging.

Runs both with a step limit (krun --depth, spectec-boot krun -depth) and
compares the normalized configurations, searching for the first differing
step by bisection. Prints both configurations at that step and the step
before, through a pretty printer if one is given.

usage:
  spec-k/scripts/stepdiff.py -k simple-untyped-kompiled prog.simple --krun-arg=--io --krun-arg=off \
      [--max N] [--pp ../k-in-p4/tools/kore-survey/pp_term.py]
"""
import argparse
import os
import re
import subprocess
import sys
import tempfile

sys.path.insert(0, os.path.dirname(__file__))
from difftest import BOOT, SPEC  # noqa: E402


def krun_at(args, stdin, n, out):
    cmd = ["krun", "-d", os.path.abspath(args.kompiled), os.path.abspath(args.program), "--output", "kore",
           "--depth", str(n)] + args.krun_arg
    p = subprocess.run(cmd, capture_output=True, text=True, input=stdin)
    with open(out, "w") as f:
        f.write(p.stdout.strip().splitlines()[-1] + "\n" if p.stdout.strip() else "")


def spec_at(args, init, n, expect, out):
    cmd = [args.boot, "krun"] + SPEC + ["-def", os.path.join(args.kompiled, "definition.kore"), "-init", init,
                                        "-depth", str(n), "-expect", expect, "-o", out]
    p = subprocess.run(cmd, capture_output=True, text=True)
    m = re.search(r"after (\d+) steps", p.stdout)
    return ("matches " in p.stdout), (int(m.group(1)) if m else None)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("-k", "--kompiled", required=True)
    ap.add_argument("program")
    ap.add_argument("--krun-arg", action="append", default=[])
    ap.add_argument("--max", type=int, default=100000)
    ap.add_argument("--boot", default=BOOT)
    ap.add_argument("--pp", default=None, help="pretty printer for KORE terms")
    args = ap.parse_args()
    stdin = open(args.program + ".in").read() if os.path.exists(args.program + ".in") else ""
    work = tempfile.mkdtemp(prefix="stepdiff-")
    p = subprocess.run(["krun", "-d", os.path.abspath(args.kompiled), os.path.abspath(args.program), "--dry-run",
                        "--save-temps", "--temp-dir", work] + args.krun_arg, capture_output=True, text=True,
                       input=stdin)
    init = re.search(r"interpreter (\S+) -1 ", p.stdout + p.stderr).group(1)

    def same(n):
        expect = os.path.join(work, "krun-%d.kore" % n)
        krun_at(args, stdin, n, expect)
        ok, steps = spec_at(args, init, n, expect, os.path.join(work, "spec-%d.kore" % n))
        return ok

    lo, hi = 0, args.max
    if not same(0):
        lo, hi = -1, 0
    elif same(hi):
        print("same configuration after %d steps" % hi)
        return
    while lo + 1 < hi:  # same(lo) and not same(hi)
        mid = (lo + hi) // 2
        if same(mid):
            lo = mid
        else:
            hi = mid
    print("first difference after step %d" % hi)
    for n in [k for k in (lo, hi) if k >= 0]:
        for who in ("krun", "spec"):
            path = os.path.join(work, "%s-%d.kore" % (who, n))
            if not os.path.exists(path):
                same(n)
            text = open(path).read()
            if args.pp:
                text = subprocess.run([sys.executable, args.pp, path], capture_output=True, text=True).stdout
            print("== %s after %d steps\n%s" % (who, n, text.strip()))


if __name__ == "__main__":
    main()
