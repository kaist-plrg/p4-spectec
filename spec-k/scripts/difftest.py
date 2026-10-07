#!/usr/bin/env python3
"""Diff test: run programs with krun and with the K spec, and compare.

For each program, krun runs once with --save-temps, which gives the step
count (--statistics), the initial term it passed to the LLVM interpreter
(tmp.in.*), and the final configuration (result.kore). The K spec then runs
from the same initial term (spectec-boot krun) and its final configuration is
compared with krun's after normalization (-expect).

When krun ends with an error (a hook reporting an invalid argument, e.g. a
division by zero), the step where it fails is found with krun --depth, and
the K spec must fail at the same step: after the same configuration as
krun --depth gives one step before, or on the initial term.

usage:
  spec-k/scripts/difftest.py -k imp-kompiled tests/*.imp
  spec-k/scripts/difftest.py -k imp-kompiled tests/ --ext imp --timeout 600

spectec-boot must be built (make boot).
"""
import argparse
import glob
import os
import re
import shutil
import subprocess
import sys
import tempfile
import time

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
SPEC = [os.path.join(ROOT, "spec-meta/common/0-stdlib.watsup")] + sorted(glob.glob(os.path.join(ROOT, "spec-k/[0-9]*.watsup")))
BOOT = os.path.join(ROOT, "spectec-boot")


def oneline(text, limit=160):
    return " ".join(text.split())[:limit]


# The executor caches calls by their arguments; the K spec passes whole
# configurations, so a smaller cache bounds memory without slowing it down
# (32K entries: 63 s and 424 MB for 1,500 steps of KOOL factorial).
SPEC_ENV = dict(os.environ, SPECTEC_CACHE_SIZE=os.environ.get("SPECTEC_CACHE_SIZE", "32768"))


def run(cmd, timeout, cwd=None, stdin=None, env=None):
    start = time.monotonic()
    try:
        p = subprocess.run(cmd, capture_output=True, text=True, timeout=timeout, cwd=cwd, input=stdin, env=env)
        return p.returncode, p.stdout, p.stderr, time.monotonic() - start
    except subprocess.TimeoutExpired:
        return None, "", "", time.monotonic() - start


def run_krun(kompiled, program, workdir, krun_args, timeout):
    """Run krun; return (steps, initial term file, result file, seconds, error).

    krun --dry-run prints the interpreter command, whose first argument is the
    initial term after parsing and macro expansion; krun keeps several
    tmp.in.* files, some of them before macro expansion. A second, real run
    gives the step count and the final configuration."""
    base = ["krun", "-d", os.path.abspath(kompiled), os.path.abspath(program)] + krun_args
    # standard input comes from <program>.in when it exists, as in the tutorial Makefiles
    stdin = open(program + ".in").read() if os.path.exists(program + ".in") else ""
    dry = os.path.join(workdir, "dry")
    os.makedirs(dry, exist_ok=True)
    rc, out, err, _ = run(base + ["--save-temps", "--temp-dir", dry, "--dry-run"], timeout, stdin=stdin)
    m = re.search(r"interpreter (\S+) -?\d+ ", out + err)
    if rc != 0 or not m or not os.path.exists(m.group(1)):
        return None, None, None, 0.0, "krun --dry-run failed (exit %s): %s" % (rc, oneline(out + err))
    init = m.group(1)
    temp = os.path.join(workdir, "krun")
    os.makedirs(temp, exist_ok=True)
    rc, out, err, secs = run(base + ["--save-temps", "--temp-dir", temp, "--output", "kore", "--statistics"], timeout,
                             stdin=stdin)
    if rc is None:
        return None, init, None, secs, "krun timeout"
    if rc != 0:
        return None, init, None, secs, "krun failed (exit %s): %s" % (rc, oneline(err))
    m = re.search(r"\[(\d+) steps\]", out + err)
    steps = int(m.group(1)) if m else None
    results = glob.glob(os.path.join(temp, ".krun-*", "result.kore"))
    if not results:
        return steps, init, None, secs, "krun left no result.kore"
    return steps, init, results[0], secs, None


def krun_at_depth(kompiled, program, workdir, krun_args, n, timeout):
    """krun stopped after n steps: the result file, or None when it fails."""
    temp = os.path.join(workdir, "depth%d" % n)
    os.makedirs(temp, exist_ok=True)
    stdin = open(program + ".in").read() if os.path.exists(program + ".in") else ""
    rc, _, _, _ = run(["krun", "-d", os.path.abspath(kompiled), os.path.abspath(program), "--depth", str(n),
                       "--save-temps", "--temp-dir", temp, "--output", "kore"] + krun_args, timeout, stdin=stdin)
    results = glob.glob(os.path.join(temp, ".krun-*", "result.kore"))
    return results[0] if rc == 0 and results else None


def failing_step(kompiled, program, workdir, krun_args, timeout):
    """For a program on which krun fails: (n, result of krun --depth n), where
    step n + 1 fails, or (None, None) when the initial term fails. krun --depth
    n succeeds exactly for the n before the failing step."""
    if krun_at_depth(kompiled, program, workdir, krun_args, 0, timeout) is None:
        return None, None
    ok, bad = 0, 1
    while krun_at_depth(kompiled, program, workdir, krun_args, bad, timeout) is not None:
        ok, bad = bad, bad * 2
        if bad > 1 << 24:
            raise RuntimeError("krun fails, but not within %d steps" % bad)
    while bad - ok > 1:
        mid = (ok + bad) // 2
        if krun_at_depth(kompiled, program, workdir, krun_args, mid, timeout) is not None:
            ok = mid
        else:
            bad = mid
    return ok, krun_at_depth(kompiled, program, workdir, krun_args, ok, timeout)


def run_search(kompiled, init, workdir, timeout, memory_max):
    """All final states krun --search would find: run the search binary of the
    kompiled definition (it needs kompile --enable-search) on the initial term.
    Returns (file with a disjunction of final states, seconds, error)."""
    out = os.path.join(workdir, "search.kore")
    binary = os.path.join(os.path.abspath(kompiled), "search")
    if not os.path.exists(binary):
        return None, 0.0, "no search binary (kompile --enable-search)"
    rc, _, err, secs = run(capped([binary, init, "-1", out], memory_max), timeout)
    if rc is None:
        return None, secs, "search timeout"
    if rc != 0 or not os.path.exists(out):
        return None, secs, "search failed (exit %s): %s" % (rc, oneline(err))
    return out, secs, None


def capped(cmd, memory_max):
    """Run under a memory cap, so that only this process is killed when it is exceeded."""
    if not memory_max:
        return cmd
    return ["systemd-run", "--user", "--scope", "--quiet", "-p", "MemoryMax=" + memory_max,
            "-p", "MemorySwapMax=0"] + cmd


def run_spec(boot, definition, init, result, out, timeout, memory_max=None, extra=(), search=False):
    """Run the K spec; return (status, steps, matches, seconds, message).
    With search, result holds all final states and the spec must reach one of them."""
    expect = "-expect-any" if search else "-expect"
    cmd = capped([boot, "krun"] + SPEC + ["-def", definition, "-init", init, expect, result, "-o", out]
                 + list(extra), memory_max)
    rc, stdout, stderr, secs = run(cmd, timeout, env=SPEC_ENV)
    if rc is None:
        return "timeout", None, False, secs, ""
    m = re.search(r"^(final|ERROR) after (\d+) steps", stdout, re.M)
    if not m and "initial term failed to evaluate" in stdout:
        return "initfail", 0, False, secs, ""
    if not m:
        msg = (stdout + stderr).strip().splitlines()
        # a negative exit code is the signal that killed the process (e.g. -9 when out of memory)
        return "error", None, False, secs, "exit %s: %s" % (rc, oneline(msg[-1]) if msg else "no output")
    status = "final" if m.group(1) == "final" else "stuck-error"
    return status, int(m.group(2)), ("matches " in stdout or "matches one of" in stdout), secs, ""


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("-k", "--kompiled", required=True, help="the <name>-kompiled directory")
    ap.add_argument("programs", nargs="+", help="program files or directories")
    ap.add_argument("--ext", default=None, help="extension of programs in directories (e.g. imp)")
    ap.add_argument("--timeout", type=float, default=1800, help="seconds per run of the spec")
    ap.add_argument("--krun-arg", action="append", default=[], help="extra argument for krun")
    ap.add_argument("--boot", default=BOOT)
    ap.add_argument("--keep", default=None, help="keep work files in this directory")
    ap.add_argument("--exclude", action="append", default=[], help="skip programs with this file name")
    ap.add_argument("--memory-max", default="2500M" if shutil.which("systemd-run") else "",
                    help="memory cap for each run of the spec (systemd-run MemoryMax; empty for none, "
                         "the default without systemd)")
    ap.add_argument("--spec-arg", action="append", default=[], help="extra argument for spectec-boot krun")
    ap.add_argument("--cover-dir", default=None,
                    help="write the spec with the instructions each run executes marked to <dir>/<program>.log "
                         "(spectec-boot krun -cover; merge them with coverage_summary.py)")
    ap.add_argument("--search-timeout", type=float, default=None, help="seconds for krun's search (default: --timeout)")
    ap.add_argument("--search-for", action="append", default=[],
                    help="compare this program against all final states krun --search finds (nondeterministic "
                         "programs, e.g. with threads); the step count is not compared")
    args = ap.parse_args()

    programs = []
    for p in args.programs:
        if os.path.isdir(p):
            ext = args.ext or ""
            programs += sorted(f for f in glob.glob(os.path.join(p, "*." + ext if ext else "*"))
                               if os.path.isfile(f) and not f.endswith((".out", ".in")))
        else:
            programs.append(p)
    programs = [p for p in programs if os.path.basename(p) not in args.exclude]
    definition = os.path.join(args.kompiled, "definition.kore")
    if not os.path.exists(definition):
        sys.exit("no definition.kore in %s" % args.kompiled)

    root = args.keep or tempfile.mkdtemp(prefix="difftest-")
    rows, failures = [], 0
    print("| program | krun steps | spec steps | result | krun s | spec s |")
    print("|---|---|---|---|---|---|")
    for prog in programs:
        name = os.path.basename(prog)
        work = os.path.join(root, name)
        os.makedirs(work, exist_ok=True)
        search = name in args.search_for
        k_steps, init, result, k_secs, err = run_krun(args.kompiled, prog, work, args.krun_arg, args.timeout)
        fails_at = False  # krun fails: after this many steps, or None on the initial term
        if err and err.startswith("krun failed") and init:
            fails_at, result = failing_step(args.kompiled, prog, work, args.krun_arg, args.timeout)
            k_steps = "fails at init" if fails_at is None else "fails after %d" % fails_at
            err = None
        if search and init:
            k_steps = "search"
            result, k_secs, err = run_search(args.kompiled, init, work, args.search_timeout or args.timeout,
                                             args.memory_max)
        if err:
            verdict, s_steps, s_secs = "skip: " + err, None, 0.0
        else:
            extra = list(args.spec_arg)
            if args.cover_dir:
                os.makedirs(args.cover_dir, exist_ok=True)
                extra += ["-cover", os.path.join(os.path.abspath(args.cover_dir), name + ".log")]
            status, s_steps, ok, s_secs, msg = run_spec(args.boot, definition, init,
                                                         result or os.devnull,
                                                         os.path.join(work, "spec.kore"), args.timeout,
                                                         args.memory_max, extra, search)
            if fails_at is None:
                verdict = "pass (both fail on the initial term)" if status == "initfail" else \
                    "FAIL: krun fails on the initial term, the spec: " + status
            elif fails_at is not False:
                if status == "stuck-error" and ok and s_steps == fails_at:
                    verdict = "pass (both fail after %d steps)" % fails_at
                else:
                    verdict = "FAIL: krun fails after %d steps, the spec: %s after %s steps%s" % (
                        fails_at, status, s_steps, "" if ok else ", configuration differs")
            elif search and status == "final":
                verdict = "pass (one of the search results)" if ok else "FAIL: not among the search results"
            elif status == "final" and ok and s_steps == k_steps:
                verdict = "pass"
            elif status == "final" and ok:
                verdict = "FAIL: steps differ"
            elif status == "final":
                verdict = "FAIL: configuration differs"
            else:
                verdict = "FAIL: " + status + (": " + msg if msg else "")
        if not verdict.startswith(("pass", "skip")):
            failures += 1
        rows.append((name, verdict))
        fmt = lambda x: "-" if x is None else str(x)
        print("| %s | %s | %s | %s | %.1f | %.1f |" % (name, fmt(k_steps), fmt(s_steps), verdict, k_secs, s_secs),
              flush=True)
    passed = sum(1 for _, v in rows if v.startswith("pass"))
    skipped = sum(1 for _, v in rows if v.startswith("skip"))
    print("\n%d passed, %d failed, %d skipped (work files: %s)" % (passed, failures, skipped, root))
    sys.exit(1 if failures else 0)


if __name__ == "__main__":
    main()
