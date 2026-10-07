#!/usr/bin/env python3
"""Diff test: run programs with krun and with the K spec, and compare.

For each program, krun --dry-run parses it and gives the command krun runs:
the LLVM backend's interpreter on the initial term. Running that command
gives the step count (--statistics) and the final configuration. The K spec
then runs from the same initial term (spectec-boot krun) and its final
configuration is compared with krun's after normalization (-expect).

When krun ends with an error (a hook reporting an invalid argument, e.g. a
division by zero), the step where it fails is found by running the
interpreter with step limits, and the K spec must fail at the same step:
after the same configuration as the run one step before, or on the initial
term.

A nondeterministic program (e.g. with threads) is checked step by step
(--check-steps-for): each configuration of the spec's run must be one of the
next configurations that the search binary of the kompiled definition (kompile
--enable-search) finds one step on, and the interpreter must take no step from
the last. The step count is not compared with krun's run. NAME:N checks only
the first N steps, for a program whose runs may not end.

usage:
  spec-k/scripts/difftest.py -k imp-kompiled tests/*.imp
  spec-k/scripts/difftest.py -k imp-kompiled tests/ --ext imp --timeout 600 -j 2

spectec-boot must be built (make boot).
"""
import argparse
import collections
import concurrent.futures
import glob
import os
import re
import shutil
import subprocess
import sys
import tempfile
import time

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
SPEC = [os.path.join(ROOT, "spec-meta/common/0-stdlib.watsup")] + sorted(
    glob.glob(os.path.join(ROOT, "spec-k/[0-9]*.watsup")))
BOOT = os.path.join(ROOT, "spectec-boot")

# The executor caches calls by their arguments; the K spec passes whole
# configurations, so a smaller cache bounds memory without slowing it down
# (32K entries: 63 s and 424 MB for 1,500 steps of KOOL factorial).
SPEC_ENV = dict(os.environ, SPECTEC_CACHE_SIZE=os.environ.get("SPECTEC_CACHE_SIZE", "32768"))

SpecRun = collections.namedtuple("SpecRun", "status steps ok secs msg")


def oneline(text, limit=160):
    return " ".join(text.split())[:limit]


def run(cmd, timeout, stdin=None, env=None):
    start = time.monotonic()
    try:
        p = subprocess.run(cmd, capture_output=True, text=True, timeout=timeout, input=stdin, env=env)
        return p.returncode, p.stdout, p.stderr, time.monotonic() - start
    except subprocess.TimeoutExpired:
        return None, "", "", time.monotonic() - start


class Interpreter:
    """The interpreter command krun runs: interpreter <initial term> <depth>
    <output>, with the program's standard input. preload is a library that
    makes it exit at once on an uncaught exception (fastthrow.c)."""

    preload = None

    def __init__(self, words, stdin):
        self.binary, self.init, self.depth = words[0], words[1], words[2]
        self.stdin = stdin

    def run(self, out, depth, timeout):
        """Run it, stopped after depth steps when depth is given; return (steps,
        result file, seconds, error). With --statistics the interpreter writes
        the step count as the first line of its output."""
        raw = out + ".raw"
        if os.path.exists(raw):
            os.remove(raw)  # the interpreter appends to its output file
        depth = self.depth if depth is None else str(depth)
        env = dict(os.environ, LD_PRELOAD=self.preload) if self.preload else None
        rc, _, err, secs = run([self.binary, self.init, depth, raw, "--statistics"], timeout, self.stdin, env)
        if rc is None:
            return None, None, secs, "krun timeout"
        if rc != 0 or not os.path.exists(raw):
            return None, None, secs, "krun failed (exit %s): %s" % (rc, oneline(err))
        count, term = open(raw).read().split("\n", 1)
        with open(out, "w") as fh:
            fh.write(term)
        return int(count), out, secs, None


def run_krun(kompiled, program, workdir, krun_args, timeout):
    """Run krun; return (steps, interpreter command, result file, seconds, error).

    krun --dry-run parses the program and prints the command it would run:
    the LLVM backend's interpreter, with the initial term after parsing and
    macro expansion as its first argument (krun keeps several tmp.in.* files,
    some of them before macro expansion). That command is then run directly,
    which is what krun does, without starting krun again."""
    # standard input comes from <program>.in when it exists, as in the tutorial Makefiles
    stdin = open(program + ".in").read() if os.path.exists(program + ".in") else ""
    dry = os.path.join(workdir, "dry")
    os.makedirs(dry, exist_ok=True)
    rc, out, err, _ = run(["krun", "-d", os.path.abspath(kompiled), os.path.abspath(program)] + krun_args
                          + ["--save-temps", "--temp-dir", dry, "--dry-run"], timeout, stdin)
    line = next((l for l in (out + err).splitlines() if re.search(r"(^|/)interpreter \S+ -?\d+ ", l)), None)
    if rc != 0 or line is None:
        return None, None, None, 0.0, "krun --dry-run failed (exit %s): %s" % (rc, oneline(out + err))
    words = line.split()
    command = Interpreter(words[next(i for i, w in enumerate(words) if w.endswith("interpreter")):], stdin)
    steps, result, secs, err = command.run(os.path.join(workdir, "result.kore"), None, timeout)
    return steps, command, result, secs, err


def fast_throw():
    """Build fastthrow.c with gcc once; the library, or None without gcc"""
    source = os.path.join(os.path.dirname(os.path.abspath(__file__)), "fastthrow.c")
    out = os.path.join(tempfile.gettempdir(), "difftest-fastthrow-%d.so" % os.getuid())
    if not os.path.exists(out) or os.path.getmtime(out) < os.path.getmtime(source):
        if not shutil.which("gcc") or subprocess.run(["gcc", "-shared", "-fPIC", "-O2", "-o", out + ".tmp", source],
                                                     capture_output=True).returncode != 0:
            return None
        os.replace(out + ".tmp", out)
    return out


def failing_step(command, workdir, timeout):
    """For a program on which krun fails: (n, result after n steps), where step
    n + 1 fails, or (None, None) when the initial term fails. The interpreter
    succeeds exactly when stopped before the failing step."""
    def at(n):
        return command.run(os.path.join(workdir, "depth%d.kore" % n), n, timeout)[1]
    if at(0) is None:
        return None, None
    ok, bad = 0, 1
    while at(bad) is not None:
        ok, bad = bad, bad * 2
        if bad > 1 << 24:
            raise RuntimeError("krun fails, but not within %d steps" % bad)
    while bad - ok > 1:
        mid = (ok + bad) // 2
        if at(mid) is not None:
            ok = mid
        else:
            bad = mid
    return ok, at(ok)


def capped(cmd, memory_max):
    """Run under a memory cap, so that only this process is killed when it is exceeded."""
    if not memory_max:
        return cmd
    return ["systemd-run", "--user", "--scope", "--quiet", "-p", "MemoryMax=" + memory_max,
            "-p", "MemorySwapMax=0"] + cmd


def run_spec(definition, init, out, compare, extra, timeout, memory_max):
    """Run the K spec, comparing as compare says (-expect FILE or -check-steps DIR)."""
    cmd = [BOOT, "krun"] + SPEC + ["-def", definition, "-init", init, "-o", out] + compare + extra
    rc, stdout, stderr, secs = run(capped(cmd, memory_max), timeout, env=SPEC_ENV)
    if rc is None:
        return SpecRun("timeout", None, False, secs, "")
    m = re.search(r"^(final|ERROR) after (\d+) steps", stdout, re.M)
    if not m:
        if "initial term failed to evaluate" in stdout:
            return SpecRun("initfail", 0, False, secs, "")
        lines = (stdout + stderr).strip().splitlines()
        # a negative exit code is the signal that killed the process (e.g. -9 when out of memory)
        return SpecRun("crash", None, False, secs, "exit %s: %s" % (rc, oneline(lines[-1]) if lines else "no output"))
    failed = re.search(r"^steps check FAILED: (.*)$", stdout, re.M)
    ok = "matches " in stdout or "steps check: each of" in stdout
    return SpecRun("final" if m.group(1) == "final" else "error", int(m.group(2)), ok, secs,
                   failed.group(1) if failed else "")


def judge(spec, k_steps, fails_at, check, limit):
    """The verdict on a run of the spec. fails_at is the step after which krun
    fails (None: on the initial term; False: krun does not fail)."""
    if fails_at is None:
        return "pass (both fail on the initial term)" if spec.status == "initfail" else \
            "FAIL: krun fails on the initial term, the spec: " + spec.status
    if fails_at is not False:
        if spec.status == "error" and spec.ok and spec.steps == fails_at:
            return "pass (both fail after %d steps)" % fails_at
        return "FAIL: krun fails after %d steps, the spec: %s after %s steps%s" % (
            fails_at, spec.status, spec.steps, "" if spec.ok else ", configuration differs")
    if spec.status != "final":
        return "FAIL: " + spec.status + (": " + spec.msg if spec.msg else "")
    if check:
        if not spec.ok:
            return "FAIL: " + (spec.msg or "steps check")
        if limit and spec.steps == limit:
            return "pass (each of the first %d steps is one K allows; the run goes on)" % limit
        return "pass (each step is one K allows)"
    if not spec.ok:
        return "FAIL: configuration differs"
    return "pass" if spec.steps == k_steps else "FAIL: steps differ"


def test_program(prog, root, definition, args):
    """Diff test one program; returns (table row, verdict)."""
    name = os.path.basename(prog)
    work = os.path.join(root, name)
    os.makedirs(work, exist_ok=True)
    limits = dict((c.split(":") + [None])[:2] for c in args.check_steps_for)
    check = name in limits
    limit = int(limits[name]) if check and limits[name] else args.depth
    krun_args = args.krun_arg + (["--depth", str(args.depth)] if args.depth else [])
    k_steps, command, result, k_secs, err = run_krun(args.kompiled, prog, work, krun_args, args.timeout)
    fails_at = False
    if err and err.startswith("krun failed") and command:
        fails_at, result = failing_step(command, work, args.timeout)
        k_steps = "fails at init" if fails_at is None else "fails after %d" % fails_at
        err = None
    if err:
        return "| %s | - | - | skip: %s | %.1f | 0.0 |" % (name, err, k_secs), "skip: " + err
    if check:
        k_steps = "each step"
    compare = ["-check-steps", os.path.abspath(args.kompiled)] if check else ["-expect", result or os.devnull]
    extra = ["-depth", str(limit)] if limit else []
    if args.cover_dir:
        os.makedirs(args.cover_dir, exist_ok=True)
        extra += ["-cover", os.path.join(os.path.abspath(args.cover_dir), name + ".log")]
    spec = run_spec(definition, command.init, os.path.join(work, "spec.kore"), compare, extra, args.timeout,
                    args.memory_max)
    verdict = judge(spec, k_steps, fails_at, check, limit)
    steps = "-" if spec.steps is None else spec.steps
    return "| %s | %s | %s | %s | %.1f | %.1f |" % (name, k_steps, steps, verdict, k_secs, spec.secs), verdict


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("-k", "--kompiled", required=True, help="the <name>-kompiled directory")
    ap.add_argument("programs", nargs="+", help="program files or directories")
    ap.add_argument("--ext", default=None, help="extension of programs in directories (e.g. imp)")
    ap.add_argument("--timeout", type=float, default=1800, help="seconds per run of the spec")
    ap.add_argument("--krun-arg", action="append", default=[], help="extra argument for krun")
    ap.add_argument("--depth", type=int, default=None, help="stop krun and the spec after this many steps")
    ap.add_argument("--keep", default=None, help="keep work files in this directory")
    ap.add_argument("--exclude", action="append", default=[], help="skip programs with this file name")
    ap.add_argument("--memory-max", default="2500M" if shutil.which("systemd-run") else "",
                    help="memory cap for each run of the spec (systemd-run MemoryMax; empty for none, "
                         "the default without systemd)")
    ap.add_argument("--cover-dir", default=None,
                    help="write the spec with the instructions each run executes marked to <dir>/<program>.log "
                         "(spectec-boot krun -cover; merge them with coverage_summary.py)")
    ap.add_argument("-j", "--jobs", type=int, default=1, help="programs to test at a time")
    ap.add_argument("--check-steps-for", action="append", default=[], metavar="NAME[:N]",
                    help="check each step of this program against the search binary instead of comparing "
                         "with krun's run (nondeterministic programs); :N checks only the first N steps")
    args = ap.parse_args()

    programs = []
    for p in args.programs:
        if os.path.isdir(p):
            programs += sorted(f for f in glob.glob(os.path.join(p, "*." + args.ext if args.ext else "*"))
                               if os.path.isfile(f) and not f.endswith((".out", ".in")))
        else:
            programs.append(p)
    programs = [p for p in programs if os.path.basename(p) not in args.exclude]
    definition = os.path.join(args.kompiled, "definition.kore")
    if not os.path.exists(definition):
        sys.exit("no definition.kore in %s" % args.kompiled)

    root = args.keep or tempfile.mkdtemp(prefix="difftest-")
    Interpreter.preload = fast_throw()
    print("| program | krun steps | spec steps | result | krun s | spec s |")
    print("|---|---|---|---|---|---|")
    # programs run --jobs at a time; rows are printed in program order
    with concurrent.futures.ThreadPoolExecutor(max_workers=args.jobs) as pool:
        futures = [pool.submit(test_program, prog, root, definition, args) for prog in programs]
        verdicts = []
        for future in futures:
            row, verdict = future.result()
            print(row, flush=True)
            verdicts.append(verdict)
    passed = sum(1 for v in verdicts if v.startswith("pass"))
    skipped = sum(1 for v in verdicts if v.startswith("skip"))
    failures = len(verdicts) - passed - skipped
    print("\n%d passed, %d failed, %d skipped (work files: %s)" % (passed, failures, skipped, root))
    sys.exit(1 if failures else 0)


if __name__ == "__main__":
    main()
