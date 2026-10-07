#!/usr/bin/env python3
"""Diff test: run programs with krun and with the K spec, and compare.

For each program, krun --dry-run parses it and gives the command krun runs:
the LLVM backend's interpreter on the initial term. Running that command
gives the step count (--statistics) and the final configuration. The K spec
then runs from the same initial term (spectec-boot krun) and its final
configuration is compared with krun's after normalization (-expect).

When krun ends with an error (a hook reporting an invalid argument, e.g. a
division by zero), the step where it fails is found by running the
interpreter with step limits (krun --depth), and the K spec must fail at the
same step: after the same configuration as the run one step before, or on
the initial term.

A nondeterministic program (e.g. with threads) is checked step by step
(--check-steps-for): each configuration of the spec's run must be one of the
next configurations that the search binary of the kompiled definition (kompile
--enable-search) finds one step on, and the interpreter must take no step from
the last. The final configuration is then one of those krun --search finds,
without exploring all runs. The step count is not compared with krun's run.
A program whose runs may not end is checked for its first N steps
(--check-steps-for NAME:N).

usage:
  spec-k/scripts/difftest.py -k imp-kompiled tests/*.imp
  spec-k/scripts/difftest.py -k imp-kompiled tests/ --ext imp --timeout 600 -j 2

spectec-boot must be built (make boot).
"""
import argparse
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
                          + ["--save-temps", "--temp-dir", dry, "--dry-run"], timeout, stdin=stdin)
    line = next((l for l in (out + err).splitlines() if re.search(r"(^|/)interpreter \S+ -?\d+ ", l)), None)
    if rc != 0 or line is None:
        return None, None, None, 0.0, "krun --dry-run failed (exit %s): %s" % (rc, oneline(out + err))
    words = line.split()
    command = Interpreter(words[next(i for i, w in enumerate(words) if w.endswith("interpreter")):], stdin)
    steps, result, secs, err = command.run(os.path.join(workdir, "result.kore"), None, timeout)
    return steps, command, result, secs, err


class Interpreter:
    """The interpreter command krun runs: interpreter <initial term> <depth>
    <output>, with the program's standard input. preload is a library that
    makes it exit at once on an uncaught exception (fast_throw)."""

    preload = None

    def __init__(self, words, stdin):
        self.words, self.stdin = words[:4], stdin
        self.init = words[1]

    def run(self, out, depth, timeout):
        """Run it, stopped after depth steps when depth is given; return (steps,
        result file, seconds, error). With --statistics the interpreter writes
        the step count as the first line of its output."""
        raw = out + ".raw"
        if os.path.exists(raw):
            os.remove(raw)  # the interpreter appends to its output file
        words = [self.words[0], self.init, str(depth) if depth is not None else self.words[2], raw, "--statistics"]
        env = dict(os.environ, LD_PRELOAD=self.preload) if self.preload else None
        rc, _, err, secs = run(words, timeout, stdin=self.stdin, env=env)
        if rc is None:
            return None, None, secs, "krun timeout"
        if rc != 0 or not os.path.exists(raw):
            return None, None, secs, "krun failed (exit %s): %s" % (rc, oneline(err))
        count, term = open(raw).read().split("\n", 1)
        with open(out, "w") as fh:
            fh.write(term)
        return int(count), out, secs, None


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
    """For a program on which krun fails: (n, result of krun --depth n), where
    step n + 1 fails, or (None, None) when the initial term fails. krun --depth
    n succeeds exactly for the n before the failing step."""
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


def run_spec(boot, definition, init, result, out, timeout, memory_max=None, extra=(), check=None):
    """Run the K spec; return (status, steps, matches, seconds, message).
    With check (a kompiled directory), each step is checked against it instead
    of comparing with result."""
    compare = ["-check-steps", check] if check else ["-expect", result]
    cmd = capped([boot, "krun"] + SPEC + ["-def", definition, "-init", init, "-o", out] + compare
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
    if check:
        failed = re.search(r"^steps check FAILED: (.*)$", stdout, re.M)
        return status, int(m.group(2)), "steps check: each of" in stdout, secs, failed.group(1) if failed else ""
    return status, int(m.group(2)), "matches " in stdout, secs, ""


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
    ap.add_argument("-j", "--jobs", type=int, default=1, help="programs to test at a time")
    ap.add_argument("--check-steps-for", action="append", default=[],
                    help="check each step of this program against the next configurations the search binary "
                         "finds, instead of comparing with krun's run (nondeterministic programs, e.g. with "
                         "threads); the step count is not compared. NAME:N checks only the first N steps "
                         "(a program whose runs may not end)")
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


def test_program(prog, root, definition, args):
    """Diff test one program; returns (table row, verdict)."""
    name = os.path.basename(prog)
    work = os.path.join(root, name)
    os.makedirs(work, exist_ok=True)
    limits = dict((c.split(":") + [None])[:2] for c in args.check_steps_for)
    check = os.path.abspath(args.kompiled) if name in limits else None
    limit = int(limits[name]) if check and limits[name] else None
    k_steps, command, result, k_secs, err = run_krun(args.kompiled, prog, work, args.krun_arg, args.timeout)
    init = command.init if command else None
    fails_at = False  # krun fails: after this many steps, or None on the initial term
    if err and err.startswith("krun failed") and command:
        fails_at, result = failing_step(command, work, args.timeout)
        k_steps = "fails at init" if fails_at is None else "fails after %d" % fails_at
        err = None
    if check and init and not err:
        k_steps = "each step"
    s_steps, s_secs = None, 0.0
    if err:
        verdict = "skip: " + err
    else:
        extra = list(args.spec_arg) + (["-depth", str(limit)] if limit else [])
        if args.cover_dir:
            os.makedirs(args.cover_dir, exist_ok=True)
            extra += ["-cover", os.path.join(os.path.abspath(args.cover_dir), name + ".log")]
        status, s_steps, ok, s_secs, msg = run_spec(args.boot, definition, init, result or os.devnull,
                                                     os.path.join(work, "spec.kore"), args.timeout,
                                                     args.memory_max, extra, check)
        if fails_at is None:
            verdict = "pass (both fail on the initial term)" if status == "initfail" else \
                "FAIL: krun fails on the initial term, the spec: " + status
        elif fails_at is not False:
            if status == "stuck-error" and ok and s_steps == fails_at:
                verdict = "pass (both fail after %d steps)" % fails_at
            else:
                verdict = "FAIL: krun fails after %d steps, the spec: %s after %s steps%s" % (
                    fails_at, status, s_steps, "" if ok else ", configuration differs")
        elif check and status == "final":
            if not ok:
                verdict = "FAIL: " + (msg or "steps check")
            elif limit and s_steps == limit:
                verdict = "pass (each of the first %d steps is one K allows; the run goes on)" % limit
            else:
                verdict = "pass (each step is one K allows)"
        elif status == "final" and ok and s_steps == k_steps:
            verdict = "pass"
        elif status == "final" and ok:
            verdict = "FAIL: steps differ"
        elif status == "final":
            verdict = "FAIL: configuration differs"
        else:
            verdict = "FAIL: " + status + (": " + msg if msg else "")
    fmt = lambda x: "-" if x is None else str(x)
    return "| %s | %s | %s | %s | %.1f | %.1f |" % (name, fmt(k_steps), fmt(s_steps), verdict, k_secs, s_secs), verdict


if __name__ == "__main__":
    main()
