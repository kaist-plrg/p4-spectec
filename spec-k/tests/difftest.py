#!/usr/bin/env python3
"""Diff test: run programs with krun and with the K spec, and compare the
final configurations and step counts (see spec-k/README.md). When they differ
and the kompiled definition has a search binary (kompile --enable-search), the
run of the spec is checked step by step instead: each step must be one K
allows, as K leaves some choices open (rules of the same priority, the order
of map elements).

The spec runs on the Rust executor (bin/p4spectec krun), which leaves the
hooks of K's modules of domain values to the K runtime of the definition: a
library llvm-kompile builds from it (runtime_library), kept beside it.

usage:
  spec-k/tests/difftest.py -k imp-kompiled tests/*.imp
  spec-k/tests/difftest.py -k imp-kompiled tests/ --ext imp --timeout 600 -j 2

bin/p4spectec must be built (make release).
"""
import argparse
import collections
import concurrent.futures
import fnmatch
import glob
import hashlib
import json
import os
import re
import shutil
import subprocess
import sys
import tempfile
import time

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
SPEC = [os.path.join(ROOT, "spec-meta/0-aux/0.0-stdlib.watsup")] + sorted(
    glob.glob(os.path.join(ROOT, "spec-k/[0-9]*.watsup")))
BIN = os.path.join(ROOT, "bin/p4spectec")

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


def program_input(program, input_dir):
    """The standard input of a program: <program>.in, or <input_dir>/<name>.in,
    when it exists, as in the tutorial Makefiles (RESULTDIR)"""
    inp = os.path.join(input_dir, os.path.basename(program)) + ".in" if input_dir else program + ".in"
    return open(inp).read() if os.path.exists(inp) else ""


def run_krun(kompiled, program, workdir, krun_args, timeout, input_dir=None, depth=None, kore_input=False):
    """Run krun; return (steps, interpreter command, result file, seconds, error).

    krun --dry-run parses the program and prints the command it would run:
    the LLVM backend's interpreter, with the initial term after parsing and
    macro expansion as its first argument (krun keeps several tmp.in.* files,
    some of them before macro expansion). That command is then run directly,
    which is what krun does, without starting krun again. With kore_input the
    program is the initial term already, and the interpreter runs it."""
    stdin = program_input(program, input_dir)
    if kore_input:
        command = Interpreter([os.path.join(os.path.abspath(kompiled), "interpreter"), os.path.abspath(program),
                               "-1"], stdin)
        steps, result, secs, err = command.run(os.path.join(workdir, "result.kore"), depth, timeout)
        return steps, command, result, secs, err
    dry = os.path.join(workdir, "dry")
    os.makedirs(dry, exist_ok=True)
    rc, out, err, _ = run(["krun", "-d", os.path.abspath(kompiled), os.path.abspath(program)] + krun_args
                          + ["--save-temps", "--temp-dir", dry, "--dry-run"], timeout, stdin)
    line = next((l for l in (out + err).splitlines() if re.search(r"(^|/)interpreter \S+ -?\d+ ", l)), None)
    if rc != 0 or line is None:
        return None, None, None, 0.0, "krun --dry-run failed (exit %s): %s" % (rc, oneline(out + err))
    words = line.split()
    command = Interpreter(words[next(i for i, w in enumerate(words) if w.endswith("interpreter")):], stdin)
    steps, result, secs, err = command.run(os.path.join(workdir, "result.kore"), depth, timeout)
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


def runtime_library(kompiled, flags):
    """The K runtime of a kompiled definition, <kompiled>/runtime.so, built by
    llvm-kompile <definition> <dt> c with the extra clang flags (e.g. the
    plugin of KEVM), again when the definition is newer. Symbols with '@' in
    their names (string literals of some rule labels) make the linker fail on
    a shared library ("undefined version"); they are renamed in the object,
    which alone defines and uses them, and the library is linked again."""
    kompiled = os.path.abspath(kompiled)
    out = os.path.join(kompiled, "runtime.so")
    definition = os.path.join(kompiled, "definition.kore")
    if os.path.exists(out) and os.path.getmtime(out) >= os.path.getmtime(definition):
        return out
    with tempfile.TemporaryDirectory() as tmp:
        base = ["llvm-kompile", definition, os.path.join(kompiled, "dt"), "c"]
        p = subprocess.run(base + ["--"] + flags + ["-o", out + ".tmp"], cwd=tmp, capture_output=True, text=True)
        if p.returncode != 0:
            if "undefined version" not in p.stderr:
                raise RuntimeError("llvm-kompile failed: " + p.stderr[-2000:])
            p = subprocess.run(base + ["--save-temps", "-v", "--"] + flags + ["-o", out + ".tmp"], cwd=tmp,
                               capture_output=True, text=True)
            link = next(l for l in reversed((p.stdout + p.stderr).splitlines())
                        if re.search(r"clang\+\+-\d+ .*-shared", l))
            link = re.sub(r"^\s*\|?\s*\+\s*", "", link)
            obj = re.search(r"tmp\.\w+\.o", link).group(0)
            nm = subprocess.run(["llvm-nm-17", obj], cwd=tmp, capture_output=True, text=True).stdout
            names = sorted({l.split()[-1] for l in nm.splitlines() if "@" in l.split()[-1]})
            with open(os.path.join(tmp, "rename.txt"), "w") as fh:
                fh.writelines("%s %s\n" % (n, n.replace("@", "_AT_")) for n in names)
            for cmd in (["llvm-objcopy-17", "--redefine-syms=rename.txt", obj], ["sh", "-c", link]):
                q = subprocess.run(cmd, cwd=tmp, capture_output=True, text=True)
                if q.returncode != 0:
                    raise RuntimeError("relinking the K runtime failed: " + q.stderr[-2000:])
    os.replace(out + ".tmp", out)
    return out


def capped(cmd, memory_max):
    """Run under a memory cap, so that only this process is killed when it is exceeded."""
    if not memory_max:
        return cmd
    return ["systemd-run", "--user", "--scope", "--quiet", "-p", "MemoryMax=" + memory_max,
            "-p", "MemorySwapMax=0"] + cmd


def run_spec(definition, library, init, out, compare, extra, timeout, memory_max):
    """Run the K spec, comparing as compare says (--expect FILE or --check-steps DIR)."""
    cmd = [BIN, "krun"] + SPEC + ["--def", definition, "--lib", library, "--init", init, "--output", out] \
        + compare + extra
    rc, stdout, stderr, secs = run(capped(cmd, memory_max), timeout)
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


def krun_outcome(prog, work, args, depth):
    """What krun does with a program: (steps, initial term, result, seconds,
    error, fails_at), where steps is a count or says where krun fails, and
    fails_at is as in judge. A run of krun depends only on the K version, the
    kompiled definition, the program, its input, the krun options, and the
    step limit, so with --krun-cache the outcome is kept under their hash and
    krun is not run again."""
    krun_args = args.krun_arg + (["--depth", str(args.depth)] if args.depth else [])
    entry = None
    if args.krun_cache:
        h = hashlib.sha256()
        for part in [args.k_version, args.definition_hash, open(prog, "rb").read().hex(),
                     program_input(prog, args.input_dir), json.dumps(krun_args), str(depth)]:
            h.update(part.encode() + b"\0")
        entry = os.path.join(args.krun_cache, h.hexdigest())
        meta = os.path.join(entry, "krun.json")
        if os.path.exists(meta):
            m = json.load(open(meta))
            result = os.path.join(entry, "result.kore") if m["result"] else None
            return (m["steps"], os.path.join(entry, "init.kore"), result, m["secs"], m["error"],
                    m["fails_at"])
    steps, command, result, secs, err = run_krun(args.kompiled, prog, work, krun_args, args.timeout,
                                                 args.input_dir, depth, args.kore_input)
    fails_at = False
    if err and err.startswith("krun failed") and command:
        fails_at, result = failing_step(command, work, args.timeout)
        steps = "fails at init" if fails_at is None else "fails after %d" % fails_at
        err = None
    init = command.init if command else None
    if entry and err != "krun timeout":
        os.makedirs(entry, exist_ok=True)
        if init:
            shutil.copyfile(init, os.path.join(entry, "init.kore"))
        if result:
            shutil.copyfile(result, os.path.join(entry, "result.kore"))
        with open(os.path.join(entry, "krun.json.tmp"), "w") as fh:
            json.dump(dict(steps=steps, secs=secs, error=err, fails_at=fails_at, result=bool(result)), fh)
        os.replace(os.path.join(entry, "krun.json.tmp"), os.path.join(entry, "krun.json"))
    return steps, init, result, secs, err, fails_at


def test_program(prog, root, definition, library, args):
    """Diff test one program; returns (table row, verdict)."""
    name = os.path.basename(prog)
    work = os.path.join(root, name)
    os.makedirs(work, exist_ok=True)
    limits = dict((c.split(":") + [None])[:2] for c in args.check_steps_for)
    check = name in limits
    limit = int(limits[name]) if check and limits[name] else args.depth
    # a run checked step by step for its first steps need not end under krun
    k_steps, init, result, k_secs, err, fails_at = krun_outcome(prog, work, args, limit if check else None)
    if err:
        return "| %s | - | - | skip: %s | %.1f | 0.0 |" % (name, err, k_secs), "skip: " + err
    if check:
        k_steps = "each step"
    compare = ["--check-steps", os.path.abspath(args.kompiled)] if check else ["--expect", result or os.devnull]
    extra = ["--depth", str(limit)] if limit else []
    if args.no_det:
        extra += ["--no-det"]
    spec = run_spec(definition, library, init, os.path.join(work, "spec.kore"), compare, extra, args.timeout,
                    args.memory_max)
    verdict = judge(spec, k_steps, fails_at, check, limit)
    # a run that ends elsewhere or in other steps than krun's may still take
    # only steps K allows
    if verdict in ("FAIL: configuration differs", "FAIL: steps differ") \
            and os.path.exists(os.path.join(args.kompiled, "search")):
        differs = verdict[len("FAIL: "):]
        compare = ["--check-steps", os.path.abspath(args.kompiled)]
        spec = run_spec(definition, library, init, os.path.join(work, "spec.kore"), compare, extra, args.timeout,
                        args.memory_max)
        verdict = judge(spec, k_steps, fails_at, True, limit)
        verdict = verdict.replace("pass (each step is one K allows)",
                                  "pass by steps (%s; each step is one K allows)" % differs)
    # a known failure: where the spec follows K's documentation and krun does
    # not; such a program must still fail
    if any(fnmatch.fnmatchcase(name, k) for k in args.known):
        if verdict.startswith("FAIL: "):
            verdict = "known failure: " + verdict[len("FAIL: "):]
        elif verdict.startswith("pass"):
            verdict = "FAIL: passes, but is a known failure"
    steps = "-" if spec.steps is None else spec.steps
    return "| %s | %s | %s | %s | %.1f | %.1f |" % (name, k_steps, steps, verdict, k_secs, spec.secs), verdict


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("-k", "--kompiled", required=True, help="the <name>-kompiled directory")
    ap.add_argument("programs", nargs="+", help="program files or directories")
    ap.add_argument("--ext", default=None, help="extension of the programs in directories (e.g. imp)")
    ap.add_argument("--timeout", type=float, default=1800, help="seconds per run of the spec")
    ap.add_argument("--krun-arg", action="append", default=[], help="extra argument for krun")
    ap.add_argument("--depth", type=int, default=None, help="stop krun and the spec after this many steps")
    ap.add_argument("--keep", default=None, help="keep work files in this directory")
    ap.add_argument("--input-dir", default=None,
                    help="read <name>.in from this directory instead of beside the program")
    ap.add_argument("--exclude", action="append", default=[],
                    help="skip programs whose file name matches this (a glob pattern)")
    ap.add_argument("--known", action="append", default=[],
                    help="a known failure (a glob pattern): reported as such when it fails, and as a "
                         "failure when it passes")
    ap.add_argument("--memory-max", default="2500M" if shutil.which("systemd-run") else "",
                    help="memory cap for each run of the spec (systemd-run MemoryMax; empty for none, "
                         "the default without systemd)")
    ap.add_argument("--no-det", action="store_true",
                    help="run the spec without the check of determinism (p4spectec krun --no-det); by default "
                         "a run fails when two rules or clauses of the spec both apply")
    ap.add_argument("--runtime-flag", action="append", default=[],
                    help="extra clang flag for the K runtime library (llvm-kompile ... c), e.g. a plugin")
    ap.add_argument("--kore-input", action="store_true",
                    help="the programs are initial terms in KORE, run by the interpreter of the kompiled "
                         "definition without krun")
    ap.add_argument("--krun-cache", default=None,
                    help="keep what krun does with each program in this directory, and reuse it")
    ap.add_argument("--k-version", default=None,
                    help="the output of krun --version, for the keys of the krun cache (default: run it)")
    ap.add_argument("-j", "--jobs", type=int, default=1, help="programs to test at a time")
    ap.add_argument("--check-steps-for", action="append", default=[], metavar="NAME[:N]",
                    help="check each step of this program against the search binary instead of comparing "
                         "with krun's run (nondeterministic programs); :N checks only the first N steps")
    args = ap.parse_args()

    # a directory gives its files with the extension; <program>.in and
    # <program>.out beside them are the input and the expected output
    programs = []
    for p in args.programs:
        if os.path.isdir(p):
            if not args.ext:
                sys.exit("--ext is needed with a directory of programs (%s)" % p)
            programs += sorted(f for f in glob.glob(os.path.join(p, "*." + args.ext)) if os.path.isfile(f))
        else:
            programs.append(p)
    programs = [p for p in programs
                if not any(fnmatch.fnmatchcase(os.path.basename(p), e) for e in args.exclude)]
    definition = os.path.join(args.kompiled, "definition.kore")
    if not os.path.exists(definition):
        sys.exit("no definition.kore in %s" % args.kompiled)

    if args.krun_cache:
        args.krun_cache = os.path.abspath(args.krun_cache)
        args.k_version = args.k_version or run(["krun", "--version"], 60)[1]
        args.definition_hash = hashlib.sha256(open(definition, "rb").read()).hexdigest()
    root = args.keep or tempfile.mkdtemp(prefix="difftest-")
    library = runtime_library(args.kompiled, args.runtime_flag)
    Interpreter.preload = fast_throw()
    print("| program | krun steps | spec steps | result | krun s | spec s |")
    print("|---|---|---|---|---|---|")
    # programs run --jobs at a time; rows are printed in program order
    with concurrent.futures.ThreadPoolExecutor(max_workers=args.jobs) as pool:
        futures = [pool.submit(test_program, prog, root, definition, library, args) for prog in programs]
        verdicts = []
        for future in futures:
            row, verdict = future.result()
            print(row, flush=True)
            verdicts.append(verdict)
    passed = sum(1 for v in verdicts if v.startswith("pass"))
    skipped = sum(1 for v in verdicts if v.startswith("skip"))
    known = sum(1 for v in verdicts if v.startswith("known failure"))
    failures = len(verdicts) - passed - skipped - known
    print("\n%d passed, %d failed, %s%d skipped (work files: %s)"
          % (passed, failures, "%d known failures, " % known if known else "", skipped, root))
    sys.exit(1 if failures else 0)


if __name__ == "__main__":
    main()
