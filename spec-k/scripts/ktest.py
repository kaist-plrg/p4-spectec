#!/usr/bin/env python3
"""Run the diff tests of the K spec against krun on the K tutorial languages,
KWasm, and the regression tests of K.

usage:
  spec-k/scripts/ktest.py                    # all suites
  spec-k/scripts/ktest.py imp lambda         # some suites
  spec-k/scripts/ktest.py kwasm --only conformance-i32.wast
  spec-k/scripts/ktest.py regression/list-set

Each definition is kompiled once into <work>/<suite>/ and kept there. Results
go to <work>/<suite>.md (the table of difftest.py), or only to the screen with
--only. Two programs are tested at a time (--jobs). The exit code is 1 if a
test fails.
"""
import argparse
import concurrent.futures
import glob
import os
import re
import shlex
import subprocess
import sys
import tomllib

sys.path.insert(0, os.path.dirname(__file__))
from difftest import ROOT  # noqa: E402

# The suites and the settings of the regression tests (suites.toml)
with open(os.path.join(os.path.dirname(__file__), "suites.toml"), "rb") as f:
    CONFIG = tomllib.load(f)
SUITES = CONFIG["suite"]
REGRESSION = CONFIG["regression"]


def run_difftest(name, t, args, jobs=None):
    """Kompile t["definition"] once into <work>/<name>/kompiled (or that of
    t["kompiled_as"]), then diff test t["programs"]. Returns (passed, last line
    of the results)."""
    work = os.path.abspath(args.work)
    kompiled = os.path.join(work, t.get("kompiled_as", name), "kompiled")
    stamp = os.path.join(kompiled, "timestamp")
    # kompile again when the definition changed since
    if not os.path.exists(stamp) or os.path.getmtime(t["definition"]) > os.path.getmtime(stamp):
        print("kompiling %s" % name, flush=True)
        p = subprocess.run(["kompile", "--backend", "llvm", t["definition"], "--output-definition", kompiled]
                           + t.get("kompile", []), capture_output=True, text=True)
        if p.returncode != 0:
            print(p.stdout + p.stderr)
            return False, "kompile failed"
    programs = t["programs"]
    if args.only:
        programs = [os.path.join(p, n) for p in programs for n in args.only if os.path.exists(os.path.join(p, n))]
        if not programs:
            return True, "no program among --only"
    cmd = [sys.executable, os.path.join(ROOT, "spec-k/scripts/difftest.py"), "-k", kompiled,
           "--timeout", str(args.timeout or t.get("timeout", 3600)),
           "--keep", os.path.join(work, name, "runs"), "-j", str(jobs or args.jobs), "--ext", t["ext"]] + programs
    cmd += ["--krun-arg=" + a for a in t.get("krun", [])]
    cmd += ["--depth=%d" % t["depth"]] if "depth" in t else []
    cmd += ["--krun-cache", os.path.join(work, "krun-cache")]
    cmd += ["--input-dir", t["inputs"]] if "inputs" in t else []
    cmd += sum((["--exclude", e] for e in t.get("exclude", [])), [])
    cmd += sum((["--check-steps-for", e] for e in t.get("check_steps", [])), [])
    if args.cover:
        cmd += ["--cover-dir", os.path.join(work, "coverage", name)]
    print("diff test %s" % name, flush=True)
    p = subprocess.run(cmd, stdout=subprocess.PIPE, text=True)
    if args.only:
        print(p.stdout, end="")
    else:
        with open(os.path.join(work, name + ".md"), "w") as out:
            out.write(p.stdout)
    lines = p.stdout.strip().splitlines()
    return p.returncode == 0, lines[-1] if lines else "no output"


def kwasm_programs(src, work):
    """The KWasm tests that kwasm's Makefile runs on the LLVM backend, after
    the preprocessing of kwasm run (pykwasm/src/pykwasm/scripts/preprocessor.py)."""
    sys.path.insert(0, os.path.join(src, "pykwasm/src/pykwasm/scripts"))
    from preprocessor import preprocess
    skip = set()
    for f in ("unparseable.txt", "unsupported-llvm.txt"):
        skip |= set(open(os.path.join(src, "tests/conformance", f)).read().split())
    tests = [("simple", f) for f in sorted(glob.glob(os.path.join(src, "tests/simple/*.wast")))]
    tests += [("conformance", f) for f in sorted(glob.glob(os.path.join(src, "tests/wasm-tests/test/core/*.wast")))
              if os.path.basename(f) not in skip]
    out = os.path.join(work, "kwasm", "programs")
    os.makedirs(out, exist_ok=True)
    for kind, f in tests:
        with open(os.path.join(out, kind + "-" + os.path.basename(f)), "w") as o:
            o.write(preprocess(open(f).read()))
    return out


def run_suite(suite, args):
    t = dict(SUITES[suite])
    src = os.path.abspath({"kwasm": args.kwasm_src, "k": args.k_src, "spec": ROOT}[t["src"]])
    t["definition"] = os.path.join(src, t["dir"], t["def"])
    if suite == "kwasm":
        t["programs"] = [kwasm_programs(src, os.path.abspath(args.work))]
    else:
        t["programs"] = [os.path.join(src, t["dir"], d) for d in t.get("tests", ["tests"])]
    if "inputs" in t:
        t["inputs"] = os.path.join(src, t["dir"], t["inputs"])
    ok, summary = run_difftest(suite, t, args)
    if not args.only:
        print(summary, flush=True)
    return ok


def read_makefile(path):
    """The variables a regression test's Makefile sets for ktest.mak."""
    v = {}
    for line in open(path):
        m = re.match(r"^([A-Z_]+)\s*(\?=|\+=|:=|=)\s*(.*?)\s*$", line.split("#")[0])
        if not m:
            continue
        name, op, value = m.groups()
        if op == "?=" and name in v:
            continue
        v[name] = (v.get(name, "") + " " + value).strip() if op == "+=" else value
    return v


def regression_tests(src):
    """Regression tests that kompile with the LLVM backend and krun programs
    through ktest.mak, as suites with the settings of their Makefiles."""
    tests = {}
    root = os.path.join(src, REGRESSION["dir"])
    for d in sorted(os.listdir(root)):
        makefile = os.path.join(root, d, "Makefile")
        if d in REGRESSION["skip"] or not os.path.isfile(makefile) or "ktest.mak" not in open(makefile).read():
            continue
        v = read_makefile(makefile)
        if v.get("KOMPILE_BACKEND", "llvm") != "llvm" or "DEF" not in v or "EXT" not in v:
            continue
        testdir = os.path.join(root, d, v.get("TESTDIR", "tests"))
        if not glob.glob(os.path.join(testdir, "*." + v["EXT"])):
            continue
        definition = os.path.join(root, d, v["DEF"] + ".k")
        if not os.path.exists(definition):
            definition = os.path.join(root, d, v["DEF"] + ".md")
        krun, flags = [], shlex.split(v.get("KRUN_FLAGS", ""))
        while flags:
            f = flags.pop(0)
            if f in ("-o", "--output"):  # difftest.py sets the output format
                flags.pop(0)
            elif f != "--profile":
                krun.append(f)
        tests[d] = dict(definition=definition, programs=[testdir], ext=v["EXT"],
                        krun=["--no-exc-wrap"] + REGRESSION["krun"].get(d, krun),
                        kompile=shlex.split(v.get("KOMPILE_FLAGS", "")) + ["--no-exc-wrap", "--type-inference-mode",
                                                                           "checked"],
                        exclude=REGRESSION["exclude"].get(d, []))
    return tests


def run_regression(names, args):
    """The regression tests of K (all, or those named), with a summary in <work>/regression.md."""
    tests = regression_tests(os.path.abspath(args.k_src))
    for n in names:
        if n not in tests:
            sys.exit("unknown regression test %s" % n)

    # the tests are small, so --jobs of them run at a time, each one program at a time
    def one(d):
        passed, summary = run_difftest("regression/" + d, tests[d], args, jobs=1)
        print("%s: %s" % (d, summary), flush=True)
        return passed, "| %s | %s |" % (d, summary)
    with concurrent.futures.ThreadPoolExecutor(max_workers=args.jobs) as pool:
        results = list(pool.map(one, names or list(tests)))
    if not names and not args.only:
        with open(os.path.join(args.work, "regression.md"), "w") as out:
            out.write("| test | result |\n|---|---|\n" + "\n".join(row for _, row in results) + "\n")
    return all(passed for passed, _ in results)


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("suites", nargs="*",
                    help="%s, regression, or regression/<test> (default: all)" % ", ".join(SUITES))
    ap.add_argument("--k-src", default=os.environ.get("K_SRC", os.path.join(ROOT, "../k")),
                    help="checkout of runtimeverification/k v7.1.337 (env K_SRC, default ../k)")
    ap.add_argument("--kwasm-src", default=os.environ.get("KWASM_SRC", os.path.join(ROOT, "../wasm-semantics")),
                    help="checkout of runtimeverification/wasm-semantics 212271b with its submodules "
                         "(env KWASM_SRC, default ../wasm-semantics)")
    ap.add_argument("--work", default=os.path.join(ROOT, "spec-k/_k-test"),
                    help="directory for kompiled definitions and results (default spec-k/_k-test)")
    ap.add_argument("--only", action="append", default=[],
                    help="run only this program (file name; repeatable); results go only to the screen")
    ap.add_argument("--timeout", type=float, default=None, help="seconds per run of the spec")
    ap.add_argument("--cover", action="store_true", help="record spec coverage in <work>/coverage/<suite>/")
    ap.add_argument("-j", "--jobs", type=int, default=2, help="programs to test at a time (default 2)")
    args = ap.parse_args()
    suites = args.suites or list(SUITES) + ["regression"]
    for s in suites:
        if s not in SUITES and s != "regression" and not s.startswith("regression/"):
            sys.exit("unknown suite %s" % s)
    os.makedirs(args.work, exist_ok=True)
    ok = True
    for s in suites:
        if s in SUITES:
            ok &= run_suite(s, args)
    named = [s.split("/", 1)[1] for s in suites if s.startswith("regression/")]
    if "regression" in suites or named:
        ok &= run_regression([] if "regression" in suites else named, args)
    sys.exit(0 if ok else 1)


if __name__ == "__main__":
    main()
