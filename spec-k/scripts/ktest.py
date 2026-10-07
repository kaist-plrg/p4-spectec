#!/usr/bin/env python3
"""Run the diff tests of the K spec against krun on the K tutorial languages,
KWasm, and the regression tests of K.

usage:
  spec-k/scripts/ktest.py                    # all suites
  spec-k/scripts/ktest.py imp lambda         # some suites
  spec-k/scripts/ktest.py kwasm --only conformance-i32.wast
  spec-k/scripts/ktest.py regression/list-set

Each definition is kompiled once into <work>/<suite>/ and kept there. Results
go to <work>/<suite>.md (the table of difftest.py). Two programs are tested
at a time (--jobs). The exit code is 1 if a test fails.
"""
import argparse
import concurrent.futures
import glob
import os
import re
import shlex
import subprocess
import sys

sys.path.insert(0, os.path.dirname(__file__))
from difftest import ROOT  # noqa: E402

TUTORIAL = "k-distribution/tests/regression-new/pl-tutorial"

# Settings follow the tutorial Makefiles. Nondeterministic SIMPLE programs are
# compared against all final states of krun's search; threads_05 and
# threads_12 are left out since the search does not end.
SIMPLE_SEARCH = ["threads_01", "threads_02", "threads_04", "threads_06", "threads_07", "threads_09",
                 "threads_10", "threads_11", "exceptions_07", "div-nondet"]
SUITES = {
    "imp": dict(src="k", dir=TUTORIAL + "/1_k/2_imp/lesson_4", def_="imp.k", ext="imp",
                kompile=["--gen-glr-bison-parser"], tests=["tests"]),
    "lambda": dict(src="k", dir=TUTORIAL + "/1_k/1_lambda/lesson_8", def_="lambda.k", ext="lambda",
                   kompile=["--gen-glr-bison-parser"], tests=["tests"]),
    "simple": dict(src="k", dir=TUTORIAL + "/2_languages/1_simple/1_untyped", def_="simple-untyped.md", ext="simple",
                   kompile=["--enable-search"], tests=["tests/diverse", "tests/exceptions", "tests/threads"],
                   krun=["--io", "off"], exclude=["threads_05.simple", "threads_12.simple"],
                   search=[t + ".simple" for t in SIMPLE_SEARCH]),
    "kool": dict(src="k", dir=TUTORIAL + "/2_languages/2_kool/1_untyped", def_="kool-untyped.md", ext="kool",
                 tests=["tests"], krun=["--io", "off"], exclude=["threads.kool"]),
    # memory_copy, memory_fill, memory_grow (7 to 13 million steps) and call
    # (517,177 steps) are left out for their length.
    "kwasm": dict(src="kwasm", dir="pykwasm/src/pykwasm/kdist/wasm-semantics", def_="test.md", ext="wast",
                  kompile=["--main-module", "WASM-TEST", "--syntax-module", "WASM-TEST-SYNTAX", "--md-selector", "k",
                           "--gen-glr-bison-parser", "-O3"],
                  exclude=["conformance-memory_copy.wast", "conformance-memory_fill.wast",
                           "conformance-memory_grow.wast", "conformance-call.wast"],
                  timeout=7200),
    # IMP stopped after 20 steps (krun --depth)
    "depth": dict(src="k", dir=TUTORIAL + "/1_k/2_imp/lesson_4", def_="imp.k", ext="imp", tests=["tests"],
                  kompiled="imp", krun=["--depth", "20"], spec=["-depth", "20"]),
}
# Definitions written for the K features and errors that the languages above
# do not exercise, in spec-k/test/<name>/<name>.k with programs in tests/
for name in ["priority", "equality", "mint", "binder", "errors"]:
    SUITES[name] = dict(src="spec", dir="spec-k/test/" + name, def_=name + ".k", ext=name, tests=["tests"])
# mint.k cannot have a module MINT, which K's domains.md has
SUITES["mint"]["kompile"] = ["--main-module", "MINT-TEST", "--syntax-module", "MINT-TEST"]

REGRESSION = "k-distribution/tests/regression-new"
# Regression tests that krun differently from a plain run of each program
REGRESSION_SKIP = {
    "proof-instrumentation": "krun --proof-hint", "proof-instrumentation-debug": "krun --proof-hint",
    "issue-1602": "krun --dry-run", "krun-deserialize": "a custom parser", "issue-582": "a custom parser",
    "star-multiplicity": "a custom parser", "issue-1169": "a preprocessed definition",
    "imp-outer-json": "a definition in JSON", "issue-2273": "kast tests", "issue-946": "a custom krun target",
    "search-bound": "krun --search --bound", "no-pattern": "krun --search-final", "imp++-llvm": "krun --search",
    "issue-3520-freshConfig": "krun --search --pattern", "io-llvm": "file IO", "rand": "random numbers",
    "exit-code-no-gen-top": "the exit code",
}
# Programs left out of a regression test, with the reason
REGRESSION_EXCLUDE = {
    # a fresh variable in a function equation is left unbound by kompile, and
    # the LLVM backend passes an undefined value for it (make_function in
    # lib/codegen/CreateTerm.cpp)
    "withConfig2": ["1.test"],
}


def run_difftest(name, definition, kompile_flags, programs, args, ext=None, krun=(), exclude=(), search=(),
                 timeout=3600, spec=(), kompiled_as=None, jobs=None):
    """Kompile the definition once into <work>/<name>/kompiled (or that of
    kompiled_as), then diff test the programs. Returns (passed, last line of
    the results)."""
    work = os.path.abspath(args.work)
    kompiled = os.path.join(work, kompiled_as or name, "kompiled")
    results = os.path.join(work, name + ".md")
    os.makedirs(os.path.dirname(results), exist_ok=True)
    stamp = os.path.join(kompiled, "timestamp")
    # kompile again when the definition changed since
    if not os.path.exists(stamp) or os.path.getmtime(definition) > os.path.getmtime(stamp):
        print("kompiling %s" % name, flush=True)
        p = subprocess.run(["kompile", "--backend", "llvm", definition, "--output-definition", kompiled]
                           + list(kompile_flags), capture_output=True, text=True)
        if p.returncode != 0:
            open(results, "w").write("kompile failed:\n" + p.stdout + p.stderr)
            return False, "kompile failed"
    if args.only:
        programs = [os.path.join(p, n) for p in programs for n in args.only if os.path.exists(os.path.join(p, n))]
        if not programs:
            return True, "no program among --only"
    cmd = [sys.executable, os.path.join(ROOT, "spec-k/scripts/difftest.py"), "-k", kompiled,
           "--timeout", str(args.timeout or timeout), "--search-timeout", "900",
           "--keep", os.path.join(work, name, "runs"), "-j", str(jobs or args.jobs)] + programs
    cmd += ["--ext", ext] if ext else []
    cmd += ["--krun-arg=" + a for a in krun]
    cmd += ["--spec-arg=" + a for a in spec]
    cmd += sum((["--exclude", e] for e in exclude), [])
    cmd += sum((["--search-for", e] for e in search), [])
    if args.cover:
        cmd += ["--cover-dir", os.path.join(work, "coverage", name)]
    print("diff test %s" % name, flush=True)
    with open(results, "w") as out:
        p = subprocess.run(cmd, stdout=out)
    lines = open(results).read().strip().splitlines()
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
    return [out]


def run_suite(suite, args):
    s = SUITES[suite]
    src = {"kwasm": args.kwasm_src, "k": args.k_src, "spec": ROOT}[s["src"]]
    src = os.path.abspath(src)
    if suite == "kwasm":
        programs = kwasm_programs(src, os.path.abspath(args.work))
    else:
        programs = [os.path.join(src, s["dir"], t) for t in s["tests"]]
    ok, summary = run_difftest(suite, os.path.join(src, s["dir"], s["def_"]), s.get("kompile", []), programs, args,
                               s["ext"], s.get("krun", []), s.get("exclude", []), s.get("search", []),
                               s.get("timeout", 3600), s.get("spec", []), s.get("kompiled"))
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
    through ktest.mak, with the settings of their Makefiles."""
    tests = {}
    root = os.path.join(src, REGRESSION)
    for d in sorted(os.listdir(root)):
        makefile = os.path.join(root, d, "Makefile")
        if d in REGRESSION_SKIP or not os.path.isfile(makefile) or "ktest.mak" not in open(makefile).read():
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
        tests[d] = dict(definition=definition, testdir=testdir, ext=v["EXT"], krun=["--no-exc-wrap"] + krun,
                        kompile=shlex.split(v.get("KOMPILE_FLAGS", "")) + ["--no-exc-wrap", "--type-inference-mode",
                                                                           "checked"])
    return tests


def run_regression(names, args):
    """The regression tests of K (all, or those named), with a summary in <work>/regression.md."""
    tests = regression_tests(os.path.abspath(args.k_src))
    for n in names:
        if n not in tests:
            sys.exit("unknown regression test %s" % n)
    # the tests are small, so --jobs of them run at a time, each one program at a time
    def one(d):
        t = tests[d]
        passed, summary = run_difftest("regression/" + d, t["definition"], t["kompile"], [t["testdir"]], args,
                                       t["ext"], t["krun"], REGRESSION_EXCLUDE.get(d, []), jobs=1)
        print("%s: %s" % (d, summary), flush=True)
        return passed, "| %s | %s |" % (d, summary)
    with concurrent.futures.ThreadPoolExecutor(max_workers=args.jobs) as pool:
        results = list(pool.map(one, names or list(tests)))
    ok = all(passed for passed, _ in results)
    rows = [row for _, row in results]
    if not names:
        with open(os.path.join(args.work, "regression.md"), "w") as out:
            out.write("| test | result |\n|---|---|\n" + "\n".join(rows) + "\n")
    return ok


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
    ap.add_argument("--only", action="append", default=[], help="run only this program (file name; repeatable)")
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
