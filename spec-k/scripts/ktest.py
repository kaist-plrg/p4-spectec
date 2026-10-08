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
import shutil
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
LLVM = CONFIG["llvm"]


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
        p = (llvm_kompile(t["definition"], kompiled) if t.get("kore") else
             subprocess.run(["kompile", "--backend", "llvm", t["definition"], "--output-definition", kompiled]
                            + [a.replace("{src}", t["src_dir"]) for a in t.get("kompile", [])],
                            capture_output=True, text=True))
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
    cmd += ["--kore-input"] if t.get("kore") or t.get("kore_input") else []
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


def llvm_kompile(definition, kompiled):
    """A kompiled directory for a definition already in KORE: the definition,
    and its interpreter built by the LLVM backend, as kompile does
    (llvm-kompile-matching for the decision trees)."""
    dt = os.path.join(kompiled, "dt")
    os.makedirs(dt, exist_ok=True)
    shutil.copyfile(definition, os.path.join(kompiled, "definition.kore"))
    for cmd in (["llvm-kompile-matching", "definition.kore", "qbaL", "dt", "0"],
                ["llvm-kompile", "definition.kore", "dt", "main", "-o", "interpreter"]):
        p = subprocess.run(cmd, cwd=kompiled, capture_output=True, text=True)
        if p.returncode != 0:
            return p
    open(os.path.join(kompiled, "timestamp"), "w").close()
    return p


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


def kevm_programs(src, work, t):
    """The initial terms of the KEVM tests in t["gst"] (GeneralStateTests,
    leaving out the directories in t["gst_exclude"]), as kevm-pyk run makes
    them (kevm_inputs.py, in the Python environment of kevm-pyk)."""
    out = os.path.join(work, "kevm", "programs")
    files = sorted(f for f in glob.glob(os.path.join(src, t["gst"], "**", "*.json"), recursive=True)
                   if not any("/%s/" % d in f for d in t.get("gst_exclude", [])))
    p = subprocess.run(["uv", "run", "--directory", os.path.join(src, "kevm-pyk"), "python",
                        os.path.join(ROOT, "spec-k/scripts/kevm_inputs.py"), out, t["mode"], t["schedule"]] + files,
                       capture_output=True, text=True)
    if p.returncode != 0:
        sys.exit("kevm_inputs.py failed: " + p.stderr[-2000:])
    return out


def run_suite(suite, args):
    t = dict(SUITES[suite])
    src = os.path.abspath({"kwasm": args.kwasm_src, "kevm": args.kevm_src, "k": args.k_src, "spec": ROOT}[t["src"]])
    t["src_dir"] = src
    t["definition"] = os.path.join(src, t["dir"], t["def"])
    if suite == "kwasm":
        t["programs"] = [kwasm_programs(src, os.path.abspath(args.work))]
    elif suite == "kevm":
        t["programs"] = [kevm_programs(src, os.path.abspath(args.work), t)]
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
        # difftest.py sets the output; a search is checked step by step, and
        # --depth limits both krun and the spec
        krun, search, depth, flags = [], False, None, shlex.split(v.get("KRUN_FLAGS", ""))
        while flags:
            f = flags.pop(0)
            if f in ("-o", "--output", "--pattern", "--bound"):
                flags.pop(0)
            elif f in ("--search", "--search-final", "--search-all"):
                search = True
            elif f == "--depth":
                depth = int(flags.pop(0))
            elif f not in ("--profile", "--no-pattern"):
                krun.append(f)
        # cells with a stream attribute buffer IO in the configuration (decisions D10 in k-in-p4)
        if 'stream="' in open(definition).read():
            krun += ["--io", "off"]
        tests[d] = dict(definition=definition, programs=[testdir], ext=v["EXT"],
                        krun=["--no-exc-wrap"] + REGRESSION["krun"].get(d, krun),
                        kompile=shlex.split(v.get("KOMPILE_FLAGS", "")) + ["--no-exc-wrap", "--type-inference-mode",
                                                                           "checked"],
                        exclude=REGRESSION["exclude"].get(d, []))
        if search:
            tests[d]["check_steps"] = [os.path.basename(f) + ("" if depth is None else ":%d" % depth)
                                       for f in glob.glob(os.path.join(testdir, "*." + v["EXT"]))]
        elif depth is not None:
            tests[d]["depth"] = depth
    return tests


def llvm_tests(src):
    """The tests of the LLVM backend (test/defn/<name>.kore) that run its
    interpreter on initial terms (test/input/<name>.in or test/input/<name>/*.in)."""
    tests = {}
    for definition in sorted(glob.glob(os.path.join(src, "test/defn/*.kore"))):
        name = os.path.basename(definition)[:-len(".kore")]
        runs = [l for l in open(definition) if l.startswith("// RUN:")]
        if name in LLVM["skip"] or not any("%interpreter" in l or "%gcs-interpreter" in l for l in runs):
            continue
        inputs = [p for p in [os.path.join(src, "test/input", name + ".in"), os.path.join(src, "test/input", name)]
                  if os.path.exists(p)]
        if inputs:
            tests[name] = dict(definition=definition, programs=inputs, ext="in", kore=True)
    return tests


def run_group(group, tests, names, args):
    """The tests of a group (regression or llvm; all, or those named), with a
    summary in <work>/<group>.md."""
    for n in names:
        if n not in tests:
            sys.exit("unknown %s test %s" % (group, n))

    # the tests are small, so --jobs of them run at a time, each one program at a time
    def one(d):
        passed, summary = run_difftest(group + "/" + d, tests[d], args, jobs=1)
        print("%s: %s" % (d, summary), flush=True)
        return passed, "| %s | %s |" % (d, summary)
    with concurrent.futures.ThreadPoolExecutor(max_workers=args.jobs) as pool:
        results = list(pool.map(one, names or list(tests)))
    if not names and not args.only:
        with open(os.path.join(args.work, group + ".md"), "w") as out:
            out.write("| test | result |\n|---|---|\n" + "\n".join(row for _, row in results) + "\n")
    return all(passed for passed, _ in results)


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("suites", nargs="*",
                    help="%s, regression, regression/<test>, llvm, or llvm/<test> (default: all)"
                    % ", ".join(SUITES))
    ap.add_argument("--k-src", default=os.environ.get("K_SRC", os.path.join(ROOT, "../k")),
                    help="checkout of runtimeverification/k v7.1.337 (env K_SRC, default ../k)")
    ap.add_argument("--kwasm-src", default=os.environ.get("KWASM_SRC", os.path.join(ROOT, "../wasm-semantics")),
                    help="checkout of runtimeverification/wasm-semantics 212271b with its submodules "
                         "(env KWASM_SRC, default ../wasm-semantics)")
    ap.add_argument("--kevm-src", default=os.environ.get("KEVM_SRC", os.path.join(ROOT, "../evm-semantics")),
                    help="checkout of runtimeverification/evm-semantics 866e563 with the blockchain plugin built "
                         "(env KEVM_SRC, default ../evm-semantics)")
    ap.add_argument("--llvm-src", default=os.environ.get("LLVM_SRC", os.path.join(ROOT, "../llvm-backend")),
                    help="checkout of runtimeverification/llvm-backend f02284f (env LLVM_SRC, default ../llvm-backend)")
    ap.add_argument("--work", default=os.path.join(ROOT, "spec-k/_k-test"),
                    help="directory for kompiled definitions and results (default spec-k/_k-test)")
    ap.add_argument("--only", action="append", default=[],
                    help="run only this program (file name; repeatable); results go only to the screen")
    ap.add_argument("--timeout", type=float, default=None, help="seconds per run of the spec")
    ap.add_argument("--cover", action="store_true", help="record spec coverage in <work>/coverage/<suite>/")
    ap.add_argument("-j", "--jobs", type=int, default=2, help="programs to test at a time (default 2)")
    args = ap.parse_args()
    groups = {"regression": lambda: regression_tests(os.path.abspath(args.k_src)),
              "llvm": lambda: llvm_tests(os.path.abspath(args.llvm_src))}
    suites = args.suites or list(SUITES) + list(groups)
    for s in suites:
        if s not in SUITES and s.split("/", 1)[0] not in groups:
            sys.exit("unknown suite %s" % s)
    os.makedirs(args.work, exist_ok=True)
    ok = True
    for s in suites:
        if s in SUITES:
            ok &= run_suite(s, args)
    for group, tests in groups.items():
        named = [s.split("/", 1)[1] for s in suites if s.startswith(group + "/")]
        if group in suites or named:
            ok &= run_group(group, tests(), [] if group in suites else named, args)
    sys.exit(0 if ok else 1)


if __name__ == "__main__":
    main()
