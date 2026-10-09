#!/usr/bin/env python3
"""Run the diff tests of the K spec against krun on the suites of
suites.toml, the regression tests of K, and the tests of the LLVM backend.

usage:
  spec-k/scripts/ktest.py                    # the quick run: the quick lists of suites.toml
  spec-k/scripts/ktest.py --full             # all programs of all suites
  spec-k/scripts/ktest.py imp lambda         # all programs of some suites
  spec-k/scripts/ktest.py kwasm --only conformance-i32.wast
  spec-k/scripts/ktest.py regression/list-set

Programs marked skip in suites.toml never run. Each run of the spec may take
two hours (--timeout).

Each definition is kompiled once into <work>/<suite>/ and kept there, with the
search binary that step checks use. Each result row is shown as its program is
done, and the table of difftest.py goes to <work>/<suite>.md when all programs
of the suite run.
Two programs are tested at a time (--jobs). The exit code is 1 if a test fails.
"""
import argparse
import concurrent.futures
import fnmatch
import functools
import glob
import hashlib
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
    """Kompile t["definition"] once into <work>/<name>/kompiled, then diff test
    t["programs"]. Returns (passed, last line of the results)."""
    work = os.path.abspath(args.work)
    kompiled = os.path.join(work, name, "kompiled")
    stamp = os.path.join(kompiled, "timestamp")
    # kompile again when the definition changed since, or when the search
    # binary is missing (not for a kompiled directory linked from elsewhere)
    no_search = not os.path.islink(kompiled) and not os.path.exists(os.path.join(kompiled, "search"))
    if not os.path.exists(stamp) or os.path.getmtime(t["definition"]) > os.path.getmtime(stamp) or no_search:
        print("kompiling %s" % name, flush=True)
        flags = [a.replace("{src}", t.get("src_dir", "")) for a in t.get("kompile", [])]
        p = (llvm_kompile(t["definition"], kompiled) if t.get("kore") else
             subprocess.run(["kompile", "--backend", "llvm", t["definition"], "--output-definition", kompiled]
                            + flags + ([] if "--enable-search" in flags else ["--enable-search"]),
                            capture_output=True, text=True))
        if p.returncode != 0:
            print(p.stdout + p.stderr)
            return False, "kompile failed"
    programs = t["make_programs"](kompiled) if "make_programs" in t else t["programs"]
    only = args.only or t.get("only", [])
    if only:
        programs = [q for p in programs
                    for q in ([os.path.join(p, n) for n in only] if os.path.isdir(p) else [p])
                    if os.path.exists(q) and os.path.basename(q) in only]
        if not programs:
            return True, "no program among those named"
    cmd = [sys.executable, os.path.join(ROOT, "spec-k/scripts/difftest.py"), "-k", kompiled,
           "--timeout", str(args.timeout or 7200),
           "--keep", os.path.join(work, name, "runs"), "-j", str(jobs or args.jobs), "--ext", t["ext"]] + programs
    cmd += ["--krun-arg=" + a for a in t.get("krun", [])]
    cmd += ["--depth=%d" % t["depth"]] if "depth" in t else []
    cmd += ["--kore-input"] if t.get("kore") or t.get("kore_input") else []
    cmd += ["--krun-cache", os.path.join(work, "krun-cache"), "--k-version", k_version()]
    cmd += ["--input-dir", t["inputs"]] if "inputs" in t else []
    cmd += sum((["--exclude", e] for e in t.get("exclude", [])), [])
    cmd += sum((["--check-steps-for", e] for e in t.get("check_steps", [])), [])
    if args.cover:
        cmd += ["--cover-dir", os.path.join(work, "coverage", name)]
    if args.det:
        cmd += ["--det"]
    print("diff test %s" % name, flush=True)
    env = dict(os.environ, SPECTEC_CACHE_SIZE=str(t["cache_size"])) if "cache_size" in t else None
    # each row is shown as its program is done, with the suite's name
    p = subprocess.Popen(cmd, stdout=subprocess.PIPE, text=True, env=env)
    output = []
    for line in p.stdout:
        output.append(line)
        if line.startswith("| ") and not line.startswith("| program"):
            print("%s %s" % (name, line.rstrip()), flush=True)
    p.wait()
    # a run in deterministic mode checks the spec, not the result: the table stays
    if not only and not args.det:
        with open(os.path.join(work, name + ".md"), "w") as out:
            out.write("".join(output))
    lines = "".join(output).strip().splitlines()
    return p.returncode == 0, lines[-1] if lines else "no output"


@functools.cache
def k_version():
    """krun --version, run once for all the difftest.py runs (0.2 s each)"""
    return subprocess.run(["krun", "--version"], capture_output=True, text=True).stdout


def llvm_kompile(definition, kompiled):
    """A kompiled directory for a definition already in KORE: the definition,
    and its interpreter and search binary built by the LLVM backend, as
    kompile --enable-search does (llvm-kompile-matching for the decision trees)."""
    dt = os.path.join(kompiled, "dt")
    os.makedirs(dt, exist_ok=True)
    shutil.copyfile(definition, os.path.join(kompiled, "definition.kore"))
    for cmd in (["llvm-kompile-matching", "definition.kore", "qbaL", "dt", "0"],
                ["llvm-kompile", "definition.kore", "dt", "main", "-o", "interpreter"],
                ["llvm-kompile", "definition.kore", "dt", "search", "-o", "search"]):
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


def kevm_programs(src, work):
    """The initial terms of the VMTests of ethereum-tests, as kevm-pyk run makes
    them (kevm_inputs.py, in the Python environment of kevm-pyk; VMTESTS mode,
    default schedule): <file>-<test>.kore for each test of each file."""
    out = os.path.join(work, "kevm", "programs")
    gst = "tests/ethereum-tests/BlockchainTests/GeneralStateTests/VMTests"
    files = sorted(glob.glob(os.path.join(src, gst, "**", "*.json"), recursive=True))
    script = os.path.join(ROOT, "spec-k/scripts/kevm_inputs.py")
    # made once (about 10 s): the stamp names the script, the mode, and the
    # files with their sizes and times, so a change makes them again
    h = hashlib.sha256(open(script, "rb").read() + b"VMTESTS DEFAULT")
    for f in files:
        st = os.stat(f)
        h.update(("%s %d %d\0" % (os.path.relpath(f, src), st.st_size, st.st_mtime_ns)).encode())
    stamp = os.path.join(out, ".stamp")
    if os.path.exists(stamp) and open(stamp).read() == h.hexdigest():
        return out
    p = subprocess.run(["uv", "run", "--directory", os.path.join(src, "kevm-pyk"), "python", script, out,
                        "VMTESTS", "DEFAULT"] + files, capture_output=True, text=True)
    if p.returncode != 0:
        sys.exit("kevm_inputs.py failed: " + p.stderr[-2000:])
    with open(stamp, "w") as fh:
        fh.write(h.hexdigest())
    return out


def kmir_programs(src, work, kompiled):
    """The initial terms of KMIR tests, as kmir run makes them for the LLVM
    backend (kmir_inputs.py, in the Python environment of kmir): those of
    exec-smir from their SMIR JSON (<dir>-<name>.kore), and those of run-rs
    and ub from their Rust source through stable-mir-json, built in the
    repository (make stable-mir-json; rs-<dir>-<name>.kore). They name symbols
    of the kompiled definition, so they are made after it."""
    out = os.path.join(work, "kmir", "programs")
    data = os.path.join(src, "kmir/src/tests/integration/data")
    files = sorted(glob.glob(os.path.join(data, "exec-smir/*/*.smir.json"))
                   + glob.glob(os.path.join(data, "run-rs/*/*.rs")) + glob.glob(os.path.join(data, "ub/*.rs")))
    script = os.path.join(ROOT, "spec-k/scripts/kmir_inputs.py")
    # made once: the stamp names the script, the kompiled definition, and the
    # files with their sizes and times, so a change makes them again
    h = hashlib.sha256(open(script, "rb").read())
    h.update(b"%d\0" % os.stat(os.path.join(kompiled, "timestamp")).st_mtime_ns)
    for f in files:
        st = os.stat(f)
        h.update(("%s %d %d\0" % (os.path.relpath(f, src), st.st_size, st.st_mtime_ns)).encode())
    stamp = os.path.join(out, ".stamp")
    if os.path.exists(stamp) and open(stamp).read() == h.hexdigest():
        return [out]
    p = subprocess.run(["uv", "run", "--project", os.path.join(src, "kmir"), "python", script, kompiled, out]
                       + files, capture_output=True, text=True)
    if p.returncode != 0:
        sys.exit("kmir_inputs.py failed: " + p.stderr[-2000:])
    with open(stamp, "w") as fh:
        fh.write(h.hexdigest())
    return [out]


def run_suite(suite, args):
    t = dict(SUITES[suite])
    if args.quick:
        if "quick" not in t:
            return True
        t["only"] = t["quick"]
    src = os.path.abspath({"kwasm": args.kwasm_src, "kevm": args.kevm_src, "kmir": args.kmir_src, "k": args.k_src,
                       "spec": ROOT}[t["src"]])
    t["src_dir"] = src
    t["definition"] = os.path.join(src, t["dir"], t["def"])
    if suite == "kwasm":
        t["programs"] = [kwasm_programs(src, os.path.abspath(args.work))]
    elif suite == "kevm":
        t["programs"] = [kevm_programs(src, os.path.abspath(args.work))]
        t["kore_input"] = True
    elif suite == "kmir":
        t["make_programs"] = functools.partial(kmir_programs, src, os.path.abspath(args.work))
        t["kore_input"] = True
    else:
        t["programs"] = [os.path.join(src, t["dir"], d) for d in t.get("tests", ["tests"])]
    if "inputs" in t:
        t["inputs"] = os.path.join(src, t["dir"], t["inputs"])
    t["exclude"] = t.get("skip", [])
    ok, summary = run_difftest(suite, t, args)
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


def group_left_out(group, d, quick):
    """Whether test d of a group (regression or llvm) is not run, the patterns
    of its programs not run (<test> or <test>/<program> in the group's skip
    list), and in a quick run the programs to run (<test>/<program> in its
    quick list)."""
    skip = group.get("skip", [])
    only = [k.split("/", 1)[1] for k in group.get("quick", []) if k.startswith(d + "/")] if quick else []
    return d in skip or (quick and not only), [k.split("/", 1)[1] for k in skip if k.startswith(d + "/")], only


def regression_tests(src, quick):
    """Regression tests that kompile with the LLVM backend and krun programs
    through ktest.mak, as suites with the settings of their Makefiles."""
    tests = {}
    root = os.path.join(src, REGRESSION["dir"])
    for d in sorted(os.listdir(root)):
        makefile = os.path.join(root, d, "Makefile")
        out, exclude, only = group_left_out(REGRESSION, d, quick)
        if out or not os.path.isfile(makefile) or "ktest.mak" not in open(makefile).read():
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
                        exclude=exclude, only=only)
        if search:
            tests[d]["check_steps"] = [os.path.basename(f) + ("" if depth is None else ":%d" % depth)
                                       for f in glob.glob(os.path.join(testdir, "*." + v["EXT"]))]
        elif depth is not None:
            tests[d]["depth"] = depth
    return tests


def llvm_tests(src, quick):
    """The tests of the LLVM backend (test/defn/<name>.kore) that run its
    interpreter on initial terms (test/input/<name>.in or test/input/<name>/*.in).
    Tests that only build the interpreter (never run it) are left out: their
    inputs need not fit the definition."""
    tests = {}
    for definition in sorted(glob.glob(os.path.join(src, "test/defn/*.kore"))):
        name = os.path.basename(definition)[:-len(".kore")]
        runs = [l for l in open(definition) if l.startswith("// RUN:")]
        out, exclude, only = group_left_out(LLVM, name, quick)
        if out or not any("%interpreter" in l or "%gcs-interpreter" in l for l in runs) \
                or not any(m in l for l in runs for m in ("%check", "%run", "%t.interpreter")):
            continue
        inputs = [p for p in [os.path.join(src, "test/input", name + ".in"), os.path.join(src, "test/input", name)]
                  if os.path.exists(p)]
        if inputs:
            tests[name] = dict(definition=definition, programs=inputs, ext="in", kore=True, exclude=exclude,
                               only=only)
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
    if not names and not args.only and not args.quick:
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
    ap.add_argument("--kmir-src", default=os.environ.get("KMIR_SRC", os.path.join(ROOT, "../mir-semantics")),
                    help="checkout of runtimeverification/mir-semantics 4d79325 with its Python environment "
                         "(uv sync --project kmir; env KMIR_SRC, default ../mir-semantics)")
    ap.add_argument("--llvm-src", default=os.environ.get("LLVM_SRC", os.path.join(ROOT, "../llvm-backend")),
                    help="checkout of runtimeverification/llvm-backend f02284f (env LLVM_SRC, default ../llvm-backend)")
    ap.add_argument("--work", default=os.path.join(ROOT, "spec-k/_k-test"),
                    help="directory for kompiled definitions and results (default spec-k/_k-test)")
    ap.add_argument("--only", action="append", default=[],
                    help="run only this program (file name; repeatable); results go only to the screen")
    ap.add_argument("--timeout", type=float, default=None, help="seconds per run of the spec (default 7200)")
    ap.add_argument("--full", action="store_true",
                    help="run all programs (default: the quick lists, or all programs of the suites named)")
    ap.add_argument("--det", action="store_true",
                    help="run the spec in deterministic mode: a run fails when two rules or clauses of the spec "
                         "both apply (the tables in <work> are not written)")
    ap.add_argument("--cover", action="store_true", help="record spec coverage in <work>/coverage/<suite>/")
    ap.add_argument("-j", "--jobs", type=int, default=2, help="programs to test at a time (default 2)")
    args = ap.parse_args()
    args.quick = not args.full and not args.suites
    groups = {"regression": lambda: regression_tests(os.path.abspath(args.k_src), args.quick),
              "llvm": lambda: llvm_tests(os.path.abspath(args.llvm_src), args.quick)}
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
