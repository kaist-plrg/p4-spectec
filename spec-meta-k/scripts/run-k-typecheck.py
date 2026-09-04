#!/usr/bin/env python3
"""Run `k-run-p4.sh` over selected positive or negative P4 programs.

Programs listed in any excludes/static/**/*.exclude file are skipped, as are
files under an include/ directory (#include fragments, not programs).  Progress
and summary output are written incrementally to a result file.

  spec-meta-k/scripts/run-k-typecheck.py             # positive tests
  spec-meta-k/scripts/run-k-typecheck.py --neg       # negative tests
"""

import argparse
import os
import subprocess
import sys
import time
from pathlib import Path
from typing import Optional

ROOT = Path(__file__).resolve().parents[2]
POSITIVE_DIR = ROOT / "p4c" / "testdata" / "p4_16_samples"
NEGATIVE_DIR = ROOT / "p4c" / "testdata" / "p4_16_errors"
EXCLUDES_DIR = ROOT / "excludes" / "static"
POSITIVE_RESULT = ROOT / "spec-meta-k" / "run-k-typecheck-pos.result"
NEGATIVE_RESULT = ROOT / "spec-meta-k" / "run-k-typecheck-neg.result"
K_RUN_P4 = ROOT / "spec-meta-k" / "scripts" / "k-run-p4.sh"

PASS, FAIL, TIMEOUT = "pass", "fail", "timeout"


def load_excludes() -> set[str]:
    """Repo-relative paths named by any .exclude file, normalized."""
    excluded = set()
    for exclude_file in sorted(EXCLUDES_DIR.rglob("*.exclude")):
        for line in exclude_file.read_text().splitlines():
            line = line.strip()
            if line and not line.startswith("#"):
                excluded.add(os.path.normpath(line))
    return excluded


def collect_programs(programs_dir: Path) -> list[str]:
    """Repo-relative paths of the P4 programs to check, minus the excluded.

    Files under an `include/` directory are #include fragments rather than
    standalone programs, so they are skipped too.
    """
    excluded = load_excludes()
    programs = []
    for path in sorted(programs_dir.glob("**/*.p4")):
        rel_path = path.relative_to(ROOT)
        if "include" in rel_path.parts[:-1]:
            continue
        rel = os.path.normpath(str(rel_path))
        if rel not in excluded:
            programs.append(rel)
    return programs


def typecheck(program: str, timeout: Optional[int]) -> dict:
    """Run one program through `k-run-p4.sh` and classify the outcome."""
    started = time.monotonic()
    try:
        proc = subprocess.run(
            [str(K_RUN_P4), program],
            cwd=ROOT,
            capture_output=True,
            text=True,
            timeout=timeout,
        )
        output_lines = proc.stdout.rstrip().splitlines()
        verdict = output_lines[-1].strip() if output_lines else ""
        status = PASS if verdict == "passed" else FAIL
        returncode, stdout, stderr = proc.returncode, proc.stdout, proc.stderr
    except subprocess.TimeoutExpired as e:
        status = TIMEOUT
        returncode = None
        stdout = e.stdout.decode(errors="replace") if e.stdout else ""
        stderr = e.stderr.decode(errors="replace") if e.stderr else ""

    return {
        "program": program,
        "status": status,
        "returncode": returncode,
        "elapsed": round(time.monotonic() - started, 2),
        "stdout": stdout[-8000:],
        "stderr": stderr[-8000:],
    }


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("-t", "--timeout", type=int, default=None,
                        help="per-program timeout in seconds (default: no timeout)")
    parser.add_argument("-n", "--neg", action="store_true",
                        help="run negative tests from p4_16_errors")
    parser.add_argument("-o", "--output", type=Path,
                        help="result file (default depends on test mode)")
    parser.add_argument("-d", "--dry-run", action="store_true",
                        help="list the programs that would be checked, then exit")
    args = parser.parse_args()

    programs_dir = NEGATIVE_DIR if args.neg else POSITIVE_DIR
    default_result = NEGATIVE_RESULT if args.neg else POSITIVE_RESULT
    output_path = args.output or default_result
    programs = collect_programs(programs_dir)
    if args.dry_run:
        for program in programs:
            print(program)
        print(f"\n{len(programs)} program(s)", file=sys.stderr)
        return 0

    total = len(programs)
    output_path.parent.mkdir(parents=True, exist_ok=True)
    with open(output_path, "w", buffering=1) as output:
        if total == 0:
            print("nothing to do", file=output)
            return 0

        counts = {PASS: 0, FAIL: 0, TIMEOUT: 0}
        unexpected = []
        started = time.monotonic()

        for i, program in enumerate(programs, start=1):
            result = typecheck(program, args.timeout)
            counts[result["status"]] += 1
            expected_status = FAIL if args.neg else PASS
            if result["status"] != expected_status:
                unexpected.append(result["program"])
            print(f"[{i}/{total}] {result['status']:<7} "
                  f"{result['elapsed']:>7.2f}s  {result['program']}",
                  file=output, flush=True)

        elapsed = time.monotonic() - started
        print(f"\n{'='*60}", file=output)
        print(f"pass {counts[PASS]}  fail {counts[FAIL]}  timeout {counts[TIMEOUT]}"
              f"  of {total} in {elapsed/60:.1f} min", file=output)
        print(f"result: {output_path}", file=output)
        if unexpected:
            label = "not failing" if args.neg else "not passing"
            print(f"\n{label} ({len(unexpected)}):", file=output)
            for program in unexpected:
                print(f"  {program}", file=output)
        return 1 if unexpected else 0


if __name__ == "__main__":
    sys.exit(main())
