#!/usr/bin/env python3
"""The initial term of one KEVM test, as kevm-pyk run makes it (VMTESTS mode,
default schedule). Runs in the Python environment of kevm-pyk
(uv run --project <evm-semantics>/kevm-pyk).

usage: init.py <file.json> <test> <out.kore>
"""
import json
import sys

from kevm_pyk.interpreter import iterate_gst


def main():
    path, test, out = sys.argv[1:]
    for name, kore in iterate_gst(json.load(open(path)), "VMTESTS", 1, True, schedule="DEFAULT"):
        if name == test:
            with open(out, "w") as fh:
                kore.write(fh)
                fh.write("\n")
            return
    sys.exit("%s: no test %s" % (path, test))


if __name__ == "__main__":
    main()
