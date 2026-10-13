#!/usr/bin/env python3
"""Initial terms of KEVM tests, as kevm-pyk run makes them: each test of each
GeneralStateTest file becomes <out>/<file>-<test>.kore. Runs in the Python
environment of kevm-pyk (uv run --directory <evm-semantics>/kevm-pyk).

usage: kevm_inputs.py <out> <mode> <schedule> <file.json>...
"""
import json
import os
import re
import sys

from kevm_pyk.interpreter import iterate_gst


def main():
    out, mode, schedule, files = sys.argv[1], sys.argv[2], sys.argv[3], sys.argv[4:]
    os.makedirs(out, exist_ok=True)
    for path in files:
        stem = os.path.basename(path)[:-len(".json")]
        for name, kore in iterate_gst(json.load(open(path)), mode, 1, True, schedule=schedule):
            target = os.path.join(out, "%s-%s.kore" % (stem, re.sub(r"[^A-Za-z0-9_.+-]", "_", name)))
            with open(target, "w") as fh:
                kore.write(fh)
                fh.write("\n")


if __name__ == "__main__":
    main()
