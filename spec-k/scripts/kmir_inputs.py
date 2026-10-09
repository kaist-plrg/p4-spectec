#!/usr/bin/env python3
"""Initial terms of KMIR tests, as kmir run makes them for the LLVM backend
(KMIR.run_smir, concrete mode, start symbol main): each <name>.smir.json
becomes <out>/<dir>-<name>.kore. Runs in the Python environment of kmir
(uv run --project <mir-semantics>/kmir).

usage: kmir_inputs.py <kompiled> <out> <file.smir.json>...
"""
import os
import sys
from pathlib import Path

from pyk.kast.inner import KSort

from kmir.kast import ConcreteMode, make_call_config
from kmir.kmir import KMIR
from kmir.smir import SMIRInfo


def main():
    kompiled, out, files = Path(sys.argv[1]), sys.argv[2], sys.argv[3:]
    os.makedirs(out, exist_ok=True)
    kmir = KMIR(kompiled)
    for path in files:
        smir_info = SMIRInfo.from_file(Path(path)).reduce_to("main")
        init_config, _ = make_call_config(kmir.definition, smir_info=smir_info, start_symbol="main",
                                          mode=ConcreteMode(), cell_maps=kmir._make_smir_maps(smir_info))
        init_kore = kmir.kast_to_kore(init_config, KSort("GeneratedTopCell"))
        name = os.path.basename(path)[:-len(".smir.json")]
        target = os.path.join(out, "%s-%s.kore" % (os.path.basename(os.path.dirname(path)), name))
        with open(target, "w") as fh:
            init_kore.write(fh)
            fh.write("\n")


if __name__ == "__main__":
    main()
