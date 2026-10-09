#!/usr/bin/env python3
"""Initial terms of KMIR tests, as kmir run makes them for the LLVM backend
(KMIR.run_smir, concrete mode, start symbol main): each <name>.smir.json
becomes <out>/<dir>-<name>.kore, and each <name>.rs, through stable-mir-json
as kmir run makes its SMIR JSON, <out>/rs-<dir>-<name>.kore. Runs in the
Python environment of kmir (uv run --project <mir-semantics>/kmir).

usage: kmir_inputs.py <kompiled> <out> <file.smir.json or file.rs>...
"""
import os
import sys
import tempfile
from pathlib import Path

from pyk.kast.inner import KSort

from kmir.cargo import cargo_get_smir_json
from kmir.kast import ConcreteMode, make_call_config
from kmir.kmir import KMIR
from kmir.smir import SMIRInfo


def main():
    kompiled, out, files = Path(sys.argv[1]), sys.argv[2], sys.argv[3:]
    os.makedirs(out, exist_ok=True)
    kmir = KMIR(kompiled)
    for path in files:
        group = os.path.basename(os.path.dirname(path))
        if path.endswith(".rs"):
            with tempfile.TemporaryDirectory() as tmp:
                smir_info = SMIRInfo(cargo_get_smir_json(Path(path), cwd=Path(tmp)))
            name = "rs-%s-%s" % (group, os.path.basename(path)[:-len(".rs")])
        else:
            smir_info = SMIRInfo.from_file(Path(path))
            name = "%s-%s" % (group, os.path.basename(path)[:-len(".smir.json")])
        smir_info = smir_info.reduce_to("main")
        init_config, _ = make_call_config(kmir.definition, smir_info=smir_info, start_symbol="main",
                                          mode=ConcreteMode(), cell_maps=kmir._make_smir_maps(smir_info))
        init_kore = kmir.kast_to_kore(init_config, KSort("GeneratedTopCell"))
        with open(os.path.join(out, name + ".kore"), "w") as fh:
            init_kore.write(fh)
            fh.write("\n")


if __name__ == "__main__":
    main()
