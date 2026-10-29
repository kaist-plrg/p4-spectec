#!/usr/bin/env python3
"""The initial term of one KMIR test, as kmir run makes it for the LLVM backend
(KMIR.run_smir, concrete mode, start symbol main), from its SMIR JSON, or from
its Rust source through stable-mir-json as kmir run makes the SMIR JSON. Runs in
the Python environment of kmir (uv run --project <mir-semantics>/kmir).

usage: init.py <kompiled> <file.smir.json or file.rs> <out.kore>
"""
import sys
import tempfile
from pathlib import Path

from pyk.kast.inner import KSort

from kmir.cargo import cargo_get_smir_json
from kmir.kast import ConcreteMode, make_call_config
from kmir.kmir import KMIR
from kmir.smir import SMIRInfo


def main():
    kompiled, path, out = Path(sys.argv[1]), sys.argv[2], sys.argv[3]
    kmir = KMIR(kompiled)
    if path.endswith(".rs"):
        with tempfile.TemporaryDirectory() as tmp:
            smir_info = SMIRInfo(cargo_get_smir_json(Path(path), cwd=Path(tmp)))
    else:
        smir_info = SMIRInfo.from_file(Path(path))
    smir_info = smir_info.reduce_to("main")
    init_config, _ = make_call_config(kmir.definition, smir_info=smir_info, start_symbol="main",
                                      mode=ConcreteMode(), cell_maps=kmir._make_smir_maps(smir_info))
    init_kore = kmir.kast_to_kore(init_config, KSort("GeneratedTopCell"))
    with open(out, "w") as fh:
        init_kore.write(fh)
        fh.write("\n")


if __name__ == "__main__":
    main()
