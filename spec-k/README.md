# K in P4-SpecTec

## Prerequisites

- The OCaml setup of the [top-level README](../README.md), and MPFR:
  ```shell
  $ apt-get install libmpfr-dev
  $ opam install mlmpfr
  ```
- [K](https://github.com/runtimeverification/k) v7.1.337, with `kompile` and `krun` on `PATH`
- Python 3.11 or later
- Test inputs, next to this repository (or set `K_SRC`, `KWASM_SRC`, and `KMIR_SRC`):
  ```shell
  $ git clone --filter=blob:none --sparse --branch v7.1.337 https://github.com/runtimeverification/k ../k
  $ git -C ../k sparse-checkout set k-distribution/tests/regression-new
  $ git clone https://github.com/runtimeverification/wasm-semantics ../wasm-semantics
  $ git -C ../wasm-semantics checkout 212271b
  $ git -C ../wasm-semantics submodule update --init --depth 1 tests/wasm-tests
  $ git clone --filter=blob:none https://github.com/runtimeverification/mir-semantics ../mir-semantics
  $ git -C ../mir-semantics checkout 4d79325
  $ git -C ../mir-semantics submodule update --init deps/stable-mir-json
  $ uv --project ../mir-semantics/kmir sync    # kmir, which makes the KMIR inputs
  $ make -C ../mir-semantics stable-mir-json   # needs rustup; for the Rust sources
  ```

Build with `make boot`.

## Tests

The spec is tested by diff tests against `krun`. Everything for them is in
`spec-k/tests/`:

| | |
|---|---|
| `suites.toml` | the suites: where each comes from, and its `skip`, `known`, and `quick` lists |
| `manual/` | the suites written for the spec (`errors`, `behavior`, `equality`, `binder`, `mint`, `priority`); the others are the tests of K, KWasm, KEVM, and KMIR |
| `ktest.py` | runs suites |
| `difftest.py` | diff tests programs against one kompiled definition |
| `coverage_summary.py` | merges coverage logs |
| `kevm_inputs.py`, `kmir_inputs.py` | make the initial terms of KEVM and KMIR |
| `_work/` | kompiled definitions, runs, and result tables (not in git) |

```shell
$ spec-k/tests/ktest.py                                    # the quick run
$ spec-k/tests/ktest.py --full                             # all programs of all suites
$ spec-k/tests/ktest.py imp kwasm                          # all programs of some suites
$ spec-k/tests/ktest.py regression/list-set                # one regression test of K
$ spec-k/tests/ktest.py kwasm --only conformance-i32.wast  # one program
```

Languages are kompiled once into `spec-k/tests/_work/<suite>/kompiled`, and
results are written to `spec-k/tests/_work/<suite>.md` (with `--only`, only to
the screen). Two programs are tested at a time (`-j`). What `krun` does with
each program is kept in `spec-k/tests/_work/krun-cache`, under the hash of the K
version, the kompiled definition, the program, its input, and the options, so
`krun` runs only for new or changed programs and definitions.

Each program is compared with `krun`: the final configuration after
normalization, and the number of rewrite steps. A program on which `krun`
fails must fail at the same step. When the final configuration or the number
of steps differs, the run is checked step by step instead: each step of the
spec must be one of the next configurations that the `search` binary of the
kompiled definition finds, as K leaves some choices open (rules of the same
priority, the elements of maps and sets).

A program in a `known` list is a known failure: the spec follows the K
documentation there and `krun` does not. It is reported as a known failure
when it fails, and as a failure when it passes.

Spec coverage: add `--cover`, then

```shell
$ spec-k/tests/coverage_summary.py spec-k/tests/_work/coverage [-o coverage.log]
```

Diff test of other programs:

```shell
$ spec-k/tests/difftest.py -k <name>-kompiled <program or directory>... [--ext <ext>] [--depth N]
```

Each run of the spec is capped at 2.5 GB through `systemd-run` when it is
available (`--memory-max`).
