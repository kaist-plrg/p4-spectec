# K in P4-SpecTec

## Prerequisites

- The OCaml setup of the [top-level README](../README.md), and MPFR:
  ```shell
  $ apt-get install libmpfr-dev
  $ opam install mlmpfr
  ```
- [K](https://github.com/runtimeverification/k) v7.1.337, with `kompile` and `krun` on `PATH`
- Python 3
- Test inputs, next to this repository (or set `K_SRC` and `KWASM_SRC`):
  ```shell
  $ git clone --filter=blob:none --sparse --branch v7.1.337 https://github.com/runtimeverification/k ../k
  $ git -C ../k sparse-checkout set k-distribution/tests/regression-new
  $ git clone https://github.com/runtimeverification/wasm-semantics ../wasm-semantics
  $ git -C ../wasm-semantics checkout 212271b
  $ git -C ../wasm-semantics submodule update --init --depth 1 tests/wasm-tests
  ```

Build with `make boot`.

## Usage

Run the diff tests against `krun`:

```shell
$ spec-k/scripts/ktest.py                                     # all
$ spec-k/scripts/ktest.py imp lambda simple kool kwasm        # some
$ spec-k/scripts/ktest.py regression/list-set                 # one regression test of K
$ spec-k/scripts/ktest.py kwasm --only conformance-i32.wast   # one program
```

Languages are kompiled once into `spec-k/_k-test/<suite>/kompiled`, and
results are written to `spec-k/_k-test/<suite>.md`. All suites take several hours on two cores;
`kwasm` takes most of it.

Spec coverage: add `--cover`, then

```shell
$ spec-k/scripts/coverage_summary.py spec-k/_k-test/coverage [-o coverage.log]
```

Diff test of other programs:

```shell
$ spec-k/scripts/difftest.py -k <name>-kompiled <program or directory>... [--ext <ext>]
```

Each run of the spec is capped at 2.5 GB through `systemd-run` when it is
available (`--memory-max`).
