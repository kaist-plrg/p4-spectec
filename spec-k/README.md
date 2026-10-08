# K in P4-SpecTec

## Prerequisites

- The OCaml setup of the [top-level README](../README.md), and MPFR:
  ```shell
  $ apt-get install libmpfr-dev
  $ opam install mlmpfr
  ```
- [K](https://github.com/runtimeverification/k) v7.1.337, with `kompile` and `krun` on `PATH`
- Python 3.11 or later
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

The suites are defined in `spec-k/scripts/suites.toml`. Languages are
kompiled once into `spec-k/_k-test/<suite>/kompiled`, and
results are written to `spec-k/_k-test/<suite>.md` (with `--only`, only to
the screen). Two programs are tested at a time (`-j`).

Each program is compared with `krun`: the final configuration after
normalization, and the number of rewrite steps. A program on which `krun`
fails must fail at the same step. A nondeterministic program (threads, or an
order K leaves open) is checked step by step instead: each step of the spec must be one of the
next configurations that the `search` binary of the kompiled definition
finds (`difftest.py --check-steps-for NAME`; `NAME:N` checks only the first
N steps, for a program whose runs may not end).

Spec coverage: add `--cover`, then

```shell
$ spec-k/scripts/coverage_summary.py spec-k/_k-test/coverage [-o coverage.log]
```

Diff test of other programs:

```shell
$ spec-k/scripts/difftest.py -k <name>-kompiled <program or directory>... [--ext <ext>] [--depth N]
```

To find the first step where the spec and `krun` disagree:

```shell
$ spec-k/scripts/stepdiff.py -k <name>-kompiled <program> [--krun-arg ...]
```

Each run of the spec is capped at 2.5 GB through `systemd-run` when it is
available (`--memory-max`).
