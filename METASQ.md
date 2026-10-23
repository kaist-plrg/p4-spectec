# Metacircular execution

`metacirc run-meta` executes a program through a required JSON tower configuration.
The native backend can be Rust AL or Rust SL. Interpreter specifications inside
the tower consume AL; the top level accepts either an AL script or a P4 program.
SL metacircular specifications and mixed AL/SL towers are not supported.

## Build

From the repository root:

```sh
make metacirc
```

This builds and installs `bin/metacirc` in release mode.

Pass `CARGO_TARGET_DIR` to select another build directory; relative paths
are resolved from the repository root.

## SpecTec programs

Create a specification source file with a zero-argument `main`:

```text
dec $main() : nat
def $main() = 42
```

Run it with the supplied AL tower:

```sh
./bin/metacirc run-meta towers/al.json -p /path/to/script.watsup
```

`towers/al.json` contains:

```json
{
  "mode": "sl",
  "levels": [
    { "specdir": "../spec-meta", "rel": "Entry", "lang": "al" }
  ]
}
```

The single level is both top level and native base. Its `Entry` interprets the
AL script. Builtin calls use the input script's definitions;
other host operations are unsupported.
Repeat `-p` for ordered specification source paths.

## P4 programs

Use the supplied AL/P4 tower:

```sh
./bin/metacirc run-meta towers/al-p4.json -i p4c/p4include -p /path/to/program.p4
```

`towers/al-p4.json` contains:

```json
{
  "mode": "sl",
  "levels": [
    { "specdir": "../spec-meta", "rel": "Entry", "lang": "al" },
    { "specdir": "../spec", "rel": "Program_inst", "lang": "p4" }
  ]
}
```

The path is Rust SL interpreter → AL specification → P4 specification →
P4 program. `Program_inst` type-checks the program and instantiates its
declarations. A P4 top level takes exactly one `-p` path.
`towers/al-p4-ok.json` is the same tower entered through `Program_ok`,
which only type-checks the program.

## Tower configuration

Levels are ordered from native base through intermediates to top level.
A single level supplies both top level and base. In a longer tower, every level
below the top level must have `"lang": "al"`; the top level may use `al` or `p4`.
`mode` selects the base's native backend (`sl` or `al`).

Each `specdir` accepts a specification file or directory and is resolved
relative to the configuration file. CLI program and include paths are
resolved relative to the current working directory.

To add an AL interpretation layer, insert another
`{ "specdir": "../spec-meta", "rel": "Entry", "lang": "al" }`
below the top level. Names such as `al-al.json` and `al-al-p4.json` describe
these additional configurations; only `al.json`, `al-p4.json`, and
`al-p4-ok.json` are supplied.

In a multi-level tower, top-level and intermediate specifications must not
define `main`.

`run-meta` and `algo-meta` accept `--no-cache`, `--no-det`, and `--guard`. `--no-det` disables
determinism checks only in the native base interpreter. Interpreter call
caching and AL encode/decode caching are enabled together by default;
`--no-cache` disables both. `run-meta` prints `passed`
only after the configured base entry succeeds; diagnostics cause a nonzero
exit code.

## Lowering through the IL-to-AL specification

`spec-meta/3-algo` specifies the IL-to-AL pass as the relation `Algo_spec`.
A tower with an `algo` entry lowers the specification under test through it:

```json
{
  "mode": "sl",
  "algo": "../spec-meta",
  "levels": [
    { "specdir": "../spec-meta", "rel": "Entry", "lang": "al" },
    { "specdir": "../spec", "rel": "Program_inst", "lang": "p4" }
  ]
}
```

The specification under test is the P4 target's specification
or, for an AL target, the input script.
It is elaborated, encoded as IL, lowered by `Algo_spec`, and decoded once
when the tower is built; every input then reuses that AL.
The IL-to-AL specification itself and the other levels are lowered natively.
`towers/algo-al.json`, `towers/algo-al-p4.json`, and `towers/algo-al-p4-ok.json`
are the supplied towers with an `algo` entry.

To print the AL produced by the specification:

```sh
./bin/metacirc algo-meta spec-meta /path/to/spec
```

## Execution corpus

```sh
make test-metacirc-micro
make test-metacirc
```

`test-metacirc-micro` runs the spec-language sources under
`testdata/spec/pos/run` and `testdata/spec/neg/run` through the AL tower,
each expected to succeed or fail as its suite in `suites/metacirc.json`
registers. `test-metacirc` runs the `test-run-sl` corpus through the AL/P4
tower and requires the p4c submodule. Both check determinism.
The driver selects suites with `--input al` or `--input p4`,
or one registered suite with `--suite <NAME>`, for example `--suite neg-run`.

## Specification snapshots

```sh
make test-metacirc-elab
make test-metacirc-algo
make test-metacirc-structure
make test-metacirc-prose
make test-metacirc-promote
```

Compare the elaborated, algorithmic, structured (with and without rule
groups), and prose forms of `spec-meta` with `expected/spec-meta/`,
as registered in `suites/metacirc/`. `test-metacirc-promote` updates the
elab, algo, and prose snapshots; structure snapshots are never promoted.

## OCaml reference

The frozen OCaml implementation and its AL/SL meta specifications are preserved
at [`v0.1.3`](https://github.com/kaist-plrg/p4-spectec/tree/v0.1.3).
See its [boot documentation](https://github.com/kaist-plrg/p4-spectec/blob/v0.1.3/BOOT.md)
for the historical tower implementation and AL/SL specification documents.
