# Metacircular execution

`metacirc run` executes a program through a required JSON tower configuration.
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
./bin/metacirc run towers/al.json -p /path/to/script.watsup
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
./bin/metacirc run towers/al-p4.json -i p4c/p4include -p /path/to/program.p4
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

The command accepts `--no-cache`, `--no-det`, and `--guard`. `--no-det` disables
determinism checks only in the native base interpreter. Interpreter call
caching and AL encode/decode caching are enabled together by default;
`--no-cache` disables both. It prints `passed`
only after the configured base entry succeeds; diagnostics cause a nonzero
exit code.

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

## OCaml reference

The frozen OCaml implementation and its AL/SL meta specifications are preserved
at [`v0.1.3`](https://github.com/kaist-plrg/p4-spectec/tree/v0.1.3).
See its [boot documentation](https://github.com/kaist-plrg/p4-spectec/blob/v0.1.3/BOOT.md)
for the historical tower implementation and AL/SL specification documents.
