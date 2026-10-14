# Metacircular execution

`metacirc run` executes a program through a required JSON tower configuration.
The native backend can be Rust AL or Rust SL. Interpreter specifications inside
the tower consume AL; the target accepts either an AL script or a P4 program.
SL metacircular specifications and mixed AL/SL towers are not supported.

## Build

From the repository root:

```sh
make metacirc
```

This builds and installs `bin/metacirc` in release mode. The root Makefile
forwards the explicit target to `METASQ.mk`, which owns the metacircular
build and test rules. The native `p4spectec` build remains independent.

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
    { "specdir": "../spec-meta", "rel": "Entry", "interface": "al" }
  ]
}
```

The single level is both target and native base. Its `Entry` interprets the
encoded AL script. A separate helper resolves builtin calls using the input
script's own definitions; other terminal host operations are unsupported.
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
    { "specdir": "../spec-meta", "rel": "Entry", "interface": "al" },
    { "specdir": "../spec", "rel": "Program_inst", "interface": "p4" }
  ]
}
```

The path is Rust SL interpreter → AL specification → P4 specification →
P4 program. `Program_inst` type-checks the program and instantiates its
declarations. A P4 target takes exactly one `-p` path.

## Tower configuration

Levels are ordered from native base through intermediates to target.
A single level supplies both target and base. In a longer tower, every level
before the target must have `"interface": "al"`; the target may use `al` or `p4`.
`mode` selects the base's native backend (`sl` or `al`). Intermediate and target
host helpers use SL; a single-level tower uses the configured mode throughout.

Each `specdir` accepts a specification file or directory and is resolved
relative to the configuration file. CLI program and include paths are
resolved relative to the current working directory.

To add an AL interpretation layer, insert another
`{ "specdir": "../spec-meta", "rel": "Entry", "interface": "al" }`
before the target. Names such as `al-al.json` and `al-al-p4.json` describe
these additional configurations; only `al.json` and `al-p4.json` are supplied.

For a multi-level tower, a synthetic `main` is added to the target specification
and each intermediate script, from target toward base. It calls that level's
single-input entry relation with the encoded input and returns `"pass"` after
success. These scripts must not already define `main`. The base is executed
natively rather than encoded as another input script.

The command accepts `--no-cache`, `--det`, and `--guard`. It prints `passed`
only after the configured base entry succeeds; diagnostics cause a nonzero
exit code. All runners share one arena. Host calls decode one representation
layer and encode their results, preserving effects. Native mismatch becomes
meta `FAIL`; fatal errors remain diagnostics.

Runtime value codecs traverse and construct interned mixops directly.
Fixed constructor shapes are cached as handles within their owning mixop
arena; matching compares canonical identities. Source AST notation and
synthetic-entry patching retain their tree representation.

## Focused test

```sh
make test-metacirc
```

This target builds `metacirc` and runs its independent driver.
The default test targets do not include it.

The `test-driver/metacirc` fixture runs only the Rust SL → AL specification →
P4 specification → P4 program `Program_inst` path through the CLI using
`towers/al-p4.json`. It uses the full P4 specification and a small constant
declaration, without requiring the p4c submodule.

Native/AL/AL→AL performance measurements are follow-up work.

## OCaml reference

The frozen OCaml implementation and its AL/SL meta specifications are preserved
at [`v0.1.3`](https://github.com/kaist-plrg/p4-spectec/tree/v0.1.3).
See its [boot documentation](https://github.com/kaist-plrg/p4-spectec/blob/v0.1.3/BOOT.md)
for the historical tower implementation and AL/SL specification documents.
