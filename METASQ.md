# Metacircular execution

`metacirc` runs the AL interpreter specification in `spec-meta`.
The native backend can be Rust AL or Rust SL. The scripts represented inside
the tower are AL; SL metacircular specifications and mixed towers are not supported.

## Build

From the repository root:

```sh
make metacirc
```

This builds and installs `bin/metacirc` in release mode.
`make build`, `make release`, and `make debug` build only `bin/p4spectec`.
Within `p4spectec`, use `cargo build --locked --release --bin metacirc`;
`cargo run` still defaults to `p4spectec`.

## Standalone AL script

Create a specification source file with a zero-argument `main`:

```text
dec $main() : nat
def $main() = 42
```

Interpret it through the AL specification:

```sh
./bin/metacirc run spec-meta --mode sl --rel Entry -p /path/to/script.watsup
```

Repeat `-p` for ordered source paths. `--mode al` selects the native AL backend.
The host handles `Call_builtin_func` using a separate runner initialized with
the script's own definitions. Other standalone host operations are unsupported.

## P4 tower

Place this configuration at the repository root as `tower.json`:

```json
{
  "mode": "sl",
  "levels": [
    { "specdir": "spec-meta", "rel": "Entry", "interface": "al" },
    { "specdir": "spec", "rel": "Program_inst", "interface": "p4" }
  ]
}
```

Run a P4 program:

```sh
./bin/metacirc tower tower.json -i p4c/p4include -p /path/to/program.p4
```

The path is Rust SL interpreter → AL specification → P4 specification →
P4 program. The entry invokes `Program_inst`, which type-checks the program
and instantiates its declarations. The CLI prints `passed` only after the
outer entry succeeds; diagnostics cause a nonzero exit code.

Levels are ordered from outermost to target. All levels except the last use
`"interface": "al"`; the last uses `"interface": "p4"`.
Each `specdir` accepts a specification file or directory and is resolved
relative to the configuration file. CLI program and include paths are
resolved relative to the current working directory.

To add an AL interpretation layer, insert another
`{ "specdir": "spec-meta", "rel": "Entry", "interface": "al" }`
before the P4 level. The number of layers is chosen at runtime.
The outer runner uses `mode`; intermediate native helpers and the P4
host-call runner use SL.

A synthetic `main` is added to the P4 specification and each intermediate
script, from inside out. It calls that level's single-input entry relation
with the encoded next input and returns `"pass"` after success.
These scripts must not already define `main`.

Both commands accept `--no-cache`, `--det`, and `--guard`.
All runners share one arena. Builtin and extern calls decode one AL
representation layer and encode their results, preserving side effects
on success and failure. Native mismatch becomes meta `FAIL`;
fatal errors remain diagnostics. Resetting a meta runner resets its
owned runners before arena execution values can be cleared.

## Focused test

```sh
make test-metacirc
```

This target builds `metacirc` and runs its independent driver; `make test`
does not include it.

The `test-driver/metacirc` fixture runs only the Rust SL → AL specification →
P4 specification → P4 program `Program_inst` path through the CLI.
The fixture uses the full P4 specification and a small constant declaration,
without requiring the p4c submodule.

Native/AL/AL→AL performance measurements are follow-up work.

## OCaml reference

The frozen OCaml implementation and its AL/SL meta specifications are preserved
at [`v0.1.3`](https://github.com/kaist-plrg/p4-spectec/tree/v0.1.3).
See its [boot documentation](https://github.com/kaist-plrg/p4-spectec/blob/v0.1.3/BOOT.md)
for the historical tower implementation and SL/AL specification documents.
