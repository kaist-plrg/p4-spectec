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
    { "specdir": "../spec-meta", "rel": "Eval_AL_func", "lang": "al" }
  ]
}
```

The single level is both top level and native base. Its `Eval_AL_func` interprets the
AL script, calling its function `main` with no inputs. Builtin calls of the script are answered by the native builtins;
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
    { "specdir": "../spec-meta", "rel": "Eval_AL_rel", "lang": "al" },
    { "specdir": "../spec", "rel": "Program_inst", "lang": "p4" }
  ]
}
```

The path is Rust SL interpreter → AL specification → P4 specification →
P4 program. `Program_inst` type-checks the program and instantiates its
declarations. The base runs ``Eval_AL_rel: specAL |- id `( val* `) : val*`` natively on
the P4 specification as AL, the name `"Program_inst"`, and the program
encoded as a spec-meta `val`; `Eval_AL_rel` calls `Program_inst` through
`Call_rel` and outputs its results. A P4 top level takes exactly one `-p` path.
`towers/al-p4-ok.json` is the same tower entered through `Program_ok`,
which only type-checks the program.

## P4 simulation

`metacirc sim` runs an STF test on an `sl` tower of two or more levels whose
top level is P4, and prints each matched packet as `p4spectec sim` does:

```sh
./bin/metacirc sim towers/al-p4.json --arch v1model -i p4c/p4include \
  -p p4c/testdata/p4_16_samples/issue1097-2-bmv2.p4 \
  --stf p4c/testdata/p4_16_samples/issue1097-2-bmv2.stf
```

```
[PASS] Transmitted (0) 001741
[PASS] Transmitted (0) 01FF29
passed
```

`--arch` selects `ebpf`, `psa`, or `v1model`. The simulator calls the P4
specification's relations directly, so the levels' `rel` entries are unused.
A call of a level above the base runs as `Call_rel` (or `Call_func`) on
spec-meta at the level below, under a context `C` that loads that level's
specification once per program; the base runs natively. In a three-level
tower such as `towers/al-al-p4.json`, a P4 call becomes `Call_rel` at the
middle level, which itself runs as `Call_rel` at the base. Host calls relay
back up one level at a time. Each added level interprets spec-meta once more,
so a three-level simulation is too slow for the STF corpus. When the
architecture reenters the P4 specification, the call runs one depth below
(see Tower structure).

## Tower configuration

Levels are ordered from native base through intermediates to top level.
A single level supplies both top level and base. `lang` names the language a
level accepts: every level below the top level uses `al` or `il`, and the
top level may also use `p4`. The level above an `al` level is passed to it as
AL converted by the native `algo` pass; the level above an `il` level is
passed as elaborated IL. `mode` selects the base's native backend
(`sl` or `al`).

Only the base runs on a native interpreter. Every level above it is
interpreted by the level below, so natively it only answers the builtin and
host calls relayed from below, without consulting its own definitions.

`rel` names the relation entering a level. A level above an `al` or `il` level
is encoded as the inputs of an entry relation at the level below: the
specification, its `rel`, and the inputs of that `rel` as `val`s.
`Eval_AL_rel` and `Eval_IL_rel` call a relation through `Call_rel`;
`Eval_AL_func` and `Eval_IL_func` call a function, such as the `main` of a
script, through `Call_func` and output its value. A level entered through a
relation of the level above, including every P4 tower, uses the `_rel` entry;
only the level below a script uses the `_func` entry.

Each `specdir` accepts a specification file or directory and is resolved
relative to the configuration file. CLI program and include paths are
resolved relative to the current working directory.

To add an AL interpretation layer, insert another
`{ "specdir": "../spec-meta", "rel": "Eval_AL_rel", "lang": "al" }`
below the top level, as the supplied three-level `al-al-p4.json` does;
`al-il-p4.json` instead makes the middle level accept IL. A name such as
`al-al.json` describes another configuration of this kind.

`run-meta` accepts `--no-cache`, `--no-det`, and `--guard`. `--no-det` disables
determinism checks only in the native base interpreter. Interpreter call
caching and AL encode/decode caching are enabled together by default;
`--no-cache` disables both. `run-meta` prints `passed`
only after the configured base entry succeeds; diagnostics cause a nonzero
exit code.

## Tower structure

`Tower` (`p4spectec/src/metacirc/tower/`) keeps every level's specification
(`PreparedLevel`), one `Protocol` per boundary, and the base runner. A P4 base is a
plain runner. A meta base's runner stores the levels above it in `Upper`:

```
Upper { caches, ctxs, hosts, top, depth_next }
  caches[k-1]  codec cache of the boundary below level k
  ctxs[k-1]    C of level k for the current program, loaded on first use
  hosts[k-1]   meta builtins of level k, between the base and the top
  top          Top::P4(P4 interface + architecture) | Top::Program(builtins)
  depth_next   reentry depths 1 and deeper
```

Above a meta top level is its AL or IL program, one more boundary whose
builtins `Top::Program` answers.

Three traits divide the work, each with one implementation stored as an enum:

| trait | implementation | role |
|---|---|---|
| `Engine` (`engine/`) | `SpecTecEngine` | runs the base natively, on AL or SL |
| `Protocol` (`protocol/`) | `MetaProtocol` | how spec-meta interprets the level above: load C, lower a call to `Call_rel`/`Call_func`, raise its result, decode and encode host calls |
| `HostCalls` (`boundary.rs`) | `Boundary` | answers a decoded host call at the level above a boundary |

A call goes down from the P4 top through each level's protocol to the engine
(`interp.rs`, `TowerInterp`); a host call goes up from the engine through
`Boundary { level }`, one level at a time, to the P4 interface and the
architecture. `run-meta` enters at the base's configured relation and `sim` at
the P4 relations the architecture calls; both lend the same levels. When a P4
extern reenters the specification, as `static_assert` does to read its
arguments and the architectures do through `find_var_e`, the call runs on the
next depth: a fresh interpreter of the base's representation and fresh meta
builtins, sharing C and the codec caches.

## Algo through the IL-to-AL specification

`spec-meta/3-algo` specifies the IL-to-AL pass as the relation `Algo_IL`.
`Eval_IL_rel` and `Eval_IL_func` convert their IL input with `Algo_IL` and
evaluate the result with `Eval_AL_rel` and `Eval_AL_func`, so a spec-meta level that accepts IL runs the specification
above it on AL produced by the specification:

```json
{
  "mode": "sl",
  "levels": [
    { "specdir": "../spec-meta", "rel": "Eval_IL_rel", "lang": "il" },
    { "specdir": "../spec", "rel": "Program_inst", "lang": "p4" }
  ]
}
```

The conversion runs for every input, in the level that accepts the IL.
Only the base, here the IL-to-AL specification itself, is converted by the
native `algo` pass; the specification above it reaches native code only
through its builtin and host calls.
`towers/il.json`, `towers/il-p4.json`, and `towers/il-p4-ok.json`
are the supplied towers entered through `Eval_IL_func` and `Eval_IL_rel`.

## Execution corpus

```sh
make test-metacirc-micro-al
make test-metacirc-micro-il
make test-metacirc-run-al
make test-metacirc-run-il
make test-metacirc-sim-al
make test-metacirc-sim-il
```

`test-metacirc-micro-al` runs the spec-language sources under
`testdata/spec/pos/run` and `testdata/spec/neg/run` through the AL tower,
each expected to succeed or fail as its suite in `suites/metacirc/run.toml`
registers. `test-metacirc-run-al` runs the `test-run-sl` corpus through the AL/P4
tower and requires the p4c submodule. Both check determinism.
`test-metacirc-sim-al` runs the `test-sim-sl` corpus, every suite of
`suites/sim.toml`, through the AL/P4 tower that `suites/metacirc/sim.toml`
names, against the native simulation expectations; it requires the p4c submodule.

The driver's `run` and `sim` subcommands take a configuration name,
`al` or `il`, which selects the language the towers' base level takes.
Each suite registers a tower per base; the `-il` targets run the same suites
through towers entered by `Eval_IL_func` or `Eval_IL_rel`, so the input is converted by `Algo_IL`
instead of the native `algo` pass. They share the expectations of the `al` runs;
sources whose outcome differs are listed in the `il` exclusions.
`run` selects suites with `--input al` or `--input p4`,
and both select one registered suite with `--suite <NAME>`,
for example `run al --suite neg-run` or `sim il --suite v1model-regression`.

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
as registered in `suites/metacirc/snapshot.toml`. `test-metacirc-promote` updates the
elab, algo, and prose snapshots; structure snapshots are never promoted.

```sh
make test-metacirc-metaparse
```

Checks that the IL and AL meta encodings of `spec` and `spec-meta` are fixed
points of decode-then-encode, as registered in `suites/metacirc/metaparse.toml`.

## OCaml reference

The frozen OCaml implementation and its AL/SL meta specifications are preserved
at [`v0.1.3`](https://github.com/kaist-plrg/p4-spectec/tree/v0.1.3).
See its [boot documentation](https://github.com/kaist-plrg/p4-spectec/blob/v0.1.3/BOOT.md)
for the historical tower implementation and AL/SL specification documents.
