# Testing

All tests live in `p4spectec/test-driver/tests/*.rs` and run with [cargo-nextest](https://nexte.st).
Their inputs live under `tests/`:

```
tests/p4spectec/<name>/   small hand-written inputs of <name>.rs, each with a .snap beside it
tests/p4/                 P4 and STF programs (p4c, regression, custom, p4testgen)
tests/p4/excludes/        known failures, skipped by the P4 tests
```

## Run tests

Install nextest once:

```shell
cargo install --locked cargo-nextest
```

| To run | Command |
|---|---|
| what CI runs | `make test PROFILE=ci` |
| everything (slow: every P4 program in AL, SL, and PL) | `make test` |

One test file:

| Command | Checks |
|---|---|
| `make test-speclang` | the spec printed at each stage |
| `make test-parse` | spec parse errors |
| `make test-elab` | elaboration errors and warnings |
| `make test-algo` | AL conversion errors |
| `make test-structure` | SL conversion errors |
| `make test-prose` | PL conversion errors |
| `make test-specdoc` | AsciiDoc rendering warnings |
| `make test-splice` | splicing errors and warnings |
| `make test-run` | P4 type checking and runtime errors |
| `make test-sim` | packet tests and simulation errors |
| `make test-p4parse` | P4 parse-print roundtrips and parse errors |
| `make test-cli` | rejected command lines |

One P4 suite in one language: `make test-<run|sim>-<suite>-<al|sl|pl>`, e.g. `make test-sim-p4c-ebpf-sl`.

| | Suites |
|---|---|
| `run` | `p4c-pos`, `p4c-neg`, `regression-pos`, `regression-neg` |
| `sim` | `p4c-v1model`, `p4c-ebpf`, `p4c-psa`, `p4testgen-v1model`, `p4testgen-ebpf`, `custom-v1model`, `regression-v1model`, `regression-ebpf` |

Anything else:

- One language of `run` or `sim`: `make test-run-sl`, `make test-sim-al`, ...
- One test: `make test FILTER='binary(elab) & test(=pos)'`
- Any [nextest filterset](https://nexte.st/docs/filtersets/): `make test FILTER='...'`
- Every test name: `cd p4spectec && cargo nextest list`

What a run shows:

- A run does not stop at the first failure: it reports every failing test.
- A failing test prints only what went wrong: the snapshot diff, or the P4 cases that did not reach their outcome.
- Each P4 test writes one line per case to `logs/<test>.log`: `pass`, `fail`, or `exclude`, the paths, and the seconds.

## Update snapshots

A `.snap` file holds the expected output of the input beside it.

When an output changes, the test fails and writes `<name>.snap.new` next to `<name>.snap`. Then:

1. Check that the new output is right.
2. Accept it:
   ```shell
   INSTA_UPDATE=always make test-elab
   ```
3. Review with `git diff` and commit the `.snap` files.

`.snap.new` files are ignored by git. Delete leftovers with `find tests -name '*.snap.new' -delete`.

## Add a test

**Add a file to an existing directory. Do not add a new `.rs` file.**
Every test file already picks up every file in its directories, so a new input needs no Rust code.

| To check | Add | Run |
|---|---|---|
| a spec parse error | `tests/p4spectec/parse/neg/<name>.watsup` | `make test-parse` |
| an elaboration error | `tests/p4spectec/elab/neg/<name>.watsup` | `make test-elab` |
| an elaboration warning | `tests/p4spectec/elab/pos/<name>.watsup` | `make test-elab` |
| an AL / SL / PL conversion error | `tests/p4spectec/{algo,structure,prose}/neg/<name>.watsup` | `make test-algo`, ... |
| an AsciiDoc rendering warning | `tests/p4spectec/specdoc/neg/<name>.watsup` | `make test-specdoc` |
| a splicing error or warning | `tests/p4spectec/splice/{neg,pos}/<name>.adoc` | `make test-splice` |
| a runtime error | `tests/p4spectec/run/neg/<name>.watsup`, where `R` applied to `1` fails | `make test-run` |
| a relation that must hold | `tests/p4spectec/run/pos/<name>.watsup` with a `Main` that holds | `make test-run` |
| a simulation error | `tests/p4spectec/sim/neg/<name>.p4` (ebpf) and `<name>.stf` | `make test-sim` |
| a P4 parse error | `tests/p4spectec/p4parse/neg/<name>.p4` | `make test-p4parse` |
| a rejected command line | `tests/p4spectec/cli/command/<name>.args`, one argument per line | `make test-cli` |
| a P4 program that must type-check | `tests/p4/regression/pos/<name>.p4` | `make test-run-regression-pos-sl` |
| a P4 program that must be rejected | `tests/p4/regression/neg/<name>.p4` | `make test-run-regression-neg-sl` |
| a packet test | `tests/p4/regression/sim/<name>.p4` and `<name>.stf` | `make test-sim-regression-v1model-sl` |

For an input under `tests/p4spectec/`, run its test once with `INSTA_UPDATE=always` to create the `.snap`, check it, and commit both files.
A passing input (`pos`) gets a `.snap` only if it prints warnings.

A P4 program under `tests/p4/` has no snapshot: it only has to pass (or fail, for `neg`).
If a program fails for a known reason, list its path in a `.exclude` file under `tests/p4/excludes/` instead of deleting it:

```
# tests/p4/excludes/static/p4c/confirmed/i72-c5092-shadow-control.exclude
tests/p4/p4c/testdata/p4_16_errors/issue2544_shadowing1.p4
```

## When you really need new Rust code

- **A new directory for an existing file**: add a constant and a one-line `#[test]` in that file, next to the others:
  ```rust
  const POS: Fixtures = Fixtures { dir: "elab/pos", outcome: Outcome::Pass };

  #[test]
  fn pos() {
      POS.check();
  }
  ```
- **A new `.rs` file**: only for a new stage that no file checks. Copy the closest file, keep its `// = Helpers`, `// = Suites`, `// = Tests` order, add `FILTER_<name> := binary(<name>)` to `p4spectec/Makefile`, and add a row to the README table.
