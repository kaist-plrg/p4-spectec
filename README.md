# P4-SpecTec

A specification mechanization toolchain for the P4 programming language.
As of March 2026, P4-SpecTec has been **conditionally accepted** as the
official P4 specification authoring toolchain by the P4 Language Design Working
Group (LDWG). This means that:

* P4 language design will take place at the mechanized specification level
  (in the `spec` directory).
* The P4 language specification will be automatically generated from the
  mechanized specification
  ([preview](https://kaist-plrg.github.io/p4-spectec/p4/P4-16-spec-release.html)).
* The mechanized specification will be tested using the `p4c` test suite.

The implementation is written in Rust. It reimplements work from
[Petr4](https://github.com/verified-network-toolchain/petr4), especially the
parser and numerics implementation, and
[Wasm-SpecTec](https://github.com/Wasm-DSL/spectec), especially the specification
parser and the high-level architecture of the tool.

## Tutorial

* [Mechanizing the Nano-P4 Specification with P4-SpecTec](https://kaist-plrg.github.io/nano-p4-tutorial/):
  a hands-on guide to syntax, type checking, evaluation, and prose generation
  for an educational subset of P4.
* [P4-SpecTec Tutorial: A Project-Based Introduction with Nano-P4](https://www.youtube.com/watch?v=wqshMP8Tz-k&t=30s):
  a video tutorial from P4 Developer Days.

## Publications and Talks

* **P4 Workshop 2024**:
  [P4-SpecTec: Mechanized Language Definition for P4](https://www.youtube.com/watch?v=AcxAYvM8pnA&t=4s).
* **P4 Developer Days 2025**:
  [Mechanizing the P4 Language Specification with P4-SpecTec](https://www.youtube.com/watch?v=2BhqyE7c-Pw).
* **PLSS 2025**:
  [P4-SpecTec: Mechanized Language Definition for P4](https://www.youtube.com/watch?v=a97P7VvCg6k&t=1070s).
* **HATRA 2025**:
  [P4-SpecTec: A Language Mechanization Framework for P4](https://www.youtube.com/watch?v=Y_lRXfUszF0&t=1278s).
* **OOPSLA 2026**:
  [P4-SpecTec: Integrating a Language Mechanization Framework into the Real-World P4 Specification](https://dl.acm.org/doi/10.1145/3839535).

## Building

Install [Rust through rustup](https://rustup.rs/), GNU Make, and a C compiler
available as `cc`. The P4 parser also uses `cc` to preprocess P4 input files.
The toolchain is pinned in `p4spectec/rust-toolchain.toml`; rustup selects it
when Make enters the Rust crate. OCaml, opam, and Dune are not required.

On Linux, install the native build tools with your package manager, for example
`sudo apt-get install build-essential` on Debian/Ubuntu. On macOS, install the
Xcode command line tools with `xcode-select --install`. On Windows, use WSL2
and the Linux instructions.

Initialize the `p4c` submodule for its P4 include files and test corpus:

```shell
git submodule update --init p4c
make build
```

This creates the optimized executable `./bin/p4spectec`. `make` and
`make release` also build in release mode. To build a debug executable:

```shell
make debug
./bin/p4spectec --help
```

All build targets replace `./bin/p4spectec`. The Cargo package and binary are
named `p4spectec`, and build output defaults to `p4spectec/target/`.
To select another build directory, pass `CARGO_TARGET_DIR` to Make; a relative
path is resolved from the directory where Make is invoked:

```shell
make build CARGO_TARGET_DIR=target
```

A `Dockerfile` provides a development environment with the pinned toolchain
and the document dependencies below.
Initialize the `p4c` submodule first, then build and enter the image:

```shell
docker build -t p4spectec .
docker run --rm -it p4spectec
```

Inside the container, `p4spectec` is on `PATH` and all Make targets work.

## Processing the specification

The specification source files live in `spec/`. The tool processes them through
EL (external language), IL (internal language), AL (algorithmic language),
SL (structured language), and PL (prose language).

```shell
# Parse and elaborate the specification to IL
./bin/p4spectec elab spec
# Translate to AL
./bin/p4spectec algo spec
# Structure the specification as SL
./bin/p4spectec struct spec
# Generate PL
./bin/p4spectec prose spec
```

Run `./bin/p4spectec <command> --help` for command-specific arguments.

## Running P4 programs

`run` evaluates a relation on a P4 program. Use `Program_ok` for type checking
or `Program_inst` for instantiation. SL is the default interpreter;
`--al`, `--sl`, and `--pl` select a language explicitly.

```shell
./bin/p4spectec run spec --rel Program_ok -i p4c/p4include \
  -p p4c/testdata/p4_16_samples/basic_routing-bmv2.p4

./bin/p4spectec run spec --rel Program_inst -i p4c/p4include \
  -p p4c/testdata/p4_16_samples/basic_routing-bmv2.p4 --al
```

`sim` executes packet tests in STF format. The supported architectures are
`v1model`, `ebpf`, and `psa`:

```shell
./bin/p4spectec sim spec --arch v1model -i p4c/p4include \
  -p p4c/testdata/p4_16_samples/basic_routing-bmv2.p4 \
  --stf testdata/p4testgen/basic_routing-bmv2/basic_routing-bmv2_1.stf
```

Both commands enable interpreter caching and deterministic execution checks by
default. Use `--no-cache` to disable caching or `--no-det` to disable determinism
checks.

## Generating specification documents

Document generation additionally requires Ruby, Python 3.10 or later, and the
`asciidoctor`, `asciidoctor-pdf`, and `rouge` Ruby gems:

```shell
gem install asciidoctor asciidoctor-pdf rouge
```

Use a Ruby installation where you can install gems. On Linux, native gem
extensions may also require the Ruby development package (`ruby-dev` on
Debian/Ubuntu). On macOS, a package-manager Ruby installation avoids modifying
the system Ruby. Ensure its gem executables and a supported `python3` are on
`PATH`; macOS may select an older system Python by default.

From the project root:

```shell
# P4 release document: HTML only, or HTML and PDF
make p4spec-release-html
make p4spec-release
# P4 draft document: HTML only, or HTML and PDF
make p4spec-draft-html
make p4spec-draft
```

The generated files are in `docs/p4/`.
These targets build the release executable and splice the specifications into
AsciiDoc skeletons before rendering. Missing prose and references are reported
in `docs/p4/splice.missing`.

To splice another skeleton, use the Rust CLI's long options:

```shell
./bin/p4spectec splice spec --splice input.adoc --out output.adoc
# Or replace the skeleton in place
./bin/p4spectec splice spec --splice input.adoc --inplace
```

## Development and tests

Run these commands from the project root:

```shell
make fmt          # Format the product and tests
make fmt-check    # Check formatting without rewriting files
make lint         # Clippy for the product and tests
make rustdoc      # Build the Rust API documentation
make test         # Run every integration test
```

Tests are Rust integration tests run by [cargo-nextest](https://nexte.st)
(`cargo install --locked cargo-nextest`), one test per corpus, specification stage, or diagnostic stage:

| Test file | Coverage |
| --- | --- |
| `p4spectec/test-driver/tests/speclang.rs` | Elaborated, AL, SL, and prose specifications; AsciiDoc, LaTeX, and anchor determinism |
| `p4spectec/test-driver/tests/p4parse.rs` | P4 parse/unparse/parse corpus |
| `p4spectec/test-driver/tests/run.rs` | AL/SL/PL P4 execution corpus |
| `p4spectec/test-driver/tests/sim.rs` | AL/SL/PL packet simulation corpus |
| `p4spectec/test-driver/tests/diagnostic.rs` | Diagnostics of each pipeline stage on fixtures under `testdata/spec` |
| `p4spectec/cli/tests/diagnostic.rs` | Diagnostics of the `p4spectec` command line |

`make test-<alias>` runs a group of tests:

```
make test-speclang                  # speclang.rs
make test-diagnostic                # diagnostics of the pipeline and the command line
make test-p4parse                   # p4parse.rs
make test-run                       # run.rs; test-run-sl: its SL tests
make test-run-pos-sl                # one corpus of run.rs in one language
make test-sim                       # sim.rs; test-sim-al: its AL tests
make test-sim-v1model-p4c-sl        # one corpus of sim.rs in one language
```

Each alias is a [nextest filterset](https://nexte.st/docs/filtersets/) named in `p4spectec/Makefile`;
`make test FILTER=EXPR` runs any other filterset, for example `make test FILTER='package(p4spectec-cli)'`.
A test is named `<package>::<file> <function>`, as `cargo nextest list` shows,
for example `p4spectec-test::run pos_sl`.

Corpus tests run each case in a worker process,
started with the specification, P4 include directories, and runner options its test file names;
`make test TIMEOUT=SECONDS` (or `P4SPECTEC_TEST_TIMEOUT`) gives each case a timeout;
without it, cases run without one.
Each corpus test names the `excludes/` directories whose `.exclude` files list the cases it skips,
and compares the outcome of every case with `p4spectec/test-driver/expected/<file>/<suite>.snap`,
which the AL, SL, and PL tests of a suite share;
a case that times out keeps its recorded outcome.
`make test TEST_LOG=DIR` also writes `DIR/<test>.log` per corpus test, such as `DIR/run-pos-sl.log`,
as the cases end: each line is the case's snapshot line followed by its duration in seconds
(empty for an excluded case), so `sort -t, -k3 -n -r DIR/run-pos-sl.log | head` lists the slowest cases.
`make test PROFILE=ci` runs the CI selection of `p4spectec/.config/nextest.toml`.

Corpus, speclang, and diagnostic tests compare text with [insta](https://insta.rs) snapshots (`.snap` files);
a failure prints only the changed lines and leaves the new text in `<name>.snap.new`.
A corpus test also prints each changed case as `pass -> fail  <paths>` with the failure details;
`INSTA_OUTPUT=minimal make test` keeps those lines and drops insta's diff.
`INSTA_UPDATE=always make test` rewrites the snapshots instead;
review their diffs before committing.

`make clean` removes `bin/p4spectec` and the selected Cargo build directory.

CI runs Rust builds, formatting, Clippy, API documentation, integration tests, and
HTML specification generation for pull requests and pushes to `rust-port`
and `main`, version tags, and manual dispatches. Version tags and manual
runs also render the P4 release PDF specification.

## Retired OCaml workflows

Fuzzing, coverage collection, and meta-circular boot execution are not supported
by the Rust release. The frozen OCaml implementation is preserved at
[`v0.1.3`](https://github.com/kaist-plrg/p4-spectec/tree/v0.1.3), with its
[installation and fuzzing instructions](https://github.com/kaist-plrg/p4-spectec/blob/v0.1.3/README.md)
and [boot documentation](https://github.com/kaist-plrg/p4-spectec/blob/v0.1.3/BOOT.md).
The meta specifications and their SL/AL documents are also available in that
frozen version. Rust boot support is tracked in
[#77](https://github.com/jaehyun1ee/p4-spectec/issues/77).

## Contributing

P4-SpecTec is an open-source project. Please feel free to contribute by opening
issues or pull requests.

## License

P4-SpecTec is released under the [Apache 2.0 license](LICENSE).
