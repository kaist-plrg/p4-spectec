# P4-SpecTec

A mechanized formal specification for the P4 programming language, using the
SpecTec framework. The implementation is written in Rust. It reuses work from
[Petr4](https://github.com/verified-network-toolchain/petr4), especially the
parser and numerics implementation, and
[Wasm-SpecTec](https://github.com/Wasm-DSL/spectec), especially the specification
parser and the high-level architecture of the tool.

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

Docker images and a Nix development shell are currently not provided.

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

Both commands enable interpreter caching by default. Use `--no-cache` to
disable it or `--det` to check deterministic execution.

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
make fmt          # Format the product and E2E driver
make fmt-check    # Check formatting without rewriting files
make lint         # Clippy for the product and E2E driver
make rustdoc      # Build the Rust API documentation
make test         # Run all registered E2E suites
```

`make test` runs specification/document snapshots, P4 parsing, diagnostics,
and AL/SL/PL execution and simulation. Execution and simulation each run with
cache enabled and determinism checking both disabled and enabled.
The suite registrations live in `p4spectec/test-driver/suites.json`.

Individual targets are available for focused runs:

| Targets | Coverage |
| --- | --- |
| `test-elab`, `test-algo`, `test-structure`, `test-prose` | Specification processing |
| `test-adoc` | AsciiDoc snapshots and anchor determinism |
| `test-p4parse` | P4 parse/unparse/parse corpus |
| `test-diagnostics` | Source/expected diagnostics, including CLI and splicing errors |
| `test-run-al`, `test-run-sl`, `test-run-pl` | P4 execution corpus |
| `test-sim-al`, `test-sim-sl`, `test-sim-pl` | Packet simulation corpus |
| `test-expected` | All of the above except simulation |

Normal test targets clear `UPDATE_EXPECT` and compare against stored results.
To deliberately regenerate the elaboration, AL, prose, and AsciiDoc snapshots,
run `make promote` (also available as `make test-promote`). Diagnostics have a
separate `make test-diagnostics-promote` target. These commands rerun the suites;
review their diffs before committing. P4 parsing, structure, execution,
simulation, and exclusions are not promoted by these targets.

`make clean` removes `bin/p4spectec` and the selected Cargo build directory.

CI runs Rust builds, formatting, Clippy, API documentation, E2E acceptance, and
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
