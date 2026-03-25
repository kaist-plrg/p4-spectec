# P4-SpecTec

A mechanized formal specification for the P4 programming language, using the
SpecTec framework. This reuses parts of the
[Petr4](https://github.com/verified-network-toolchain/petr4) codebase,
especially the parser and numerics implementation. This also reuses parts of
the [Wasm-SpecTec](https://github.com/Wasm-DSL/spectec) codebase, especially
the specification parser and the high-level architecture of the tool.

## Table of Contents

- [Building the Project](#building-the-project)
  - [Building from Source](#building-from-source)
    - [Submodule(s)](#submodules)
    - [Prerequisites (Linux)](#prerequisites-linux)
    - [Prerequisites (MacOS)](#prerequisites-macos)
    - [Prerequisites (Windows)](#prerequisites-windows)
    - [OCaml compiler and packages](#ocaml-compiler-and-packages)
    - [Compiling the Project](#compiling-the-project)
    - [Additional Notes](#additional-notes)
  - [Docker builds](#docker-builds)
- [P4-SpecTec: A language specification framework for P4](#p4-spectec-a-language-specification-framework-for-p4)
  - [Processing the specification](#processing-the-specification)
  - [Generating the specification document](#generating-the-specification-document)
  - [Running the specification](#running-the-specification)
  - [Running the specification against packet inputs](#running-the-specification-against-packet-inputs)
  - [To initiate a fuzz loop generating (intentionally) ill-typed P4 programs](#to-initiate-a-fuzz-loop-generating-intentionally-ill-typed-p4-programs)
- [Contributing](#contributing)
- [License](#license)

## Building the Project

### Building from Source

#### Submodule(s)

* `p4c` is a submodule of this project, as we reuse the tests and the P4 include files from `p4c`.
  You can initialize it by running:
  ```shell
  $ git submodule update --init
  ```

#### Prerequisites (Linux)

* Install `opam` version 2.0.5 or higher.
  ```shell
  $ apt-get install opam
  $ opam init
  ```

* `creduce` is used in the test generation beckend to reduce the generated test cases.
  You can install it by running:
  ```shell
  $ apt-get install creduce
  ```
  <!-- Apply the patch in `creduce-patches/creduce` to adapt it to our use case. -->
  Note that `creduce` is only necessary for the test generation backend, so you
  can skip this step if you only want to use the specification and the
  simulation features.

* `asciidoctor` is used to generate HTML/PDF document from the AsciiDoc source files.
  Use `docs/install-asciidoctor-linux.sh` to install it on Linux.

#### Prerequisites (MacOS)

* Install `opam` version 2.0.5 or higher following the instructions [here](https://ocaml.org/docs/installing-ocaml).

* Install `creduce` using [Homebrew](https://formulae.brew.sh/formula/creduce).
  <!-- Apply the patch in `creduce-patches/creduce` to adapt it to our use case. -->
  Note that `creduce` is only necessary for the test generation backend, so you
  can skip this step if you only want to use the specification and the
  simulation features.

* Install `asciidoctor` following the instructions [here](https://docs.asciidoctor.org/asciidoctor/latest/install/macos/).

#### Prerequisites (Windows)

* For now, we do *not* have instructions for building on Windows.
  We recommend using WSL2 or using [Docker](#docker-builds) to build and run P4-SpecTec on Windows.

#### OCaml compiler and packages

* Create OCaml switch for version 5.1.0
  Install `dune` version 3.16.1, `bignum` version v0.17.0, `menhir` version 20240715, `core` version v0.17.1, `core_unix` version v0.17.0, and `bisect_ppx` version 2.8.3 via `opam`.
  ```shell
  $ opam switch create 5.1.0
  $ eval $(opam env)
  $ opam install dune bignum 'menhir=20240715' 'menhirLib=20240715' core core_unix bisect_ppx yojson ppx_deriving_yojson
  ```

#### Compiling the Project

```shell
$ make build
```

This creates an executable `p4spectec` in the project root.

#### Additional Notes

You may also need `libgmp-dev` and `pkg-config`, depending on your system.

### Docker builds

We provide two dockerfiles, `p4spectec.dockerfile` for P4-SpecTec and
`p4spectec_p4c.dockerfile` for both P4-SpecTec and p4c. The former is useful
for users who only want to use P4-SpecTec, while the latter is useful for users
who also want to use p4c tools.

```shell
# without p4c
$ docker build -f p4spectec.dockerfile -t p4spectec:latest .

# with p4c, for RQ3-b branch coverage measurement
$ docker build -f p4spectec_p4c.dockerfile -t p4spectec_with_p4c:latest .
```

## P4-SpecTec: A language specification framework for P4

The spec source files are located in the `spec` directory.

### Processing the specification

The specification is processed in multiple stages: parsing, elaboration, structuring, and prose generation.

* Parsing: The input spec files are parsed.
  At this stage, the spec is called EL (external language).
* Elaboration: The parsed spec files are type checked, and auxiliary information is annotated on the spec.
  At this stage, the spec is called IL (internal language).
  ```shell
  $ ./p4spectec elab spec/*/*.watsup
  ```
* Structuring: The elaborated spec is *structured*, where structured control flow is introduced.
  At this stage, the spec is called SL (structured language).
  ```shell
  $ ./p4spectec struct spec/*/*.watsup
  ```
* Prose generation: The structured spec is converted to a human-readable format, in AsciiDoc format.
  At this stage, the spec is called PL (prose language).
  ```shell
  $ ./p4spectec prose spec/*/*.watsup
  ```

### Generating the specification document

From the project root, run the following command to generate the specification document in HTML and PDF format.

```shell
# Only HTML
make spec-release-html
# Both HTML and PDF (takes more time)
make spec-release
```

The generated documents can be found in the `docs` directory.

### Running the specification

Given a P4 program, below command runs a particular relation on it.
`RELNAME` may be `Program_ok` (for typing) or `Program_inst` (for instantiation).

```shell
# To run the IL
$ ./p4spectec run spec/*/*.watsup -rel [RELNAME] -i p4c/p4include -p [FILENAME].p4 -il
# To run the SL
$ ./p4spectec run spec/*/*.watsup -rel [RELNAME] -i p4c/p4include -p [FILENAME].p4 -sl
```

e.g., `$ ./p4spectec run spec/*/*.watsup -rel Program_ok -i
p4c/p4include -p p4c/testdata/p4_16_samples/basic_routing-bmv2.p4 -sl` type
checks the `basic_routing-bmv2.p4` program using the relation `Program_ok`
specified in the spec.

### Running the specification against packet inputs

We currently support the V1Model and eBPF architectures, and the STF format for specifying input/output packets.
`ARCH` may be `v1model` or `ebpf`.

```shell
# To run the IL
$ ./p4spectec sim spec/*/*.watsup -arch [ARCH] -i p4c/p4include -p [FILENAME].p4 -stf [STF_FILENAME].stf -il
# To run the SL
$ ./p4spectec sim spec/*/*.watsup -arch [ARCH] -i p4c/p4include -p [FILENAME].p4 -stf [STF_FILENAME].stf -sl
```

e.g., `$ ./p4spectec sim spec/*/*.watsup -arch v1model -i
p4c/p4include -p p4c/testdata/p4_16_samples/basic_routing-bmv2.p4 -stf
testdata/p4testgen/basic_routing-bmv2/basic_routing-bmv2_1.stf -sl` runs the
`basic_routing-bmv2.p4` program on the input packet specified in
`basic_routing-bmv2_1.stf`, and checks if the output packet matches the
expected output specified in the same STF file.

### To initiate a fuzz loop generating (intentionally) ill-typed P4 programs

P4-SpecTec also supports fuzzing negative type checker tests for P4 type checkers.
i.e., it can generate various ill-typed P4 programs that should be rejected by the type checker.

```shell
$ mkdir [GEN_DIR]
$ ./p4spectec testgen spec/*/*.watsup -rel Program_ok -i p4c/p4include -gen [GEN_DIR] -fuel [NUM] -boot-dir [BOOT_DIR]
```

This will generate P4 programs in the directory `[GEN_DIR]` using the seed
files in the directory `[BOOT_DIR]`. `[NUM]` is the number of fuzz cycles to
run. For instance, you may set `[BOOT_DIR]` to `p4c/testdata/p4_16_samples`,
and `[NUM]` to `10` to run 10 fuzzing iterations starting from the sample P4
programs in `p4c/testdata/p4_16_samples`.

After the fuzz loop, you may find the generated P4 programs in the directory `[GEN_DIR]`, with the log file `fuzz.log`,
query files for mutations `query.log` and an initial coverage file `boot.coverage`.

In later runs with the same boot directory, you can use warm boot to skip the initial coverage collection phase, and directly start with the fuzz loop.

```shell
$ ./p4spectec testgen spec/*/*.watsup -rel Program_ok -i p4c/p4include -gen [GEN_DIR] -fuel [NUM] -boot-file [BOOT_FILE].coverage
```

## Contributing

P4-SpecTec is an open-source project. Please feel free to contribute by opening issues or pull requests.

## License

P4-SpecTec is released under the [Apache 2.0 license](LICENSE).
