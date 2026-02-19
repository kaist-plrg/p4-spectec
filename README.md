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
    - [Prerequisites](#prerequisites)
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

#### Prerequisites

* Install `opam` version 2.0.5 or higher.
  ```shell
  $ apt-get install opam
  $ opam init
  ```

* Create OCaml switch for version 5.1.0
  Install `dune` version 3.16.1, `bignum` version v0.17.0, `menhir` version 20240715, `core` version v0.17.1, `core_unix` version v0.17.0, and `bisect_ppx` version 2.8.3 via `opam`.
  ```shell
  $ opam switch create 5.1.0
  $ eval $(opam env)
  $ opam install dune bignum 'menhir=20240715' 'menhirLib=20240715' core core_unix bisect_ppx yojson ppx_deriving_yojson
  ```

* `p4c` is a submodule of this project, as we reuse the tests and the P4 include files from `p4c`.
  You can initialize it by running:
  ```shell
  $ git submodule update --init
  ```

* `creduce` is used in the test generation beckend to reduce the generated test cases.
  You can install it by running:
  ```shell
  $ apt-get install creduce
  ```
  Apply the patch in `creduce-patches/creduce` to adapt it to our use case.

* `asciidoctor` is used to generate HTML/PDF document from the AsciiDoc source files.
  Use `docs/install-asciidoctor-linux.sh` to install it on Linux.

#### Compiling the Project

```shell
$ make build-spec
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

The spec source files are located in the `spec-concrete` directory.

### Processing the specification

The specification is processed in multiple stages: parsing, elaboration, structuring, and prose generation.

* Parsing: The input spec files are parsed.
  At this stage, the spec is called EL (external language).
* Elaboration: The parsed spec files are type checked, and auxiliary information is annotated on the spec.
  At this stage, the spec is called IL (internal language).
  ```shell
  $ ./p4spectec elab spec-concrete/*/*.watsup
  ```
* Structuring: The elaborated spec is *structured*, where structured control flow is introduced.
  At this stage, the spec is called SL (structured language).
  ```shell
  $ ./p4spectec struct spec-concrete/*/*.watsup
  ```
* Prose generation: The structured spec is converted to a human-readable format, in AsciiDoc format.
  At this stage, the spec is called PL (prose language).
  ```shell
  $ ./p4spectec prose spec-concrete/*/*.watsup
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
$ ./p4spectec run spec-concrete/*/*.watsup -rel [RELNAME] -i p4c/p4include -p [FILENAME].p4 -il
# To run the SL
$ ./p4spectec run spec-concrete/*/*.watsup -rel [RELNAME] -i p4c/p4include -p [FILENAME].p4 -sl
```

### Running the specification against packet inputs

We currently support the V1Model and eBPF architectures, and the STF format for specifying input/output packets.
`ARCH` may be `v1model` or `ebpf`.

```shell
# To run the IL
$ ./p4spectec sim spec-concrete/*/*.watsup -arch v1model -i p4c/p4include -p [FILENAME].p4 -stf [STF_FILENAME].stf -il
# To run the SL
$ ./p4spectec sim spec-concrete/*/*.watsup -arch v1model -i p4c/p4include -p [FILENAME].p4 -stf [STF_FILENAME].stf -sl
```

### To initiate a fuzz loop generating (intentionally) ill-typed P4 programs

P4-SpecTec also supports fuzzing negative type checker tests for P4 type checkers.
i.e., it can generate various ill-typed P4 programs that should be rejected by the type checker.

```shell
$ ./p4spectec testgen spec-concrete/*/*.watsup -rel Program_ok -i p4c/p4include -gen [GEN_DIR] -fuel [NUM] -boot-dir [BOOT_DIR]
```

This will generate P4 programs in the directory `[GEN_DIR]` using the seed files in the directory `[BOOT_DIR]`.
`[NUM]` is the number of fuzz cycles to run.

After the fuzz loop, you may find the generated P4 programs in the directory `[GEN_DIR]`, with the log file `fuzz.log`,
query files for mutations `query.log` and an initial coverage file `boot.coverage`.

In later runs with the same boot directory, you can use warm boot to skip the initial coverage collection phase, and directly start with the fuzz loop.

```shell
$ ./p4spectec testgen spec-concrete/*/*.watsup -rel Program_ok -i p4c/p4include -gen [GEN_DIR] -fuel [NUM] -boot-file [BOOT_FILE].coverage
```

## Contributing

P4-SpecTec is an open-source project. Please feel free to contribute by opening issues or pull requests.

## License

P4-SpecTec is released under the [Apache 2.0 license](LICENSE).
