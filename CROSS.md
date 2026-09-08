# Cross-checking P4-SpecTec AL against K

`spec-meta-k/` is a K implementation of the P4-SpecTec AL meta-language. It
mirrors the AL specification in `spec-meta/{common,al}` and provides an
independent way to execute the same AL programs.

The K implementation supports two main uses:

- running a single P4-SpecTec program that defines `$main()`, and
- running the P4 specification in `spec/` to type-check a P4 program.

This makes it possible to compare the existing OCaml interpreter with an
implementation based on K rewriting.

## K configuration

The evaluator is organized around the configuration in
[`al/0-config.k`](spec-meta-k/al/0-config.k):

```k
configuration
  <al>
    <k> initFFI() ~> logDebug(textV("entry-al")) ~> $PGM:Script ~> afterLoad() </k>
    <p4prog> $P4:P4Opt </p4prog>
    <specdir> $SPEC:String </specdir>
    <global>
      <gtdenv> .Map </gtdenv> <grenv> .Map </grenv> <gfenv> .Map </gfenv>
    </global>
    <local>
      <ltdenv> .Map </ltdenv> <lfenv> .Map </lfenv> <lvenv> .Map </lvenv>
    </local>
    <caller> <cfenv> .Map </cfenv> </caller>
    <saves> .List </saves>
    <callstack> .List </callstack>
  </al>
```

`<k>` drives evaluation, while `<p4prog>` distinguishes a standalone AL run
from a P4 type-checking run. The remaining cells store the specification path
and the evaluator's environments and call state.

## Building and running

Build the K definition from the repository root:

```sh
make k-spec
```

Run a single P4-SpecTec program with `$main()`:

```sh
./spec-meta-k/scripts/k-run.sh examples/add.watsup
```

The script is converted to KAST, loaded by K, and evaluated through `$main()`.
Its return value is printed as JSON.

Type-check a P4 program against a P4-SpecTec specification:

```text
P4 program
  |> P4 specification in P4-SpecTec
  |> P4-SpecTec AL specification in K
  |> K framework
```

Each layer supplies the semantics for the preceding one: the P4 specification
checks the program, the K specification executes P4-SpecTec AL, and K performs
the rewriting.

```sh
./spec-meta-k/scripts/k-run-p4.sh \
  spec \
  p4c/testdata/p4_16_samples/action-bind.p4
```

The final output line is `passed` when the specification's `Program_ok`
relation holds and `fail` otherwise.

Run the integration test suite with:

```sh
make k-test
```

The shell entry points are
[`k-run.sh`](spec-meta-k/scripts/k-run.sh) for AL programs and
[`k-run-p4.sh`](spec-meta-k/scripts/k-run-p4.sh) for P4 programs. Their parser
wrappers convert source inputs to the terms expected by `krun`.

## OCaml support and KAST

The OCaml implementation provides two `spectec-boot` subcommands for preparing
K inputs:

| Command | Purpose |
| --- | --- |
| `kast PATH [-o FILE]` | Boot a P4-SpecTec program or specification and emit a `Script` term. |
| `kast-p4 -p FILE [-i DIR]... [-o FILE]` | Parse a P4 program and emit it as the `<p4prog>` input. |

For example:

```sh
./spectec-boot kast examples/add.watsup -o add.json
./spectec-boot kast-p4 \
  -p p4c/testdata/p4_16_samples/action-bind.p4 \
  -i p4c/p4include \
  -o program.json
```

These commands emit KAST JSON version 4. Each document has a `format`, a
`version`, and a `term`; labels and sorts use their structured version 4 forms.
A minimal token has this shape:

```json
{
  "format": "KAST",
  "version": 4,
  "term": {
    "node": "KToken",
    "sort": {"node": "KSort", "name": "String", "params": []},
    "token": "\"main\""
  }
}
```

Complete AL and P4 inputs are trees of constructor applications and tokens in
the same envelope. The emitter is implemented in
[`kast.ml`](p4spec/lib/interface/spectec/ali/kast.ml).

[`kffi.ml`](p4spec/bin/kffi.ml) is an additional OCaml object target used by the
K interpreter. It initializes the SpecTec runner and dispatches builtin and
external calls made during K evaluation.

## K–OCaml wire

K delegates builtins and external definitions to the OCaml implementation so
that both evaluators use the same host-side behavior. Calls cross the boundary
as JSON requests and responses:

```text
K rules -> JSON codec -> C shim -> kffi.ml -> SpecTec runner
```

The K side is implemented by
[`al/4.1-extern-json.k`](spec-meta-k/al/4.1-extern-json.k) and
[`al/4.2-extern-ffi.k`](spec-meta-k/al/4.2-extern-ffi.k). The thin C bridge is
[`ffi/shim.c`](spec-meta-k/ffi/shim.c), which starts the embedded OCaml runtime
and forwards requests to callbacks registered by `kffi.ml`. `make k-spec`
builds and links these pieces into the K interpreter.
