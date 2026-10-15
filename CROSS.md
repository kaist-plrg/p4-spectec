# Cross-checking P4-SpecTec AL

`spec-meta-k/` and `spec-meta-redex/` specify P4-SpecTec AL in K and PLT Redex,
respectively, following the AL specification in `spec-meta/{common,al}`. Both
can execute standalone AL programs or use `spec/` to type-check P4 programs.
Comparing their results with the existing OCaml interpreter provides a
cross-check of AL evaluation across the three implementations.

## K

`spec-meta-k/` is a K implementation of the P4-SpecTec AL meta-language. It
mirrors the AL specification in `spec-meta/{common,al}` and provides an
independent way to execute the same AL programs.

The K implementation supports two main uses:

- running a single P4-SpecTec program that defines `$main()`, and
- running the P4 specification in `spec/` to type-check a P4 program.

This makes it possible to compare the existing OCaml interpreter with an
implementation based on K rewriting.

### K configuration

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

### Building and running

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

### OCaml support and KAST

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

[`ffi.ml`](p4spec/bin/ffi.ml) is an additional OCaml object target used by the
K interpreter and the Redex port. It initializes the SpecTec runner and
dispatches builtin and external calls made during evaluation.

### K–OCaml wire

K delegates builtins and external definitions to the OCaml implementation so
that both evaluators use the same host-side behavior. Calls cross the boundary
as JSON requests and responses:

```text
K rules -> JSON codec -> C shim -> ffi.ml -> SpecTec runner
```

The K side is implemented by
[`al/4.1-extern-json.k`](spec-meta-k/al/4.1-extern-json.k) and
[`al/4.2-extern-ffi.k`](spec-meta-k/al/4.2-extern-ffi.k). The thin C bridge is
[`ffi/shim.c`](spec-meta-k/ffi/shim.c), which starts the embedded OCaml runtime
and forwards requests to callbacks registered by `ffi.ml`. `make k-spec`
builds and links these pieces into the K interpreter.

## PLT Redex

`spec-meta-redex/` is a PLT Redex implementation of the P4-SpecTec AL
meta-language. It transcribes the AL specification in `spec-meta/{common,al}`
as a small-step reduction semantics and provides a third evaluator alongside
the OCaml interpreter and the K implementation.

The Redex implementation supports the same two main uses:

- running a single P4-SpecTec program that defines `$main()`, and
- running the P4 specification in `spec/` to type-check a P4 program.

### Redex reduction semantics

The machine language in
[`al/4-relation.rkt`](spec-meta-redex/al/4-relation.rkt) defines configurations
of the form `(G e)`. `G` is a handle to the loaded global layer, which remains
in a registry for the duration of a run. The term `(IN L e)` evaluates `e`
under the local layer `L`; the innermost `IN` supplies the local bindings at
each evaluation position.

A standalone run starts with:

```racket
(G (IN L (CALL "main" () ())))
```

Evaluation frames specify where the next subterm runs. Evaluating an
expression replaces it with `(OK val)` or `FAIL`; successful premises and
assignments update the local layer and reduce to `OK`. Callees, clause and
rule attempts, and iterations introduce nested `IN` scopes. A failed attempt
discards its scope, preserving the enclosing layer for the next alternative.
`FAIL` propagates through frames until a rule handles it or the run fails.

The evaluator in [`al/5-eval.rkt`](spec-meta-redex/al/5-eval.rkt) combines the
rule fragments into three relations:

| Relation | Purpose |
| --- | --- |
| `->term` | Reduce a term without reading or updating the context. |
| `->term+ctx` | Reduce `(term G L)`, reading the context or updating `L`. |
| `->al` | Apply these rules within evaluation contexts to step a whole configuration. |

Pure helpers use Redex metafunctions; partial helpers use judgment forms,
where a missing derivation can fail a premise. Programs run through
[`al/6-driver.rkt`](spec-meta-redex/al/6-driver.rkt), which computes the
reduction steps while keeping a cursor at the current subterm. The driver
avoids repeatedly decomposing the whole configuration; `->al` remains the
relation for inspecting and displaying the semantics.

### Building and running

Build the Redex implementation from the repository root:

```sh
make redex
```

This builds `spectec-boot`, the OCaml host and C bridge, and compiles the
Racket entry module with `raco make`. Rerun it after changing the OCaml or
Redex sources to keep the compiled modules and host libraries up to date.

Run a single P4-SpecTec program with `$main()`:

```sh
racket spec-meta-redex/al/7-entry.rkt examples/add.watsup
```

The script is converted to an s-expression, loaded, and evaluated through
`$main()`. Its return value is printed as JSON in the same format as
`k-run.sh`; this example prints `["intN","119"]`. A failed evaluation prints
`fail`. Debug messages go to stderr.

Type-check a P4 program against a P4-SpecTec specification:

```text
P4 program
  |> P4 specification in P4-SpecTec
  |> P4-SpecTec AL specification in PLT Redex
  |> Racket and PLT Redex
```

Each layer supplies the semantics for the preceding one: the P4 specification
checks the program, the Redex specification executes P4-SpecTec AL, and Racket
and Redex run the reduction rules.

```sh
racket spec-meta-redex/al/7-entry.rkt \
  --p4 p4c/testdata/p4_16_samples/action-bind.p4 \
  -i p4c/p4include \
  spec
```

The output is `passed` when the specification's `Program_ok` relation holds
and `fail` otherwise. With `--p4`, at least one `-i DIR` is required; repeat it
for additional include directories.

Run the integration test suite with:

```sh
make redex-test
```

The suite checks `p4c/testdata/p4_16_samples` as positive tests and
`p4c/testdata/p4_16_errors` as negative tests, skipping programs named by
`excludes/static`. Results are written to
`spec-meta-redex/p4-typecheck-pos.result` and
`spec-meta-redex/p4-typecheck-neg.result`.

The entry point is [`al/7-entry.rkt`](spec-meta-redex/al/7-entry.rkt) for both
AL and P4 programs. [`test/p4-typecheck.rkt`](spec-meta-redex/test/p4-typecheck.rkt)
loads the specification once and reuses it across the P4 programs in a suite.

### OCaml support and s-expressions

The OCaml implementation provides two `spectec-boot` subcommands for preparing
Redex inputs:

| Command | Purpose |
| --- | --- |
| `sexp PATH [-o FILE]` | Boot a P4-SpecTec program or specification and emit a `script` term. |
| `sexp-p4 -p FILE [-i DIR]... [-o FILE]` | Parse a P4 program and emit a `val` term. |

For example:

```sh
./spectec-boot sexp examples/add.watsup -o add.sexp
./spectec-boot sexp-p4 \
  -p p4c/testdata/p4_16_samples/action-bind.p4 \
  -i p4c/p4include \
  -o program.sexp
```

These commands emit one Racket-readable s-expression. Constructors keep their
AL names, sequence fields become lists, and optional fields become lists of
length zero or one. Identifiers, atoms, and text are strings; numbers are
exact integers, and booleans are `#t` or `#f`. For example:

```racket
(CALL "fibo" () ((EXP (VAR "n"))))
(OPT ())
(OPT ((NAT 3)))
```

The emitter is implemented in
[`sexp.ml`](p4spec/lib/interface/spectec/ali/sexp.ml).
[`al/0-boot.rkt`](spec-meta-redex/al/0-boot.rkt) runs these commands and reads
their output with Racket's `read`. It also decodes the JSON text in P4
`(EXT json)` values into Racket JSON values.

### Redex–OCaml wire

Redex delegates object-level builtins and external definitions to the OCaml
implementation over the same JSON wire as K:

```text
Redex rules -> JSON codec -> Racket ffi2 -> C shim -> ffi.ml -> SpecTec runner
```

The Redex side is implemented by
[`common/0.1-extern-json.rkt`](spec-meta-redex/common/0.1-extern-json.rkt) and
[`common/0.2-extern-ffi.rkt`](spec-meta-redex/common/0.2-extern-ffi.rkt). The C
bridge is [`ffi/shim.c`](spec-meta-redex/ffi/shim.c), which forwards requests
to callbacks registered by the shared [`ffi.ml`](p4spec/bin/ffi.ml).

`make redex` builds `_build/default/p4spec/bin/ffi.so`, containing the OCaml
runtime and host implementation, and `spec-meta-redex/ffi/shim.so`, which
Racket loads through `ffi2`. The command-line entry point initializes the
host with the script or specification path before evaluation. The OCaml
runtime remains in the Racket process across calls.

A host failure reply becomes `FAIL`; a host error reply raises a Racket
error. Redex delegates all object-level builtins, including map operations,
to this host. Meta-level map helpers used by the evaluator operate on Redex's
own environments in [`common/0.0-stdlib.rkt`](spec-meta-redex/common/0.0-stdlib.rkt).

### Exploring in a REPL

Start `racket` from the repository root. This creates a small configuration
and shows its next step, with the name of the rule applied:

```racket
(require redex/reduction-semantics
         "spec-meta-redex/al/3-context.rkt"
         "spec-meta-redex/al/5-eval.rkt")

(define G (register-global! (term (empty-layer))))
(define conf (term (,G (IN (empty-layer) (UN NOT (BOOL #t))))))
(apply-reduction-relation/tag-with-names ->al conf)
```

To explore this run in Redex's graphical stepper:

```racket
(require redex/gui)
(traces ->al conf)
```

Use `traces` for small runs, since it retains the reduction graph. A run that
calls builtins or externs also needs `host-init!` from
[`common/0.2-extern-ffi.rkt`](spec-meta-redex/common/0.2-extern-ffi.rkt), with
the script or specification path. `load-script` from
[`al/7-entry.rkt`](spec-meta-redex/al/7-entry.rkt) loads a booted script and
returns `(G L)`, registering its global layer for use in a configuration.

To render a named reduction rule as a PNG figure:

```racket
(require racket/class pict redex/pict)
(parameterize ([render-reduction-relation-rules '("eval-exp/unary/boolean")])
  (send (pict->bitmap (reduction-relation->pict ->term))
        save-file "/tmp/redex-rules.png" 'png))
```
