# Cross-checking P4-SpecTec AL

`spec-meta-k/` and `spec-meta-redex/` specify P4-SpecTec AL in K and PLT Redex,
respectively, following the AL specification in `spec-meta/`. Both can execute
standalone AL programs or use `spec/` to type-check P4 programs. Comparing
their results with the Rust implementation in `p4spectec/` provides a
cross-check of AL evaluation across the three implementations.

The Rust implementation takes part twice. `bin/metacross` converts P4-SpecTec
sources and P4 programs into the terms the two specifications read, and
`bin/libp4spectec.so` is the host library both call back into for builtins
and external definitions while they evaluate. From the repository root:

```sh
make metacross   # bin/metacross, from p4spectec/src/bin/metacross.rs
make k           # the K definition, into al-kompiled/
make redex       # the Redex modules, with raco make
```

`make k` and `make redex` are local targets: CI builds and tests the Rust
implementation only, since K and Racket are not installed there.

`spec-meta/`, `spec-meta-k/`, and `spec-meta-redex/` are each one flat
directory, and a K or Redex file is numbered after the `spec-meta/` file it
mirrors (K keeps its own `5.x` numbers, since argument evaluation lives in its
`5.3-eval-exp.k`). The frozen syntax modules predate the signature on `FUNC`
types: `metacross` emits a `FUNC` type without its signature, as `funcT()`
and `FUNC`.

## K

`spec-meta-k/` is a K implementation of the P4-SpecTec AL meta-language. It
mirrors the AL specification in `spec-meta/` and provides an independent way to
execute the same AL programs.

The K implementation supports two main uses:

- running a single P4-SpecTec program that defines `$main()`, and
- running the P4 specification in `spec/` to type-check a P4 program.

This makes it possible to compare the Rust interpreter with an implementation
based on K rewriting.

### K configuration

The evaluator is organized around the configuration in
[`0.1-config.k`](spec-meta-k/0.1-config.k):

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
    <callstack> .List </callstack>
  </al>
```

`<k>` drives evaluation, while `<p4prog>` distinguishes a standalone AL run
from a P4 type-checking run. The remaining cells store the specification path
and the evaluator's environments and call state. `initFFI()` starts the host
library on the specification path before anything else runs.

### Evaluation, failure, and scoping

Expressions and premises evaluate in place. The productions of
[`1-syntax.k`](spec-meta-k/1-syntax.k) carry `strict`/`seqstrict`
attributes, a value is an expression (`Exp ::= Val`), and an expression list,
field, or case whose parts are all values is the value list, field, or case
(`overload`), so the semantic rules match on value constructors:
`binE(O, Nl:Num, Nr:Num)`, `tupE(Vs:ValList) => tupV(Vs)`. A premise that
holds rewrites to `.K`, an assignment adds its bindings to `<local>`, and a
premise list runs as a sequence of statements.

watsup's `res<X> = OK X | FAIL` is the outcome of one alternative: a clause, a
rule path, a table row, a cast, or the relation under `IFNOTHOLD`. A rule with
no applicable case rewrites to `backtrack`, which discards the pending work of
the current alternative up to the boundary that started it
([`3-context.k`](spec-meta-k/3-context.k)):

- `try(K)` runs `K` under a `checkpoint` of the local bindings. It yields
  `ok(R)` or `FAIL` and restores the bindings either way. A list of tries
  evaluates in order to a list of outcomes; `det` picks the single success
  among a function's clauses or a relation's rule paths (two successes fail,
  none runs the `otherwise` clause or else group), and `firstOf` takes the
  first success among a table's rows.
- `inScope(K)` ends with `endScope`, which restores `<lvenv>` after the body
  of an iteration or of a list assignment, whether `K` succeeded or
  backtracked.
- `enterFrame(tdenv)` pushes the local layer and `<cfenv>` onto `<callstack>`
  for a call, and `endFrame` pops them on the result or on backtracking, so
  `<callstack>` is empty whenever `<k>` is.

### Building and running

K is pinned by the `k` submodule; this port is tested with K v7.1.337, which
is also available from nix. Build the K definition from the repository root:

```sh
make k
```

This builds `bin/metacross` and `bin/libp4spectec.so`, then kompiles
[`6-entry.k`](spec-meta-k/6-entry.k) into `al-kompiled/`. The
interpreter is linked against the host library with an `$ORIGIN`-relative
rpath, so it finds `bin/libp4spectec.so` from `al-kompiled/` wherever the
Rust target directory is. Rerun `make k` after changing the K or Rust sources.

Run a single P4-SpecTec program with `$main()`:

```sh
./spec-meta-k/scripts/k-run.sh examples/add.watsup
```

The script is converted to KAST, loaded by K, and evaluated through `$main()`.
Its return value is printed as JSON; this example prints `["intN","119"]`.

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
relation holds and `fail` otherwise. The KAST of `spec` is cached in
`spec-meta-k/.kore-cache`, keyed by the sources and the `metacross` binary,
so only the first run pays for its conversion.

Run the integration test suite with:

```sh
make test-k
```

The suite checks `p4c/testdata/p4_16_samples` as positive tests and
`p4c/testdata/p4_16_errors` as negative tests, skipping programs named by
`excludes/static`. Results are written to
`spec-meta-k/run-k-typecheck-pos.result` and
`spec-meta-k/run-k-typecheck-neg.result`.

The shell entry points are
[`k-run.sh`](spec-meta-k/scripts/k-run.sh) for AL programs and
[`k-run-p4.sh`](spec-meta-k/scripts/k-run-p4.sh) for P4 programs. Their parser
wrappers, [`kast-spec.sh`](spec-meta-k/scripts/kast-spec.sh) and
[`kast-p4.sh`](spec-meta-k/scripts/kast-p4.sh), run `bin/metacross` to convert
source inputs to the terms expected by `krun`.
[`run-k-typecheck.py`](spec-meta-k/scripts/run-k-typecheck.py) is the test
harness.

### `metacross` and KAST

`bin/metacross` prepares K inputs with two subcommands:

| Command | Purpose |
| --- | --- |
| `kast PATH [-o FILE]` | Lower a P4-SpecTec program or specification to AL and emit a `Script` term. |
| `kast-p4 -p FILE [-i DIR]... [-o FILE]` | Parse a P4 program and emit it as the `<p4prog>` input. |

For example:

```sh
./bin/metacross kast examples/add.watsup -o add.json
./bin/metacross kast-p4 \
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
the same envelope. The emitter is
[`kast.rs`](p4spectec/src/metacross/kast.rs). Both emitters walk the AL
syntax of `p4spectec::lang::al::ast` and runtime values directly, as the
OCaml `kast.ml` and `sexp.ml` did; `metacross` depends on neither the
metacircular interpreter nor its codec.

### K–Rust wire

K delegates builtins and external definitions to the Rust implementation so
that both evaluators use the same host-side behavior. Calls cross the boundary
as JSON requests and responses:

```text
K rules -> JSON codec -> #ffiCall -> bin/libp4spectec.so -> p4spectec runner
```

The K side is implemented by
[`4.1-extern-json.k`](spec-meta-k/4.1-extern-json.k), the codec, and
[`4.2-extern-ffi.k`](spec-meta-k/4.2-extern-ffi.k), the transport, which
resolves the library's `host_init`, `host_eval`, and `host_strlen` symbols with
`#functionAddress` and calls them with `#ffiCall`. The Rust side is
[`ffi.rs`](p4spectec/src/metacross/ffi.rs), the `extern "C"` surface and the
only module of the crate that uses `unsafe`;
[`json.rs`](p4spectec/src/metacross/json.rs), the codec; and
[`host.rs`](p4spectec/src/metacross/host.rs), which owns the runner built on
`host_init` and dispatches each request. A builtin request goes to the
interface's builtin table directly, so a builtin the script does not declare
still resolves; an extern function or relation request runs through the loaded
specification, and a derivation that does not exist is the `fail` reply.

The reply of `host_eval` is a string the library owns, valid until the next
`host_*` call; nothing is freed by the caller. The library keeps one runner
for the whole process and never resets it between requests, so stateful
builtins such as `fresh_typeId` behave as they do in a native run. All calls
must come from the thread that called `host_init`.

The wire itself, as the codec documents it:

```text
val      ::= ["boolV", bool] | ["natN", "<decimal>"] | ["intN", "<decimal>"]
           | ["textV", text] | ["strV", [[atom, val]...]]
           | ["injV", mixop, [val...]] | ["tupV", [val...]]
           | ["optV", null | val] | ["listV", [val...]]
           | ["funcV", id] | ["extV", json]
mixop    ::= [[atom...]...]                  atoms between argument positions
typ      ::= ["boolT"] | ["natT"] | ["intT"] | ["textT"]
           | ["varT", id, [typ...]] | ["tupT", [typ...]]
           | ["iterT", typ, "?" | "*"] | ["funcT"]
request  ::= {"builtin": id, "targs": [typ...], "args": [val...]}
           | {"extern-func": id, "targs": [typ...], "args": [val...]}
           | {"extern-rel": id, "args": [val...]}
response ::= {"ok": val} | {"ok": [val...]} | {"fail": null} | {"error": text}
```

## PLT Redex

`spec-meta-redex/` is a PLT Redex implementation of the P4-SpecTec AL
meta-language. It transcribes the AL specification in `spec-meta/` as a
small-step reduction semantics and provides a third evaluator alongside the
Rust interpreter and the K implementation.

The Redex implementation supports the same two main uses:

- running a single P4-SpecTec program that defines `$main()`, and
- running the P4 specification in `spec/` to type-check a P4 program.

### Redex reduction semantics

The machine language in
[`4-relation.rkt`](spec-meta-redex/4-relation.rkt) defines configurations
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

The evaluator in [`5.8-eval.rkt`](spec-meta-redex/5.8-eval.rkt) combines the
rule fragments into three relations:

| Relation | Purpose |
| --- | --- |
| `->term` | Reduce a term without reading or updating the context. |
| `->term+ctx` | Reduce `(term G L)`, reading the context or updating `L`. |
| `->al` | Apply these rules within evaluation contexts to step a whole configuration. |

Pure helpers use Redex metafunctions; partial helpers use judgment forms,
where a missing derivation can fail a premise. Programs run through
[`6-driver.rkt`](spec-meta-redex/6-driver.rkt), which computes the
reduction steps while keeping a cursor at the current subterm. The driver
avoids repeatedly decomposing the whole configuration; `->al` remains the
relation for inspecting and displaying the semantics.

### Prerequisites

Install [Racket 9.3](https://download.racket-lang.org/). Racket's
[`ffi2`](https://docs.racket-lang.org/ffi2/index.html?lang=ar&fam=Zuo) library
is also needed to reach the host library. Install it with:

```sh
raco pkg install ffi2-lib
```

### Building and running

Build the Redex implementation from the repository root:

```sh
make redex
```

This builds `bin/metacross` and `bin/libp4spectec.so`, then compiles the
Racket entry module with `raco make`. Rerun it after changing the Redex
or Rust sources to keep the compiled modules and the library up to date.

Run a single P4-SpecTec program with `$main()`:

```sh
racket spec-meta-redex/7-entry.rkt examples/add.watsup
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
racket spec-meta-redex/7-entry.rkt \
  --p4 p4c/testdata/p4_16_samples/action-bind.p4 \
  -i p4c/p4include \
  spec
```

The output is `passed` when the specification's `Program_ok` relation holds
and `fail` otherwise. With `--p4`, at least one `-i DIR` is required; repeat it
for additional include directories.

Run the integration test suite with:

```sh
make test-redex
```

The suite checks `p4c/testdata/p4_16_samples` as positive tests and
`p4c/testdata/p4_16_errors` as negative tests, skipping programs named by
`excludes/static`. Results are written to
`spec-meta-redex/p4-typecheck-pos.result` and
`spec-meta-redex/p4-typecheck-neg.result`.

The entry point is [`7-entry.rkt`](spec-meta-redex/7-entry.rkt) for both
AL and P4 programs. [`test/p4-typecheck.rkt`](spec-meta-redex/test/p4-typecheck.rkt)
loads the specification once and reuses it across the P4 programs in a suite.

### `metacross` and s-expressions

`bin/metacross` prepares Redex inputs with two subcommands:

| Command | Purpose |
| --- | --- |
| `sexp PATH [-o FILE]` | Lower a P4-SpecTec program or specification to AL and emit a `script` term. |
| `sexp-p4 -p FILE [-i DIR]... [-o FILE]` | Parse a P4 program and emit a `val` term. |

For example:

```sh
./bin/metacross sexp examples/add.watsup -o add.sexp
./bin/metacross sexp-p4 \
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

The emitter is [`sexp.rs`](p4spectec/src/metacross/sexp.rs), a printer of the
same meta values `kast.rs` walks.
[`0.3-boot.rkt`](spec-meta-redex/0.3-boot.rkt) runs these commands and reads
their output with Racket's `read`. It also decodes the JSON text in P4
`(EXT json)` values into Racket JSON values.

### Redex–Rust wire

Redex delegates object-level builtins and external definitions to the Rust
implementation over the same JSON wire as K:

```text
Redex rules -> JSON codec -> Racket ffi2 -> bin/libp4spectec.so -> p4spectec runner
```

The Redex side is implemented by
[`0.1-extern-json.rkt`](spec-meta-redex/0.1-extern-json.rkt), the
codec, and [`0.2-extern-ffi.rkt`](spec-meta-redex/0.2-extern-ffi.rkt),
which loads `bin/libp4spectec.so` with `ffi2` and binds `host_init` and
`host_eval`. The Rust side is the one K uses, and the reply ownership rule of
the K–Rust wire applies: the command-line entry point initializes the host
with the script or specification path before evaluation, and the host runner
stays in the Racket process across calls.

A host failure reply becomes `FAIL`; a host error reply raises a Racket
error. Redex delegates all object-level builtins, including map operations,
to this host. Meta-level map helpers used by the evaluator operate on Redex's
own environments in [`0.0-stdlib.rkt`](spec-meta-redex/0.0-stdlib.rkt).

## Measurements

Type-checking three P4 programs, with `spec` already converted and the
native `bin/p4spectec run` as the reference. Wall-clock time and peak
resident memory of the whole command, on one WSL2 machine:

| Program | Native | K | Redex |
| --- | --- | --- | --- |
| `p4_16_samples/action-bind.p4` | passed, 0.8 s | passed, 7.5 s, 280 MB | passed, 12.5 s, 313 MB |
| `p4_16_samples/action-synth.p4` | passed, 0.8 s | passed, 9.0 s, 303 MB | passed, 4 min 29 s, 312 MB |
| `p4_16_errors/action-bind.p4` | fail, 0.8 s | fail, 7.6 s, 280 MB | fail, 6.7 s, 313 MB |

The first K run of `spec` also converts it to KAST, which takes about 18 s and
2.8 GB before the cache in `spec-meta-k/.kore-cache` exists. The host library
accounts for none of the difference between the three columns; the time is in
K rewriting and Redex matching.

## Exploring in a REPL

Start `racket` from the repository root. Redex renders the specification one
definition at a time, with `language->pict`, `reduction-relation->pict`,
`metafunction->pict`, and `judgment-form->pict`. This saves a few of them as
PDF files in the current directory:

```racket
(require pict file/convertible redex/pict)

;; Evaluates form inside a spec module, where its unexported names are visible
(define (in m form)
  (define p (simplify-path (path->complete-path (build-path "spec-meta-redex" m))))
  (dynamic-require p #f)
  (parameterize ([current-namespace (module->namespace p)])
    (namespace-require 'redex/pict)
    (eval form)))

;; Saves a pict as a PDF page sized to fit it
(define (save p file)
  (with-output-to-file file #:exists 'replace
    (λ () (void (write-bytes (convert p 'pdf-bytes))))))

(save (in "4-relation.rkt" '(language->pict al)) "al.pdf")
(save (in "5.8-eval.rkt" '(reduction-relation->pict ->term)) "term.pdf")
```
