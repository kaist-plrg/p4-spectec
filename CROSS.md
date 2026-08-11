# Cross-checking P4-SpecTec AL against K

`spec-meta-k/` holds a K specification of the P4-SpecTec meta-language,
mirroring `spec-meta/`. Currently only the syntax is defined:

- `spec-meta-k/common/1-syntax.k` — module `COMMON-SYNTAX`
- `spec-meta-k/al/1-syntax.k` — module `AL-SYNTAX`

It is an abstract syntax: every production is a constructor application, so K
never parses AL concrete syntax.

## 1. Install

`p4spectec_k.dockerfile` builds P4-SpecTec and the K framework (LLVM backend
only; the Haskell backend is skipped) in one image.

```sh
docker build -f p4spectec_k.dockerfile -t p4spectec-k .
docker run -it --rm p4spectec-k
```

The build clones the repo from GitHub and checks out `meta2-cross`, so the
container holds a copy of the repo as of build time; local uncommitted changes
are not in it.

To run against local files instead, share a host directory with the container
using `-v HOST_PATH:CONTAINER_PATH`. Everything in the container that reads
`CONTAINER_PATH` then sees the host directory, and edits on either side are the
same files, so no image rebuild is needed:

```sh
docker run -it --rm -v "$PWD/spec-meta-k":/home/p4-spectec/spec-meta-k p4spectec-k
```

Share subdirectories only. The `k/` submodule contains the K framework source;
`kompile` and `krun` exist only after it is compiled, which the image build
does with `mvn package`, producing `k/k-distribution/target/release/k/bin`.
Sharing the whole repo with `-v "$PWD":/home/p4-spectec` would hide that
compiled K behind the host checkout, and `kompile` would disappear from `PATH`
unless K has been compiled on the host as well.

Inside the container the working directory is `/home/p4-spectec` and
`kompile`, `krun` and `kast` are on `PATH`
(`k/k-distribution/target/release/k/bin`).

## 2. Check the syntax

Compiling `AL-SYNTAX` alone verifies that the definition is well-formed:

```sh
kompile spec-meta-k/al/1-syntax.k --main-module AL-SYNTAX --syntax-module AL-SYNTAX
```

To also check that AL terms can be built and matched, write `check.k` in
`/home/p4-spectec`:

```k
requires "spec-meta-k/al/1-syntax.k"

module CHECK
  imports AL-SYNTAX
  imports INT

  syntax Int ::= numDefns(Script) [function]
  rule numDefns(.Script) => 0
  rule numDefns(_:Defn, Ds:Script) => 1 +Int numDefns(Ds)

  syntax Script ::= sample() [function]
  rule sample() =>
    typD("nat", .TParamList, aliasDT(natT())),
    funcD("$succ", .TParamList, expParam(natT()), natT(),
          clause(expA(varE("n")), binE(addOp(), varE("n"), natN(1)), .PremList),
          noElsClause()),
    .Script
endmodule
```

Then:

```sh
kompile check.k --main-module CHECK --syntax-module CHECK
echo 'numDefns(sample())' > check.al
krun --definition check-kompiled check.al
```

Expected output:

```
<k>
  2 ~> .K
</k>
```

The sample term exercises constructors from both modules, list terminators
(`.Script`, `.TParamList`, `.PremList`), and the option sorts (`noElsClause()`).

Notes:

- `.Sort` terminators as written above are rule syntax. K rewrites list
  grammars for program parsing, where an empty list is the empty string
  instead; this only matters if terms are fed through `kast` as program text.
- Constructor labels are pinned with `symbol(_)` in the spec, so the KORE
  symbol of e.g. `varT` is `varT`.

## 3. Plan

Goal: run `examples/add.watsup` and `examples/fibo.watsup` under the AL
specification written in K.

**Status: done.** All four steps are implemented, and every example reproduces
the oracle end-to-end from the `.watsup` source — see step 4 for the results.

### Reference oracle

The same runs under the existing OCaml meta-circular interpreter, whose output
K must reproduce:

```sh
make boot
./spectec-boot run spec-meta/al -rel Entry \
  -tec examples/add.watsup -ali              # => INT +119
./spectec-boot run spec-meta/al -rel Entry \
  -tec examples/fibo.watsup -ali             # => INT +89
./spectec-boot run spec-meta/al -rel Entry \
  -tec examples/iter-nontrivial.watsup -ali  # => INT -42
./spectec-boot run spec-meta/al -rel Entry \
  -tec examples/builtin-map.watsup -ali      # => INT +45
```

`-ali` runs `Pass.algo` on the target `.watsup`, and
`p4spec/lib/interface/spectec/ali/boot.ml` converts the resulting IL into a
value of the meta-language `script` syntax. The `Entry` relation
(`spec-meta/al/6-entry.watsup`) loads that script and evaluates
`CALL "main" eps eps`.

The AL specification is the K definition; the elaborated target is the only
runtime input. A second input (a P4 program, at the target level) would be
passed as a further configuration variable, `-cPROG=... -pPROG=...`.

### Step 1: port the semantics to K

Naive, rule-for-rule port of `spec-meta/{common,al}` alongside the existing
syntax modules, in dependency order:

| watsup | K | as ported |
| --- | --- | --- |
| `common/0-stdlib.watsup` | list/map/set helpers | `common/0-stdlib.k` |
| `common/2-env.watsup`, `al/2-env.watsup` | `varr`, `venv`, `tdenv`, `theta`, `reldef`, `funcdef` | `common/2-env.k`, `al/2-env.k` |
| `al/3-context.watsup` | `layer`/`ctx`, `$empty_ctx`, `$load*` (largest chunk, ~220 lines) | `al/3-context.k` |
| `common/4-relation.watsup` | `res<X>` — K has no generics, so instantiate `ValRes`, `ValsRes`, `UnitRes` | `common/3-relation.k` |
| `common/5.0-eval-typ.watsup`, `common/5.1-eval-ops.watsup` | type and operator helpers | `common/4.0-eval-typ.k`, `common/5.1-eval-ops.k` |
| `al/5.1`–`5.7` | `Eval_typ`, `Assign_exp`, `Eval_exp`, `Eval_arg`, `Eval_prem`, `Call_func`, `Call_rel` | `al/5.1`–`5.6` (six files, not seven: argument handling is split across `5.2-eval-assign.k`, `5.3-eval-exp.k` and `5.5-eval-call-func.k` rather than getting a file of its own) |
| `al/6-entry.watsup` | `Entry` | `al/6-entry.k` (module `AL`) |

The `al/` numbering follows the watsup numbering; `common/` still drifts from
it, so the third column is the mapping as it actually landed. `CtxRes` is absent: the cell
design keeps the context in configuration cells rather than threading it as a
value, so no relation returns one.

Evaluation is driven by K cells, not by pure functions:

```k
configuration
  <al>
    <k> $PGM:Script ~> callMain("main") </k>
    <global> <tdenv> .Map </tdenv> <renv> .Map </renv>
             <fenv>  .Map </fenv>  <venv> .Map </venv> </global>
    <local>  <tdenv> .Map </tdenv> <renv> .Map </renv>
             <fenv>  .Map </fenv>  <venv> .Map </venv> </local>
    <log> .List </log>
    <result> .K </result>
  </al>
```

`$PGM:Script` is how the term arrives. Loading consumes definitions off the
head of the script into the environment cells:

```k
rule <k> (funcD(F:Id, _, _, _, Cs:ClauseList, _), Ds:Script) => Ds ... </k>
     <fenv> M:Map => M[F <- Cs] </fenv>
rule <k> (_D:Defn, Ds:Script) => Ds ... </k> [owise]
rule <k> .Script => .K ... </k>
```

Two points to settle before porting the evaluation rules, and how each was
settled:

- Backtracking. AL reifies failure as `FAIL` in `res<X>`, and K has no built-in
  backtracking — `krun --search` is symbolic-backend only, so it is unavailable
  on the LLVM backend. `FAIL` therefore needs an explicit `<k>` item plus a
  frame continuation that restores the saved cell state and tries the next
  clause or rulegroup. Worth prototyping on `fibo` (two clauses plus
  `-- otherwise`) before porting the rest.

  As ported: two separate stacks. `<saves>` holds `<local>` snapshots for
  intra-relation save/restore (clause backtracking, and iteration where one
  element's bindings must not leak into the next); `<callstack>` holds full
  call frames, where the callee replaces `<local>` wholesale rather than
  layering on top of it. Conflating the two is a real hazard — it produced a
  bug where a relation call's `ValList` result met a continuation expecting a
  bare `Val`.
- `debug` premises. Either append to `<log>`, or use `<out stream="stdout">`
  with `imports K-IO` for output during the run.

  As ported: `<log>`, via a `logDebug(Val)` `<k>` item (`al/0-config.k`).

Order of work: `add` needs script loading, a zero-argument clause, `LetPr` with
`Assign_exp` on a plain variable, `IfPr`, `binE`/`cmpE`, `debugPr`,
`callE("main", ...)` and `Entry`. Note that `-- if i = $(42 + 77)` becomes a
*let* premise after `Pass.algo`, so assignment is required even for `add`.
`fibo` adds recursion, multi-clause backtracking on argument patterns, and the
else-clause. `examples/iter-nontrivial.watsup` exercises `relD`/`relPr`
dispatch together with iterated premises (`iterPr`/`iterPrem`) and an iterated
`letPr` binding — checked against
`./spectec-boot run spec-meta/al -rel Entry -tec examples/iter-nontrivial.watsup
-ali` (`INT -42`). `examples/builtin-map.watsup` is the next target after that.

#### Testing the specification

Step 1 predates the emitter, so a script under test is written by hand
directly as a term of the `Script` sort and fed to `krun` as *program text*.
This route is still the convenient one for a small hand-built test that no
`.watsup` file corresponds to; for a real target, steps 2–4 below emit the
term instead. Compile the whole chain from the entry module — its
`--syntax-module` is `AL-SYNTAX`, since that is where the program being run is
parsed, not the module that defines the entry point:

```sh
kompile spec-meta-k/al/6-entry.k --main-module AL --syntax-module AL-SYNTAX \
  -o al-kompiled
krun --definition al-kompiled spec-meta-k/test/add.script
```

`krun` prints the whole configuration; the answer is in `<result>`. A run
should also leave `<saves>` and `<callstack>` as `.List` — either one holding
a leftover frame means a save/restore path returned without popping, the
usual symptom of a bug in the backtracking machinery.

Two things about program-text syntax are easy to get backwards, both because
term construction in *rules* looks different from a term written as *program
input*:

- An empty list is the empty string, not `.Sort`. `.Sort` is rule syntax; in
  a `.script` file the corresponding argument position is left blank, e.g.
  `funcD("main", , , intT(), ...)` has two empty lists (`TParamList`,
  `ParamList`) written as bare commas.
- A single-element list is written bare, with no enclosing parens and no
  trailing comma: `expA(varE("i"))`, not `(expA(varE("i")), )`. Parenthesized
  comma-lists are for two-or-more-element lists only.

`spec-meta-k/test/` holds the scripts exercised so far, each written by hand
in this style and checked against the oracle (`add` and `fibo`, matching
`./spectec-boot run spec-meta/al -rel Entry -tec examples/{add,fibo}.watsup
-ali`) or by inspection of the expected arithmetic (`rel`, `iter` — a relation
call and a list-iteration/struct/option case, respectively, neither of which
`add`/`fibo` exercise).

### Step 2: dump `Value.t` to KAST

Add a `spectec-boot` subcommand that boots the target as usual but, instead of
running `Entry`, prints the `Value.t` as KAST JSON. The mapping is mechanical:
`CaseV(mixop, vs)` to the constructor label, `ListV` to a cons list, `OptV` to
`noX`/`someX`, `NumV`/`TextV`/`BoolV` to tokens. Derive the mixop-to-label table
from `p4spec/lib/interface/spectec/ali/mixops.ml` so the two cannot drift.

The source of `spectec-boot` is located in `p4spec/bin/boot.ml`.

The format is version **4**, in which `label` and `sort` are objects (older
version 3 examples use bare strings and are rejected):

```json
{"format":"KAST","version":4,"term":
 {"node":"KApply","label":{"node":"KLabel","name":"funcD","params":[]},
  "arity":6,"args":[
   {"node":"KToken","sort":{"node":"KSort","name":"String","params":[]},
    "token":"\"$succ\""}
  ]}}
```

- Constructor labels are the `symbol(_)` names from `spec-meta-k`.
- For a list, the cons label is the list's `symbol(_)` (`script`, `clauseList`,
  `argList`) and the empty list is its `terminator-symbol(_)` (`.script`,
  `.clauseList`, `.argList`).
- `String` tokens include their quotes inside the `token` field.
- Injections are not written out; `kast` infers `String -> Id`, `Num -> Exp`
  and so on.

The emitter can be checked against the syntax modules alone, before any
semantics exist:

```sh
kompile spec-meta-k/al/1-syntax.k --main-module AL-SYNTAX \
  --syntax-module AL-SYNTAX -o al-syntax-kompiled
kast -d al-syntax-kompiled --input json --output pretty add.json
```

This prints the script back as `typD("nat", .TParamList, aliasDT(natT())), ...`.

#### As implemented

The subcommand is `spectec-boot kast`, taking one target `.watsup` and writing
to stdout or to `-o FILE`:

```sh
./spectec-boot kast examples/add.watsup -o add.json
```

It boots the target through the same path the `-ali` interface uses
(`Interface.SpecTec_AL.parse_program`, i.e. `Pass.algo` then
`Ali.Boot.boot_spec`), so the emitted value is the one `Entry` would have run.

The emitter is `p4spec/lib/interface/spectec/ali/kast.ml`. Its table is keyed
by the pair *(sort, mixop)*, not by mixop alone: notations are reused across
sorts, so `BOOL bool` is both `boolV` and `boolE`, and `clause` and `tblrow`
share a notation entirely. Every `CaseV` that `boot.ml` builds is noted with
the watsup sort it inhabits (the `<<|!` operator), which supplies the first
component. The mixops themselves come from `Common.Mixops` and `Ali.Mixops` —
the same constants `boot.ml` constructs values with — so a renamed mixop stops
matching and the emitter reports an unknown constructor rather than emitting a
wrong label.

Three wrinkles in that keying, all consequences of watsup sorts that K has no
direct counterpart for:

- Watsup aliases are inlined in K, and the booter notes a value with whichever
  name the position calls for. `boot_targ` re-notes a type as `targ`, and
  `boot_elsclause_opt` notes its option with `elsclause`; both are unaliased
  before lookup. Likewise `mixop = atom**` and `script = defn*` are noted with
  the alias rather than an iterated type, so the list handler maps them to
  their element sorts by name.
- A watsup case that K models as a subsort is reachable under either name.
  `boot_targ` re-notes as `targ` whatever narrower sort `boot_typ` produced, so
  `optyp` and `numtyp` constructors are registered under `typ` as well.
- `boot_num_exp` re-notes a `num` value as `exp` without changing its
  constructor, since `num` is a case of both `val` and `exp`; `Num` is
  correspondingly a subsort of both `Val` and `Exp` in K, so `natN`/`intN` are
  registered under all three sorts.

Checked on all four examples: each round-trips through
`kast --input json --output pretty` against the kompiled `AL-SYNTAX`, and the
`add` output is byte-identical to the hand-written `spec-meta-k/test/add.script`
parsed the same way. `fibo` differs from its hand-written script in one
identifier only — `Pass.algo` names the else-clause wildcard `_i` where the
script had abbreviated it to `_`.

Run end-to-end against the full semantics, the emitted JSON reproduces the
oracle on every example the semantics support — see step 4 for the results.

### Step 3: wrapper

`krun` has no `--input json`, so the JSON is converted by a wrapper script,
`kast-json.sh` in the repo root, passed to `krun --parser`.

The wrapper accepts a `.watsup` as well as a `.json`, doing the boot itself
when given one, so that running a target takes a single command (see step 4).
Anything that is not a `.watsup` is assumed to be KAST JSON already:

```sh
mkdir -p ./.tmp

case "$1" in
  *.watsup)
    json=$(mktemp ./.tmp/spectec-k-kast-XXXXXX.json)
    "${SPECTEC_BOOT:-./spectec-boot}" kast "$1" -o "$json"
    status=0
    kast --definition "$KDEF" --input json --output kore --sort Script "$json" \
      || status=$?
    rm -f "$json"
    exit $status
    ;;
  *)
    json="$1"
    ;;
esac

exec kast --definition "$KDEF" --input json --output kore --sort Script "$json"
```

Three constraints shape this:

- `--parser` takes a single executable, not a command string. The `krun --help`
  text suggests otherwise (`krun --parser cat foo.kore`), but K 7.1.337 execs
  the flag's whole value as one filename, so `--parser "./x.sh arg"` fails with
  *No such file or directory*. Anything the wrapper needs beyond the input file
  has to reach it another way — hence `$KDEF` for the definition, and
  `$SPECTEC_BOOT` to override the path to `spectec-boot`.
- `krun` requires `$PGM` to be a *file*; it appends that path to the parser
  command. There is no stdin route for the program term, which is why the two
  steps cannot simply be piped together.
- The booted JSON cannot be cleaned up by an `EXIT` trap alongside `exec`:
  `exec` replaces this shell, so the trap never fires and one file accumulates
  per run. `kast` runs as a child in that branch instead, with the `rm` after
  it and its status passed on. The `mkdir` at the top is also what creates
  `.tmp/` for the builtin request file that K itself writes.

`spectec-boot kast` exits non-zero and reports to stderr on failure, so `set -e`
in the wrapper stops before `kast` is handed a file that was never written.

### Step 4: run

Three Makefile targets cover the whole workflow. **Run them from the repo
root** — a spec that calls a builtin shells out to `./spectec-boot` at that
relative path (see *Builtin functions* below). Each depends on `spectec-boot`
and builds it if missing.

```sh
make k-build                          # compile the K definition -> al-kompiled/
make k-run TEC=examples/add.watsup    # run one target
make k-clean                          # drop al-kompiled/ and .tmp/
```

To check a target against the OCaml, run it both ways and compare `<result>`
with the oracle's last line:

```sh
make k-run TEC=examples/add.watsup
./spectec-boot run spec-meta/al -rel Entry -tec examples/add.watsup -ali
```

Underneath, the entry module is `spec-meta-k/al/6-entry.k` — there is no
`main.k` — and the two commands are:

```sh
kompile spec-meta-k/al/6-entry.k --main-module AL --syntax-module AL-SYNTAX -o al-kompiled
mkdir -p .tmp
KDEF=al-kompiled krun -d al-kompiled --parser ./kast-json.sh examples/add.watsup
```

The wrapper boots the `.watsup` itself, so no separate `spectec-boot kast` step
is needed. `KDEF` must be set even though `-d` already names the definition on
the `krun` line: `krun` passes the wrapper only the input file, so that is the
one channel it has for the definition. `.tmp/` must exist before `krun` — the
wrapper creates it, which is why the bare `krun` form above needs the `mkdir`.

To keep the intermediate JSON — to inspect it, or to diff two revisions of the
emitter — emit it explicitly and hand `krun` that instead; the wrapper takes
either:

```sh
./spectec-boot kast examples/add.watsup -o add.json
KDEF=al-kompiled krun -d al-kompiled --parser ./kast-json.sh add.json
```

`krun` prints the final configuration, so the answer appears in the cell it was
written to:

```
<al>
  <k> .K </k>
  ...
  <result> intN(119) </result>
</al>
```

Use `--output json` to diff mechanically rather than by eye.

#### Results

Run from the repo root against every example:

| example | `<result>` | oracle | exercises | |
| --- | --- | --- | --- | --- |
| `add` | `intN(119)` | `INT +119` | arithmetic, `debug` | ✓ |
| `fibo` | `intN(89)` | `INT +89` | recursion, `otherwise` | ✓ |
| `iter-nontrivial` | `intN(-42)` | `INT -42` | iteration premises | ✓ |
| `builtin-map` | `intN(45)` | `INT +45` | map builtins | ✓ |
| `builtin-list` | `intN(19)` | `INT +19` | list builtins | ✓ |
| `variant-tree` | `intN(6)` | `INT +6` | variant types, ADT recursion | ✓ |
| `relation-typing` | `intN(110)` | `INT +110` | multi-rule relations, FAIL recovery | ✓ |
| `iter-sequence` | `intN(1085)` | `INT +1085` | sequences, `::`/`++`, comprehensions | ✓ |
| `builtin-nested` | `intN(65)` | `INT +65` | builtins over nested data | ✓ |
| `mutual-recursion` | `intN(289)` | `INT +289` | mutual recursion, deep call stacks | ✓ |
| `builtin-extra` | `intN(277)` | *n/a* | 11 builtins K never implemented | ✓ |
| `extern-call` | `intN(12138)` | *n/a* | `extern dec`, `extern relation`, FAIL recovery | ✓ |

All twelve end with `<k>`, `<saves>` and `<callstack>` empty, and `add`'s
`<log>` ends `textV("Add")`, `intN(119)` — the `debug` premise the oracle
prints as `TEXT Add`.

Two examples have no oracle column, for different reasons:

- `builtin-extra` uses builtins the oracle cannot dispatch (see *Builtin
  functions* below), so K is the only engine that runs it. It is the direct
  demonstration that the external interface reaches past the eight builtins the
  K rules used to implement.
- `extern-call` uses `extern dec` / `extern relation`, for which **no oracle
  exists at all**: `spectec-boot run ... -ali` builds its runner with
  `Make_null`, which errors on both `Call_extern_func` and `Call_extern_rel`
  (`backend-boot/spectec.ml:82-101`). Its `<result>` is hand-derived —
  `1000*12 + 100*0 + 10*14 + (-1) + (-1) = 12138` — and confirmed against the
  lower spec's definitions rather than against a second engine.

### Builtin functions

`builtin-map` originally stopped with `<k>` headed by
`callBuiltinFunc("add_map", ...)`: `al/3-context.k` loaded `builtinFuncD` into
the context, but nothing consumed the resulting call.

`Call_builtin_func` is an `extern relation` in watsup, for a reason unlike
`Call_extern_func`/`Call_extern_rel`, which stay undefined: a builtin's meaning
lives in the host interpreter (`p4spec/lib/interface/builtin/`), not in the
meta-language. K therefore implements no builtin of its own. Instead
`al/5.5-eval-call-func.k` serializes the call to JSON, shells out to
`spectec-boot builtin`, and reads the result back, which keeps the OCaml the
single authority for what a builtin computes.

An earlier revision reimplemented eight builtins natively in K rules. Those
were mirrors of the OCaml, so they duplicated authority and could drift from
it; they have been removed.

The external route covers **all 44** entries of the OCaml registry
(`builtin/call.ml`) rather than the eight. A target only has to declare the
builtin it wants — `builtin dec $sum_nat(nat*) : nat` — and the call goes out
over the wire like any other. Confirmed under K for `$sum_nat`, `$max_nat`,
`$text_to_int`, `$int_to_text`, `$strip_prefix`, `$concat_`, and the
`Numerics` operations `$pow2`, `$shl`, `$band`, `$bor`, none of which the K
rules ever implemented.

Worth noting the asymmetry this creates: **the OCaml oracle cannot run these.**
`spectec-boot run spec-meta/al ... -ali` dispatches `Call_builtin_func` through
`backend-boot/spectec.ml`, which resolves the name against the *meta-spec's own*
functions, so it is limited to the eight that
`spec-meta/common/0-stdlib.watsup` declares. A target using `$text_to_int`
therefore runs under K but fails under the oracle — the reverse of the usual
direction, and something to keep in mind when cross-checking.

#### Runtime dependency

**A K run now requires `./spectec-boot` to exist, and must start from the repo
root.** The path is hardcoded in `builtinCmd()`: K rules cannot read
environment variables, so unlike `kast-json.sh` there is no `$SPECTEC_BOOT`
override available on this path. Build it with `make boot` (never `dune build`
directly, and note that `make clean` leaves a stale `./spectec-boot` behind).

Scratch files go under **`./.tmp/`** (gitignored), not `/tmp`. The directory is
created by `kast-json.sh`, because K rules cannot `mkdir` and the request file
is written from inside K. Running `krun` without that wrapper — against a
pre-booted `.json`, say — means creating `./.tmp/` yourself first, or the
`#mkstemp` in `callBuiltinFunc` returns an `IOError` and the term sticks there.

**One request file per run, not per call.** `#mkstemp` mints it on the first
builtin call; `<builtinreq>` remembers the path, and every later call reopens
that same file in mode `"w"`, which truncates. `finish()` removes it at the end
of the run. `#mkstemp` is *documented* to delete its files when rewriting ends,
but K 7.1.337's LLVM backend does not, so the cleanup is explicit.

A run that dies on a stuck term never reaches `finish()` and so leaves its
request file behind — deliberately useful, since that file is the last request
sent and the stuck term holds the reply. `kast-json.sh` sweeps stale ones at
the start of the next run, so they do not accumulate.

#### The wire

The codec is `spec-meta-k/al/4-extern-json.k` on the K side and
`p4spec/lib/interface/spectec/ali/extern.ml` on the OCaml side; the format is
documented in full at the head of each. In brief:

```
request  ::= {"builtin": <id>, "targs": [typ, ...], "args": [val, ...]}
response ::= {"ok": val}

val   ::= ["boolV", <bool>] | ["natN", "<decimal>"] | ["intN", "<decimal>"]
        | ["textV", <string>] | ["strV", [[<atom>, val], ...]]
        | ["injV", mixop, [val, ...]] | ["tupV", [val, ...]]
        | ["optV", null] | ["optV", val] | ["listV", [val, ...]]
        | ["funcV", <id>] | ["extV", <json>]
mixop ::= [[<atom>, ...], ...]        // an atoms matrix, exactly K's MixOp
```

It is deliberately neither KAST JSON nor `Value.t`'s derived yojson. KAST JSON
cannot express it: `kast.ml`'s `sort_of_typ` accepts only a bare
`VarT(id, [])`, but builtin results are noted with `IterT(VarT("pair",_),List)`
and `VarT("map",[K;V])`. The derived yojson carries `vid`, and importing vids
minted by another process would collide with locally allocated ones — and
`Value.compare` short-circuits on vid equality, so two structurally distinct
values would compare *equal*. Everything decoded is rebuilt through
`Value.Make.*`, which mints fresh vids.

Numbers cross as decimal **strings**, not JSON numbers: both sides hold
arbitrary-precision integers and P4 routinely exceeds 64 bits.

Four mechanics worth recording, each found the hard way:

- The request goes through a **temp file, never argv**. Operator atoms render
  with quotes (`':'`), and `#system` passes its argument through a shell, which
  fails on those with *Unterminated quoted string* (exit 2). Only the
  `#mkstemp` path is interpolated into the command line. It cannot go through
  stdin either: `#system` has no way to write to the child's.
- `#open(path, "w")` **truncates**, following C `fopen`, which is what lets one
  file be reused across calls without a short request reading back the tail of
  a longer predecessor. There is no `#truncate` in K-IO, so reopening is the
  only way to get that.
- `#system` needs no `kompile` flag — `imports K-IO` suffices — but `JSON`
  needs an explicit `requires "json.md"`; `imports JSON` alone fails with *Could
  not find module: JSON*.
- `#write`/`#close`/`#remove` have sort `K`, so they can only be sequenced
  directly in the `<k>` cell. A `strict(1)` wrapper around them does not
  kompile (*Cannot heat a nonterminal of sort K*).

Errors never travel in stdout: the subcommand writes to stderr and exits
non-zero, stdout stays empty. K's only error signal is the exit code from
`#systemResult`, and it has to be able to tell "OCaml said no" from "the wire
broke". A non-zero exit has no rule on the K side — a builtin is total in the
meta-language, so a failure is a defect rather than something the spec can
recover from, and the stuck term carries the child's stderr.

#### Encoding notes

Two details of the value encoding, read off the emitted KAST rather than
guessed:

- `map<K, V>` is `set<pair<K, V>>`, so a map value is
  `injV(valCase(<mixop>, listV(pairs)))` where the mixop is
  `` ["`{"] ; ["`}"] ``, and each pair is
  `injV(valCase([] ; ["':'"] ; [], (k, v)))`.
  One atom group per notation position, empty where an argument goes. The
  `':'` atom carries its quotes inside the string.
- A map is an association *list*, and `$find_map` reads it front-to-back, so
  `$add_map` on a key already present replaces that entry in place rather than
  appending. This is what makes the fourth `$add_map` in `builtin-map`
  overwrite `"three"` with 42.

Unlike everywhere else in this port, **type arguments are not ignored**. A
builtin derives the `note` type of its result from its `targs`, and the host
runs a typed value representation, so `TypList` goes out on the wire
faithfully. Argument values, by contrast, cross without their `note.typ`:
`extern.ml` rebuilds a structural placeholder on decode. That is sound because
no builtin reads an argument's `note.typ` — `Value.compare`, `Value.eq`,
`Value.Get.*` and `Mixfix.eq_mixop` are all purely structural, and the map
builtins dispatch on the mixop. `extern.ml` records this as an invariant: if a
future builtin breaks it, it breaks there.

`examples/builtin-list.watsup` covers the list builtins `builtin-map` does not
reach (`$rev_`, `$transpose_`, `$assoc_`). `$find_maps`, `$adds_map` and
`$update_map` have no example exercising them.

#### Limitations

- **`fresh_typeId` is wrong under K.** It closes over a `ctr : int ref`
  (`builtin/call.ml:27,81`) which under `Make (...) ()` is per-process. Each
  `#system` call is a fresh process, so it returns the same value every time.
  No example uses it. Fixing it needs either the counter on the wire or a
  persistent server.
- **One process spawn per call** (~20-100 ms). `builtin-map` makes ~9 calls, so
  it is fine at this size; a real P4 program would not be. The schema is a
  self-contained request/response object precisely so the transport can be
  swapped for a persistent co-process later without changing the format.
- Replay is safe: K does not re-execute the `#system` hook on backtracking, and
  43 of the 44 builtins are pure anyway — `fresh_typeId` is the sole exception,
  already noted.

### Extern functions and relations

`Call_extern_func` and `Call_extern_rel` were the two remaining stuck terms:
both were produced by dispatch but had no consuming rule, so an AL spec
declaring an `extern dec` or `extern relation` left a visible stuck term. They
now go over the same external interface as a builtin.

They are **not just two more builtins**, and the difference is structural. A
builtin is a static registry lookup needing no state. These are the hooks by
which a spec running under K reaches the externs of the level *below* it: in
OCaml that is `Make_parametric` (`backend-boot/spectec.ml:173-215`), which
routes them into a **lower runner's** `Interp.eval_func` / `eval_rel`. They
therefore need a *loaded lower spec*, which is why they get their own
subcommand rather than sharing `builtin`:

```
spectec-boot extern -lower SPEC_DIR -al -ali -i REQUEST.json [-o RESPONSE.json]
```

`Backend_boot.Build.build_null` already produces such a runner, and
`eval_func`/`eval_rel` are on the public `RUNNER` signature
(`runtime/dynamic-runner/signature.ml:82-95`), so no widening of `RUNNER` was
needed — the `Extern` module is never reached.

The lower spec K uses is `examples/lower/lower.watsup`, named by `externArgs()`
in `al/4-extern-json.k`. It is hardcoded for the same reason
`builtinCmd()` is: K rules cannot read the environment, and the `.watsup` being
run does not itself say which spec sits below it.

#### The extended wire

The phase-one format is *extended*, not replaced. Every builtin request and
response is byte-identical to what it was, so that path is untouched:

```
request  ::= {"builtin":     <id>, "targs": [typ, ...], "args": [val, ...]}
           | {"extern-func": <id>, "targs": [typ, ...], "args": [val, ...]}
           | {"extern-rel":  <id>, "args": [val, ...]}

response ::= {"ok": val}         // builtin and extern-func
           | {"ok": [val, ...]}  // extern-rel: a relation yields val*
           | {"fail": null}      // recoverable failure; extern only
```

The kinds are discriminated by **which key is present**, not by a separate
`"kind"` field, which keeps a phase-one request unambiguously a phase-one
request. Exactly one of the three must appear: none, or more than one, is an
error rather than something guessed at. An `extern-rel` request carries no
`targs` — an extern relation takes only `id val*`
(`spec-meta/common/4-relation.watsup:26-30`). The `val`, `mixop` and `typ`
productions are unchanged.

#### `{"fail": null}` and why it is not a non-zero exit

An extern can **FAIL**, and unlike a builtin failure that is *recoverable*.
`rel_result`/`func_result` are `Pass | Fail of region * string`
(`signature.ml:12-14`), and a failed extern relation must become K's `FAIL`
KItem (`common/3-relation.k:27`) so it backtracks through `tryNextRul` exactly
like a failed rule.

So a spec-level failure travels as `{"fail": null}` **on stdout with exit
status 0**, and the non-zero exit stays reserved for "the wire broke" —
preserving the distinction phase one established. On the K side a non-zero exit
still has no rule, so a genuine wire defect sticks visibly with the child's
stderr in the term.

`Fail`'s region and message are **lost** crossing the wire: K's `FAIL` is
nullary and has nowhere to put them. They are logged to the child's stderr,
where a failing run can still read them, while stdout carries the clean
response.

Verified end to end in `examples/extern-call.watsup`, whose two FAIL cases fail
for genuinely different reasons — `NEG 7` finds no matching rule head, `POS -3`
matches the head but fails the rule's `i >= 0` side condition — and both are
recovered by a `def ... -- otherwise` clause rather than sticking.

One wrinkle worth knowing: an **unknown extern name** also comes back as
`{"fail": null}` with exit 0, not as an error, because the interpreter reports
an undefined relation as a `Fail` rather than by raising. A typo'd extern name
therefore looks to K like a legitimate spec-level failure and is silently
recovered by an `otherwise` clause. The child's stderr says
``relation `X` is undefined``, which is where to look.

#### Placeholder notes, re-examined

Phase one justified dropping argument `note.typ`s on the grounds that builtins
read their arguments purely structurally. Externs do **not** inherit that
guarantee — they run arbitrary spec code in the lower interpreter, which does
type-directed work — so the invariant was re-checked before any K was written.
It holds, for a different reason: the two places that could read a note do not.

- The type checks `check_func_output` / `check_rel_outputs`
  (`interp/interp-al/interp.ml:114,69`) go through `Value.Match.sub_`
  (`runtime/value/match.ml:11-87`), which matches the *declared* `typ.it`
  against the value's *constructor* `value.it`. It never reads
  `value.note.typ`.
- The one place the AL interpreter does read an argument note is `assign_exp`
  (`interp.ml:152`), which threads it into `assign_cons_exp`
  (`interp.ml:210-215`) to note the tail of a matched `x :: xs`. A decoded list
  is already noted structurally, so the tail inherits a structural note — which
  again only ever reaches `sub_`.

`examples/lower/lower.watsup` exercises exactly this on purpose: `$lower_sum`
destructures its list argument with `::` rather than an index, so the
cons-assignment path is on the tested route. Had this not held, the fallback
was to put the argument `typ` on the wire for extern requests only;
`json_of_typ`/`typ_of_json` are already faithful enough to carry it.

#### One shared transport

The `#system` pipeline is now shared by all three call kinds and lives in
`al/4-extern-json.k`, next to the codec it serves, rather than being copied
per kind. Two things vary and are threaded through the pipeline states: the
subcommand argument string, and an `ExternKind` (`builtinK()`,
`externFuncK()`, `externRelK()`) telling the last step which decoder to apply.

The kind rides in a *following* KItem rather than inside `#systemResult`, whose
shape is fixed by K-IO. Everything else is unchanged from phase one: the
one-file-per-run `<builtinreq>` protocol, `#write`/`#close` sequenced directly
in `<k>`, and `dropBuiltinReq()` in `finish()`.

#### Limitations

- **A full spec load per call.** Each extern call spawns a process that parses
  and elaborates the lower spec from scratch — far worse than phase one's
  ~20-100 ms builtin spawn. Fine for a small example, unusable for a real P4
  program. The fix is the same persistent co-process phase one anticipated, and
  the schema is unchanged by it.
- **Caching is bypassed.** The lower runner's caches (`spectec.ml:157,170`
  `push_cache`/`pop_cache`) live in a process that dies after one call, so
  nothing is memoized across calls. Phase one's `fresh_typeId` limitation is
  the same defect in smaller form.
- **`Fail`'s region and message are lost**, as above.
- **An unknown extern name is indistinguishable from a spec-level failure**, as
  above.
- **`targs` are dead in one of the two OCaml routes.** `interp.ml:1451` passes
  `[]` to `eval_extern_func`, and `backend-sim` names the parameter `_typs`;
  only `Make_parametric.call_extern_func` unboots and forwards them. K sends
  them faithfully regardless, since the request should say what was called.
- **The lower spec is hardcoded** in `externArgs()`. Making it per-target needs
  a channel K does not currently have.
- Pre-existing and *not* fixed here, but worth flagging while in the area:
  `spectec.ml:191` notes `call_extern_func`'s result as `valsres`, where
  `spec-meta/common/4-relation.watsup:11-12` declares `res<val>` = `valres`.
  It looks like a copy-paste slip from `call_extern_rel` directly below it.
