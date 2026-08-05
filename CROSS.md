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

**Status: done.** All four steps are implemented, and `add`, `fibo` and
`iter-nontrivial` reproduce the oracle end-to-end from the `.watsup` source —
see step 4 for the results and the one example that does not yet run.

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
| `al/5.1`–`5.7` | `Eval_typ`, `Assign_exp`, `Eval_exp`, `Eval_arg`, `Eval_prem`, `Call_func`, `Call_rel` | `al/4.1`–`4.6` (six files, not seven: argument handling is split across `4.2-eval-assign.k`, `4.3-eval-exp.k` and `4.5-eval-call-func.k` rather than getting a file of its own) |
| `al/6-entry.watsup` | `Entry` | `al/5-entry.k` (module `AL`) |

The K file numbering drifted from the watsup numbering during the port, so the
third column is the mapping as it actually landed. `CtxRes` is absent: the cell
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
kompile spec-meta-k/al/5-entry.k --main-module AL --syntax-module AL-SYNTAX \
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
case "$1" in
  *.watsup)
    json=$(mktemp)
    trap 'rm -f "$json"' EXIT
    "${SPECTEC_BOOT:-./spectec-boot}" kast "$1" -o "$json"
    ;;
  *)
    json="$1"
    ;;
esac

exec kast --definition "$KDEF" --input json --output kore --sort Script "$json"
```

Two constraints shape this:

- `--parser` takes a single executable, not a command string. The `krun --help`
  text suggests otherwise (`krun --parser cat foo.kore`), but K 7.1.337 execs
  the flag's whole value as one filename, so `--parser "./x.sh arg"` fails with
  *No such file or directory*. Anything the wrapper needs beyond the input file
  has to reach it another way — hence `$KDEF` for the definition, and
  `$SPECTEC_BOOT` to override the path to `spectec-boot`.
- `krun` requires `$PGM` to be a *file*; it appends that path to the parser
  command. There is no stdin route for the program term, which is why the two
  steps cannot simply be piped together.

`spectec-boot kast` exits non-zero and reports to stderr on failure, so `set -e`
in the wrapper stops before `kast` is handed a file that was never written.

### Step 4: run

The entry module is `spec-meta-k/al/5-entry.k`; there is no `main.k`. Compile
once, then run a target directly:

```sh
kompile spec-meta-k/al/5-entry.k --main-module AL --syntax-module AL-SYNTAX -o al-kompiled
KDEF=al-kompiled krun -d al-kompiled --parser ./kast-json.sh examples/add.watsup
```

The wrapper boots the `.watsup` itself, so no separate `spectec-boot kast` step
is needed. `KDEF` must be set even though `-d` already names the definition on
the `krun` line: `krun` passes the wrapper only the input file, so that is the
one channel it has for the definition.

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

Run from the repo root against all four examples:

| example | `<result>` | oracle | |
| --- | --- | --- | --- |
| `add` | `intN(119)` | `INT +119` | ✓ |
| `fibo` | `intN(89)` | `INT +89` | ✓ |
| `iter-nontrivial` | `intN(-42)` | `INT -42` | ✓ |
| `builtin-map` | — | `INT +45` | stuck |

The three that match also end with `<saves>` and `<callstack>` empty, and
`add`'s `<log>` ends `textV("Add")`, `intN(119)` — the `debug` premise the
oracle prints as `TEXT Add`.

`builtin-map` stops with `<k>` headed by
`callBuiltinFunc("add_map", ...)`, and a frame left in each stack. Nothing
consumes that item: `al/3-context.k` loads `builtinFuncD` into the context, but
the invocation of `$empty_map`/`$find_map`/`$add_map` is not implemented. That
is the outstanding step-1 item — the term itself emits and parses correctly.
