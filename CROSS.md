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

### Reference oracle

The same runs under the existing OCaml meta-circular interpreter, whose output
K must reproduce:

```sh
make boot
./spectec-boot run spec-meta/common/*.watsup spec-meta/al/[1-6]*.watsup \
  -rel Entry -tec examples/add.watsup -ali     # => INT +119
./spectec-boot run spec-meta/common/*.watsup spec-meta/al/[1-6]*.watsup \
  -rel Entry -tec examples/fibo.watsup -ali    # => INT +89
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

| watsup | K |
| --- | --- |
| `common/0-stdlib.watsup` | list/map/set helpers |
| `common/2-env.watsup`, `al/2-env.watsup` | `varr`, `venv`, `tdenv`, `theta`, `reldef`, `funcdef` |
| `al/3-context.watsup` | `layer`/`ctx`, `$empty_ctx`, `$load*` (largest chunk, ~220 lines) |
| `common/4-relation.watsup` | `res<X>` — K has no generics, so instantiate `ValRes`, `ValsRes`, `CtxRes`, `UnitRes` |
| `al/5.1`–`5.7` | `Eval_typ`, `Assign_exp`, `Eval_exp`, `Eval_arg`, `Eval_prem`, `Call_func`, `Call_rel` |
| `al/6-entry.watsup` | `Entry` |

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

Two points to settle before porting the evaluation rules:

- Backtracking. AL reifies failure as `FAIL` in `res<X>`, and K has no built-in
  backtracking — `krun --search` is symbolic-backend only, so it is unavailable
  on the LLVM backend. `FAIL` therefore needs an explicit `<k>` item plus a
  frame continuation that restores the saved cell state and tries the next
  clause or rulegroup. Worth prototyping on `fibo` (two clauses plus
  `-- otherwise`) before porting the rest.
- `debug` premises. Either append to `<log>`, or use `<out stream="stdout">`
  with `imports K-IO` for output during the run.

Order of work: `add` needs script loading, a zero-argument clause, `LetPr` with
`Assign_exp` on a plain variable, `IfPr`, `binE`/`cmpE`, `debugPr`,
`callE("main", ...)` and `Entry`. Note that `-- if i = $(42 + 77)` becomes a
*let* premise after `Pass.algo`, so assignment is required even for `add`.
`fibo` adds recursion, multi-clause backtracking on argument patterns, and the
else-clause. `examples/iter-nontrivial.watsup` and `examples/builtin-map.watsup`
are the next targets after that.

### Step 2: dump `Value.t` to KAST

Add a `spectec-boot` subcommand that boots the target as usual but, instead of
running `Entry`, prints the `Value.t` as KAST JSON. The mapping is mechanical:
`CaseV(mixop, vs)` to the constructor label, `ListV` to a cons list, `OptV` to
`noX`/`someX`, `NumV`/`TextV`/`BoolV` to tokens. Derive the mixop-to-label table
from `ali/mixops.ml` so the two cannot drift.

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

### Step 3: wrapper

`krun` has no `--input json`, and its `--parser` flag takes a single executable
rather than a command string, so the JSON is converted by a wrapper script:

```sh
cat > kast-json.sh <<'EOF'
#!/bin/sh
exec kast --definition "$KDEF" --input json --output kore --sort Script "$1"
EOF
chmod +x kast-json.sh
```

### Step 4: run

```sh
kompile spec-meta-k/al/main.k --main-module AL --syntax-module AL-SYNTAX -o al-kompiled
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

`intN(119)` and `intN(89)` correspond to the oracle's `INT +119` and `INT +89`.
Use `--output json` to diff mechanically rather than by eye.
