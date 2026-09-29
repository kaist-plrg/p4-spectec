# Specifying P4-SpecTec AL in PLT Redex

Status: Steps 1 (syntax), 2 (s-expression bridge), 3 (common metafunctions),
and 4 (AL environments and context) are done. Steps 5–11 are planned.

`spec-meta-redex/` will hold a PLT Redex specification of the P4-SpecTec AL
meta-language. It transcribes the AL specification in
[`spec-meta/common`](spec-meta/common) and [`spec-meta/al`](spec-meta/al) rule
by rule, as [`spec-meta-k/`](spec-meta-k) does in K. Like the K version, it
should be able to:

- run a single P4-SpecTec program that defines `$main()`, and
- run the P4 specification in `spec/` to type-check a P4 program.

This adds a third executable AL semantics next to the OCaml interpreter and
the K port. Redex can also typeset the rules as inference-rule figures.

SL (`spec-meta/sl`) is out of scope. The same layout can take it later and
reuse `common/`.

## Design

The experiments behind the claims marked *(checked)* were run against the
installed Redex (Racket 9.3).

### Big-step judgments, as in the source

AL is specified with big-step relations (`ctx |- exp : res<val>`). Redex's
`define-judgment-form` with `#:mode` computes these directly, so most AL rules
become one Redex rule with the same premises in the same order. The exception
is rules that watsup tells apart by the result of a shared premise; see
[Evaluating each premise once](#evaluating-each-premise-once). The K port had
to turn every relation into a continuation machine on `<k>`
(`evalExp(E) ~> binaryAwaitLeftK(...)`). The Redex port needs no reduction
relations or evaluation contexts.

AL premises are already in algorithmic, left-to-right order, so Redex's
static mode check should accept them unchanged. A mode error means a
transcription slip.

### Relations and functions

The split is the same as in the K port. watsup's `relation` and `dec`/`def`
must stay distinguishable:

| watsup | Redex |
| --- | --- |
| `relation R` and its `rule R/...` | `define-judgment-form`; `hint(input ...)` gives the `I` positions, the rest are `O` |
| `dec $f` and its `def $f` | `define-metafunction` |
| `builtin dec $f` | metafunction whose body escapes to Racket |
| `extern relation R` | judgment form with one rule that calls Racket |

Names stay as in watsup. `Eval_exp`, `Call_func`, `find_map` and `subst_typ`
are all legal Redex names *(checked)*. The one exception is `'`, which is a
Racket reader delimiter: `$subst_typ'` becomes `subst_type_inner`, and primed
metavariables `C'` and `C''` become `C_1` and `C_2`. watsup subscripts
(`exp_l`, `val_h`) are already Redex subscripts. Repeated metavariables are
equality constraints in both languages.

Rule names combine the rulegroup and the rule, because watsup reuses rule
names across groups. For example, `Eval_exp/boolean` appears in both `literal`
and `unary`, so the Redex rules are named `"literal/boolean"` and
`"unary/boolean"`.

### Disjoint clauses

Every metafunction and every judgment form has disjoint clauses: for any
input, at most one clause applies, whatever order the clauses are in. Each
clause is told apart from the others by its input patterns and
side-conditions. Redex checks neither property. A metafunction silently takes
the first clause that applies, and a judgment form silently returns every
output it can derive. [Verification](#verification) describes how the port
checks disjointness.

watsup relies on clause order in one place: `-- otherwise`, which means that
no earlier clause applies. Each `otherwise` becomes an explicit complement:

- **Metafunctions:** the `otherwise` clause gets the negation of the earlier
  clauses' conditions:

  ```racket
  ;; def $is_tup(TUP val*) = true
  ;; def $is_tup(val) = false  -- otherwise
  [(is_tup (TUP (val ...))) #t]
  [(is_tup val) #f
   (side-condition (not (redex-match? AL (TUP (val ...)) (term val))))]
  ```

  The complement must also cover an earlier clause whose pattern matched but
  whose premises failed. For example, `$upcast(C, TUP typ*, TUP val*)` fails
  when some component upcast fails, and watsup's `otherwise` then returns
  `OK val`. The complement therefore includes "some component upcast fails".
  Compute such results once and branch on them with a helper metafunction,
  rather than computing them in both clauses. Transcribe cases like this
  exactly as the spec states them; `$upcast` stays as it is.
- **Judgment forms:** the nine `otherwise` rules (`Eval_exp/fail`,
  `Eval_path/fail`, `Eval_path_upd/fail`, `Eval_arg/fail`, `Eval_prem/fail`,
  `Eval_clause/fail`, `Eval_tblrow/fail`, `Eval_rul/fail`, and
  `Eval_rulgroup/fail`) become explicit `"fail"` rules. Each is the
  complement of the success rules, and each lives in an auxiliary judgment
  described in the next section.

Two more kinds of case need explicit handling:

- **Iteration over two sequences.** A watsup iteration over two sequences,
  such as `$subtyp(tdenv, typ, val)*` over `typ*` and `val*`, only applies
  when the lengths are equal. In Redex, a template over lists of different
  lengths raises an error instead of failing to match *(checked)*. Such
  clauses need an explicit length side-condition, and their complement needs
  its negation.
- **Partial `def`s.** Some `def`s have no clause for some inputs;
  `$find_vari` has none for an unbound variable. In P4-SpecTec, such a call is
  a failing premise, not an error. A Redex metafunction raises an error when
  no clause applies, so the definition macro appends one last clause,
  `[(f any ...) ⊥]`, and adds `⊥` to the range. `⊥` is not a term of any AL
  nonterminal, so a premise `(where p (f ...))` fails on it *(checked)*,
  unless `p` is `any` or `_`. This
  generated clause stands for "no clause matched". It is not a transcribed
  clause, and it is the only clause whose position matters. Two rules follow
  from it:
  - Bind every metafunction call with `where` before using its result, and
    match it against a pattern narrower than `any`. `⊥` can then only make a
    premise fail, and never flows into a term.
  - When a rule dispatches on a call's result instead of matching it, `⊥` is
    one of the cases its complement covers.

  Judgment forms need nothing extra: a relation with no applicable rule has no
  derivation, and the premise that called it fails.

### Evaluating each premise once

In a judgment form, every rule whose conclusion matches the input evaluates
its own premises, so a premise shared by two rules is evaluated twice. watsup
sometimes tells rules apart only by a shared premise's result:

```text
rule Eval_prem/true:   C |- IF exp : OK C   -- Eval_exp: C |- exp : OK (BOOL true)
rule Eval_prem/false:  C |- IF exp : FAIL   -- Eval_exp: C |- exp : OK (BOOL false)
```

The explicit `"fail"` rules create the same situation for every rule that has
a premise. Without the cache (see [Caching](#caching)), the cost doubles with
each level of nesting. With `IF`s nested 10 deep, the innermost expression was
evaluated 1024 times, and only once after factoring *(checked)*. Evaluating a
premise again would also repeat its side effects, such as `$fresh_typeId`.

A shared premise therefore moves into a single rule that binds its whole
result. An auxiliary judgment then dispatches on that result, and its rules
are disjoint by input:

```racket
[(Eval_exp C exp valres)
 (Eval_prem/ifpr C valres ctxres)
 ------------------------------- "ifpr"
 (Eval_prem C (IF exp) ctxres)]

(define-judgment-form AL
  #:mode (Eval_prem/ifpr I I O)
  [---------------------------------------- "true"
   (Eval_prem/ifpr C (OK (BOOL #t)) (OK C))]
  [---------------------------------------- "false"
   (Eval_prem/ifpr C (OK (BOOL #f)) FAIL)]
  [(side-condition ,(not (redex-match? AL (OK (BOOL boolean)) (term valres))))
   ---------------------------------------- "fail"
   (Eval_prem/ifpr C valres FAIL)])
```

Each auxiliary judgment is named after the watsup rulegroup it implements, and
its rules are named after that group's rules. Every watsup rule still has one
Redex counterpart, and the `"fail"` rules hold the cases that watsup's
`otherwise` covered.

The same technique has three variants:

- **Iterated premises.** An iterated premise whose failure matters, such as
  `(Eval_exp: C |- exp : OK val)*`, goes through a sequence judgment
  (`Eval_exps`) that stops at the first `FAIL`. Later elements are then not
  evaluated, and their side effects do not happen.
- **Relations with no `FAIL` output.** `Assign_exp(s)` and `Assign_arg(s)`
  simply have no derivation when they don't apply. A caller captures this in
  one evaluation, with
  `(where (C_1 ...) ,(judgment-holds (Assign_exp C exp val C_out) C_out))`,
  and then dispatches on the empty or one-element list *(checked)*.
- **Repeated calls.** A pure metafunction call may appear in several
  complementary rules, because repeating it costs only time. A judgment
  premise may not, because it evaluates AL and can reach side effects.

### Caching

Redex caches the results of metafunctions and judgment forms under one global
parameter, `caching-enabled?`. It stores every result and has no notion of
side effects. In a probe, a judgment backed by a counter stood in for
`$fresh_typeId`. With caching on, two calls both returned `FRESH__0`; with it
off, they returned `FRESH__0` and `FRESH__1` *(checked)*.

This is how pure `spec-meta/{common,al}` is:

- **Pure:** every `dec`/`def`. No `def` has a relation premise, and the only
  builtins are the list and map ones. The judgments that only call them are
  also pure: `Assign_exp(s)`, `Assign_arg(s)`, and `Eval_targs`.
- **Impure at the source:** `Call_extern_func`, `Call_builtin_func`, and
  `Call_extern_rel`, which reach host state. The OCaml AL interpreter counts a
  change to its builtin counter (`$fresh_typeId`) or to extern state as a side
  effect, and does not cache a call that made one (see the two `CCache.add`
  sites in [`interp-al/interp.ml`](p4spec/lib/interp/interp-al/interp.ml)).
  The `debug` premises (`Eval_prem/dbg`, `Entry`) write output.
- **Impure transitively:** every other relation, since each one can reach
  `Call_func` or `Call_rel`.

The policy that follows:

- **Caching is off.** `caching-enabled?` is set to `#f` in `common/0.0-prelude.rkt`,
  which every module requires. The two previous sections ensure that nothing
  is evaluated twice, so the cache isn't needed. Turning it off also disables Redex's memo of
  nonterminal matches, which Step 4 measured as most of `$load`'s cost.
- **If Step 10 needs caching back,** it follows the OCaml policy instead of
  Redex's:
  - Memoize `Call_func` and `Call_rel` in Racket, with keys as in OCaml.
  - Skip extern calls, calls to functions passed as arguments, and
    higher-order calls.
  - Keep a result only if no side effect happened during the call. The host
    reports this in each `extern-serve` response.

  Pure metafunctions that profile hot can get their own memo, for example
  `$subtyp`, which the OCaml interpreter caches in `sub_cache`.

### Term encoding

Terms mirror the watsup constructors:

| watsup | Redex term |
| --- | --- |
| case `ATOM x y` | `(ATOM x y)`; nullary cases are bare symbols (`NAT`, `QUEST`, `ROOT`, `FAIL`) |
| `x*` field | one list `(x ...)` |
| `x?` field | a list of length 0 or 1 |
| mixfix separators `':'`, `'->'`, `'='`, `'-'` | dropped |
| untagged tuple syntax (`rulmatch`, `clause`, `vari`, `valfield`, ...) | plain list |
| `id`, `atom`, `text` | Racket string |
| `nat`, `int` | exact integer |
| `bool` | `#t` / `#f` |
| `json` (in `EXT json`) | a jsexpr from Racket's `json` library; `json` matches what `jsexpr?` accepts |
| `mixop = atom**` | `((atom ...) ...)` |
| `eps` | `()` |
| struct `{F1 x, F2 y}` | `{F1 x F2 y}`; Racket reads `{}` as parentheses |

For example:

```racket
(CALL "fibo" () ((EXP (VAR "n"))))      ; CALL id targ* arg*
(OPT ())   (OPT ((NAT 3)))              ; OPT val?
(INJ ((("Some") ()) ((NAT 3))))         ; INJ (mixop val*)
(REL "Sub" ((VAR "t1") (VAR "t2")) ())  ; REL id ':' exp* '->' exp*
```

Because `x?` is a list, the pattern `(OPT (val ...))` binds `val` at depth 1.
watsup's `?`-iteration then becomes Redex's `...`, the same as `*`-iteration:

```racket
;; def $upcast(C, ITER typ QUEST, OPT val?) = OK (OPT val_upcast?)
;;   -- if (OK val_upcast = $upcast(C, typ, val))?
[(upcast C (ITER typ QUEST) (OPT (val ...)))
 (OK (OPT (val_upcast ...)))
 (where ((OK val_upcast) ...) ((upcast C typ val) ...))]
```

Meta-level maps (`venv`, `tdenv`, `fenv`, `renv`, `theta`) are association
lists `((key value) ...)`. They are only accessed through the builtin
metafunctions (`find_map`, `add_map`, and the rest), which are implemented in
Racket. Step 10 can therefore change their representation without touching a
rule.

### Premises

| watsup premise | Redex premise |
| --- | --- |
| `-- R: C \|- e : OK v` | `(R C e (OK v))`, or `(R C e valres)` plus dispatch when failure matters |
| `-- (R: C \|- e : OK v)*` | `(R C e (OK v)) ...`, or a sequence judgment when failure matters |
| `-- if p = e` (binding) | `(where p e)` |
| `-- (if p = e)*` | `(where (p ...) (e ...))`; add a length side-condition when `e` iterates over several sequences |
| `-- if e` (boolean) | `(where #t e)` |
| `-- if ~(val <: num)` | `(side-condition ,(not (redex-match? AL num (term val))))` |
| `-- otherwise` | the complement of the other clauses; see [Disjoint clauses](#disjoint-clauses) |
| `-- debug e` | `(where _ ,(debug (term e)))`, printing to stderr |
| `$(n - 1)`, `\|x*\|`, `x*[n]`, slices, `x*[[n] = y]`, `++` | Racket escapes, collected as helpers in `common/0.1-stdlib.rkt` |

### Layout and languages

Racket modules cannot require each other in a cycle, but AL's evaluation
relations are mutually recursive (`Eval_exp` → `Call_func` → `Eval_clauses` →
`Eval_prems` → `Eval_exp`). The watsup files 5.3–5.7 therefore become
fragments that are `include`d into one module *(checked)*. Within that module,
judgment forms can refer to ones defined later *(checked)*. Every other watsup
file becomes one module.

Modules in `common/` never require modules in `al/`. That is why the extern
codec and wire live in `common/`: the extern relations in
`common/4-relation.rkt` call them.

```text
spec-meta-redex/
  common/
    0.0-prelude.rkt       Redex re-exports; caching off; definition macros
    0-extern-json.rkt     codec for the extern JSON wire
    0-extern-wire.rkt     transport to the OCaml host
    0.1-stdlib.rkt        language Stdlib; $ite, $opt_as_seq_, $exists_, ...; builtins
    1-syntax.rkt          language Common
    2-env.rkt             language Common-env; $extend_tdenv, $theta_of_tdenv, ...
    3-context.rkt         language Common-context (cursor)
    4-relation.rkt        language Common-relation (res<X>); the three extern relations
    5.0-eval-typ.rkt      $subst_typ
    5.1-eval-ops.rkt      $unop_number, $binop_*, $cmpop_*, $is_tup, $is_fun
  al/
    0-boot.rkt            boot-script, boot-p4: run spectec-boot sexp(-p4), read
    1-syntax.rkt          language AL-syntax
    2-env.rkt             languages AL-base (the union) and AL-env (reldef, funcdef)
    3-context.rkt         language AL-context (layer, ctx, ctx-shallow); $load, $add_*, ...
    4-relation.rkt        ctxres
    5.1-eval-typ.rkt      $upcast, $downcast, $subtyp
    5.2-eval-assign.rkt   Assign_exp(s), Assign_arg(s)
    5-eval.rkt            includes the five fragments below
    5.3-eval-exp.rktl
    5.4-eval-arg.rktl
    5.5-eval-prem.rktl
    5.6-eval-call-func.rktl
    5.7-eval-call-rel.rktl
    6-entry.rkt           Entry
  main.rkt                command-line driver
  test/                   unnumbered: prelude.rkt, syntax.rkt, boot.rkt, ...
```

The definition macros in `common/0.0-prelude.rkt` are `define-dec`, which wraps
`define-metafunction`, and `define-relation`, which wraps
`define-judgment-form`. `define-dec` appends the `⊥` clause (see
[Disjoint clauses](#disjoint-clauses)). `SPECTEC_REDEX_CONTRACTS=0`, read when
a module is compiled, drops the contracts of both (see
[Verification](#verification)).

Each file in `common/` from `0.1-stdlib` to `4-relation` extends the previous
file's language with its own syntax: `Stdlib` (the `var`s, sets and maps),
`Common`, `Common-env`, `Common-context`, and `Common-relation`.
`AL-syntax` extends `Common` with `al/1-syntax`.

`AL-base` (in `al/2-env.rkt`) is the `define-union-language` of
`Common-relation` and `AL-syntax`. `AL-env`, `AL-context`, and then `AL` extend
it with `al/2-env`, `al/3-context`, and `al/4-relation`. Shared nonterminals
merge without duplicate matches *(checked)*. Metafunctions
defined on a `common/` language work on `AL` terms, so common helpers stay in
`common/`.

### Getting scripts into Redex

`spectec-boot kast` emits KAST JSON for K. Redex gets two sibling
subcommands:

- `spectec-boot sexp PATH [-o FILE]` prints the booted `Al.spec` as one
  s-expression in the encoding above.
- `spectec-boot sexp-p4 -p FILE [-i DIR]... [-o FILE]` prints a P4 program as
  a `val`.

Racket's `read` parses this output directly, so Redex needs no parser. The
emitter is [`sexp.ml`](p4spec/lib/interface/spectec/ali/sexp.ml), which
follows the traversal in [`kast.ml`](p4spec/lib/interface/spectec/ali/kast.ml).
`al/0-boot.rkt` runs these subcommands and `read`s their output, with
`SPECTEC_BOOT` overriding the binary as in the K scripts. `spec/` boots to
2.8 MB in 0.5 s; its KAST JSON is 50 MB.

`sexp-p4` writes the JSON of `EXT json` as a string of JSON text, and
`boot-p4` decodes it with `string->jsexpr`. A value arriving over the extern
wire in Step 9 is decoded by the same library, so both have one encoding.

### Builtins and externs

AL reaches the host through three extern relations: `Call_builtin_func`,
`Call_extern_func`, and `Call_extern_rel`. As in the K port, calls go to the
OCaml implementation over the existing JSON wire
([`extern_json.ml`](p4spec/lib/interface/spectec/ali/extern_json.ml)), so all
three evaluators share host behavior:

```text
Redex rule -> common/0-extern-json.rkt -> common/0-extern-wire.rkt -> spectec-boot extern-serve -> SpecTec runner
```

The transport is a single long-lived `spectec-boot extern-serve SPECDIR`
subprocess per run. It reads one JSON request per line on stdin and writes one
response per line on stdout. Its dispatch is `eval` from
[`kffi.ml`](p4spec/bin/kffi.ml), moved into the library so that `kffi.ml` and
`boot.ml` share it. Starting one process per run, not one per call, avoids the
`#system` failure the K port hit, and keeps `$fresh_typeId`'s counter
consistent across calls. If the pipe turns out to be a bottleneck, the C shim
can instead be loaded as a shared object through Racket's `ffi/unsafe`
(Step 10).

As in the K port, the hot object-level map builtins (`find_map`, `find_maps`,
`add_map`, `adds_map`, `update_map`, `assoc_`) are native Racket that works on
AL map values (`INJ` with the `` `{ `} `` mixop). They are separate from the
meta-level `find_map` in `common/0.1-stdlib.rkt`, which works on Redex's own
environments.

## Verification

- **Oracles.** There are two: the K port (`./spec-meta-k/scripts/k-run.sh
  FILE`, which prints the result as JSON), and the OCaml meta-circular run
  (`./spectec-boot run spec-meta/al -rel Entry -tec FILE -ali`, which prints
  the value through `Entry`'s `debug`). `main.rkt` prints results in
  `k-run.sh`'s JSON format so the outputs can be compared as text.
- **Unit tests.** `test-equal`, `test-judgment-holds`, and `test-match` go
  under `spec-meta-redex/test/` and run with `raco test spec-meta-redex/test`.
  They include inputs on both sides of every complementary pair of clauses.
- **Disjointness.** For judgment forms, `main.rkt` and the test helpers fail
  loudly if a judgment returns more than one output, since that means two
  rules overlap. For metafunctions, the tests cover both sides of every
  complement. Running them with a metafunction's clauses reversed by hand
  (keeping `⊥` last) is a useful spot check: disjoint clauses give the same
  results. It cannot detect overlapping clauses that agree on the overlap,
  such as `$subst_typ` on an empty `theta`.
- **No caching.** A test asserts that `caching-enabled?` is `#f` once
  `common/0.0-prelude.rkt` is loaded. The `$fresh_typeId` test in Step 9 catches a cache
  that comes back by some other route.
- **Contracts.** Every judgment form and metafunction gets a contract built
  from its watsup declaration, which catches transcription slips early.
  Contract checks cost time linear in the size of the terms they check, and
  `C` contains the whole loaded spec. That is why the definition macros have a
  switch to turn contracts off for P4 runs.

## Checking it yourself

Run these from the repository root.

### Booting

```sh
make boot                                        # rebuild spectec-boot after editing sexp.ml
./spectec-boot sexp examples/add.watsup          # a script as a Redex term
./spectec-boot sexp spec -o /tmp/spec.sexp       # a directory, to a file
./spectec-boot sexp-p4 -p p4c/testdata/p4_16_samples/action-bind.p4 -i p4c/p4include
```

To print the elaborated AL of one definition (here `$find_vari`), to compare
it with its transcription:

```sh
racket -e '(require racket/pretty (file "spec-meta-redex/al/0-boot.rkt"))
           (for ([d (boot-script "spec-meta/al")] #:when (equal? (cadr d) "find_vari"))
             (pretty-write d))'
```

### Tests

```sh
raco test spec-meta-redex/test                   # everything, about a minute
raco test spec-meta-redex/test/al-context.rkt    # one file
SPECTEC_REDEX_CONTRACTS=0 raco test spec-meta-redex/test
```

The contract switch is read at compile time, so it only takes effect while
`spec-meta-redex/` has no `compiled/` directories. `test/prelude.rkt` fails if
the loaded code was compiled with the other setting.

To spot-check disjointness, run the suite on a copy whose `define-dec`
reverses every metafunction's clauses. All tests should still pass:

```sh
REV=/tmp/redex-rev
rm -rf "$REV" && mkdir -p "$REV" && cp -r spec-meta-redex "$REV"/
for d in examples spec spec-meta p4c spectec-boot; do ln -s "$PWD/$d" "$REV/$d"; done
python3 - "$REV" <<'EOF'
import sys
p = sys.argv[1] + "/spec-meta-redex/common/0.0-prelude.rkt"
s = open(p).read()
s = s.replace("(with-syntax ([(contract ...)",
              "(with-syntax ([(clause ...) (reverse (syntax->list #'(clause ...)))]\n"
              "                   [(contract ...)", 1)
open(p, "w").write(s)
EOF
(cd "$REV" && raco test spec-meta-redex/test)
```

### Exploring in a REPL

```sh
racket -i -e '(require (file "spec-meta-redex/common/0.0-prelude.rkt")
                       (file "spec-meta-redex/al/0-boot.rkt")
                       (file "spec-meta-redex/al/3-context.rkt"))'
```

Then, for example:

```racket
(term (load (empty_ctx) ,(boot-script "examples/add.watsup")))   ; a loaded context
(redex-match? AL-context ctx (term (empty_ctx)))                 ; grammar membership
(current-traced-metafunctions '(find_vari sub_list))             ; print calls and results
(current-traced-metafunctions '())
```

To see a language as a grammar figure (only the nonterminals it adds to the
language it extends):

```sh
racket -e '(require racket/class pict redex/pict (file "spec-meta-redex/al/3-context.rkt"))
           (send (pict->bitmap (language->pict AL-context)) save-file "/tmp/al-context.png" (quote png))'
```

### Oracles

What a script should evaluate to, for the comparisons from Step 8 on:

```sh
./spectec-boot run spec-meta/al -rel Entry -tec examples/add.watsup -ali   # OCaml, meta-circular
make k-spec && ./spec-meta-k/scripts/k-run.sh examples/add.watsup          # K, JSON output
```

## Steps

### Step 1: Syntax

Transcribe `common/1-syntax.watsup` and `al/1-syntax.watsup` into Redex
languages. This step fixes the term encoding that every later step and the
Step 2 emitter depend on.

- `common/0.0-prelude.rkt`: re-exports `redex/reduction-semantics` and sets
  `caching-enabled?` to `#f`. Every module requires it instead of Redex
  itself, so the caching policy holds from the start. The definition macros
  come in Step 3.
- `common/1-syntax.rkt`: `(define-language Common ...)` with one nonterminal
  per watsup syntax:
  - identifiers: `id`, `atom`, `mixop`
  - types: `numtyp`, `optyp`, `typ`, `deftyp`, `typfield`, `typcase`, `iter`,
    `vari`
  - operators: `num`, `boolunop`, `boolbinop`, `numunop`, `numbinop`,
    `numcmpop`, `polycmpop`, `unop`, `binop`, `cmpop`
  - values: `val`, `valfield`, `valcase`
  - expressions: `exp`, `expcase`, `expfield`, `iterexp`, `listpattern`,
    `optpattern`, `pattern`, `path`
  - arguments: `targ`, `arg`, `tparam`

  The `var` declarations in `common/0-stdlib.watsup` become nonterminal
  aliases: `(bool b ::= boolean)`, `(int i ::= integer)`,
  `(nat n ::= natural)`, and `(text t ::= string)`. Step 3 moved them to
  `Stdlib` in `common/0.1-stdlib.rkt`, which `Common` extends. `extern syntax
  json` becomes a `json` that matches what `jsexpr?` accepts.
- `al/1-syntax.rkt`: `(define-extended-language AL-syntax Common ...)` adding
  `param`, `iterprem`, `prem`, `rulmatch`, `rulpath`, `rulgroup`, `elsgroup`,
  `clause`, `elsclause`, `tblrow`, `defn`, and `script`.
- Replace `a.rkt`.

The `exp` nonterminal shows the encoding:

```racket
(exp ::= (BOOL bool) num (TEXT text) (VAR id)
         (UN unop exp) (BIN binop exp exp) (CMP cmpop exp exp)
         (UPCAST typ exp) (DOWNCAST typ exp) (SUB exp typ) (MATCH exp pattern)
         (TUP (exp ...)) (INJ expcase) (STR (expfield ...))
         (OPT ()) (OPT (exp))
         (LIST (exp ...)) (CONS exp exp) (CAT exp exp) (MEM exp exp) (LEN exp)
         (DOT exp atom) (IDX exp exp) (SLICE exp exp exp) (UPD exp path exp)
         (CALL id (targ ...) (arg ...)) (ITER exp iterexp))
(expcase ::= (mixop (exp ...)))
(expfield ::= (atom exp))
```

Tests in `test/syntax.rkt`:

- `test-match` for every production, and `test-no-match` for near misses
  such as `(OPT ((NAT 1) (NAT 2)))`, an `ITER` with a bad iterator, and bare
  numbers where `num` is expected.
- [`examples/add.watsup`](examples/add.watsup), encoded by hand, matches
  `script`.
- Optional: `render-language` on both languages, to compare against the
  watsup grammar by eye.

Done when `raco test spec-meta-redex/test/syntax.rkt` passes.

### Step 2: The s-expression bridge

- Add `sexp.ml` and the `sexp` and `sexp-p4` subcommands in `boot.ml`, next to
  `kast` and `kast-p4`, then run `make boot`.
- Add a Racket helper, `(boot-script path)`, that runs `spectec-boot sexp` and
  `read`s the result, so that tests and `main.rkt` accept `.watsup` paths
  directly.
- Test: every `examples/*.watsup` file, and the whole of `spec/`, boots to a
  term matching `script` in `AL-syntax` (`redex-match?`). This is the first
  time Step 1's grammar meets real input, so a mismatch is a bug in either the
  grammar or the emitter.

Done when all of them match.

### Step 3: Common metafunctions

Transcribe `common/0-stdlib`, `2-env`, `4-relation`, `5.0-eval-typ`, and
`5.1-eval-ops`: every `dec`/`def` as a metafunction, plus `res<X>`.

- Add the definition macros to `common/0.0-prelude.rkt`: the `⊥` clause, and the contract
  switch.
- Write every `otherwise` as an explicit complement. The ones in this step are
  small: `$extend_tdenv`, `$is_iter_on_var`, `$subst_typ`, `$is_tup`, and
  `$is_fun`.
- Builtins (`$rev_`, `$assoc_`, `$transpose_`, `$find_map`, `$find_maps`,
  `$add_map`, `$adds_map`) are metafunctions that escape to Racket.
- Type parameters (`$ite<X>`, `$repeat_<X>`) are dropped because Redex terms
  are untyped. Contracts use `any` where watsup has a type parameter.
- Arithmetic must match
  [`p4spec/lib/lang/xl/num.ml`](p4spec/lib/lang/xl/num.ml): `DIV` and `MOD`
  truncate, so they map to `quotient` and `remainder`, not `modulo`.
- The extern relations are declared here as judgment forms. Their single rule
  raises an error until Step 9.
- Tests:
  - `test-equal` for each metafunction, covering both sides of every
    complement and the edge cases of `$transpose_` (empty outer list, empty
    rows).
  - An input no clause matches, such as a `$theta_of_tdenv` entry that is a
    `DEF` with type parameters, gives `⊥`. A caller's premise on it fails
    instead of raising an error.
- Outcome:
  - `common/3-context` (`cursor`) got its own module and language.
  - Partial `def`s found here, which give `⊥`: `$theta_of_tdenv` on a `DEF`
    with type parameters or a non-`ALIAS` body, `$subst_type_inner` on a bound
    `VAR` with type arguments, and `$binop_number` and `$cmpop_number` on
    mixed `NAT` and `INT`.
  - `$is_iter_on_var`'s `ITER` clause also requires `iterexp` to have exactly
    one variable, with the same id and inner iterators. (The K port does not
    check this.) Its premise result goes through a helper,
    `is_iter_on_var/iter`, so it is computed once.
  - `num.ml` has no `POW` and asserts false on division by zero. Here both
    division by zero and a negative exponent raise an error.
  - `cmpop_poly` compares with `equal?`. For `EXT` values this compares
    jsexprs, so the key order of a JSON object does not matter, whereas
    OCaml's `Stdlib.compare` on Yojson does distinguish it.

### Step 4: AL environments and context

Transcribe `al/2-env` and `al/3-context`: `reldef`, `funcdef`, `layer`,
`ctx`, and the `$load`, `$add_*`, `$find_*`, `$sub_opt`, and `$sub_list`
metafunctions.

- `C[ .LOCAL.VAL = x ]`-style updates become patterns that rebuild the record,
  `{GLOBAL layer_g LOCAL {TYP tdenv REL renv FUNC fenv VAL venv}}`.
- Complements: `$find_func`'s `otherwise` means "found in neither layer".
  `$find_varis`, `$find_varrs`, and `$finds_vari` fall back when some lookup
  fails. `$sub_opt` has one clause for "every lookup is `OPT val`" and one for
  "every lookup is `OPT eps`". A mix matches neither, so it gives `⊥`, and
  both `Eval_prem/iterpr-opt` rules fail on it.
- Tests: run `$load` on the scripts booted in Step 2 and check which ids end up
  in `GLOBAL`'s `FUNC`, `REL`, and `TYP`. Test `$sub_list` with no iterated
  variables and with empty lists.
- Outcome:
  - `$find_vari` has no clause for an unbound variable, so it gives `⊥`, never
    `eps`. The complements of `$find_varis`, `$find_varrs`, and
    `$finds_vari` repeat their pure lookups in a side-condition, and those of
    `$find_typ` and `$find_func` dispatch on the same lookups.
  - `$sub_opt`'s two clauses overlap on an empty `vari*`, which watsup
    resolves by order: the first applies, giving `C`. The second clause
    requires a non-empty `vari*`.
  - Performance: a nonterminal in a pattern is a deep membership check.
    Redex's matcher memoizes these checks, but only while `caching-enabled?`
    is on, and that parameter also turns on the result cache that AL cannot
    use (see [Caching](#caching)). With caching on, `$load` on `spec-meta/al`
    takes 0.69 s with contracts off and 1.5 s with them on. Written with
    precise patterns, every recursive `$load` call rechecks `C`, the remaining
    `defn_t ...`, and the loaded maps, so `$load` is quadratic in the script.
    On `spec-meta/al` (159 definitions) that took 8.8 s with contracts off and
    20.5 s with them on, and on `spec/` (1,672 definitions) 27 minutes with
    contracts off.
  - So `$load` is the one place with shallow patterns. Its clauses run as
    `load/shallow` on a `ctx-shallow`, which checks the record shape but not
    the maps, with the remaining script matched as `any`. `load_typdef`,
    `load_reldef`, and `load_funcdef`, which only `$load` calls, take a
    `ctx-shallow` too. `$load` itself keeps watsup's signature and checks its
    input and result against `ctx` once. On `spec-meta/al` it takes 0.19 s
    with contracts off and 0.33 s with them on. A scratch variant built the
    same way loaded `spec/` in 8.5 s. The tests do not load `spec/`. Every
    other pattern stays precise until Step 10.
  - With every metafunction's clauses reversed by hand in a scratch copy, the
    whole suite still passes.

### Step 5: Type casts and subtyping

Transcribe `al/5.1-eval-typ`: `$upcast`, `$downcast`, `$subtyp`, and
`$subtyps`. These have the spec's largest complements. The `otherwise` of
`$upcast` and `$downcast` covers every type they don't cast, aliases that
aren't found, component casts that fail, and tuples of the wrong length. The
`otherwise` of `$subtyp` covers every mismatch.

- Compute component results once in a shared clause and branch on them, since
  these metafunctions are recursive and computing them in two clauses
  compounds with depth.
- In `$subtyp`, the `VARIANT` case checks `mixop <- mixop_case*` and then uses
  `$assoc_` to look up the case's types. The `VAR` clauses are disjoint by the
  kind of `typdef` found (`EXT`, `ALIAS`, `VARIANT`, or `STRUCT`).
- Tests: one per clause, and one per complement case.

### Step 6: Assignment

Transcribe `al/5.2-eval-assign`: `Assign_exp`, `Assign_exps`, `Assign_arg`,
and `Assign_args` as judgment forms. These relations have no `otherwise` and
no `FAIL` output. An assignment that does not apply simply has no derivation.
Callers capture that with `judgment-holds` (see
[Evaluating each premise once](#evaluating-each-premise-once)).

- The four `Assign_exp/iter` rules are disjoint by the result of
  `$is_iter_on_var` (a pure call) and by the shape of the value: `simple`,
  `opt-none`, `opt-some`, and `list`. Test them hardest.
- Tests: cover every constructor, including values that match no rule.

### Step 7: Expressions, premises, and calls

Build `al/5-eval.rkt` and its five fragments. The auxiliary dispatch
judgments, the `"fail"` rules, and the `Eval_exps` sequence judgment first
appear here. Work through the relations in this order, with tests before
moving on:

1. `Eval_exp` without `CALL`, plus `Eval_path` and `Eval_path_upd`. Before
   relying on Racket's string operations for text indexing, slicing, and
   length, check which semantics OCaml uses for them.
2. `Eval_targs` and `Eval_arg`.
3. `Eval_prem` and `Eval_prems`, including both `Eval_prem/iterpr-*` groups.
   `Eval_prems/head-fail` and `head-succ` share their premise, so they become
   one rule plus a dispatch.
4. Function calls: `Eval_clause(s)`, `Eval_tblrow(s)`, `Call_table_func`,
   `Call_defined_func`, `Call_func_dispatch`, and `Call_func`, then
   `Eval_exp/call`. The `cons-succ`/`cons-fail` pairs share their premise in
   the same way.
5. Relation calls: `Eval_rul(s)`, `Eval_rulgroup(s)`, `Call_defined_rel`,
   `Call_rel_dispatch`, and `Call_rel`.

- Tests: small hand-built `script` terms for each group, or scripts booted
  through Step 2. Include a nested case for each auxiliary judgment, to catch
  a shared premise that was not factored: with caching off, it shows up as
  exponential running time.

### Step 8: Entry and driver

- `al/6-entry.rkt`: `Entry` as `#:mode (Entry I O)`. It `$load`s the script
  into the empty context, then evaluates `(CALL "main" () ())`.
- `main.rkt`: `racket spec-meta-redex/main.rkt FILE.watsup` boots the file,
  derives `Entry`, checks that there is exactly one result, and prints it in
  `k-run.sh`'s JSON format, or prints `fail`.
- Compile with `raco make spec-meta-redex/main.rkt`, and add the `compiled/`
  directories to `.gitignore`.
- Test: the examples that need no builtins produce the same output as
  `k-run.sh`. Those are `add`, `fibo`, `iter-nontrivial`, `iter-sequence`,
  `mutual-recursion`, `relation-typing`, and `variant-tree`.

### Step 9: Builtins and externs

- `common/0-extern-json.rkt`: a Racket codec for the wire format documented in
  `extern_json.ml` (`val`, `typ`, `mixop`, request, and response), built on
  Racket's `json` library.
- Add `spectec-boot extern-serve`. `common/0-extern-wire.rkt` starts it on the first
  extern call and shuts it down at exit.
- Replace the Step 3 stubs for `Call_builtin_func`, `Call_extern_func`, and
  `Call_extern_rel`, and add the native map builtins.
- Tests:
  - The `builtin-*.watsup` examples match `k-run.sh`.
  - Values from `sexp-p4` survive a round trip through the codec.
  - A script that declares `builtin dec $fresh_typeId` and calls it twice
    gets two distinct ids. This fails if any cache sits between the call and
    the host.

### Step 10: P4 type checking and performance

- `racket spec-meta-redex/main.rkt --p4 PROGRAM spec` boots `spec/`, boots
  PROGRAM with `sexp-p4`, calls the relation `Program_ok` on it the way K's
  `afterLoad` does, and prints `passed` or `fail`.
- Run only the three benchmark programs, smallest first:
  `p4c/testdata/p4_16_samples/action-bind.p4`, `checksum-l4-bmv2.p4`, and
  `dash/dash-pipeline-v1model-bmv2.p4`. Also run the negative
  `p4c/testdata/p4_16_errors/action-bind.p4`, which must print `fail`.
- Expect this to be much slower than K, which already takes about 3 minutes on
  the large program. Profile first (Racket's `profile` library). Then try
  these, roughly in order of expected payoff:
  1. turn contracts off for P4 runs;
  2. match large structures shallowly in rule patterns, as `$load` already
     does with `ctx-shallow` (Step 4). Every evaluation rule has `C` in its
     conclusion, so with precise patterns each step checks the whole loaded
     spec;
  3. add OCaml-style caching for `Call_func` and `Call_rel`, and memos for hot
     pure metafunctions (see [Caching](#caching)). This needs a side-effect
     flag in the responses. Only `extern-serve` adds it, so the K wire is
     unchanged;
  4. store the global maps as Racket immutable hashes behind the same builtin
     metafunctions;
  5. move the wire to `ffi/unsafe`, if extern calls show up in the profile.

  After each change, rerun the `$fresh_typeId` test and the negative program.
- Done when the small program passes and medium and large results are recorded
  with timings. This step answers whether the large program is within reach.

### Step 11: Test targets and docs

- `make redex-test`: runs `raco test spec-meta-redex/test`. It also checks
  the examples against checked-in expected outputs, so K need not be built.
  `SPECTEC_REDEX_CONTRACTS` is read at compile time, so a run with contracts
  off needs its own compiled code (no `compiled/`, or a separate
  `PLTCOMPILEDROOTS`).
- Optional: render every judgment form and metafunction to figures with
  `render-judgment-form` and `render-metafunction`, for side-by-side review
  against the watsup rules.
- Rewrite this file as an overview of the finished port, like
  [`CROSS.md`](CROSS.md).
