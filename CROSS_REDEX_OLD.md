# Specifying P4-SpecTec AL in PLT Redex

Status: Steps 1 (syntax), 2 (s-expression bridge), 3 (common metafunctions),
4 (AL environments and context), 5 (type casts and subtyping), 6
(assignment), and 7 (expressions, premises, and calls) are done. Steps 8–11
are planned.

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

Languages, judgment forms, and metafunctions are named in Racket's
kebab-case: a watsup name is lowercased, and every `_` becomes `-`.
`Eval_exp` becomes `eval-exp` and `$find_map` becomes `find-map`. A trailing
`_` stays as `-`, so `$exists_` becomes `exists-`: `define-metafunction`
binds its name in the module, and `assoc` would shadow Racket's `assoc`
*(checked)*. `'` is a Racket reader delimiter, so `$subst_typ'` becomes
`subst-type-inner`. Nonterminals and metavariables keep their watsup names,
except that primed metavariables `C'` and `C''` become `C_1` and `C_2`.
watsup subscripts (`exp_l`, `val_h`) are already Redex subscripts. Repeated
metavariables are equality constraints in both languages.

In this file, `$f` and `Eval_exp` name watsup definitions, and kebab-case
names their Redex transcriptions.

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
  [(is-tup (TUP (val ...))) #t]
  [(is-tup val) #f
   (side-condition (not (redex-match? al (TUP (val ...)) (term val))))]
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
  `Eval_rulgroup/fail`) become explicit `"fail"` rules, spread over the
  auxiliary judgments described in the next section. Together they are the
  complement of the success rules. In the SL interpreter, a premise whose
  relation has no derivation also makes its rule fail, so the complement
  covers that case too.

Two more kinds of case need explicit handling:

- **Iteration over two sequences.** A watsup iteration over two sequences,
  such as `$subtyp(tdenv, typ, val)*` over `typ*` and `val*`, only applies
  when the lengths are equal. In Redex, a template over lists of different
  lengths raises an error instead of failing to match *(checked)*. Such
  clauses need an explicit length constraint, and their complement needs its
  negation. The constraint is a named ellipsis where the sequences are bound
  (`(TUP (typ ..._n)) (TUP (val ..._n))`), or else a side-condition. A named
  ellipsis bound in a clause's pattern also constrains its `where` patterns
  *(checked)*.
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
    one of the cases its complement covers. A `"fail"` rule matches it with
    the literal pattern: `(where ⊥ (binop-number numbinop num_l num_r))`.

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
[(eval-exp C exp valres)
 (eval-prem/ifpr C valres ctxres)
 ------------------------------- "ifpr"
 (eval-prem C (IF exp) ctxres)]

(define-judgment-form al
  #:mode (eval-prem/ifpr I I O)
  [---------------------------------------- "true"
   (eval-prem/ifpr C (OK (BOOL #t)) (OK C))]
  [---------------------------------------- "false"
   (eval-prem/ifpr C (OK (BOOL #f)) FAIL)]
  [(side-condition ,(not (redex-match? al (OK (BOOL boolean)) (term valres))))
   ---------------------------------------- "fail"
   (eval-prem/ifpr C valres FAIL)])
```

Each auxiliary judgment is named after the watsup rulegroup, or the rule, it
implements (`<judgment>/<group>`), and its rules are named after that group's
rules. The `"fail"` rules hold the cases that watsup's `otherwise` covered.

A later premise is evaluated only if watsup would evaluate it. So when a
group's rules evaluate further premises, each later result goes to a judgment
of its own, `<judgment>/<group>-<premise>`. For example, `eval-exp/binary`
gets the left operand and evaluates the right one only if the left one fits a
rule, and `eval-exp/binary-right` gets both. A watsup rule then has one Redex
rule at each of these steps, all named after it. Rule names are unique within
a judgment, so where a rule's input matched but a pure premise failed, the
complement gets a rule of its own, `"<rule>-fail"`.

The same technique has three variants:

- **Iterated premises.** An iterated premise whose failure matters, such as
  `(Eval_exp: C |- exp : OK val)*`, goes through a sequence judgment
  (`eval-exps`) that stops at the first `FAIL`. Later elements are then not
  evaluated, and their side effects do not happen.
- **Relations that may have no derivation.** `assign-exp(s)`,
  `assign-arg(s)`, `eval-targs`, `call-func`, and `call-rel` have no
  derivation when they don't apply. A caller captures this in one evaluation,
  with
  `(where (C_1 ...) ,(judgment-holds (assign-exp C exp val C_out) C_out))`,
  and then dispatches on the empty or one-element list *(checked)*. Two
  outputs, from overlapping rules, match neither, so they leave no derivation
  rather than a `FAIL`. The captured derivations are not part of the
  caller's.
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
  - Memoize `call-func` and `call-rel` in Racket, with keys as in OCaml.
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
metafunctions (`find-map`, `add-map`, and the rest), which are implemented in
Racket. Step 10 can therefore change their representation without touching a
rule.

### Premises

| watsup premise | Redex premise |
| --- | --- |
| `-- R: C \|- e : OK v` | `(R C e (OK v))`, or `(R C e valres)` plus dispatch when failure matters |
| `-- (R: C \|- e : OK v)*` | `(R C e (OK v)) ...`, or a sequence judgment when failure matters |
| `-- if p = e` (binding) | `(where p e)` |
| `-- (if p = e)*` | `(where (p ...) (e ...))`; constrain the lengths when `e` iterates over several sequences |
| `-- if e` (boolean) | `(where #t e)` |
| `-- if ~e` (boolean) | `(where #f e)` |
| `-- if ~(val <: num)` | `(side-condition ,(not (redex-match? al num (term val))))` |
| `-- otherwise` | the complement of the other clauses; see [Disjoint clauses](#disjoint-clauses) |
| `-- debug e` | `(where _ ,(debug (term e)))`, printing to stderr |
| `$(n - 1)`, `\|x*\|`, `x*[n]`, slices, `x*[[n] = y]`, `++` | Racket escapes; indexing, slicing, and updating are helpers in `common/0.1-stdlib.rkt` (`list-idx`, `text-slice`, ...) |

A `where` pattern that reuses a variable bound earlier in the clause is an
equality constraint, not a new binding *(checked)*, just as a repeated
variable within one pattern is. A binding premise therefore needs a fresh name,
unless watsup repeats the metavariable on purpose.

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
    0.1-stdlib.rkt        language stdlib; $ite, $opt_as_seq_, $exists_, ...; builtins
    1-syntax.rkt          language common
    2-env.rkt             language common-env; $extend_tdenv, $theta_of_tdenv, ...
    3-context.rkt         language common-context (cursor)
    4-relation.rkt        language common-relation (res<X>); the three extern relations
    5.0-eval-typ.rkt      $subst_typ
    5.1-eval-ops.rkt      $unop_number, $binop_*, $cmpop_*, $is_tup, $is_fun
  al/
    0-boot.rkt            boot-script, boot-p4: run spectec-boot sexp(-p4), read
    1-syntax.rkt          language al-syntax
    2-env.rkt             languages al-base (the union) and al-env (reldef, funcdef)
    3-context.rkt         language al-context (layer, ctx, ctx-shallow); $load, $add_*, ...
    4-relation.rkt        language al (ctxres, ctxsres); cons-valsres, cons-ctxsres
    5.1-eval-typ.rkt      $upcast, $downcast, $subtyp
    5.2-eval-assign.rkt   Assign_exp(s), Assign_arg(s)
    5-eval.rkt            includes the five fragments below
    5.3-eval-exp.rktl     Eval_exp, Eval_path, Eval_path_upd; eval-exps, eval-exp-subs
    5.4-eval-arg.rktl     Eval_arg, Eval_targs; eval-args
    5.5-eval-prem.rktl    Eval_prem, Eval_prems; eval-prem-subs
    5.6-eval-call-func.rktl  Eval_clause(s), Eval_tblrow(s), Call_*_func, Call_func
    5.7-eval-call-rel.rktl   Eval_rul(s), Eval_rulgroup(s), Call_*_rel, Call_rel
    6-entry.rkt           Entry
  main.rkt                command-line driver
  test/                   unnumbered: prelude.rkt, syntax.rkt, boot.rkt, ...;
                          judgment.rkt holds helpers for judgment forms
```

The definition macros in `common/0.0-prelude.rkt` are `define-dec`, which wraps
`define-metafunction`, and `define-relation`, which wraps
`define-judgment-form`. `define-dec` appends the `⊥` clause (see
[Disjoint clauses](#disjoint-clauses)). `SPECTEC_REDEX_CONTRACTS=0`, read when
a module is compiled, drops the contracts of both (see
[Verification](#verification)).

Each file in `common/` from `0.1-stdlib` to `4-relation` extends the previous
file's language with its own syntax: `stdlib` (the `var`s, sets and maps),
`common`, `common-env`, `common-context`, and `common-relation`.
`al-syntax` extends `common` with `al/1-syntax`.

`al-base` (in `al/2-env.rkt`) is the `define-union-language` of
`common-relation` and `al-syntax`. `al-env`, `al-context`, and then `al` extend
it with `al/2-env`, `al/3-context`, and `al/4-relation`. Shared nonterminals
merge without duplicate matches *(checked)*. Metafunctions
defined on a `common/` language work on `al` terms, so common helpers stay in
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
meta-level `find-map` in `common/0.1-stdlib.rkt`, which works on Redex's own
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
  loudly if a judgment has more than one derivation, since that means two
  rules overlap. The helpers count derivations, not outputs, because
  `judgment-holds` merges equal outputs of overlapping rules *(checked)*.
  `outputs` in `test/judgment.rkt` does this. For metafunctions, the tests cover both sides of every
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
raco test spec-meta-redex/test                   # everything, about two minutes
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
find "$REV" -name compiled -type d -prune -exec rm -rf {} +   # stale code would not reverse
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

To list the clauses a test file never reaches, here for `$upcast` and
`$subtyp`, run the following. Only the generated `⊥` clause, which Redex
locates at the `define-dec` head, should be listed:

```sh
racket -e '(require redex/reduction-semantics racket/port (file "spec-meta-redex/al/5.1-eval-typ.rkt"))
           (define cs (list (make-coverage upcast) (make-coverage subtyp)))
           (parameterize ([relation-coverage cs] [current-output-port (open-output-nowhere)])
             (dynamic-require (quote (file "spec-meta-redex/test/al-eval-typ.rkt")) #f))
           (for* ([c cs] [p (covered-cases c)] #:when (zero? (cdr p))) (displayln (car p)))'
```

A module's unexported helpers, such as `upcast/var`, need a copy of the
module that provides them. `make-coverage` does not take judgment forms
*(checked)*. For them, a test file ends with `check-rules-used` from
`test/judgment.rkt`, which fails if some rule of a judgment is in none of the
derivations that `outputs` built. `#:except` skips rules that cannot have a
derivation yet, such as the extern stubs. A relation captured with
`judgment-holds` is not in its caller's derivations, so it needs tests of its
own.

`count-calls` in `test/judgment.rkt` counts a judgment's calls in Redex's
trace. The nesting tests use it to check that a shared premise is evaluated
once: the calls at depth 10 must be fewer than 10 times those at depth 4.
Evaluating a premise twice at each level multiplies them by 64.

### Exploring in a REPL

```sh
racket -i -e '(require (file "spec-meta-redex/common/0.0-prelude.rkt")
                       (file "spec-meta-redex/al/0-boot.rkt")
                       (file "spec-meta-redex/al/3-context.rkt"))'
```

Then, for example:

```racket
(term (load (empty-ctx) ,(boot-script "examples/add.watsup")))   ; a loaded context
(redex-match? al-context ctx (term (empty-ctx)))                 ; grammar membership
(current-traced-metafunctions '(find-vari sub-list))             ; print calls and results
(current-traced-metafunctions '())
```

To see a language as a grammar figure (only the nonterminals it adds to the
language it extends):

```sh
racket -e '(require racket/class pict redex/pict (file "spec-meta-redex/al/3-context.rkt"))
           (send (pict->bitmap (language->pict al-context)) save-file "/tmp/al-context.png" (quote png))'
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
- `common/1-syntax.rkt`: `(define-language common ...)` with one nonterminal
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
  `stdlib` in `common/0.1-stdlib.rkt`, which `common` extends. `extern syntax
  json` becomes a `json` that matches what `jsexpr?` accepts.
- `al/1-syntax.rkt`: `(define-extended-language al-syntax common ...)` adding
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
  term matching `script` in `al-syntax` (`redex-match?`). This is the first
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
    with type parameters or a non-`ALIAS` body, `$subst_typ'` on a bound
    `VAR` with type arguments, and `$binop_number` and `$cmpop_number` on
    mixed `NAT` and `INT`.
  - `$is_iter_on_var`'s `ITER` clause also requires `iterexp` to have exactly
    one variable, with the same id and inner iterators. (The K port does not
    check this.) Its premise result goes through a helper,
    `is-iter-on-var/iter`, so it is computed once.
  - `num.ml` has no `POW` and asserts false on division by zero. Here both
    division by zero and a negative exponent raise an error.
  - `cmpop-poly` compares with `equal?`. For `EXT` values this compares
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
    the maps, with the remaining script matched as `any`. `load-typdef`,
    `load-reldef`, and `load-funcdef`, which only `$load` calls, take a
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
- Outcome:
  - A clause whose premise results decide between it and `otherwise` computes
    them once and passes them to a helper named after it: `upcast/var`,
    `upcast/tup`, `upcast/opt`, and `upcast/list`, the same four for
    `$downcast`, and `subtyp/var`. The helpers branch on the `typdef` that
    `$find_typ` or `$find_map` found, or on the component casts. The
    complements in `subtyp/var` recompute the pure premises (`$subst_typ`,
    `$assoc_`, and the membership check), but never `$subtyp`, which appears
    only in results.
  - `$upcast` and `$downcast` give `FAIL` only where `INT` (for `$downcast`,
    `NAT`) meets a non-number, or `TUP` meets a non-tuple, possibly through
    aliases. Every other miss is `OK val`, with the value unchanged. That
    includes a failing component cast, so a component's `FAIL` never reaches
    the caller: `$upcast(C, TUP INT, TUP (BOOL true))` is
    `OK (TUP (BOOL true))`. It also includes `$downcast(C, NAT, INT -1)`, which
    is `OK (INT -1)`, and a `VAR` that is not found, is not an `ALIAS`, gets
    the wrong number of type arguments, or fails to substitute.
  - The K port differs: its `resTup`, `resOpt`, and `resList` turn a failing
    component into `FAIL`, and its `zipThetaMap` has no rule for the wrong
    number of type arguments.
  - `$subtyp` has no `FUNC` clause, so no value matches type `FUNC`.
  - `al/5.1-eval-typ.rkt` is written on `al-context`. Nothing it uses is in
    `al/4-relation`, so that module and the `al` language wait for Step 7.
  - Every clause except `⊥` is reached by the tests (checked with
    `make-coverage`; see [Tests](#tests)). One test counts calls in Redex's
    trace output: an upcast of tuples nested 8 deep, each with a failing
    sibling, makes 17 calls. A scratch variant that recomputed the components
    in its complement made 1,021.
  - With every metafunction's clauses reversed, the whole suite still passes.

### Step 6: Assignment

Transcribe `al/5.2-eval-assign`: `Assign_exp`, `Assign_exps`, `Assign_arg`,
and `Assign_args` as the judgment forms `assign-exp`, `assign-exps`,
`assign-arg`, and `assign-args`. These relations have no `otherwise` and no
`FAIL` output. An assignment that does not apply simply has no derivation.
Callers capture that with `judgment-holds` (see
[Evaluating each premise once](#evaluating-each-premise-once)).

- The four `Assign_exp/iter` rules are disjoint by the result of
  `$is_iter_on_var` (a pure call) and by the shape of the value: `simple`,
  `opt-none`, `opt-some`, and `list`. Test them hardest.
- Tests: cover every constructor, including values that match no rule.
- Outcome:
  - Each watsup rule is one Redex rule, with the same premises in the same
    order. Rules inside a rulegroup are named with it, as in `"opt/opt-some"`
    and `"iter/list"`. No two rules evaluate a relation premise on the same
    input: the `iter` rules check the pure `$is_iter_on_var` and the value's
    shape before their `assign-exp` premises.
  - `iter/opt-some` deviates from its source, by the user's decision. The
    source's last premise, `$add_varis(C', vari_iter*, OPT val_sub)*`,
    elaborates to a call per `val_sub`, each with the one value
    `OPT val_sub`, and expects exactly one result. As written, it assigns only
    when `iterexp` has exactly one variable. With two or more, `$adds_map`
    raises: in a probe, the meta-circular run gave `OPT Some(NAT 3)` for
    `(W a)? = ...` and crashed on `(a, b)? = ...` with
    `Invalid_argument List.fold_left2`. With none, the rule fails. The Redex
    rule makes the intended single call with `(OPT val_sub)*`, the form
    `5.5-eval-prem.watsup:67` uses, and binds every variable, as OCaml's AL
    interpreter and the K port do. `spec-meta/` is unchanged, so on these
    inputs the meta-circular oracle disagrees with Redex and K.
  - Other behaviour the tests pin down:
    - An expression other than `VAR`, `TUP`, `INJ`, `STR`, `OPT`, `LIST`,
      `CONS`, and `ITER` has no rule, including a literal. (The elaborator
      turns a literal in a pattern into a fresh variable plus a check: the `3`
      in `(a, 3)?` became a variable `nat`.)
    - A repeated variable takes the last value, with no equality check.
    - `iter/simple` binds the iterated variable to any value.
    - `iter/opt-some` keeps the inner assignment's bindings in the context.
      `iter/list` does not, because it assigns each element under an empty
      local `VAL` map. For the same reason, `iter/list` does not find a
      variable bound only outside the iteration.
  - `test/judgment.rkt` provides `outputs`, which fails on more than one
    derivation, and `check-rules-used`. Every rule of the four relations
    appears in some derivation in the tests.

### Step 7: Expressions, premises, and calls

Build `al/5-eval.rkt` and its five fragments. The auxiliary dispatch
judgments, the `"fail"` rules, and the `eval-exps` sequence judgment first
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
- Outcome:
  - 101 judgment forms with 272 rules: the 22 watsup relations, 4 sequence
    judgments, and 75 auxiliary judgments (see
    [Evaluating each premise once](#evaluating-each-premise-once)). A rule
    whose premises cannot fail stays in the main judgment
    (`"literal/boolean"`, `"root"`). A group whose rules share a premise has
    one rule there, named after the group, which evaluates the premise and
    hands the result on.
  - Premises keep watsup's order, and none is evaluated where watsup would
    not evaluate it. `Eval_exp/binary`, `compare`, and `concat` check the left
    operand's kind before evaluating the right one. `Eval_path/slice` checks
    each index before evaluating the next. `Eval_path_upd/slice` checks that
    the new value is a text or a list before evaluating anything. The tests
    put `(BIN DIV (NAT 1) (NAT 0))`, which raises, where a premise must not be
    evaluated. The K port evaluates both operands of a binary operator
    regardless.
  - The sequence judgments `eval-exps`, `eval-args`, `eval-exp-subs`, and
    `eval-prem-subs` stop at the first `FAIL`. The elaborated AL evaluates
    every element before it checks any, so the two differ only when a later
    element raises or has a side effect. `CONS`, `MEM`, and `UPD` evaluate
    their two operands with `eval-exps` too, which keeps watsup's order.
    `Eval_exp/opt`'s two rules are one rule over `(OPT (exp ...))`, whose
    premise is `(Eval_exp: C |- exp : OK val)?`.
  - `Eval_prem/iterpr-*` and `Eval_exp/iter` decide between their rules by
    pure calls (`$sub_opt`, `$sub_list`, `$find_varr`), so their main rule
    hands on the premise's parts instead of a result. `(where ⊥ ...)` covers a
    `$sub_opt` or `$sub_list` with no clause.
  - Deviations, by the user's decision (see
    [Step 6](#step-6-assignment) for the first):
    - `Eval_exp/call` passes `typ_input*` to `Call_func`. watsup computes it
      with `Eval_targs` and then passes the unsubstituted `targ*`, so a type
      parameter passed on through a polymorphic call was resolved in the
      callee. OCaml's AL interpreter (`eval_call_exp`) and the K port
      substitute. A test calls `$g<X>` from a context where `X` is `NAT`.
    - `Eval_path_upd/slice/list` takes `LIST val_n*`, whose elements replace
      the slice, as the text rule takes `TEXT t_n`. watsup writes
      `val*[[n_i : n_n] = val_n]`, which the elaborator makes `[val_n]`: the
      meta-circular run gives `[1, [8], 3, 4]` for `[1, 2, 3, 4][[1 : 1] = [8]]`
      and errors on a longer slice. As in OCaml's AL interpreter, a list of
      another length than the slice raises an error.
    - On these inputs the meta-circular oracle disagrees with Redex.
  - Texts are indexed by UTF-8 bytes, as OCaml's `String` is: `|"é"|` is 2.
    A result that splits a character raises an error. Racket's own string
    operations count characters, so the helpers in `common/0.1-stdlib.rkt`
    work on bytes.
  - The elaborator adds `n < |x|` to `Eval_path/idx`, so an index out of
    bounds is `FAIL`. Nothing bounds slices or updates, so an index or slice
    out of bounds there raises an error, as in the meta-circular run. The K
    port gives `FAIL` for all of them.
  - `debug` writes the Redex term to stderr.
  - Tests: `test/al-eval-{exp,arg,prem,call-func,call-rel,examples}.rkt`.
    Every rule is in some derivation, except the stubs for the three extern
    relations. Every auxiliary judgment that gets a judgment's result has a
    nesting test, which also checks that the nesting reaches its bottom. A
    scratch variant that evaluated `UN`'s operand twice went from 31 calls at
    depth 4 to 2,047 at depth 10. The suite passes with contracts off and
    with every metafunction's clauses reversed.
  - `$main()` of the seven examples that need no builtins gives the
    meta-circular oracle's values: `add` 119, `fibo` 89, `iter-nontrivial`
    -42, `iter-sequence` 1085, `mutual-recursion` 289, `relation-typing` 110,
    and `variant-tree` 6. With contracts on, these take 16 ms, 10 s, 0.3 s,
    18 s, 52 s, 5.8 s, and 4.9 s; with them off, about 20% less. A profile of
    `fibo` spends 98% in Redex's matcher: every rule's conclusion checks `C`
    against `ctx` in full. That is Step 10's second item. The tests run the
    four fast examples.

### Step 8: Entry and driver

- `al/6-entry.rkt`: `Entry` as `#:mode (entry I O)`. It `$load`s the script
  into the empty context, then evaluates `(CALL "main" () ())`.
- `main.rkt`: `racket spec-meta-redex/main.rkt FILE.watsup` boots the file,
  derives `entry`, checks that there is exactly one result, and prints it in
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
- Replace the Step 3 stubs for `call-builtin-func`, `call-extern-func`, and
  `call-extern-rel`, and add the native map builtins.
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
  3. add OCaml-style caching for `call-func` and `call-rel`, and memos for hot
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
