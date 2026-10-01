# Specifying P4-SpecTec AL in PLT Redex

Status: in progress. Steps 1 to 6 are done.

`spec-meta-redex/` will hold a PLT Redex specification of the P4-SpecTec AL
meta-language, as a small-step reduction semantics. It transcribes the AL
specification in [`spec-meta/common`](spec-meta/common) and
[`spec-meta/al`](spec-meta/al) rule by rule, and follows the evaluation
structure of the K port in [`spec-meta-k/`](spec-meta-k). Like the K version,
it should be able to:

- run a single P4-SpecTec program that defines `$main()`, and
- run the P4 specification in `spec/` to type-check a P4 program.

This adds a third executable AL semantics next to the OCaml interpreter and
the K port. Redex can also draw the grammar and the reduction rules as
figures, and step through a small run in its `traces` GUI.

SL (`spec-meta/sl`) is out of scope. The same layout can take it later and
reuse `common/`.

## Design

Claims marked *(measured)* come from probes run against the installed Redex
(Racket 9.3) while this plan was written. Most probes used toy languages.
Steps 6 and 13 repeat the measurements on the real rules.

### Small-step, as in K

watsup specifies AL with big-step relations (`ctx |- exp : res<val>`). The
Redex port turns them into a reduction relation on configurations, as the K
port turns them into rewrite rules on `<k>`. A watsup premise that evaluates a
sub-relation becomes an evaluation position. The rule's remaining premises run
once that position holds a result.

The K port is the model for the evaluation structure, not for the code:

| K (`spec-meta-k/`) | Redex (`spec-meta-redex/`) |
| --- | --- |
| `<global>` cells | `G`, a component of the configuration |
| `<local>` cells (`ltdenv`, `lfenv`, `lvenv`) | `L` in the innermost `(IN L ...)` around the redex |
| `evalExp(E) ~> binaryAwaitLeftK(O, R)` | the nested term `(BIN binop exp_l exp_r)`, whose frame `(BIN binop hole exp)` says where evaluation proceeds |
| `pushLocalSave`/`popLocalSave` around clause and rule attempts and iterations | each attempt or iteration runs in its own `(IN L ...)`; the enclosing `IN` keeps the original layer |
| `pushCallFrame`/`popCallFrame` | an `(IN L_callee ...)` nested inside the caller's term |
| `<caller>`'s `cfenv` | the caller's layer, carried as an explicit argument of the clause forms |
| one `FAIL ~> xK => FAIL` rule per marker | one rule for every frame that FAIL passes through |
| `[owise]` | explicit complement rules |

Where K and watsup differ, the port follows watsup (the elaborated AL). For
example, K evaluates both operands of a binary operator, and gives `FAIL` for
out-of-bounds slices.

### Relations and functions

The split between watsup's `relation` and `dec`/`def` stays visible:

| watsup | Redex |
| --- | --- |
| `relation R` and its `rule R/...` | reduction rules on R's machine form (see [Machine terms](#machine-terms)) |
| `dec $f` and its `def $f` | `define-metafunction`, through `define-dec` |
| `builtin dec $f` | a metafunction whose body escapes to Racket (list and map builtins) |
| `extern relation R` | one reduction rule whose contractum a Racket procedure computes by calling the host |

Every relation becomes reduction rules. That includes the pure ones
(`Assign_exp(s)`, `Assign_arg(s)`, `Eval_targs`), which run as machine steps
that update the local layer, as in K. The port has no judgment forms.

Languages, metafunctions, machine forms, and reduction relations are named in
Racket's kebab-case: a watsup name is lowercased, and every `_` becomes `-`.
`Eval_exp` becomes `eval-exp` and `$find_map` becomes `find-map`. A trailing
`_` stays as `-`, so `$exists_` becomes `exists-`, because `define-metafunction`
binds its name in the module, and `assoc` would shadow Racket's `assoc`. `'` is
a Racket reader delimiter, so `$subst_typ'` becomes `subst-typ-inner`.
Nonterminals and metavariables keep their watsup names, except that primed
metavariables `C'` and `C''` become `C_1` and `C_2`. watsup subscripts
(`exp_l`, `val_h`) are already Redex subscripts. Repeated metavariables are
equality constraints in both languages. `G` and `L` are new metavariables for
the two layers of watsup's `C`.

In this file, `$f` and `Eval_exp` name watsup definitions, and kebab-case
names their Redex transcriptions.

Rule names combine the relation, the rulegroup, and the rule, for example
`"eval-exp/unary/boolean"` and `"eval-exp/unary/fail"`. watsup reuses rule names
across groups and relations, and `union-reduction-relations` rejects two rules
with the same name *(measured)*.

### Configurations

```racket
(conf ::= (G e))
```

`G` is the global layer that `$load` builds. It never changes during a run,
because no reduction rule writes it. `e` is a machine term, and `(IN L e)`
inside it means that `e` is evaluated under the local layer `L`. watsup's `C`
at a redex is the pair of `G` and the layer of the innermost `IN` around the
redex. A run starts from `(G (IN L_0 (CALL "main" () ())))`, where `L_0` is
`$empty_layer`, and ends at `(G (IN L res))`, where `res` is a result.

An `IN` node appears wherever watsup builds a new context, or uses one context
for several attempts:

- **A callee.** `Call_defined_func`, `Eval_tblrow`, and `Call_defined_rel`
  build `C[ .LOCAL = ... ]`, so they reduce to `(IN L_callee ...)`.
- **An attempt.** `Eval_clauses/cons-fail` and `Eval_ruls/cons-fail` pass the
  same `C` to the next attempt. So each clause, and each rule path, runs in its
  own `(IN L ...)` with a copy of the enclosing layer, and a failed attempt
  leaves nothing behind.
- **An iteration.** `$sub_opt` and `$sub_list` build sub-contexts, and
  `Assign_exp/list` builds `C_local`. Each one becomes `(IN L_sub exp)`,
  `(IN L_sub prem)`, or `(IN L_local (assign-exp ...))`.

Premises and assignments give a new context in watsup (`OK C'`). The machine
updates the layer in place instead: such a step rewrites the `L` of the
innermost `IN` and reduces to `OK`. So `(IN L' OK)` is the machine's `OK C'`.
Where watsup reads a sub-context's result, as `Eval_prem/iterpr-*` does with
`C_sub_res`, the parent reads `L'` from the finished `(IN L' OK)`.

### Machine terms

The results are the watsup ones, plus one for `Eval_targs`, whose output is
`typ*` and not a `res`:

| Result | Of |
| --- | --- |
| `(OK val)` or `FAIL` | `Eval_exp`, `Eval_arg`, `Eval_path`, `Eval_path_upd`, function calls |
| `(OK (val ...))` or `FAIL` | relation calls |
| `(OK (typ ...))` or `FAIL` | `Eval_targs` |
| `OK` or `FAIL` (`unitres`) | premises and assignments, which update the layer in place |

AL phrases (`exp`, `arg`, `prem`) evaluate in place, like the expressions in
Redex's tutorial. Evaluated subterms are replaced by their results:
`(BIN ADD (NAT 1) (VAR "x"))` steps to `(BIN ADD (OK (NAT 1)) (VAR "x"))`.
Every other relation has a form named after it, which holds its inputs. A
context input other than the innermost `IN`'s layer becomes an explicit
argument: this is the caller's layer, written `C_caller` in watsup.

| watsup relation | Machine form | Evaluated under |
| --- | --- | --- |
| `Eval_exp`, `Eval_arg`, `Eval_prem` | the `exp`, `arg`, or `prem` itself | the innermost `IN` |
| `Eval_targs` | `(eval-targs (targ ...))` | the innermost `IN` |
| `Eval_prems` | `(eval-prems (prem ...))` | the innermost `IN` |
| `Eval_path`, `Eval_path_upd` | `(eval-path val path)`, `(eval-path-upd val path val)` | the innermost `IN` |
| `Assign_exp(s)` | `(assign-exp exp val)`, `(assign-exps (exp ...) (val ...))` | the layer it updates |
| `Assign_arg(s)` | `(assign-arg L_caller arg val)`, `(assign-args L_caller (arg ...) (val ...))` | the callee's layer |
| `Eval_clause(s)` | `(eval-clause L_caller clause (val ...))`, `(eval-clauses L_caller (clause ...) (val ...))` | the callee's layer |
| `Eval_tblrow(s)`, `Call_table_func` | `(eval-tblrow tblrow (val ...))`, `(eval-tblrows ...)`, `(call-table-func ...)` | the caller's layer |
| `Call_defined_func`, `Call_func_dispatch`, `Call_func` | `(call-defined-func ...)`, `(call-func-dispatch ...)`, `(call-func id (typ ...) (val ...))` | the caller's layer |
| `Eval_rul(s)`, `Eval_rulgroup(s)` | `(eval-rul rulmatch rulpath (val ...))`, `(eval-ruls ...)`, `(eval-rulgroup ...)`, `(eval-rulgroups ...)` | the relation's layer |
| `Call_defined_rel`, `Call_rel_dispatch`, `Call_rel` | `(call-defined-rel ...)`, `(call-rel-dispatch ...)`, `(call-rel id (val ...))` | the caller's layer |
| the three extern relations | `(call-extern-func ...)`, `(call-builtin-func ...)`, `(call-extern-rel ...)` | none |
| `Entry` | the driver in `al/6-entry.rkt` | |

A rule whose premises evaluate sub-relations one after another has an
intermediate form for each point where it waits. The form is named
`<relation>/<rule>`, or `<relation>/<group>` when a group's rules share the
premise, and later stages add a suffix for what they wait for. For example,
`Eval_path/idx` becomes `(eval-path/idx (eval-path val path) exp_i)`: the
inner path first, and the index once the base is a text or a list. These forms
play the role of K's `...AwaitK` items.

A rule that inspects a premise's result before it evaluates the next premise
waits in a frame whose production requires that result. For example,
`(BIN boolbinop (OK (BOOL b)) hole)` and `(BIN numbinop (OK num) hole)` are the
only frames with the hole in the right operand. So the right operand is
evaluated only when the left one fits a rule, as in watsup.

### Frames and contexts

```racket
(Fr ::= Fr-pass Fr-catch)          ; one frame; its hole is an evaluation position
(F ::= hole (in-hole Fr F))        ; frames within one local context
(E ::= F (in-hole F (IN L E)))     ; frames across local contexts
(done ::= res (IN L OK))           ; a finished subterm
```

`Fr` lists every evaluation position of every machine form, and the pending
positions keep their precise nonterminals: `(BIN binop hole exp)`,
`(TUP ((OK val) ... hole exp ...))`. `Fr-catch` holds the frames where watsup
looks at a `FAIL` (see [FAIL and backtracking](#fail-and-backtracking)).
Nonterminals defined with `in-hole` work *(measured)*. Every redex has the
form `(G (in-hole E (IN L (in-hole F r))))`, so `L` is the layer of the
innermost `IN` around `r`.

### The notion of reduction and its closure

The rules are written on the redex, not on the whole configuration:

- `->redex` holds the rules that neither read nor write the context. They
  rewrite a redex `r` to `r'`. Most rules are here.
- `->ctx` holds the rules that read `G` or `L`, or write `L`. They rewrite a
  focus triple `(r G L)` to `(r' G L')`. The redex comes first; see
  [Patterns and their cost](#patterns-and-their-cost).

```racket
;; rulegroup Eval_exp/unary, in ->redex
(--> (UN NOT (OK (BOOL b))) (OK (BOOL b_res))
     (where b_res ,(not (term b)))
     "eval-exp/unary/boolean")
(--> (UN numunop (OK num)) (OK val_res)
     (where val_res (unop-number numunop num))
     "eval-exp/unary/number")
(--> (UN unop (OK val)) FAIL
     ;; otherwise
     (side-condition (not (redex-match? al (NOT (OK (BOOL b))) (term (unop (OK val))))))
     (side-condition (not (redex-match? al (numunop (OK num)) (term (unop (OK val))))))
     "eval-exp/unary/fail")

;; rule Eval_exp/variable, in ->ctx
(--> ((VAR id) G L) ((OK val) G L)
     (where (val) (find-varr L (id ())))
     "eval-exp/variable")
(--> ((VAR id) G L) (FAIL G L)
     (where () (find-varr L (id ())))
     "eval-exp/variable/fail")

;; rule Assign_exp/variable, in ->ctx
(--> ((assign-exp (VAR id) val) G L) (OK G L_1)
     (where L_1 (add-varr L (id ()) val))
     "assign-exp/variable")
```

Each fragment module defines its part of the two relations, and
`al/5-eval.rkt` combines them with `union-reduction-relations`. It also
defines `->al`, the relation on whole configurations, as the closure of the
two through the contexts:

```racket
(define ->al
  (reduction-relation al
   (--> (G (in-hole E (IN L (in-hole F e_r))))
        (G (in-hole E (IN L_1 (in-hole F e_1))))
        (where ((e_1 G L_1)) ,(focus-steps (term (e_r G L))))
        "closure")))
```

`focus-steps` returns every `(r' G L')` that `->redex` (with `L' = L`) or
`->ctx` gives for the triple. `->al` is the specification that tests and
figures refer to, and the relation `traces` can show. Programs are run by a
driver that computes the same relation faster:

1. It descends from the root, remembering the innermost `IN`. At each node it
   matches `Fr` one level deep, and moves into the one evaluation position
   whose subterm is not `done`.
2. The node where no such position exists is the redex. The driver applies
   `->redex` and `->ctx` to it once.
3. It plugs `r'` back, and replaces the innermost `IN`'s layer with `L'`.

The driver fails loudly if two evaluation positions are pending at one node,
if the rules give two results, or if a configuration that is not final has no
step. It loops with `apply-reduction-relation`, and never uses
`apply-reduction-relation*` (see below). The rules, the grammar, and
`->al` are the specification; the driver adds no semantics. Tests check that
its step equals `->al`'s on every step of small runs (see
[Verification](#verification)).

The tutorial's style, with the contexts in every rule's left-hand side, costs
far too much:

- **Every rule decomposes the whole term.** Redex matches `in-hole` by building
  a context for every candidate hole, once per rule. With 150 rules and caching
  on, one step took 5.4 ms at depth 10, 57 ms at depth 50, and 295 ms at
  depth 200 *(measured)*, and 4 to 14 times more with caching off. With rules on
  the redex and one decomposition per step, a toy `fib 14` (10,879 steps) took
  0.26 ms per step with 150 rules *(measured)*. Call depth makes AL terms deep,
  so only the second shape can run the examples.
- **No `#:domain`.** A domain check matches the whole configuration against a
  nonterminal after every step. In the toy with caching on, removing it cut the
  step time by 30% *(measured)*.
- **No `apply-reduction-relation*`.** It keeps every term on the path in a hash,
  to detect cycles, and it recurses once per step. Racket's `equal-hash-code`
  stops after a bounded traversal (37 µs on the booted `spec/`), so all
  configurations with the same large `G` get the same hash *(measured)*. The
  hash then degrades to `equal?` comparisons against every earlier term. The
  same applies to `test-->>` and `traces`, so they are for small runs only.

### FAIL and backtracking

`FAIL` propagates like an exception, up to the nearest frame where watsup
looks at it:

- `(in-hole Fr-pass FAIL)` steps to `FAIL`, in one rule, `"frame/fail"`.
  `(IN L FAIL)` steps to `FAIL` too, and `(IN L (OK any))` to `(OK any)`
  (`"in/fail"` and `"in/ok"`).
- The catchers are the frames of `Eval_clauses/cons-fail`,
  `Eval_tblrows/cons-fail`, `Eval_ruls/cons-fail`, and
  `Eval_rulgroups/cons-fail`, which try the next alternative. The fifth is the
  frame of `Eval_prem/nothold`, which turns `FAIL` into `OK`.
- The rules `Eval_exp/fail`, `Eval_path/fail`, `Eval_path_upd/fail`,
  `Eval_arg/fail`, `Eval_prem/fail`, `Eval_clause/fail`, `Eval_tblrow/fail`,
  `Eval_rul/fail`, and `Eval_rulgroup/fail` are `otherwise` rules. Each becomes
  explicit complement rules, `".../fail"`, next to the rules it complements.

watsup relations without an `otherwise` have no derivation when no rule
applies. These are `Assign_*`, `Eval_targs`, `Call_func`, `Call_rel`,
`Call_func_dispatch`, and `Call_defined_func`. A caller of such a relation
always reaches an `otherwise` that gives `FAIL`, with no catcher in between.
So the machine reduces such a form to `FAIL` directly, with a complement rule
of its own:

- An assignment that matches no rule fails its premise, clause, or rule path.
- `(call-func id ...)` with an unknown `id` gives `FAIL` at the `CALL`, as
  `Eval_exp/fail` would.

There is one exception. `Eval_prem/nothold` needs `Call_rel` to give `FAIL`.
An unknown relation `id` gives no derivation, which `Eval_prem/fail` turns into
`FAIL` for the premise. So the `IFNOTHOLD` rule looks the relation up itself, at
the point where `Call_rel` would. It fails on an unknown `id` before its
catcher frame exists.

Runtime errors are not `FAIL`: a metafunction that raises (division by zero, a
slice out of bounds) raises out of the driver, as it does in OCaml.

### Determinism and disjoint clauses

- **Reduction rules.** At most one rule of `->redex` and `->ctx` applies to any
  redex. The driver checks this on every step, so the order of the rules never
  matters. Each `otherwise` becomes complement rules, as in the example above.
- **Metafunctions.** Their clauses are disjoint: for any input, at most one
  clause applies, whatever order the clauses are in. Each clause is told apart
  from the others by its input patterns and side-conditions. Redex does not
  check this: a metafunction silently takes the first clause that applies.
  `-- otherwise` becomes the negation of the earlier clauses' conditions:

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
  `OK val`. Compute such results once, and branch on them in a helper
  metafunction named after the clause (`upcast/tup`).
- **Iteration over two sequences.** A watsup iteration over two sequences,
  such as `$subtyp(tdenv, typ, val)*` over `typ*` and `val*`, only applies when
  the lengths are equal. A Redex template over lists of different lengths
  raises an error instead of failing to match *(measured)*. Such clauses and
  rules need an explicit length constraint, as a named ellipsis
  (`(TUP (typ ..._n)) (TUP (val ..._n))`) or a side-condition, and their
  complement needs its negation.
- **Partial `def`s.** Some `def`s have no clause for some inputs;
  `$find_vari` has none for an unbound variable. In P4-SpecTec, such a call is
  a failing premise, not an error. A Redex metafunction raises an error when no
  clause applies, so `define-dec` appends one last clause, `[(f any ...) ⊥]`,
  and adds `⊥` to the range. `⊥` is not a term of any AL nonterminal, so a
  premise `(where p (f ...))` fails on it, unless `p` is `any` or `_`. Bind
  every metafunction call with `where` before using its result, against a
  pattern narrower than `any`. Where a complement rule dispatches on a call's
  result, it matches `⊥` literally: `(where ⊥ (binop-number numbinop num_l num_r))`.

### Caching

Caching is on (`caching-enabled?` is `#t`, set in `common/0.0-prelude.rkt`).
It is safe because every impure operation happens in a reduction rule, and
Redex caches metafunctions, judgment forms, and nonterminal matches, but not
reduction relations:

- Every `dec`/`def` is pure. No `def` has a relation premise, and the only
  builtins that are metafunctions are the list and map ones.
- The impure operations are the three extern relations, which reach host state
  (the counter behind `$fresh_typeId`, and extern state), and `debug`. Each is
  one reduction rule, whose `where` clause calls a Racket procedure.
- The port has no judgment forms.

In a probe with caching on, a counter bumped in a reduction rule advanced on
every application, and the same counter in a metafunction returned the same
value twice *(measured)*. So never call an impure Racket procedure from a
metafunction, or from anything a metafunction calls.

The driver applies the rules once per step, so each side effect happens once.
Anything else that applies them again repeats the side effects. This includes
the `->al` cross-check and a second `apply-reduction-relation` on the same
term.

The cache makes precise patterns affordable (see the next section), and
memoizes hot pure metafunctions such as `$subtyp`. Its limits matter for
performance, not for correctness:

- Every nonterminal and every metafunction has a table of only 63 entries.
- Keys are compared with `equal?` after a bounded hash, so terms that share a
  large prefix collide *(measured)*.
- `equal?` on two equal but not `eq?` copies of the booted `spec/` took
  194 ms *(measured)*. A large argument that is rebuilt on every call, like
  `$extend_tdenv`'s result, can therefore make lookups slow.
- Redex looks a metafunction call up in its cache even with caching off, and
  checks the flag only after the lookup. So a call with caching off still pays
  a deep `equal?` when the cache holds an equal but not `eq?` copy of its
  arguments. Loading a copy of `spec/` with caching off, after loading another
  copy with caching on, took 243 s instead of 4.7 s *(measured)*. The
  nonterminal memo checks the flag first.

`$load` is the one place that runs with caching off: its one clause runs
`load/shallow` under `(parameterize ([caching-enabled? #f]) ...)`. None of
`load/shallow`'s calls repeat, so on `spec/` the cache only added 4 to 5 s of
hashing and comparisons. Nothing else runs `load/shallow` on a large script with
caching on, so its cache never holds a copy that a lookup would compare deeply.

`set-cache-size!` and the argument order of metafunctions are Step 13's knobs.

### Patterns and their cost

Patterns use the precise nonterminals, as close to watsup as possible,
including `G` and `L` (aliases of `layer`) in every `->ctx` rule and in
metafunction contracts. This is affordable only with caching on and with the
redex first in focus triples. The probe used 150 rules, the booted `spec/`
loaded into `G`, and a precise `layer` check for `G` and `L` *(measured)*:

| One application of 150 rules | Caching on | Caching off |
| --- | --- | --- |
| focus triple `(G L r)` | 9.75 ms | 38 s |
| focus triple `(r G L)` | 0.05 ms | 0.24 s |
| `with` shortcut lifting redex rules to `(r G L)` | 10.35 ms | 34 s |

Redex matches a list pattern left to right. With the redex first, a rule whose
redex does not match fails before it checks `G`, and the one rule that matches
finds `G` in the matcher's memo. A `with` shortcut matches its outer pattern
first, which is why the rules on the redex alone form a relation of their own
instead of being lifted into triples.

There are two exceptions to precise patterns:

- **`$load` recurses as `load/shallow`,** over a `ctx-shallow` that checks the
  record's shape but not its maps, with the script matched as `any`. With
  precise patterns, every recursive call rechecks the context and the rest of
  the script, so `$load` is quadratic. On `spec/`, that took 27 minutes with
  caching off, and 274 s with it on. `load/shallow` takes `defn_h :: defn_t*`
  apart with a Racket escape, `uncons`, because matching `defn_t*` with an
  ellipsis costs quadratic time (see Step 4). It runs with caching off (see
  [Caching](#caching)). It loads `spec/` in 2.3 s, or 1.7 s with contracts off.
  `$load` itself keeps watsup's signature, and checks its input and result
  against `ctx` once.
- **No `#:domain` on the relations.**

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

Because `x?` is a list, the pattern `(OPT (val ...))` binds `val` at depth 1,
and watsup's `?`-iteration becomes Redex's `...`, the same as `*`-iteration:

```racket
;; def $upcast(C, ITER typ QUEST, OPT val?) = OK (OPT val_upcast?)
;;   -- if (OK val_upcast = $upcast(C, typ, val))?
[(upcast G L (ITER typ QUEST) (OPT (val ...)))
 (upcast/opt (OPT (val ...)) (valres ...))
 (where (valres ...) ((upcast G L typ val) ...))]
```

Meta-level maps (`venv`, `tdenv`, `fenv`, `renv`, `theta`) are association
lists `((key value) ...)`. They are only accessed through the builtin
metafunctions (`find-map`, `add-map`, and the rest), which are implemented in
Racket. Step 13 can therefore change their representation without touching a
rule.

A `dec` over `ctx` becomes a metafunction over `G L` if it reads `C.GLOBAL`,
and over `L` alone if it reads only `C.LOCAL`. A result of type `ctx` becomes
the new `L`. `$find_varr(C, varr)` becomes `(find-varr L varr)`,
`$find_func(C, id)` becomes `(find-func G L id)`, and `$sub_list(C, vari*)`
returns a list of layers. `$load` and `$empty_ctx` keep `ctx`, because
loading writes `GLOBAL`. A record update `C[ .LOCAL.VAL = x ]` is a pattern
that rebuilds the layer, `{TYP tdenv REL renv FUNC fenv VAL venv}`.

### Premises

| watsup premise | In a reduction rule |
| --- | --- |
| `-- R: C \|- e : OK v`, R a relation | R's form in an evaluation position of an intermediate form; a rule on `(OK v)` continues, a complement rule turns other results into `FAIL`, and `"frame/fail"` passes `FAIL` up |
| `-- (R: C \|- e : OK v)*` | the evaluation positions of a list, left to right, stopping at the first `FAIL` |
| `-- if p = e` (binding) | `(where p e)` |
| `-- (if p = e)*` | `(where (p ...) (e ...))`; constrain the lengths when `e` iterates over several sequences |
| `-- if e` (boolean) | `(where #t e)` |
| `-- if ~e` (boolean) | `(where #f e)` |
| `-- if ~(val <: num)` | `(side-condition (not (redex-match? al num (term val))))` |
| `-- otherwise` | complement rules; see [FAIL and backtracking](#fail-and-backtracking) |
| `-- debug e` | `(where _ ,(debug (term val)))` in the rule of `Eval_prem/dbg`, printing to stderr |
| `$(n - 1)`, `\|x*\|`, `x*[n]`, slices, `x*[[n] = y]`, `++` | Racket escapes; indexing, slicing, and updating are helpers in `common/0.1-stdlib.rkt` (`list-idx`, `text-slice`, ...) |

A `where` pattern that reuses a variable bound earlier in the rule is an
equality constraint, not a new binding, just as a repeated variable within one
pattern is. A binding premise therefore needs a fresh name, unless watsup
repeats the metavariable on purpose.

The machine evaluates the elements of an iterated premise one at a time and
stops at the first `FAIL`. The elaborated AL evaluates every element before it
checks any, so the two differ only when a later element raises an error or has
a side effect. The same holds for tuples, lists, arguments, and iterated
premises and expressions.

### Layout and languages

Racket modules cannot require each other in a cycle. The AL relations are
mutually recursive, but only through the term: a rule produces another
relation's form, and does not call that relation. So every watsup file becomes
one module. `al/5-eval.rkt` combines the fragments' relations.

Modules in `common/` never require modules in `al/`. That is why the extern
codec and wire live in `common/`: the host procedures in
`common/4-relation.rkt` call them.

```text
spec-meta-redex/
  common/
    0.0-prelude.rkt       Redex re-exports; caching on; define-dec
    0-extern-json.rkt     codec for the extern JSON wire
    0-extern-wire.rkt     transport to the OCaml host
    0.1-stdlib.rkt        language stdlib; $ite, $opt_as_seq_, $exists_, ...; builtins; text and list helpers; debug
    1-syntax.rkt          language common
    2-env.rkt             language common-env; $extend_tdenv, $theta_of_tdenv, $is_iter_on_var, ...
    3-context.rkt         language common-context (cursor)
    4-relation.rkt        language common-relation (unitres, res<X>); host procedures for the three extern relations
    5.0-eval-typ.rkt      $subst_typ
    5.1-eval-ops.rkt      $unop_number, $binop_*, $cmpop_*, $is_tup, $is_fun
  al/
    0-boot.rkt            boot-script, boot-p4: run spectec-boot sexp(-p4), read
    1-syntax.rkt          language al-syntax
    2-env.rkt             languages al-base (the union) and al-env (reldef, funcdef)
    3-context.rkt         language al-context (layer, G, L, ctx, ctx-shallow); $load, $add_*, $find_*, $sub_*
    4-relation.rkt        language al: conf, results, machine terms, done, Fr, F, E
    5.1-eval-typ.rkt      $upcast, $downcast, $subtyp
    5.2-eval-assign.rkt   rules of Assign_exp(s), Assign_arg(s)
    5.3-eval-exp.rkt      rules of Eval_exp, Eval_path, Eval_path_upd
    5.4-eval-arg.rkt      rules of Eval_arg, Eval_targs
    5.5-eval-prem.rkt     rules of Eval_prem, Eval_prems
    5.6-eval-call-func.rkt  rules of Eval_clause(s), Eval_tblrow(s), Call_*_func, Call_func
    5.7-eval-call-rel.rkt   rules of Eval_rul(s), Eval_rulgroup(s), Call_*_rel, Call_rel
    5-eval.rkt            the IN and FAIL rules; ->redex, ->ctx, ->al; the driver
    6-entry.rkt           Entry: load, then run $main() or a relation
  main.rkt                command-line driver
  test/                   unnumbered: prelude.rkt, syntax.rkt, boot.rkt, machine.rkt, ...
```

`common/0.0-prelude.rkt` defines `define-dec`, which wraps
`define-metafunction`. It appends the `⊥` clause, and builds the contract from
the watsup declaration. `SPECTEC_REDEX_CONTRACTS=0`, read when a module is
compiled, drops the contracts.

Each file in `common/` from `0.1-stdlib` to `4-relation` extends the previous
file's language with its own syntax: `stdlib` (the `var`s, sets and maps),
`common`, `common-env`, `common-context`, and `common-relation`. `al-syntax`
extends `common` with `al/1-syntax`. `al-base` (in `al/2-env.rkt`) is the
`define-union-language` of `common-relation` and `al-syntax`. `al-env`,
`al-context`, and then `al` extend it with `al/2-env`, `al/3-context`, and
`al/4-relation`. Metafunctions defined on a `common/` language work on `al`
terms, so common helpers stay in `common/`.

`al/4-relation.rkt` declares every machine form and frame, as
`al/4-relation.watsup` declares every relation. Each step adds the forms and
frames its rules need.

### Getting scripts into Redex

`spectec-boot` already has two subcommands for Redex, next to `kast` and
`kast-p4`. The emitter is
[`sexp.ml`](p4spec/lib/interface/spectec/ali/sexp.ml):

- `spectec-boot sexp PATH [-o FILE]` prints the booted `Al.spec` as one
  s-expression in the encoding above.
- `spectec-boot sexp-p4 -p FILE [-i DIR]... [-o FILE]` prints a P4 program as
  a `val`.

Racket's `read` parses this output directly, so Redex needs no parser.
`al/0-boot.rkt` runs these subcommands and `read`s their output, with
`SPECTEC_BOOT` overriding the binary as in the K scripts. `spec/` boots to
2.8 MB. `sexp-p4` writes the JSON of `EXT json` as a string of JSON text, and
`boot-p4` decodes it with `string->jsexpr`. A value arriving over the extern
wire in Step 12 is decoded by the same library, so both have one encoding.

### Builtins and externs

AL reaches the host through three extern relations: `Call_builtin_func`,
`Call_extern_func`, and `Call_extern_rel`. As in the K port, calls go to the
OCaml implementation over the existing JSON wire
([`extern_json.ml`](p4spec/lib/interface/spectec/ali/extern_json.ml)), so all
three evaluators share host behavior:

```text
reduction rule -> common/4-relation.rkt -> common/0-extern-json.rkt
  -> common/0-extern-wire.rkt -> spectec-boot extern-serve -> SpecTec runner
```

The transport is a single long-lived `spectec-boot extern-serve SPECDIR`
subprocess per run. It reads one JSON request per line on stdin and writes one
response per line on stdout. Its dispatch is `eval` from
[`kffi.ml`](p4spec/bin/kffi.ml), moved into the library so that `kffi.ml` and
`boot.ml` share it. One process per run keeps `$fresh_typeId`'s counter
consistent across calls. If the pipe turns out to be a bottleneck, the C shim
in `spec-meta-k/ffi/` can instead be loaded as a shared object through
Racket's `ffi/unsafe` (Step 13).

As in the K port, the hot object-level map builtins (`find_map`, `find_maps`,
`add_map`, `adds_map`, `update_map`, `assoc_`) are native Racket that works on
AL map values (`INJ` with the `` `{ `} `` mixop). They are pure, so the
`call-builtin-func` rule calls them in place of the host. They are separate
from the meta-level `find-map` in `common/0.1-stdlib.rkt`, which works on
Redex's own environments.

### Deviations from spec-meta

Where a `spec-meta/al` rule is evidently buggy as written, the port transcribes
the intended rule, and `spec-meta/` stays unchanged. Each deviation gets a
short comment at the rule. On these inputs the meta-circular oracle disagrees
with Redex, while OCaml's AL interpreter and the K port agree with it:

- **`Assign_exp/iter/opt-some`.** Its last premise,
  `$add_varis(C', vari_iter*, OPT val_sub)*`, elaborates to one call per
  `val_sub`, each with the one value `OPT val_sub`. So it only works with
  exactly one variable, and the meta-circular run crashes on two. The port
  makes the intended single call with `(OPT val_sub)*`, the form
  `5.5-eval-prem.watsup` uses for `Eval_prem/some`.
- **`Eval_exp/call`.** It computes `typ_input*` with `Eval_targs`, and then
  passes the unsubstituted `targ*` to `Call_func`. So a type parameter passed
  on through a polymorphic call is resolved in the callee. The port passes
  `typ_input*`.
- **`Eval_path_upd/slice/list`.** `val*[[n_i : n_n] = val_n]` elaborates to
  `[val_n]`. The port takes `LIST val_n*`, whose elements replace the slice, as
  the text rule takes `TEXT t_n`. A list of another length than the slice
  raises an error, as in OCaml's AL interpreter.

This is not a licence to fix surprising semantics. `$upcast` returning
`OK val` when a component cast fails is consistent, and is transcribed as it
stands. For an apparent bug, look at the elaborated AL and probe an oracle,
and decide with the user before deviating. Record each deviation here.

## Verification

- **Oracles.** There are two: the K port (`./spec-meta-k/scripts/k-run.sh
  FILE`, which prints the result as JSON), and the OCaml meta-circular run
  (`./spectec-boot run spec-meta/al -rel Entry -tec FILE -ali`, which prints
  the value through `Entry`'s `debug`). `main.rkt` prints results in
  `k-run.sh`'s JSON format, so the outputs can be compared as text. Where K
  differs from watsup, compare with the meta-circular run.
- **Unit tests.** `test-equal` and `test-match` go under `spec-meta-redex/test/`
  and run with `raco test spec-meta-redex/test`. Machine tests run a term
  under a given `G` and `L` through the driver, with helpers in
  `test/machine.rkt`. They check the result and the final layer. They include
  inputs on both sides of every complement, and a case for every point where
  watsup must not evaluate a premise. Such a case puts
  `(BIN DIV (NAT 1) (NAT 0))`, which raises, where a premise must not be
  evaluated.
- **Determinism.** The driver fails on a step with two results, or a node with
  two pending positions. So every test run checks that no two rules overlap on
  the terms it reaches.
- **The driver against `->al`.** In cross-check mode, the driver also applies
  `->al` to each configuration, and fails unless `->al` gives exactly its
  successor. The check applies the rules a second time, so it runs only on
  scripts without externs, builtins, or `debug` output that matters.
- **Coverage.** `make-coverage` takes reduction relations. A test file ends by
  checking that every rule of the relations it exercises was used, except the
  extern rules until Step 12. Coverage is recorded through the driver:
  `->al` applies the rules at every candidate position, so its counts are
  inflated.
- **Disjoint metafunctions.** The tests cover both sides of every complement.
- **Caching.** A test asserts that `caching-enabled?` is `#t` once
  `common/0.0-prelude.rkt` is loaded. The `$fresh_typeId` test in Step 12
  catches an impure call that reached a cached metafunction.
- **Contracts.** Every metafunction gets a contract built from its watsup
  declaration, which catches transcription slips early. With caching on, a
  contract check on `G` or `L` costs a memo lookup. The switch turns contracts
  off for P4 runs.
- **Grammar.** In debug mode, the driver checks every configuration against
  `conf`. That costs a full match per step, so the tests use it only on small
  runs.

## Checking it yourself

Run these from the repository root. Run `raco make` before `raco test` or
`racket`: once `compiled/` exists, `racket` loads each `.zo` by its own
timestamp.

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
raco make spec-meta-redex/test/*.rkt && raco test spec-meta-redex/test
raco make spec-meta-redex/test/machine.rkt && raco test spec-meta-redex/test/machine.rkt
```

The contract switch is read at compile time, so it only takes effect on code
compiled with it. To run the tests with contracts off, use a scratch copy
without `compiled/`:

```sh
COPY=/tmp/redex-nc
rm -rf "$COPY" && mkdir -p "$COPY" && cp -r spec-meta-redex "$COPY"/
find "$COPY" -name compiled -type d -prune -exec rm -rf {} +
for d in examples spec spec-meta p4c spectec-boot; do ln -s "$PWD/$d" "$COPY/$d"; done
(cd "$COPY" && export SPECTEC_REDEX_CONTRACTS=0 &&
   raco make spec-meta-redex/test/*.rkt && raco test spec-meta-redex/test)
```

A separate `PLTCOMPILEDROOTS` would recompile Redex and its dependencies too,
in memory for every test file, which takes more than 10 minutes for the suite.
`test/prelude.rkt` fails if the loaded code was compiled with the other
setting.

To list the rules a test file never reaches (here the expression rules):

```sh
racket -e '(require redex/reduction-semantics racket/port (file "spec-meta-redex/al/5-eval.rkt"))
           (define c (make-coverage ->redex))
           (parameterize ([relation-coverage (list c)] [current-output-port (open-output-nowhere)])
             (dynamic-require (quote (file "spec-meta-redex/test/eval-exp.rkt")) #f))
           (for ([p (covered-cases c)] #:when (zero? (cdr p))) (displayln (car p)))'
```

### Exploring in a REPL

```sh
racket -i -e '(require (file "spec-meta-redex/common/0.0-prelude.rkt")
                       (file "spec-meta-redex/al/0-boot.rkt")
                       (file "spec-meta-redex/al/3-context.rkt")
                       (file "spec-meta-redex/al/5-eval.rkt"))'
```

Then, for example:

```racket
(term (load (empty-ctx) ,(boot-script "examples/add.watsup")))   ; a loaded context
(redex-match? al-context layer (term (empty-layer)))             ; grammar membership
(apply-reduction-relation/tag-with-names ->redex (term (UN NOT (OK (BOOL #t)))))
(current-traced-metafunctions '(find-vari sub-list))             ; print calls and results
(current-traced-metafunctions '())
```

A small run can be stepped through in Redex's GUI, with `(traces ->al conf)`
from `redex/gui`. `traces` explores the reduction graph like
`apply-reduction-relation*`, so it is for small runs only.

To see a language or the rules as a figure:

```sh
racket -e '(require racket/class pict redex/pict (file "spec-meta-redex/al/5-eval.rkt"))
           (send (pict->bitmap (reduction-relation->pict ->redex)) save-file "/tmp/redex.png" (quote png))'
```

### Oracles

What a script should evaluate to, for the comparisons from Step 11 on:

```sh
./spectec-boot run spec-meta/al -rel Entry -tec examples/add.watsup -ali   # OCaml, meta-circular
make k-spec && ./spec-meta-k/scripts/k-run.sh examples/add.watsup          # K, JSON output
```

## Steps

Each step ends with tests, and records what it found in an Outcome list under
the step.

### Step 1: Prelude and syntax

Transcribe `common/1-syntax.watsup` and `al/1-syntax.watsup` into Redex
languages. This step fixes the term encoding that every later step and the
`sexp` emitter depend on.

- `common/0.0-prelude.rkt`: re-exports `redex/reduction-semantics`, sets
  `caching-enabled?` to `#t`, and defines `define-dec` with the `⊥` clause and
  the contract switch. Every module requires it instead of Redex itself.
- `common/0.1-stdlib.rkt`: the language `stdlib`. The `var` declarations in
  `common/0-stdlib.watsup` become nonterminal aliases:
  `(bool b ::= boolean)`, `(int i ::= integer)`, `(nat n ::= natural)`, and
  `(text t ::= string)`. It also has the shapes of sets and maps.
- `common/1-syntax.rkt`: `(define-extended-language common stdlib ...)` with
  one nonterminal per watsup syntax:
  - identifiers: `id`, `atom`, `mixop`
  - types: `numtyp`, `optyp`, `typ`, `deftyp`, `typfield`, `typcase`, `iter`,
    `vari`
  - operators: `num`, `boolunop`, `boolbinop`, `numunop`, `numbinop`,
    `numcmpop`, `polycmpop`, `unop`, `binop`, `cmpop`
  - values: `val`, `valfield`, `valcase`; `extern syntax json` becomes a
    `json` that matches what `jsexpr?` accepts
  - expressions: `exp`, `expcase`, `expfield`, `iterexp`, `listpattern`,
    `optpattern`, `pattern`, `path`
  - arguments: `targ`, `arg`, `tparam`
- `al/1-syntax.rkt`: `(define-extended-language al-syntax common ...)` adding
  `param`, `iterprem`, `prem`, `rulmatch`, `rulpath`, `rulgroup`, `elsgroup`,
  `clause`, `elsclause`, `tblrow`, `defn`, and `script`.
- Add `spec-meta-redex/**/compiled/` to `.gitignore`.

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

Tests:

- `test/syntax.rkt`: `test-match` for every production, and `test-no-match`
  for near misses such as `(OPT ((NAT 1) (NAT 2)))`, an `ITER` with a bad
  iterator, and bare numbers where `num` is expected.
- [`examples/add.watsup`](examples/add.watsup), encoded by hand, matches
  `script`.
- `test/prelude.rkt`: caching is on; a partial metafunction gives `⊥`;
  contracts reject inputs outside the domain; the contract switch matches the
  environment.

Done when `raco test spec-meta-redex/test/syntax.rkt spec-meta-redex/test/prelude.rkt` passes.

Outcome:

- Done. `test/syntax.rkt` and `test/prelude.rkt` pass (380 checks), with
  contracts on, and with `SPECTEC_REDEX_CONTRACTS=0` under a separate
  `PLTCOMPILEDROOTS` (376). Running `test/prelude.rkt` with the switch set
  differently from the compiled code fails, as intended.
- The hand-encoded `examples/add.watsup` is exactly what `spectec-boot sexp`
  prints, so the grammar and the emitter agree on the encoding.
- `common/0.1-stdlib.rkt` holds only the language `stdlib`. Its metafunctions
  and helpers come in Step 3.
- Redex's `test-match` and `test-no-match` report to rackunit, so `raco test`
  counts their failures. A range violation raises "codomain test failed", not
  "not in my range".

### Step 2: Booting scripts

- `al/0-boot.rkt`: `(boot-script path)` runs `spectec-boot sexp` and `read`s
  the result, so that tests and `main.rkt` accept `.watsup` paths directly.
  `(boot-p4 path #:includes dirs)` does the same with `sexp-p4`, and decodes
  `EXT` values.
- Test: every `examples/*.watsup` file, and the whole of `spec/`, boots to a
  term matching `script` in `al-syntax` (`redex-match?`). This is the first
  time Step 1's grammar meets real input, so a mismatch is a bug in either the
  grammar or the emitter.

Done when all of them match.

Outcome:

- Done. `test/boot.rkt` passes. All 11 examples, `spec/` (1,672
  definitions), and `spec-meta/al` match `script`. The four P4 programs of
  Step 13 boot through `sexp-p4` and match `val`. The grammar and the emitter
  needed no change.
- With caching on, `spec/` boots in 2.1 s and matches `script` in 0.2 s. The
  large P4 program (dash) boots in 0.2 s and matches `val` in 0.15 s. The test
  file takes 4.5 s.
- No P4 program produces an `EXT` value, so the test decodes a hand-written
  `EXT` to check `decode-ext`.

### Step 3: Common metafunctions

Transcribe `common/0-stdlib`, `2-env`, `3-context`, `4-relation`,
`5.0-eval-typ`, and `5.1-eval-ops`: every `dec`/`def` as a metafunction, and
`res<X>`.

- Write every `otherwise` as an explicit complement. The ones in this step are
  small: `$extend_tdenv`, `$is_iter_on_var`, `$subst_typ`, `$is_tup`, and
  `$is_fun`.
- Builtins (`$rev_`, `$assoc_`, `$transpose_`, `$find_map`, `$find_maps`,
  `$add_map`, `$adds_map`) are metafunctions that escape to Racket.
- Type parameters (`$ite<X>`, `$repeat_<X>`) are dropped because Redex terms
  are untyped. Contracts use `any` where watsup has a type parameter.
- Arithmetic must match [`p4spec/lib/lang/xl/num.ml`](p4spec/lib/lang/xl/num.ml):
  `DIV` and `MOD` truncate, so they map to `quotient` and `remainder`, not
  `modulo`. `num.ml` has no `POW`, and asserts false on division by zero;
  here division by zero and a negative exponent raise an error.
- `$cmpop_poly` compares with `equal?`. For `EXT` values this compares
  jsexprs, so the key order of a JSON object does not matter, whereas OCaml's
  `Stdlib.compare` on Yojson distinguishes it.
- `$is_iter_on_var`'s `ITER` clause requires `iterexp` to have exactly one
  variable, with the same id and inner iterators. Its premise result goes
  through a helper, `is-iter-on-var/iter`, so it is computed once.
- `common/4-relation.rkt` also declares the Racket procedures behind the three
  extern relations. They raise an error until Step 12.
- Tests:
  - `test-equal` for each metafunction, covering both sides of every
    complement and the edge cases of `$transpose_` (empty outer list, empty
    rows).
  - An input no clause matches gives `⊥`, and a caller's `where` on it fails
    instead of raising an error. Examples are `$theta_of_tdenv` on a `DEF` with
    type parameters or a non-`ALIAS` body, `$subst_typ'` on a bound `VAR` with
    type arguments, and `$binop_number` on mixed `NAT` and `INT`.

Outcome:

- Done. `test/common-stdlib.rkt`, `common-env.rkt`, `common-relation.rkt`,
  `common-eval-typ.rkt`, and `common-eval-ops.rkt` pass. The whole suite runs
  2,611 checks in 10 s, and 2,589 with contracts off, on a scratch copy.
- There is no clause-coverage check for metafunctions for now. A probe found
  that `make-coverage` on a metafunction counts only clauses that give a
  result, not the ones whose side-condition or `where` fails, and that caching
  does not hide calls from it. It reports the `⊥` clause at the location of
  the `define-dec` header *(measured)*.
- `define-dec` takes `range ∨ range ...`, so a watsup `X?` result has a
  precise contract, such as `() ∨ (varr)` for `$is_iter_on_var`.
- The map and list builtins follow `maps.ml` and `lists.ml`:
  - `$find_map`, `$find_maps`, and `$assoc_` take the first match.
  - `$add_map` replaces the first pair with the key where it stands, or
    appends a pair.
  - `$adds_map` with more keys than values, or fewer, raises an error, as
    OCaml's `fold_left2` does. `$transpose_` raises on rows of different
    lengths.
- DIV and MOD truncate in the meta-circular run: `-7 / 2` is `-3`, `-7 \ 2`
  is `-1`, and `7 \ -2` is `1` *(measured)*. `num.ml` has no `POW` clause, so
  `POW` has no oracle.
- `$subst_typ'` is `subst-typ-inner`. The host procedures are
  `host-call-extern-func`, `host-call-builtin-func`, and
  `host-call-extern-rel`, which keeps them apart from the machine forms of the
  same relations.
- The text and list helpers and `debug`, which the layout puts in
  `common/0.1-stdlib.rkt`, are left to Step 8, which specifies and uses them.

### Step 4: AL environments and contexts

Transcribe `al/2-env` and `al/3-context`: `reldef`, `funcdef`, `layer`, `ctx`,
and the `$load`, `$add_*`, `$find_*`, `$sub_opt`, and `$sub_list`
metafunctions.

- `al-context` declares `(layer G L ::= {TYP tdenv REL renv FUNC fenv VAL venv})`
  and `(ctx C ::= {GLOBAL layer LOCAL layer})`.
- The context metafunctions follow the `G L` signature rule in
  [Term encoding](#term-encoding). This is the port's one change to
  metafunction signatures.
- `$load` recurses as `load/shallow` (see
  [Patterns and their cost](#patterns-and-their-cost)). `load-typdef`,
  `load-reldef`, and `load-funcdef`, which only `$load` calls, take a
  `ctx-shallow` too.
- Complements:
  - `$find_func`'s `otherwise` means "found in neither layer".
  - `$find_varis`, `$find_varrs`, and `$finds_vari` fall back when some lookup
    fails.
  - `$sub_opt` has one clause for "every lookup is `OPT val`", and one for
    "every lookup is `OPT eps`". A mix matches neither, so it gives `⊥`. The
    two clauses overlap on an empty `vari*`, which watsup resolves by order:
    the first applies. So the second clause requires a non-empty `vari*`.
- `$find_vari` has no clause for an unbound variable, so it gives `⊥`, never
  `eps`.
- Tests: run `$load` on the scripts booted in Step 2, and check which ids end
  up in `GLOBAL`'s `FUNC`, `REL`, and `TYP`. Load `spec/` once, and check the
  result against `ctx`, recording the time. Test `$sub_list` with no iterated
  variables and with empty lists.

Outcome:

- Done. `test/env.rkt` and `test/context.rkt` pass (195 checks).
- Signatures: the adders, the value finders, `sub-opt`, and `sub-list` take
  `L`, and `finds-vari` takes `(L ...)`. `find-typ`, `find-func`, and
  `find-rel` take `G L`. `find-rel` reads only `GLOBAL`, but the rule still
  gives it both layers. `sub-opt` returns `() ∨ (L)`, and `sub-list` returns
  `(L ...)`.
- Clauses match every layer as `{TYP tdenv REL renv FUNC fenv VAL venv}`,
  including maps that the clause does not read, and a `vari` as
  `(id typ (iter ...))` where watsup writes `id _ iter*`.
- `$load` on `spec/` takes 2.3 s with contracts on, and 1.7 s with them off.
  Loading an equal copy again is a cache hit of `load`, in 0.18 s. A fresh
  check of the result against `ctx` takes 0.3 s. `spec-meta/al` loads in
  0.1 s. `test/context.rkt` takes 4.6 s, and the whole suite 17 s.
- Written as watsup's recursion, with `((EXTTYP id) any_t ...)`-style clauses
  and caching on, `$load` took 15.6 s on `spec/`. Two causes made up most of
  it, and `load/shallow` now avoids both:
  - Redex matches an ellipsis in a pattern with bindings in quadratic time. At
    every point where the ellipsis could stop, it rebuilds the bindings so
    far, even when nothing follows the ellipsis. One match of `(any ...)` on
    1,000, 2,000, and 4,000 elements took 4.6, 14, and 42 ms *(measured)*.
    Matching the rest of the script once per definition cost about 8 s on
    `spec/`. Taking the script apart with `uncons` instead brought the load to
    about 8 s.
  - `load/shallow`'s calls never repeat, so caching gave no hits. It still
    cost 4 to 5 s of hashing and comparisons, mostly in the nonterminal memo.
    With caching off around `load/shallow`, the load takes 2.3 s. Placing the
    `parameterize` around the call to `load` instead is fragile. A cached
    `load/shallow` call on another copy of the script then makes every lookup
    a deep `equal?` (see [Caching](#caching)).
- A nonterminal is matched without bindings, in linear time. The nonterminal
  `mp ::= (pr ...)` checked 8,000 pairs in 1.4 ms, while the pattern
  `(pr ...)` took 207 ms *(measured)*. So the `map` contracts on the global
  maps are cheap, and only ellipses in clause, `where`, and domain patterns
  cost quadratic time.
  This is input for Step 6 and Step 13, for frames such as
  `(TUP ((OK val) ... hole exp ...))` over long lists.
- A Racket fold over the definitions was about as fast as `uncons` (3.1 s
  against 3.4 s, with caching off, in a probe). It hides watsup's recursion,
  so `load/shallow` keeps one recursive clause per watsup clause.

### Step 5: Type casts and subtyping

Transcribe `al/5.1-eval-typ`: `$upcast`, `$downcast`, `$subtyp`, and
`$subtyps`. These have the spec's largest complements. The `otherwise` of
`$upcast` and `$downcast` covers every type they don't cast, aliases that
aren't found, component casts that fail, and tuples of the wrong length. The
`otherwise` of `$subtyp` covers every mismatch.

- A clause whose premise results decide between it and `otherwise` computes
  them once, and passes them to a helper named after it (`upcast/var`,
  `upcast/tup`, `upcast/opt`, `upcast/list`, the same for `$downcast`, and
  `subtyp/var`). The helpers branch on the `typdef` found, or on the component
  casts.
- In `$subtyp`, the `VARIANT` case checks `mixop <- mixop_case*` and then uses
  `$assoc_` to look up the case's types. The `VAR` clauses are disjoint by the
  kind of `typdef` found (`EXT`, `ALIAS`, `VARIANT`, or `STRUCT`). `$subtyp`
  has no `FUNC` clause, so no value matches type `FUNC`.
- `$upcast` and `$downcast` give `FAIL` only where `INT` (for `$downcast`,
  `NAT`) meets a non-number, or `TUP` meets a non-tuple, possibly through
  aliases. Every other miss is `OK val` with the value unchanged, including a
  failing component cast: `$upcast(C, TUP INT, TUP (BOOL true))` is
  `OK (TUP (BOOL true))`. The K port differs here.
- Tests: one per clause and one per complement case, and every clause except
  `⊥` reached (`make-coverage` on the metafunctions).

Outcome:

- Done. `test/eval-typ.rkt` passes (132 checks). The whole suite runs 2,938
  checks, and 2,908 with contracts off, on a scratch copy.
- `upcast` and `downcast` take `G L`, since `$find_typ` reads both layers.
  So do `upcast/var` and `downcast/var`. `subtyp` keeps `tdenv`. The module
  is written on `al-context`; the language `al` comes in Step 6.
- A complement that dispatches on a failed `$subst_typ` matches `⊥`
  literally, with `(where ⊥ (subst-typ theta typ))`. The complements in
  `subtyp/var` recompute the pure premises (`$subst_typ`, `$assoc_`, and the
  membership check), but never `$subtyp`, which appears only in results.
- In the `VARIANT` clause of `subtyp/var`, the named ellipsis `..._m` of `val*`
  also constrains the `where` that binds the substituted case types. So a case
  with another number of types than values fails that clause and reaches its
  complement, instead of raising an ellipsis error.
- One test counts calls in Redex's trace, with caching off. An upcast of tuples
  nested 8 deep, each with a failing sibling, makes 17 calls, so each
  component is cast once. The same holds for `downcast`.
- There is no `make-coverage` check, as in Step 3. The tests have a case for
  every clause and every complement.

### Step 6: The machine

Build the language `al`, the contexts, the IN and FAIL rules, `->al`, and the
driver. Try them on a first slice of `Eval_exp` before writing the rest.

- `al/4-relation.rkt`: the language `al`, with `conf`, `typsres`, `res`, the
  machine terms `e` (starting with `IN` and the literal, variable, unary, and
  tuple forms of `Eval_exp`), `done`, `Fr-pass`, `Fr-catch`, `Fr`, `F`, and
  `E`.
- `al/5.3-eval-exp.rkt`: the rules of `Eval_exp/literal`, `Eval_exp/variable`,
  `Eval_exp/unary`, and `Eval_exp/tuple`, with their complements.
- `al/5-eval.rkt`:
  - the rules `"in/ok"`, `"in/fail"`, and `"frame/fail"`
  - `->redex` and `->ctx`, as unions of the fragments' relations
  - `->al`
  - the driver: `(step conf)`, `(run conf)`, the checks listed in
    [The notion of reduction and its closure](#the-notion-of-reduction-and-its-closure),
    and a cross-check mode against `->al`
- `test/machine.rkt`: helpers that run a term under a given `G` and `L` and
  return the result and final layer, and that check rule coverage.
- Tests:
  - every step of the slice's tests agrees with `->al`
  - a relation with two overlapping rules, and a term with two pending
    positions, make the driver fail; a stuck term reports itself
  - `FAIL` propagates through nested frames and `IN` nodes, and `(IN L OK)`
    stays put
- Measure the step time on the slice, with nested `IN` nodes and the real
  `G` of `spec/`, and record it.

Done when the slice's tests pass through the driver and the cross-check.

Outcome:

- Done. `test/eval-exp.rkt` (37 checks) and the `test` submodule of
  `test/machine.rkt` (31) pass, with every step cross-checked against `->al`
  and every configuration checked against `conf`. The whole suite runs 3,006
  checks in 26 s, and 2,976 with contracts off, on a scratch copy.
- `e` contains all of `exp`, plus the partly evaluated forms `(UN unop e)` and
  `(TUP ((OK val) ... e exp ...))`. An `IN` can only be at a pending position,
  so the grammar rejects `(TUP ((VAR "x") (IN L e)))`.
- Redex rejects a nonterminal with no productions. So `Fr` is `Fr-pass` alone
  until the first frame that catches `FAIL`.
- The driver provides `step`, `run`, `run/trace` (which also returns the rule
  names in order), and `final?`, with the parameters `cross-check?` and
  `check-conf?`. A `machine` holds the frames, `->redex`, `->ctx`, and the
  `->al` to cross-check against. `(frames-of lang)` matches
  `(in-hole Fr any)` one level deep. The tests use these to build drivers with
  an extra frame or rule; `al-machine` is the specification's.
- A `->ctx` result must keep `G` (by `equal?`), as the closure rule requires
  by repeating `G`. In `->al`, `(where (_ ... (e_1 G L_1) _ ...) ...)` gives
  one successor per focus step, so two rules show up as two successors.
- `make-coverage` is a macro. On a fragment relation, it counts the fragment's
  rules when the union `->redex` applies them *(measured)*.
  `apply-reduction-relation/tag-with-names` records coverage as well. The
  cross-check runs with `relation-coverage` empty, so only the driver's
  applications count.
- Step time, with contracts on and the `G` of `spec/`, on a tuple nested
  D deep with an `IN` at each level *(measured)*:

  | Depth D | 10 | 50 | 200 |
  | --- | --- | --- | --- |
  | driver | 0.15 ms | 0.7 ms | 6.8 ms |
  | driver, a 27,000-cons value in each tuple's evaluated prefix | 3.2 ms | 15 ms | 76 ms |
  | cross-check on | 1.5 ms | 12 ms | |
  | `conf` check on | 0.54 ms | 4.2 ms | |

  An empty `G` gives the same driver times. The first rule that matches a
  fresh `G` against `layer` takes 0.58 s, and later matches hit the memo.
  If another layer takes `G`'s slot in the 63-entry memo, the next match pays
  that again.
- Step time grows faster than depth because of the nonterminal memo. Every step
  matches `Fr` at each node on the path to the redex, and the memo's key is
  the node. Nodes along a deep path look alike to the bounded hash, so they
  collide, and each lookup costs a deep `equal?`.
  - The driver checks `done` and `res` by their outer shape; `check-conf?`
    checks them precisely. With the `done` nonterminal, depth 200 took
    36 ms per step.
  - Matching `Fr` with caching off avoids the collisions (1.7 ms per step at
    depth 200). But it then rechecks every evaluated sibling on every step:
    66 ms per step at depth 10 with the large value, against 3.2 ms with
    caching on. So `Fr` stays cached.
  - Step 13's "resume from the last hole" avoids matching the path again.

### Step 7: Assignment

Transcribe `al/5.2-eval-assign` as reduction rules on `assign-exp`,
`assign-exps`, `assign-arg`, and `assign-args`, and their intermediate forms
(`assign-exp/cons`, `assign-exp/opt-some`, `assign-exp/list`,
`assign-exps/cons`, `assign-args/cons`). A successful assignment updates the
innermost `IN`'s layer and reduces to `OK`. An input no rule matches reduces to
`FAIL` (see [FAIL and backtracking](#fail-and-backtracking)).

- The four `Assign_exp/iter` rules are disjoint by the result of
  `$is_iter_on_var` (a pure call) and by the shape of the value: `simple`,
  `opt-none`, `opt-some`, and `list`. Test them hardest.
- `Assign_exp/list` assigns each element under `C_local`, the layer with an
  empty `VAL` map, as `(IN L_local (assign-exp exp val))`. It then collects
  the bindings with `$finds_vari` from the finished `(IN L_sub OK)`s.
- `Assign_exp/opt-some` takes the fix in
  [Deviations from spec-meta](#deviations-from-spec-meta).
- `Assign_arg/fun` looks the function up with `(find-func G L_caller id)`.
- Behavior the tests pin down, as the spec states it:
  - An expression other than `VAR`, `TUP`, `INJ`, `STR`, `OPT`, `LIST`, `CONS`,
    and `ITER` has no rule, including a literal. The elaborator turns a literal
    in a pattern into a fresh variable plus a check.
  - A repeated variable takes the last value, with no equality check.
  - `iter/simple` binds the iterated variable to any value.
  - `iter/opt-some` keeps the inner assignment's bindings in the context.
    `iter/list` does not, because it assigns under `C_local`, and so it does
    not find a variable bound only outside the iteration.
- Tests: every constructor, values that match no rule, and every rule used.

### Step 8: Expressions, paths, and arguments

Complete `al/5.3-eval-exp` and `al/5.4-eval-arg`: `Eval_exp` without `CALL`,
`Eval_path`, `Eval_path_upd`, `Eval_arg`, and `Eval_targs`.

- Keep watsup's evaluation order, and evaluate nothing watsup would not:
  - `Eval_exp/binary`, `compare`, and `concat` check the left operand's kind
    before evaluating the right one.
  - `Eval_path/slice` checks each index before evaluating the next.
  - `Eval_path_upd/slice` needs the new value to be a text or a list before it
    evaluates anything.
  - `CONS`, `MEM`, and `UPD` evaluate their two operands left to right.
- `Eval_exp/iter` decides between its rules by pure calls (`$is_iter_on_var`,
  `$find_varr`, `$sub_opt`, `$sub_list`). The `opt` and `list` rules reduce to
  intermediate forms whose elements are `(IN L_sub exp)`, one per sub-context.
  `$sub_opt` or `$sub_list` giving `⊥` is a complement rule to `FAIL`.
- `Eval_exp/opt`'s two rules are one rule over `(OPT (exp ...))`, whose premise
  is `(Eval_exp: C |- exp : OK val)?`.
- Texts are indexed by UTF-8 bytes, as OCaml's `String` is: `|"é"|` is 2. A
  result that splits a character raises an error. Racket's own string
  operations count characters, so the helpers in `common/0.1-stdlib.rkt` work
  on bytes.
- The elaborator adds `n < |x|` to `Eval_path/idx`, so an index out of bounds
  is `FAIL`. Nothing bounds slices or updates, so an index or slice out of
  bounds there raises an error, as in the meta-circular run. Read the elaborated
  AL for such inserted premises; they do not show in the `.watsup`.
- `Eval_path_upd/slice/list` takes the fix in
  [Deviations from spec-meta](#deviations-from-spec-meta).
- `Eval_targs` reduces `(eval-targs (targ ...))` to `(OK (typ ...))`, or to
  `FAIL` when `$theta_of_tdenv` or `$subst_typ` gives `⊥`.
- `debug` writes the Redex term to stderr.
- Tests: `test/eval-exp.rkt` and `test/eval-arg.rkt`, with every rule used,
  and a case for each evaluation-order rule above.

### Step 9: Premises

Transcribe `al/5.5-eval-prem`: `Eval_prem` and `Eval_prems`.

- A premise updates the innermost `IN`'s layer and reduces to `OK`.
  `(eval-prems (prem ...))` runs the premises in order, in one `IN`.
- `Eval_prems/head-fail` and `head-succ` share their premise, so they become
  one intermediate form that waits for the head.
- `Eval_prem/iterpr-opt` and `iterpr-list` run the premise in
  `(IN L_sub prem)` per sub-context, and bind the results from the finished
  `(IN L_sub_res OK)`s into the enclosing layer.
- `IFNOTHOLD` looks the relation up itself (see
  [FAIL and backtracking](#fail-and-backtracking)).
- `Eval_prem/dbg` prints in its rule's `where` clause.
- Tests: `test/eval-prem.rkt`. `REL`, `IFHOLD`, and `IFNOTHOLD` need
  `Call_rel`, so their tests come with Step 10.

### Step 10: Function and relation calls

Transcribe `al/5.6-eval-call-func` and `al/5.7-eval-call-rel`, and
`Eval_exp/call`. Work through them in this order, with tests before moving on:

1. `Eval_clause(s)`, `Eval_tblrow(s)`, `Call_table_func`, `Call_defined_func`,
   `Call_func_dispatch`, and `Call_func`, then `Eval_exp/call`.
   - `Call_defined_func` reduces to
     `(IN L_callee (eval-clauses L (clause_all ...) (val ...)))`, where `L` is
     the caller's layer and `L_callee` binds the type parameters. It gives
     `FAIL` when the numbers of type parameters and type arguments differ.
   - `eval-clauses` runs each clause in `(IN L_callee (eval-clause ...))`, a
     copy of the callee layer, so a failed clause leaves no bindings. The
     `cons-succ`/`cons-fail` pairs share their premise, so they become one
     intermediate form with the catcher frame.
   - `eval-tblrow` builds its own `(IN L_empty ...)`, and assigns the arguments
     with the caller's layer as `L_caller`.
   - `Call_func_dispatch/table` with type arguments has no rule in watsup, so
     it is a complement rule to `FAIL`.
   - `Eval_exp/call` evaluates `Eval_targs`, then the arguments, then calls
     `Call_func` with `typ_input*` (see
     [Deviations from spec-meta](#deviations-from-spec-meta)).
2. `Eval_rul(s)`, `Eval_rulgroup(s)`, `Call_defined_rel`, `Call_rel_dispatch`,
   and `Call_rel`, then the `REL`, `IFHOLD`, and `IFNOTHOLD` premises.
   - `Call_defined_rel` reduces to `(IN L_empty (eval-rulgroups ...))`, and
     `eval-ruls` runs each rule path in a copy of that layer.
3. The extern rules, whose Racket procedures still raise an error.

- Tests: `test/eval-call-func.rkt` and `test/eval-call-rel.rkt`, with small
  hand-built scripts or scripts booted through Step 2. They cover clause
  backtracking that must leave no bindings, a table function, a `FUN`
  argument, a polymorphic call from a context where `X` is `NAT`, an unknown
  relation under `IFNOTHOLD`, and deep recursion. Every rule used, except the
  extern rules.

### Step 11: Entry and driver

- `al/6-entry.rkt`: `Entry` loads the script into `$empty_ctx`, then runs
  `(G (IN L_0 (CALL "main" () ())))` to a result, printing `Entry`'s `debug`
  messages to stderr. For a P4 program it runs
  `(G (IN L_0 (call-rel "Program_ok" (val_p4))))`, as K's `afterLoad` does.
- `main.rkt`: `racket spec-meta-redex/main.rkt FILE.watsup` boots the file,
  runs `Entry`, and prints the value in `k-run.sh`'s JSON format, or `fail`.
- Compile with `raco make spec-meta-redex/main.rkt`.
- Test: the examples that need no builtins produce the same output as
  `k-run.sh` and the meta-circular run: `add` 119, `fibo` 89,
  `iter-nontrivial` -42, `iter-sequence` 1085, `mutual-recursion` 289,
  `relation-typing` 110, and `variant-tree` 6. Record each one's time and
  number of steps. The tests run the fast ones, with the cross-check on.

### Step 12: Builtins and externs

- `common/0-extern-json.rkt`: a Racket codec for the wire format documented in
  `extern_json.ml` (`val`, `typ`, `mixop`, request, and response), built on
  Racket's `json` library.
- Add `spectec-boot extern-serve`. `common/0-extern-wire.rkt` starts it on the
  first extern call and shuts it down at exit.
- Replace the Step 3 stubs for the three host procedures, and add the native
  map builtins.
- Tests:
  - The `builtin-*.watsup` examples match `k-run.sh`.
  - Values from `sexp-p4` survive a round trip through the codec.
  - A script that declares `builtin dec $fresh_typeId` and calls it twice gets
    two distinct ids, with caching on. This fails if an impure call sits behind
    any cache.

### Step 13: P4 type checking and performance

- `racket spec-meta-redex/main.rkt --p4 PROGRAM spec` boots `spec/`, boots
  PROGRAM with `sexp-p4`, runs `Program_ok` on it, and prints `passed` or
  `fail`.
- Run only the three benchmark programs, smallest first:
  `p4c/testdata/p4_16_samples/action-bind.p4`, `checksum-l4-bmv2.p4`, and
  `dash/dash-pipeline-v1model-bmv2.p4`. Also run the negative
  `p4c/testdata/p4_16_errors/action-bind.p4`, which must print `fail`.
- Expect this to be much slower than K, which already takes about 3 minutes on
  the large program. Profile first (Racket's `profile` library). Then try
  these, roughly in order of expected payoff:
  1. turn contracts off for P4 runs;
  2. let the driver resume from the last hole instead of descending from the
     root on every step;
  3. dispatch on the redex's head symbol to the few rules for that form, with
     one sub-relation per form, still combined into `->redex` and `->ctx`;
  4. tune `set-cache-size!`, and order metafunction arguments so that the
     large ones (`G`, `tdenv`) come last and the cache's hash sees the small
     ones;
  5. match `G` and `L` shallowly in rules, but only after asking the user,
     who asked for precise patterns;
  6. store the global maps as Racket immutable hashes behind the same builtin
     metafunctions;
  7. move the wire to `ffi/unsafe`, if extern calls show up in the profile.

  After each change, rerun the `$fresh_typeId` test and the negative program.
- Done when the small program passes, and the medium and large results are
  recorded with timings. This step answers whether the large program is within
  reach.

### Step 14: Test targets and docs

- `make redex-test`: runs `raco make` and `raco test` on
  `spec-meta-redex/test`. It also checks the examples against checked-in
  expected outputs, so K need not be built. `SPECTEC_REDEX_CONTRACTS` is read
  at compile time, so a run with contracts off needs its own compiled code.
- Render the grammar, `->redex`, `->ctx`, and the closure rule to figures with
  `language->pict` and `reduction-relation->pict`, for side-by-side review
  against the watsup rules.
- Rewrite this file as an overview of the finished port, like
  [`CROSS.md`](CROSS.md).
