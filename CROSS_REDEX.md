# Specifying P4-SpecTec AL in PLT Redex

Status: done. Steps 1 to 14 are done.

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
with the same name *(measured)*. A rule that runs in stages keeps its name for
the first stage, and each later stage adds a word for what it does:
`"assign-exp/cons"` assigns the head, and `"assign-exp/cons/tail"` the tail.
Two rules that share their first premise share that stage, which is named
after the common part of their names: `"eval-clauses/cons"` runs the head
clause, and then `"eval-clauses/cons-succ"` or `"eval-clauses/cons-fail"`
applies. Complement rules end in `fail`:

- `".../fail-<subterm>"` where a subterm just evaluated fits no rule, as in
  `"eval-exp/binary/fail-left"`;
- `".../fail"` where the last subterm evaluated fits no rule;
- `".../<rule>/fail"` where a rule's own premise fails, as in
  `"eval-exp/binary/number/fail"`, on a `⊥`.

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
intermediate form that holds them. The form is named after the rule that
builds it: `<relation>/<rule>`, `<relation>/<group>/<rule>` for a rule in a
group, or `<relation>/<group>` when the group's rules share the premise. When
two rules share their first premise, the form is named after the common part
of their names, as `eval-clauses/cons` is for `Eval_clauses/cons-succ` and
`cons-fail`. Its frames give the positions in the order the premises run. For
example, `Eval_path/idx` becomes `(eval-path/idx (eval-path val path) exp_i)`:
the inner path first, and the index once the base is a text or a list. These
forms play the role of K's `...AwaitK` items.

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

1. It holds the configuration as a cursor: a subterm in focus, the layer of
   its innermost `IN`, and the nodes on the path from it to the root. A run
   starts with the root's body in focus.
2. It climbs from the focus past the nodes whose subterm is `done`, and
   rebuilds each one. From the first node that is not `done`, it descends: at
   each node it matches `Fr` one level deep, and moves into the one evaluation
   position whose subterm is not `done`.
3. The node where no such position exists is the redex. The driver applies
   the rules of `->redex` and `->ctx` for the redex's head symbol to it once,
   and puts `r'` in focus under `L'`.

A node's other subterms do not change while its pending subterm is evaluated,
so its pending position stays the same until that subterm is `done`. So a step
matches `Fr` only at the nodes it enters, not along the whole path from the
root. The driver fails loudly if two evaluation positions are pending at a
node it enters, if the rules give two results, or if a configuration that is
not final has no step. It loops with `apply-reduction-relation`, and never
uses `apply-reduction-relation*` (see below). The rules, the grammar, and
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
codec and transport live in `common/`: the host procedures in
`common/4-relation.rkt` call them. They mirror K's `4.1-extern-json.k` and
`4.2-extern-ffi.k`. The transport's C shim is in `ffi/`, as K's is in
`spec-meta-k/ffi/`.

```text
spec-meta-redex/
  common/
    0.0-prelude.rkt       Redex re-exports; caching on; define-dec; reduction-relation/forms
    0.1-stdlib.rkt        language stdlib; $ite, $opt_as_seq_, $exists_, ...; builtins; text and list helpers; debug
    0.2-extern-json.rkt   codec for the extern JSON wire
    0.3-extern-ffi.rkt    transport to the OCaml host, over ffi2
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
    6-entry.rkt           Entry: load, then run $main() or a relation; the command-line driver
  test/                   unnumbered
    p4-typecheck.rkt      make redex-test: type-checks the P4 samples; raco test skips it
  ffi/
    shim.c                C shim between 0.3-extern-ffi.rkt and p4spec/bin/ffi.ml
```

`common/0.0-prelude.rkt` defines `define-dec`, which wraps
`define-metafunction`. It appends the `⊥` clause, and builds the contract from
the watsup declaration. `SPECTEC_REDEX_CONTRACTS=0`, read when a module is
compiled, drops the contracts.

The fragments build their relations with `reduction-relation/forms`, in place
of `reduction-relation`. It groups the rules by the head symbol of their
left-hand side, which for a focus triple is the head of the redex. The
relation is the union of one relation per run of consecutive rules with the
same head, in the order of the source. `union-reduction-relations/forms`
keeps the groups, and `(relation-for-head rel head)` gives the rules of `rel`
for a head. A rule whose left-hand side has no literal head symbol, such as
`(in-hole Fr-pass FAIL)` or `num`, belongs to every head. Every term that a
rule's left-hand side matches has that rule's head, so the rules for a head
give the same steps as the whole relation.

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
reduction rule -> common/4-relation.rkt -> common/0.2-extern-json.rkt
  -> common/0.3-extern-ffi.rkt --ffi2--> spec-meta-redex/ffi/shim.c
  --caml_callback--> p4spec/bin/ffi.ml -> SpecTec runner
```

The OCaml side is [`ffi.ml`](p4spec/bin/ffi.ml), shared with the K port, with the
same JSON requests and replies. K links the OCaml runtime, `p4spec/`,
`ffi.ml`, and its C shim, [`spec-meta-k/ffi/shim.c`](spec-meta-k/ffi/shim.c),
into its interpreter. Redex has a C shim of its own,
`spec-meta-redex/ffi/shim.c`, and loads it and the OCaml side into the Racket
process as shared objects. K's shim and its build stay as they are. Redex
calls the shim with Racket's
[`ffi2`](https://docs.racket-lang.org/ffi2/index.html) library, a more static
C FFI than `ffi/unsafe`, which compiles foreign calls at `raco make` time. One
OCaml runtime per Racket process keeps `$fresh_typeId`'s counter consistent
across calls, as one per `krun` does in K. Probes in a scratch layout checked
each point below *(measured)*. The OCaml shared object was linked by hand, with
dune's link command plus `-runtime-variant _pic`, and a toy dune project
checked the stanza:

- **Building.** There are two shared objects:
  - `_build/default/p4spec/bin/ffi.so` holds the OCaml runtime, `p4spec/`,
    and `ffi.ml`. Dune builds it once the `ffi` executable's modes are
    `object shared_object`. Dune links the `shared_object` mode with
    `-runtime-variant _pic`, and the `object` mode as before, so K's
    `ffi.exe.o` does not change. `ffi.exe.o` itself cannot go into a shared
    object: it embeds the non-PIC `libasmrun.a`, and `gcc -shared` rejects a
    `R_X86_64_TPOFF32` relocation against `domain_self`. The link reuses the
    compiled libraries and takes under a second.
  - `spec-meta-redex/ffi/shim.so` is the Redex shim, compiled against
    `ffi.so` with `-l:ffi.so` and the run path
    `$ORIGIN/../../_build/default/p4spec/bin`. Racket loads only `shim.so`,
    and the dynamic loader finds `ffi.so` from the shim's own location,
    whatever the working directory.
- **The shim.** It provides two functions:
  - `host_init(spec)` starts the OCaml runtime on its first call. It then
    builds the runner for `spec` through `ffi.ml`'s `ml_init`, called with
    `caml_callback_exn`. It returns 1, 0 if `ml_init` raised (for example, on
    a spec path that does not exist), and -1 if `ffi.ml`'s callbacks are
    missing. The transport raises a Racket error on anything but 1, and the
    process survives.
  - `host_eval(request)` returns the reply in a buffer that the shim owns and
    frees on the next call. An OCaml exception gives the same
    `{"error": ...}` reply as K's shim. So the Racket side declares it as
    `(-> string_t string_t)`, which copies the reply into a Racket string
    before the next call, and needs no `free` or length function.
- **Loading.** `0.3-extern-ffi.rkt` loads `shim.so` on the first extern call,
  so a run without one never loads it. If the file is missing, the error names
  `make redex`. It binds each function with
  `(ffi2-procedure (ffi2-lib-ref lib "host_eval") (-> string_t string_t))`.
  The docs give `(ffi2-lib-ref name lib)`, but ffi2-lib 1.1 takes the library
  first. The spec path goes as `string_t`: ffi2's `path_t` accepts only path
  objects. `dlopen` takes 12 ms.
- **Initialization.** As with K's `<specdir>`, the spec path is the file being
  run: the script, or `spec/` for a P4 program. `entry` takes a booted script,
  so the path comes from a parameter, `host-spec`, that the command-line
  driver and the tests set. A call while it is `#f` raises. The path goes to
  OCaml as a complete path, since OCaml resolves a relative one against the
  process's working directory, not Racket's `current-directory`. A call under
  a different path runs `host_init` again. That replaces the runner, while
  `$fresh_typeId`'s counter keeps counting. If `host_init` fails, the previous
  runner stays. The runner cannot be built from a script with a relation that
  has no input hint, so a test script that is also `host-spec` gives each
  relation a `hint(input ...)`. `host_init` takes 95 to 115 ms on the
  examples and 1.1 s on `spec/`.
- **Calls.** Text crosses as UTF-8 in both directions. A small builtin call
  takes 5 µs (20,000 calls); Racket's JSON encoding and decoding added about
  10 µs per call in an earlier probe. `{"fail": null}` becomes `FAIL`.
  `{"error": msg}` raises a Racket error with OCaml's message, since runtime
  errors are not `FAIL`. K has no rule for it, so it gets stuck there.
- **One OS thread.** The OCaml runtime belongs to the OS thread that started
  it. All Racket threads of one place run on that place's OS thread, so the
  driver can call it freely, but no other place may. `raco test` runs several
  files in separate processes by default, so each file gets its own runtime;
  do not pass `--place`.
- **Output.** OCaml writes its diagnostics (elaboration warnings, failed
  extern calls) to file descriptor 2 directly, not through Racket's
  `current-error-port`. So a test that captures stderr cannot see or silence
  them, and only scripts without extern calls should have their stderr
  compared. Subprocesses, such as the one `boot-script` starts, still work
  after the runtime is up.

Unlike K's interpreter, which embeds a snapshot of `p4spec/` when it is
kompiled, Redex loads `ffi.so` at run time. After editing `p4spec/`,
`make redex` rebuilds it, and its `raco make` has nothing to do.

Every object-level builtin goes to the host, including the map builtins
(`find_map`, `find_maps`, `add_map`, `adds_map`, `update_map`, `assoc_`) that
the K port implements natively (`nativeBuiltin` in `5.5-eval-call-func.k`).
Step 13 lists native versions as an option, to take only after asking the
user. The object-level builtins are separate from the meta-level ones,
such as `find-map` in `common/0.1-stdlib.rkt`, which work on Redex's own
environments and stay metafunctions.

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
  the value through `Entry`'s `debug`). `al/6-entry.rkt`, run as a program,
  prints results in `k-run.sh`'s JSON format, so the outputs can be compared
  as text. `k-run.sh` prints the debug messages on stdout before the result,
  and `al/6-entry.rkt` prints them on stderr, so its stdout is the last line
  of `k-run.sh`'s. Where K differs from watsup, compare with the meta-circular
  run.
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
- **Coverage.** A test file ends by checking that every rule of the relations
  it exercises was applied. The helpers
  in `test/machine.rkt` count the rules that the driver applies, by name, as
  it applies them, so a run that raises still counts the rules before.
  Redex's `make-coverage` is not used: it counts a rule once the rule's
  left-hand side and first `where` match, even when a later premise fails (see
  Step 7), and `->al` applies the rules at every candidate position.
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

### Building

```sh
make redex    # spectec-boot, ffi.so, shim.so, and raco make on al/6-entry.rkt
```

`spectec-boot` boots scripts and P4 programs, and `ffi.so` and `shim.so` are
the host of builtins and externs. The Racket modules run without `raco make`,
but compile in memory on every run: `examples/add.watsup` takes 8.6 s that
way, and 0.9 s compiled *(measured)*. Once `compiled/` exists, a stale `.zo`
is loaded without a check of its dependencies, so run `make redex`, or
`raco make` on the module to run, after editing a module.

### Running a script

```sh
raco make spec-meta-redex/al/6-entry.rkt
racket spec-meta-redex/al/6-entry.rkt examples/add.watsup   # ["intN","119"]; debug messages on stderr
racket spec-meta-redex/al/6-entry.rkt --p4 p4c/testdata/p4_16_samples/action-bind.p4 -i p4c/p4include spec   # passed, in about 15 s
```

`--p4` takes `-i DIR` for each P4 include directory, and needs at least one.

### Type-checking the P4 samples

```sh
make redex-test                       # every sample and every error program, minus the excludes
raco make spec-meta-redex/test/p4-typecheck.rkt
racket spec-meta-redex/test/p4-typecheck.rkt \
  --p4-dir p4spec/test/micro/programs -e excludes/static -i p4c/p4include spec
```

`make redex-test` runs the same command twice: with
`--p4-dir p4c/testdata/p4_16_samples`, whose programs must pass, and with
`--p4-dir p4c/testdata/p4_16_errors --neg`, whose programs must fail. It
fails if either run does. `--p4-dir`, `-e`, `-i`, and the spec are required.
Each of the three flags can be repeated. `-d` lists the programs that would
be checked. Entries in the `.exclude` files are relative to the repository
root. The 11 micro programs take about 5 minutes.

`make redex-test` does not run the other tests in `spec-meta-redex/test`, and
`raco test` does not run `p4-typecheck.rkt`'s programs. There is no timeout.
Each program passes or fails, and one that raises fails. The outcomes go to
`spec-meta-redex/p4-typecheck-pos.result`, or `p4-typecheck-neg.result` with
`--neg`, as they come; `-o FILE` names another. Under each program that
raises, the result file also has its stderr and error message, indented.
The programs without the expected result are listed at the end, under
`failing`.

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

To compare Redex's output with K's:

```sh
diff <(racket spec-meta-redex/al/6-entry.rkt examples/add.watsup 2>/dev/null) \
     <(./spec-meta-k/scripts/k-run.sh examples/add.watsup | tail -n 1)
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
- Add `**/compiled/` to `spec-meta-redex/.gitignore`.

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
  the result, so that tests and `al/6-entry.rkt` accept `.watsup` paths
  directly.
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
(`assign-exp/cons`, `assign-exp/iter/opt-some`, `assign-exp/iter/list`,
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

Outcome:

- Done. `test/eval-assign.rkt` passes (101 checks), with every step
  cross-checked against `->al`, every configuration checked against `conf`,
  and every rule of `->redex/eval-assign` and `->ctx/eval-assign` applied.
  With Step 8, the whole suite runs 3,389 checks in 22 s, and 3,359 with
  contracts off, on a scratch copy.
- The forms of the two `Assign_exp/iter` rules are `assign-exp/iter/opt-some`
  and `assign-exp/iter/list`. The planned `assign-exp/opt-some` and
  `assign-exp/list` read as the forms of `Assign_exp/opt/opt-some` and
  `Assign_exp/list`, which need none. So a form is now named after the rule
  that builds it, with the rule's group (see [Machine terms](#machine-terms)).
- The rules that write `L` or read `G` are in `->ctx`: `variable`,
  `iter/simple`, `iter/opt-none`, the start of `iter/list` (which builds
  `C_local` from `L`), the binding stages of `iter/opt-some` and `iter/list`,
  and `Assign_arg/fun`. The rest are in `->redex`.
- `Assign_exp/str` matches `(STR ((atom exp) ...))` against
  `(STR ((atom val) ...))`. The repeated `atom` makes the atoms equal in order,
  and so the lengths equal, as the checks in the elaborated rule do.
- The complement of `Assign_exp` is two rules: `"assign-exp/fail"` for an exp
  other than `ITER` that no rule matches, and `"assign-exp/iter/fail"` for an
  `ITER` whose iterator and value fit no rule. `Assign_arg/fun` has one
  complement per premise: a value other than `FUNC` (in `->redex`), and a
  function that the caller lacks (in `->ctx`, since it reads `G`).
- An assignment that fails partway leaves the bindings made so far in the
  layer: `TUP (VAR a) := TUP (1, 2)` binds `a` before `Assign_exps` fails. The
  enclosing premise, clause, or rule path drops that layer with its `IN`, so
  the tests check only the result.
- Redex's `make-coverage` counts a reduction rule once its left-hand side and
  its first `where` match, even when a later `where` or `side-condition` fails
  *(measured)*. `"assign-exp/iter/fail"`, which four tests reach, was counted
  26 times. In Redex's `build-rewrite-proc/leaf`, a later premise that fails
  gives `'()`, which counts as a match. So `test/machine.rkt` now counts the
  rules that the driver applies, from `run/trace`. `start-coverage` takes the
  relations, and `check-coverage` reports their rules that were never applied.

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

Outcome:

- Done. `test/eval-exp.rkt` (266 checks), `test/eval-arg.rkt` (19), and the
  helper tests in `test/common-stdlib.rkt` pass, with every rule of the three
  fragments applied.
- `al/4-relation.rkt` has a frame for every evaluation position. A position
  that waits on an earlier result requires that result in its frame, as
  `(BIN boolbinop (OK (BOOL b)) hole)` does. That covers each case of
  evaluation order above, and the tests put `(BIN DIV (NAT 1) (NAT 0))` at each
  position that must not be evaluated.
- Complement rules follow the naming in
  [Relations and functions](#relations-and-functions). `Eval_path/slice` has
  `"fail-base"`, `"fail-index"`, and `"fail"`, one for each result that can fit
  no rule. `"eval-exp/binary/number/fail"` and `"eval-exp/compare/number/fail"`
  match the `⊥` of `$binop_number` and `$cmpop_number` on a nat and an int.
- `$upcast`, `$downcast`, and `$subtyp` are total, so `Eval_exp/upcast`,
  `downcast`, and `subtype` have no complement. A `⊥` would leave the driver
  stuck, and it would report the term.
- With the fix to `Eval_path_upd/slice/list`, both slice rules take the new
  value's kind as an input pattern. So a new value that is neither a text nor a
  list fails before anything is evaluated (`"eval-path-upd/slice/fail-value"`),
  and a base of the other kind fails before the indices are
  (`"eval-path-upd/slice/fail-base"`). `Eval_path_upd/idx/list` takes any new
  value, so `IDX` always reads the inner path.
- The meta-circular run indexes texts by UTF-8 bytes, and a slice is a start
  and a length: `|"é"| + 10·|"abcd"[1 : 2]| + 100·|"aéb"[3]| + 1000·|"aé"[[0] = "x"]|`
  is 3122 *(measured)*. The helpers in `common/0.1-stdlib.rkt` (`text-len`,
  `text-idx`, `text-slice`, `text-upd`, `text-upd-slice`, and the `list-`
  ones) give the same results, and their errors follow the messages of
  `interp.ml`. Where OCaml returns a text that splits a character, the helpers
  raise.
- Division by zero raises Racket's own error (`quotient: undefined for 0`) out
  of the driver.
- `Eval_path_upd/dot` updates every field with the atom, and leaves a struct
  without the atom unchanged, as the rule states.
- `$exists_((val_e = val)*)` in `Eval_exp/mem`, and `atom = atom_field` in
  `Eval_path_upd/dot`, compare in a Racket escape. A Redex template cannot
  unquote under an ellipsis.
- `debug` writes the term to stderr with `writeln`. `Eval_prem/dbg` uses it in
  Step 9.

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

Outcome:

- Done. `test/eval-prem.rkt` passes (75 checks), with every rule of
  `->redex/eval-prem` and `->ctx/eval-prem` applied. Step 10 was done with
  this step, so the file also tests `REL`, `IFHOLD`, and `IFNOTHOLD`, on
  relations booted from a watsup text.
- `IF`, `LET`, `DEBUG`, and the inputs of `REL`, `IFHOLD`, and `IFNOTHOLD`
  evaluate in place. The relation premises then call `call-rel` in
  `eval-prem/relpr`, `eval-prem/ifholdpr/hold`, or
  `eval-prem/ifholdpr/nothold`, whose frame is a catcher. The iterations wait
  in `eval-prem/iterpr-opt/some` and `eval-prem/iterpr-list/list`, and
  `Eval_prems` in `eval-prems/head`, which `head-fail` and `head-succ` share.
- `Eval_prem/fail` is `"frame/fail"` wherever a sub-relation fails, and
  `Eval_prems/head-fail` is too. The other cases are complement rules:
  - `"eval-prem/ifpr/fail"`: the condition is not a boolean.
  - `"eval-prem/ifholdpr/hold/fail"`: the relation has outputs.
  - `"eval-prem/ifholdpr/nothold/fail"`: the relation holds.
  - `"eval-prem/ifholdpr/nothold/fail-rel"`: there is no such relation.
  - `"eval-prem/iterpr-opt/fail"` and `"eval-prem/iterpr-list/fail"`:
    `$sub_opt` or `$sub_list` gives `⊥`.
  - `"eval-prem/iterpr-opt/some/fail"` and `"eval-prem/iterpr-list/list/fail"`:
    the premise leaves a variable to bind unbound.
- `IFHOLD` holds only if the relation gives `OK eps`, as watsup states.
  OCaml's AL interpreter accepts any outputs. The 14 hold premises of `spec/`
  all name relations without outputs *(measured)*, so only a hand-built term
  tells the two apart.
- `IFNOTHOLD` evaluates its inputs before it looks the relation up, so an
  input that raises also raises for an unknown relation.
- An optional iteration with no bound variable runs its premise once, and a
  list iteration with none runs it zero times, as `$sub_opt` and `$sub_list`
  give.
- `debug` writes once per step of the driver. The cross-check applies the
  rule again and writes a second time, which a test pins down.
- Changes to `test/machine.rkt`:
  - `run-in`, `eval-in`, and `trace-in` take `#:cross-check?`.
  - `boot-text` boots a watsup text through a temporary file, and
    `global-of` loads a script and gives its `GLOBAL` layer.
  - Coverage counts each rule as the driver applies it, through `step/rule`,
    which `al/5-eval.rkt` now provides. So a run that raises, such as an
    extern call, still counts the rules before.

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

Outcome:

- Done. `test/eval-call-func.rkt` (52 checks), `test/eval-call-rel.rkt` (22),
  and the call tests in `test/eval-exp.rkt` pass. Every rule of the two
  fragments and of `Eval_exp/call` is applied, except the three extern rules.
  The whole suite runs 3,544 checks in 34 s, and 3,514 with contracts off, on
  a scratch copy.
- A function call reduces through `eval-exp/call`, `call-func`,
  `call-func-dispatch`, and `call-defined-func` to
  `(IN L_callee (eval-clauses L (clause_all ...) (val ...)))`. Each clause
  then runs in `(eval-clauses/cons (IN L_callee (eval-clause ...)) ...)`. A
  relation call reduces through `call-rel`, `call-rel-dispatch`, and
  `call-defined-rel` to `(IN L_local (eval-rulgroups ...))`, and each rule
  path runs in `(eval-ruls/cons (IN L_local (eval-rul ...)) ...)`.
- `eval-clause/succ`, `eval-tblrow/succ`, and `eval-rul/succ` have three
  positions, in the order the premises run: the assignment,
  `(eval-prems (prem ...))`, and the output or outputs. Each position waits
  for `OK` in the one before.
- The rules in `->ctx` copy the layer (`eval-clauses/cons`,
  `eval-ruls/cons`), read the caller's layer (`eval-tblrow/succ`,
  `call-defined-func`), or look a definition up (`call-func`, `call-rel`).
  `call-defined-rel` builds its layer from `$empty_layer` alone, so it is in
  `->redex`.
- `Eval_rulgroup/succ` and `Eval_rulgroup/fail` are one rule,
  `"eval-rulgroup"`, which reduces to `eval-ruls`: the group's result is that
  of its paths.
- The elaborator adds `|tparam*| = |typ*|` to `Call_defined_func`. The rule
  states it as a named ellipsis that its left-hand side and a `where` share,
  and `"call-defined-func/fail"` is its complement.
- `outer<NAT>` calls `fits<Y>`, and the test checks that `fits` sees `NAT`,
  which needs the fix to `Eval_exp/call`. `thrice(def $inc, 3)` passes its
  function argument `k` on to `twice`, which finds `k` in `thrice`'s layer.
- The seven examples of Step 11 that need no builtins already give their
  expected values. Without the cross-check, `fibo` takes 6,478 steps in 6.4 s,
  and `mutual-recursion` 5,638 steps in 4.8 s.
- Step time grows with call depth. `$sum(n)` takes about 42 steps per level
  of recursion, and each level nests five nodes (`BIN`, two `IN` nodes,
  `eval-clauses/cons`, and `eval-clause/succ`). Mean time per step
  *(measured)*:

  | n | 10 | 30 | 50 | 100 | 200 |
  | --- | --- | --- | --- | --- | --- |
  | driver | 0.68 ms | | 3.7 ms | 6.6 ms | 12 ms |
  | cross-check on | 6.7 ms | 25 ms | | | |

  The driver walks the whole path on every step, which Step 13's "resume from
  the last hole" avoids. The test runs `$sum(30)` without the cross-check.
- The front end accepts a table row's pattern only as a variable or an upcast
  case. So the test script declares each case in a type of its own, and
  `color` as their union.

### Step 11: Entry and driver

- `al/6-entry.rkt`: `Entry` loads the script into `$empty_ctx`, then runs
  `(G (IN L_0 (CALL "main" () ())))` to a result, printing `Entry`'s `debug`
  messages to stderr. For a P4 program it runs
  `(G (IN L_0 (call-rel "Program_ok" (val_p4))))`, as K's `afterLoad` does.
- The command-line driver, in `al/6-entry.rkt`'s `main` submodule:
  `racket spec-meta-redex/al/6-entry.rkt FILE.watsup` boots the file, runs
  `Entry`, and prints the value in `k-run.sh`'s JSON format, or `fail`.
- Compile with `raco make spec-meta-redex/al/6-entry.rkt`.
- Test: the examples that need no builtins produce the same output as
  `k-run.sh` and the meta-circular run: `add` 119, `fibo` 89,
  `iter-nontrivial` -42, `iter-sequence` 1085, `mutual-recursion` 289,
  `relation-typing` 110, and `variant-tree` 6. Record each one's time and
  number of steps. The tests run the fast ones, with the cross-check on.

Outcome:

- Done. `test/entry.rkt` passes (20 checks, 14 s). The whole suite runs 3,564
  checks in 54 s, and 3,534 with contracts off, on a scratch copy.
- `entry` and `entry-p4` in `al/6-entry.rkt` are Racket procedures around the
  driver. `entry` gives `(OK val)`, or `FAIL` where `Entry` has no derivation.
  It writes `Entry`'s debug messages with `debug`, including the last one,
  `debug val`. The run starts under the `LOCAL` layer of the loaded context,
  which is `$empty_layer`. `entry-p4` gives `(OK (val ...))` or `FAIL`. It is
  tested on a toy `Program_ok`, and the driver's `--p4` is left to Step 13.
- The driver prints the value with `val->jsexpr` and `jsexpr->string`.
  `val->jsexpr` is the extern wire's encoding of `val`, so it is in
  `common/0.2-extern-json.rkt`, which Step 12 completes. The test checks a
  value with every kind except `FUNC` and `EXT` against `k-run.sh`'s output.
- K writes `Entry`'s debug messages and `-- debug` premises on stdout, as JSON,
  before the result. The driver writes them on stderr, as terms, as `debug`
  does since Step 8. So the driver's stdout is the last line of `k-run.sh`'s,
  and the tests compare with that line.
- K garbles non-ASCII text: it prints `"é"` as the bytes `8D 79`, where UTF-8
  is `C3 A9` *(measured)*. The driver writes UTF-8, and escapes only control
  characters, `"`, and `\`.
- All seven examples print what `k-run.sh` prints. A failing `$main()`, and a
  script without `$main`, print `fail` in both, and the meta-circular run
  reports that `Entry` failed.
- Steps and times *(measured)*. The driver columns leave out booting, and
  `$load` takes at most 6 ms. The program's wall clock includes Racket's
  startup and booting, about 0.9 s:

  | Example | Steps | Driver | Cross-check on | `al/6-entry.rkt` |
  | --- | --- | --- | --- | --- |
  | `add` | 27 | 4 ms | 19 ms | 0.9 s |
  | `iter-nontrivial` | 170 | 47 ms | 0.35 s | 1.0 s |
  | `variant-tree` | 963 | 0.46 s | 4.1 s | 1.6 s |
  | `relation-typing` | 1,284 | 0.51 s | 4.6 s | 1.9 s |
  | `iter-sequence` | 1,937 | 0.92 s | 7.8 s | 2.1 s |
  | `mutual-recursion` | 5,638 | 5.3 s | 44 s | 5.3 s |
  | `fibo` | 6,478 | 7.1 s | 62 s | 7.0 s |

  `k-run.sh` takes 8 to 11 s on each, and the meta-circular run 0.2 to 0.3 s.
- The tests run `add`, `iter-nontrivial`, `variant-tree`, and
  `relation-typing` with the cross-check on, and `iter-sequence` without it.
  `fibo` and `mutual-recursion` are left out; Step 14 checks every example.
- `Entry`'s own debug messages are written once, since no rule writes them.
  The cross-check writes a `-- debug` premise's message twice, as in Step 9,
  so the test of `add`'s stderr runs without it.

### Step 12: Builtins and externs

- Build the host as two shared objects (see
  [Builtins and externs](#builtins-and-externs)):
  - In `p4spec/bin/dune`, the `ffi` executable's modes become
    `object shared_object`.
  - `spec-meta-redex/ffi/shim.c`: the Redex shim, with `host_init` and
    `host_eval`.
  - `make redex-ffi` runs `dune build bin/ffi.so`, as `$(KFFI_OBJ)` does for
    `bin/ffi.exe.o`. It then compiles `shim.c` into
    `spec-meta-redex/ffi/shim.so` against `ffi.so`. `make clean` removes
    `shim.so`, and `spec-meta-redex/.gitignore` gets `ffi/*.so`.
  - K's shim, its Makefile rules, and `ffi.exe.o` stay unchanged.
- `common/0.2-extern-json.rkt`: a Racket codec for the wire format documented
  in `extern_json.ml` (`val`, `typ`, `mixop`, request, and response), built on
  Racket's `json` library. Step 11 added the encoding of `val`.
- `common/0.3-extern-ffi.rkt`: the transport, with the parameter `host-spec`
  and `(host-eval request)`, which returns the reply text. It loads,
  initializes, and calls as described in
  [Builtins and externs](#builtins-and-externs).
- Replace the Step 3 stubs for the three host procedures. Every builtin,
  including the map builtins, goes to the host. The `main` submodule of
  `al/6-entry.rkt` sets `host-spec` to
  the file it boots.
- Tests:
  - The `builtin-*.watsup` examples match `k-run.sh`.
  - Values from `sexp-p4` survive a round trip through the codec.
  - A script that declares `builtin dec $fresh_typeId` and calls it twice gets
    two distinct ids, with caching on. This fails if an impure call sits behind
    any cache.
  - An extern function that the runner lacks gives `FAIL`, and a malformed
    request sent to `host-eval` raises with OCaml's message.
  - A `host-spec` that names no file raises an error, and a later call under
    a valid `host-spec` still succeeds.

Outcome:

- Done. `test/extern.rkt` (44 checks) is new. `test/common-relation.rkt`
  (17), `eval-call-func.rkt` (54), `eval-call-rel.rkt` (23), and `entry.rkt`
  (28) now call the host, and coverage no longer leaves out the extern rules.
  The whole suite runs 3,622 checks in 55 s, and 3,592 with contracts off, on
  a scratch copy.
- The four `builtin-*.watsup` examples print what `k-run.sh` prints:
  `builtin-extra` 277, `builtin-list` 19, `builtin-map` 45, and
  `builtin-nested` 65. The tests run them without the cross-check. Steps,
  host calls, and times *(measured)*. The driver column leaves out booting and
  `host_init`. The program's wall clock includes Racket's startup, booting,
  and `host_init`:

  | Example | Steps | Host calls | Driver | `al/6-entry.rkt` |
  | --- | --- | --- | --- | --- |
  | `builtin-list` | 179 | 4 | 90 ms | 1.2 s |
  | `builtin-map` | 187 | 7 | 90 ms | 1.2 s |
  | `builtin-extra` | 308 | 17 | 0.12 s | 1.3 s |
  | `builtin-nested` | 803 | 11 | 0.5 s | 1.6 s |

  `k-run.sh` takes 8 to 9 s on each.
- `make redex-ffi` takes about 2 s once `p4spec/` is compiled. `ffi.exe.o`
  is byte for byte the same after the change to the modes, so K's build is
  unaffected. `shim.so` is linked again only when `shim.c` changes: `ffi.so`
  is an order-only prerequisite, since the shim finds it when it is loaded.
- The codec adds `jsexpr->val`, `typ->jsexpr`, `builtin-request`,
  `extern-func-request`, `extern-rel-request`, `response->valres`, and
  `response->valsres` to `val->jsexpr`. Types are only encoded, since no
  response carries one. Decoding is strict: a nat or int must be the decimal
  digits that `Bigint.to_string` writes, and a response must have exactly one
  field. Anything else raises `extern-json: expected ...`.
- `host-eval` returns the reply text, as planned, so a malformed request gives
  `{"error": msg}` and does not raise by itself. The host procedures in
  `common/4-relation.rkt` decode every reply, and `{"error": msg}` raises
  `host: msg`; the test checks both. Builtins never reply `{"fail": null}`: a
  missing builtin or a wrong number of arguments is an error. An extern
  function or relation that the runner lacks, or declares `extern` itself,
  gives `FAIL`, and OCaml writes why on file descriptor 2.
- `extern-func` and `extern-rel` evaluate the function or relation of that
  name in the spec of `host-spec`. So the tests reach a function and a
  relation of their script on the host through hand-written `(EXT "inc")` and
  `(EXT "Halves")` entries in `G`.
- The `$fresh_typeId` test calls it twice directly and twice through a
  defined function, and gets four distinct ids, and four more under a second
  `host-spec`. The values of the four P4 programs survive a trip through the
  codec, and another through the host as `$rev_`'s argument.
- The shim's `host_eval` gives an `{"error": ...}` reply if it is called
  before `host_init`, which the transport never does.
- `test/machine.rkt` has `text-file`, which writes a script to a temporary
  file that is deleted when the process exits, for a script that is also
  `host-spec`.

### Step 13: P4 type checking and performance

- `racket spec-meta-redex/al/6-entry.rkt --p4 PROGRAM spec` boots `spec/`,
  boots PROGRAM with `sexp-p4`, runs `Program_ok` on it, and prints `passed`
  or `fail`.
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
  7. implement the object-level map builtins natively, as K does, if host
     calls show up in the profile, but only after asking the user, who asked
     for every builtin to go to the host.

  After each change, rerun the `$fresh_typeId` test and the negative program.
- Done when the small program passes, and the medium and large results are
  recorded with timings. This step answers whether the large program is within
  reach.

Outcome:

- Closed here, at the user's request. The small program passes. The large
  program was not run, and items 1 and 4 to 7 were not taken up.
- `--p4` is in `al/6-entry.rkt`'s `main` submodule. It boots the spec and the
  program, sets `host-spec` to the spec, runs `entry-p4`, and prints `passed`
  or `fail`. `-i DIR` adds a P4 include directory; without one, it uses
  `p4c/p4include`, as `k-run-p4.sh` does. There is no test of it yet.
- The small program passes, and the negative one fails. The large program
  has not been run. Times *(measured)*, with contracts and caching on, before
  profiling or any optimization:

  | Program | Value (conses) | Result | Steps | Host calls | Driver | `al/6-entry.rkt` |
  | --- | --- | --- | --- | --- | --- | --- |
  | `p4_16_samples/action-bind.p4` | 949 | `passed` | 73,932 | 102 | 193 s | 194 s |
  | `p4_16_errors/action-bind.p4` | 1,006 | `fail` | 24,444 | 53 | 79 s | 77 s |
  | `checksum-l4-bmv2.p4` | 27,311 | none | over 1,537 | none yet | over 286 s | stopped at 5 min |

  The driver column includes `host_init` (1.1 s). The last two columns come
  from separate runs, which differ by a few seconds. Booting `spec/` takes 1.1 s,
  booting the program 0.03 to 0.06 s, and `$load` about 2.1 s. The peak
  resident memory on `action-bind.p4` is 520 MB. All host calls are builtins.
- On `action-bind.p4`, a step takes 2.6 ms on average. Over each 5,000
  steps, the mean step time varies between 1.2 and 3.8 ms. GC takes 1.8 s of
  the 193 s. The rules applied most often are
  `assign-exp/variable` (6,061 times), `eval-exp/variable` (4,891), and
  `assign-exps/cons` (4,767).
- On `checksum-l4-bmv2.p4`, no single step takes over 0.5 s, but every step
  is slow. Steps average 73 ms over the first 500, 200 ms over the next 500,
  and 280 ms by step 1,500, which is 40 to 150 times `action-bind.p4`'s rate.
  The run never reached a host call. Its program value is 29 times larger, and
  about the size of the 27,000-cons value that slowed the driver in Step 6.
  The interrupted run was in Redex's nonterminal matcher
  (`match-nt/boolean`). That fits Step 6's finding, where the driver's
  matching of `Fr` along the path collides in the memo and costs a deep
  `equal?`, but no profile confirms it yet.
- The profile of `action-bind.p4`'s first 15,000 steps put 93% of the time
  in the driver's `pending`, which matches `(in-hole Fr any)` at each node on
  the path to the redex. Applying the rules took about 7%, and metafunction
  calls under 1%. The driver matched `Fr` 50 times per step, at 0.03 to
  0.09 ms each, mostly at the five nodes of each relation call
  (`eval-rulgroups/cons`, `eval-ruls/cons`, `eval-rul/succ`, `eval-prems/head`,
  and `eval-prem/relpr`). The typing relations nest about 9 calls deep on
  average *(measured)*.
- Item 2 is done. The driver keeps a cursor between steps, as
  [The notion of reduction and its closure](#the-notion-of-reduction-and-its-closure)
  describes. `al/5-eval.rkt` provides `conf->cursor`, `cursor-step`, and
  `cursor->conf`. `run` and `run/trace` step a cursor, and `step/rule` on a
  configuration starts one from the root. The tests step a cursor too, so the
  cross-check compares every resumed step with `->al`. A new test checks that
  20 nested negations match `Fr` 41 times, where descending from the root
  matched it 231 times. The whole suite runs 3,624 checks in 51 s.
- With the cursor, on `action-bind.p4` *(measured)*:

  | | Before | With the cursor |
  | --- | --- | --- |
  | Steps | 73,932 | 73,932 |
  | `Fr` matches per step | 50 | 1.4 |
  | Driver | 193 s | 14.6 s |
  | Mean step | 2.6 ms | 0.20 ms |
  | `al/6-entry.rkt` | 194 s | 18.3 s |

  The steps, the rules applied, and the 102 host calls are the same. The step
  time stays flat over the run. Matching `Fr` now takes 2.6 s of the run, so
  applying the rules is most of the rest.
- With the cursor, the profile put 84% of `action-bind.p4`'s run in
  `reduce-redex`, which tried all 197 rules of `->redex` and `->ctx` on every
  redex. 39% was self time in Redex's memo of pattern matches
  (`matcher.rkt:1166`), which hashes each term and compares it with `equal?`.
  Each rule tried looks its left-hand side up there *(measured)*.
- Item 3 is done with `reduction-relation/forms` (see
  [Layout and languages](#layout-and-languages)). Every fragment builds its
  relations with it, and `al/5-eval.rkt` combines them with
  `union-reduction-relations/forms`. The driver's `reduce-redex` applies
  `(relation-for-head ->redex (term-head r))`, and the same for `->ctx` on the
  triple. `->al` and `focus-steps` keep the whole relations, so the
  cross-check compares the dispatched rules with all of them on every step.
  A redex now meets 2 to 16 rules instead of 197. Only `"frame/fail"` and
  `"eval-exp/literal/number"` belong to every head. `test/prelude.rkt` tests
  the grouping, including a head that is a nonterminal, a head under an
  ellipsis, `in-hole`, a focus triple, a union, and a relation that the macro
  did not build. The whole suite runs 3,636 checks in 48 s.
- With the cursor and dispatch *(measured)*:

  | Program | Result | Steps | Driver | `al/6-entry.rkt` |
  | --- | --- | --- | --- | --- |
  | `p4_16_samples/action-bind.p4` | `passed` | 73,932 | 10.0 s | 15.4 s |
  | `p4_16_errors/action-bind.p4` | `fail` | 24,444 | 4.4 s | 9.8 s |
  | `checksum-l4-bmv2.p4` | none | over 1,090,000 | over 295 s | stopped at 5 min |

  On `action-bind.p4`, the driver went from 193 s, to 14.6 s with the cursor,
  to 10.0 s with dispatch, 0.14 ms per step. The steps and host calls of both
  `action-bind.p4` programs are the same as before.
  `checksum-l4-bmv2.p4` reached step 1,537 in 286 s before the cursor. Now it
  takes 11.5 s for the first 10,000 steps, and then 0.2 to 0.3 ms per step,
  up to 1 ms between steps 610,000 and 690,000. Its total number of steps is
  not known.
- With dispatch, the memo is 56% of `action-bind.p4`'s self time, and
  metafunction calls 26% of its total. Which keys make the memo slow is not
  measured yet. Items 4 and 5 are about the large `G` and `L` that keys can
  hold.

### Step 14: Type-checking the P4 samples

This replaces the earlier plan for Step 14 (a `make redex-test` over
`spec-meta-redex/test` and the examples, figures of the rules, and a rewrite
of this file as an overview).

- `make redex-test` type-checks the P4 programs of
  `p4c/testdata/p4_16_samples/` with `spec/`, which must pass, and those of
  `p4c/testdata/p4_16_errors/`, which must fail. It does not run the tests in
  `spec-meta-redex/test`.
- The runner is `test/p4-typecheck.rkt`, after K's
  `spec-meta-k/scripts/run-k-typecheck.py`. It skips the programs named in
  `excludes/static/**/*.exclude` and the files under `include/` directories.
- The Makefile, not the runner, names the directories and the spec, with
  required arguments, as `p4spec/test/run/test.ml` takes them:

  ```sh
  racket spec-meta-redex/test/p4-typecheck.rkt --p4-dir p4c/testdata/p4_16_samples \
    -e excludes/static -i p4c/p4include spec
  racket spec-meta-redex/test/p4-typecheck.rkt --p4-dir p4c/testdata/p4_16_errors \
    -e excludes/static -i p4c/p4include --neg spec
  ```
- It boots and loads `spec/` once, and runs every program under that context,
  in one Racket process. The user chose this over a worker process driven by a
  Python runner, over caching the loaded context on disk with one process per
  program, and over parallel workers.

Outcome:

- Done. `make redex` builds what the Redex spec needs: `spectec-boot` (through
  `boot`), `ffi.so`, and `shim.so`, and runs `raco make` on
  `al/6-entry.rkt`. It replaces `make redex-ffi`, and takes 10 s when nothing
  is stale *(measured)*.
- `make redex-test` runs `raco make` on `test/p4-typecheck.rkt`, after
  `make redex`, and then the positive and the negative run, as K's
  `make k-test` does. `make clean` removes the two result files, which
  `spec-meta-redex/.gitignore` lists. An empty `test` submodule is what
  `raco test` runs, in 0.6 s.
- `al/6-entry.rkt` provides `load-script`, which gives the context a script
  loads into. `entry-p4` takes that context in place of the script.
  `test/entry.rkt` passes (28 checks).
- On the samples, the runner checks 1,267 programs, the same list as K's, in
  the same order. `--neg` on `p4_16_errors` gives K's `--neg` list, checked
  on the four `action-bind*` programs. The runner differs from K's script in
  four ways:
  - It has no timeout, and each program passes or fails, both at the
    user's request. A run that raises, such as on a stuck term or a host
    error, fails, and its message goes to the result file. An error leaves
    Redex's caches whole for the next program: a memo stores an answer only
    once it is computed.
  - The directories and the spec are arguments. `--p4-dir`, `-e`, and `-i`
    are required and can be repeated, as in `test.ml`, which spells the first
    `-p4-dir`. `racket/cmdline` takes no flag of that form.
    Entries in the `.exclude` files are relative to the repository root, as
    for K's and OCaml's tests, whatever the current directory.
  - The stderr that Racket code writes during each program, with `Entry`'s
    debug messages, is kept apart from the terminal, and goes to the result
    file for a run that raises. OCaml's diagnostics still go to file
    descriptor 2.
  - Progress goes to stdout as well as the result file. The list of programs
    without the expected result is `failing` in both modes, where K's says
    `not failing` for negative tests.
- Before the timeout was removed, a run with `-t 5`, checked between steps,
  timed out on `action-bind.p4` after 35,955 steps. In the same run,
  `empty.p4` then passed, the negative `p4_16_errors/action-bind.p4` gave
  `fail`, and a program that does not parse gave `error`, a status since
  merged into `fail`.
- The P4 preprocessor ignores the exit status of `cc`
  ([`preprocessor.ml`](p4spec/lib/interface/p4/preprocessor.ml)). So
  `sexp-p4` boots a missing file as an empty program, which passes. The OCaml
  and K front ends share the preprocessor, and `p4spec/` is unchanged. The
  runner only checks files that it finds under `--p4-dir`, and refuses a
  `--p4-dir`, `-e`, or `-i` that is not a directory.
- With a filter on `action-bind` in `collect-programs`, which the user added
  for testing, `make redex-test` passes in 35 s *(measured)*. The positive
  `action-bind.p4` passes, and the negative `action-bind.p4` and
  `action-bind1.p4` to `action-bind3.p4` fail. With `--neg`, the positive
  `action-bind.p4` is listed under `failing`, and the run exits with 1.
- The 11 programs of `p4spec/test/micro/programs` pass, in 10 to 67 s each
  *(measured)*. Nine ran in one run, which a 5-minute limit stopped, and the
  last two, `types.p4` and `types_adv.p4`, in another.
- Running each program as its own process cost 5.3 s before the program
  started *(measured)*:

  | Stage | Time |
  | --- | --- |
  | Racket and the compiled modules | 1.1 s |
  | Booting `spec/` | 1.0 s |
  | `$load` | 2.0 s |
  | `host_init` | 1.1 s |

  Over the 1,267 programs, that is about 1.9 hours. The runner pays it once,
  and starts the host before the first program with a call to `$rev_`.
- On `action-bind.p4` and `action-uses.p4` *(measured)*:

  | | One process per program | One process |
  | --- | --- | --- |
  | `action-bind.p4` | 14.1 s | 10.3 s |
  | `action-uses.p4` | 13.2 s | 8.6 s |
  | Loading `spec/` | in each | 4.7 s, once |
  | Total | 27.3 s | 24.7 s |

  The peak resident memory was 534 MB. Booting a program takes 30 to 40 ms.
  Redex's caches stay warm from one program to the next, so a program's time
  depends on what ran before it. In an earlier probe, the negative
  `action-bind.p4` took 3.0 s after three positive programs, and about 4.4 s
  on its own. The host's `$fresh_typeId` counter keeps counting across
  programs, as in OCaml's own harness (`p4spec/test/run/test.ml`).
- One crash of the process, such as running out of memory, ends the run.
