# Cross-checking P4-SpecTec AL against K

`spec-meta-k/` is a K specification of the P4-SpecTec **AL** meta-language: a
rule-for-rule port of `spec-meta/{common,al}`, which specifies AL in P4-SpecTec
itself. Where the OCaml `spectec-boot` runs an AL script by interpreting
`spec-meta/al` meta-circularly, K runs the same script by rewriting under
`spec-meta-k/al`.

Two things can therefore be run under K:

- a self-contained AL script with a `$main()` function (`examples/*.watsup`), and
- the **P4 spec** itself (`spec/`), applied to a P4 program — i.e. type-checking
  a `.p4` file with the specification executing inside K.

## 1. Layout and the correspondence to `spec-meta/`

Each K file mirrors one watsup file and says so in its header comment.

| `spec-meta/` | `spec-meta-k/` |
| --- | --- |
| `common/0-stdlib.watsup` | `common/0-stdlib.k` |
| `common/1-syntax.watsup` | `common/1-syntax.k` (`COMMON-SYNTAX`) |
| `common/2-env.watsup`, `al/2-env.watsup` | `common/2-env.k`, `al/2-env.k` |
| `common/4-relation.watsup` | `common/3-relation.k` |
| `common/5.0`, `5.1-eval-*` | `common/4.0-eval-typ.k`, `common/5.1-eval-ops.k` |
| `al/1-syntax.watsup` | `al/1-syntax.k` (`AL-SYNTAX`) |
| `al/3-context.watsup` | `al/3-context.k` |
| `al/5.1`–`5.7` | `al/5.1`–`5.6` (six files; argument handling is split across `5.2`, `5.3` and `5.5` instead of getting its own file) |
| `al/6-entry.watsup` | `al/6-entry.k` (module `AL`, the entry module) |
| — | `al/0-config.k` (configuration), `al/4-extern-json.k` (external interface: codec), `al/4-extern-ffi.k` (external interface: transport) |

Three structural differences from the watsup source:

- **Abstract syntax only.** Every production is a constructor application with a
  pinned `symbol(_)` label, so K never parses AL concrete syntax. A script
  arrives as a term of sort `Script`.
- **`res<X>` is monomorphised.** K has no generics, so `res<val>` and `res<val*>`
  become `ValRes`/`ValsRes` ([common/3-relation.k](spec-meta-k/common/3-relation.k)),
  and failure is a single nullary `KItem`, `FAIL`.
- **The context is cells, not a threaded value.** watsup passes `ctx` explicitly
  through every relation; K keeps it in configuration cells. Consequently no
  relation returns a `ctxres`, and the context's *scoping* has to be re-created
  by an explicit save/restore discipline (§4).

## 2. The K configuration

From [al/0-config.k](spec-meta-k/al/0-config.k):

```k
configuration
  <al>
    <k> initFFI() ~> logDebug(textV("entry-al")) ~> $PGM:Script ~> afterLoad() </k>
    <p4prog> $P4:P4Opt </p4prog>
    <global>  <gtdenv> .Map </gtdenv> <grenv> .Map </grenv> <gfenv> .Map </gfenv> </global>
    <local>   <ltdenv> .Map </ltdenv> <lfenv> .Map </lfenv> <lvenv> .Map </lvenv> </local>
    <caller>  <cfenv> .Map </cfenv> </caller>
    <saves>     .List </saves>
    <callstack> .List </callstack>
    <log>       .List </log>
    <result>    .K    </result>
    <ffiinit>   0     </ffiinit>
  </al>
```

- **`initFFI()`** — at the very *head* of `<k>`, ahead of `$PGM`, so the OCaml
  runtime behind the external interface is started before any rule that could
  reach a builtin (§7). Ordering is guaranteed by `<k>` sequencing, not a flag.
- **`<k>`** — the AL script arrives as `$PGM` and is *consumed* by loading:
  [al/3-context.k](spec-meta-k/al/3-context.k) peels definitions off its head
  into the global cells (this is `$load` / `$load_typdef` / `$load_reldef` /
  `$load_funcdef`), leaving `.K` and then `afterLoad()`.
- **`<p4prog>`** — the second krun input, `noP4()` or `someP4(val)`. It selects
  the entry (§3).
- **`<global>` / `<local>`** — the two layers of the watsup `ctx`: type
  definitions, relations (global only, matching `$find_rel`), functions and
  value bindings. `$find_func` checks `<lfenv>` then `<gfenv>`.
- **`<caller>`/`<cfenv>`** — the caller's function environment, for
  `Assign_arg/fun`. In watsup this is `C_caller`, a *second context passed as an
  argument* to `Eval_clause`; K needs a cell for it, and that cell must be
  stacked per call frame (§4, and see §6 for the bug this caused).
- **`<saves>` / `<callstack>`** — the two backtracking stacks (§4).
- **`<log>`** — where `debug` premises accumulate, via a `logDebug(Val)` item.
- **`<result>`** — the final answer; `krun` prints the whole configuration, so
  this is the cell to read.
- **`<ffiinit>`** — `ml_init_c`'s return value: `1` once `initFFI()` has
  reduced, so the final configuration carries proof that the OCaml runtime came
  up. A `0` means it never did (§7).

## 3. Running

```sh
make k-build                                # kompile -> al-kompiled/
make k-run TEC=examples/add.watsup          # a self-contained script
make k-typecheck P4=p4c/testdata/p4_16_samples/action-bind.p4
make k-clean                                # drop al-kompiled/ and scratch files
```

### Runtime dependency

**A K run must start from the repo root**, because the spec paths an external
call resolves against (`builtinSpec()`/`externSpec()`) are relative and
hardcoded — K rules cannot read environment variables, so unlike
`scripts/kast-json.sh`
there is no override on this path.

**`./spectec-boot` must still exist**, though no longer for external calls: the
two parser wrappers (§6) invoke it to boot `$PGM` and parse `$P4`, entirely
outside the interpreter. Build it with `make boot` (never `dune build` directly;
note that `make clean` leaves a stale `./spectec-boot` behind).

**`al-kompiled/interpreter` embeds a snapshot of the OCaml implementation.**
`kffi.exe.o` — the whole of `p4spec/` as one object, OCaml runtime included — is
linked into it at kompile time (§7). So after editing `p4spec/`, `make boot`
alone leaves a **stale interpreter** that silently keeps using the old builtins.
The workflow is:

```sh
make boot && make k-build
```

Scratch files are `mktemp`'d straight into **`./spec-meta-k/`** (gitignored),
by the two parser wrappers, before K starts; there is no scratch directory to
create. Nothing writes there from inside K any more — external calls cross by
FFI rather than through a request file.

### A simple AL program with a `main`

`make k-run TEC=examples/add.watsup` expands to

```sh
KDEF=al-kompiled krun -d al-kompiled \
  --parser ./spec-meta-k/scripts/kast-json.sh examples/add.watsup \
  -cP4= -pP4=./spec-meta-k/scripts/kast-p4.sh
```

The `.watsup` is booted to a term by the parser wrapper; `-cP4=` is empty, so
`<p4prog>` is `noP4()` and `afterLoad()` fires the first entry rule:

```k
rule <k> afterLoad() => ... ~> callFunc("main", .TypList, .ValList) ~> finish() ... </k>
     <p4prog> noP4() </p4prog>
```

This is exactly `spec-meta/al/6-entry.watsup`'s `Entry`. The answer appears in
`<result>` (`intN(119)` for `add`), and the run should end with `<k>`, `<saves>`
and `<callstack>` all empty — a leftover frame in either stack means a
save/restore path returned without popping.

Cross-check against the OCaml:

```sh
./spectec-boot run spec-meta/al -rel Entry -tec examples/add.watsup -ali   # INT +119
```

Underneath the Makefile there is no `main.k`: the entry module is
`spec-meta-k/al/6-entry.k`, and the two commands are

```sh
kompile spec-meta-k/al/6-entry.k --main-module AL --syntax-module AL-SYNTAX -o al-kompiled
KDEF=al-kompiled krun -d al-kompiled \
  --parser ./spec-meta-k/scripts/kast-json.sh examples/add.watsup
```

`KDEF` must be set even though `-d` already names the definition on the `krun`
line — `krun` passes the wrapper only the input file, so that is the one channel
it has (§6).

To keep the intermediate JSON — to inspect it, or to diff two revisions of the
emitter — emit it explicitly and hand `krun` that instead; the wrapper takes
either:

```sh
./spectec-boot kast examples/add.watsup -o add.json
KDEF=al-kompiled krun -d al-kompiled --parser ./spec-meta-k/scripts/kast-json.sh add.json
```

Use `krun --output json` to diff results mechanically rather than by eye.

### Type-checking a P4 program

Here `$PGM` is the **P4 spec** (`spec/`, booted to a `Script` exactly as any
other AL script) and `$P4` is the **program**. With `<p4prog>` non-empty the
second entry rule fires instead:

```k
rule <k> afterLoad() => ... ~> callRel(entryRel(), (V, .ValList)) ~> finish() ... </k>
     <p4prog> someP4(V:Val) </p4prog>
```

`entryRel()` is `"Program_ok"`. Only the entry differs — loading, evaluation and
`finish()` are shared with the `$main()` case. Since a relation yields `val*`,
`finish()` has a separate `ValList` rule alongside the `Val` one.

```sh
make k-typecheck P4=p4c/testdata/p4_16_samples/action-bind.p4
```

The program is parsed by the ordinary P4 front end — whose menhir grammar builds
a `Value.t` directly, so there is no separate P4 AST — and emitted as K's
*structural* `Val` constructors rather than through the meta-language
constructor table, which knows nothing about the sorts a `p4program` inhabits.

## 4. How a relation is evaluated, and backtracking

AL reifies failure as `FAIL` in `res<X>`. K has no built-in backtracking
(`krun --search` is symbolic-backend only, unavailable on the LLVM backend), so
it is explicit: `FAIL` is a `KItem`, and every choice point pushes a
*continuation* item that inspects what came back.

A relation call
([al/5.6-eval-call-rel.k](spec-meta-k/al/5.6-eval-call-rel.k)) descends
`Call_rel` → `Call_rel_dispatch` → `Call_defined_rel` → `Eval_rulgroups` →
`Eval_ruls` → `Eval_rul`, mirroring the watsup relation names. The choice point
is `Eval_ruls`:

```k
rule <k> evalRuls(_, .RulPathList, _) => FAIL ... </k>
rule <k> evalRuls(M, (P, Ps), Vs)
      => pushSave() ~> evalRul(M, P, Vs) ~> tryNextRul(M, Ps, Vs) ... </k>

rule <k> Vs:ValList ~> tryNextRul(_, _, _)  => popSaveRestore() ~> Vs ... </k>
rule <k> FAIL       ~> tryNextRul(M, Ps, Vs) => popSaveRestore() ~> evalRuls(M, Ps, Vs) ... </k>
```

`tryNextRul` is the backtracking point: a `ValList` means the rule succeeded and
the remaining paths are dropped; `FAIL` means restore and try the next one. An
exhausted rule-path list is `FAIL`, which propagates up to `tryNextRulGroup`,
which moves to the next rule group. Within a single rule, `Eval_rul` runs
`assignExps` (head match) then `evalPrems` then `evalExps` on the outputs, and
each stage has a matching `FAIL` rule that short-circuits the rest.

Function calls have the identical shape one level over:
`evalClauses`/`tryNextClause`
([al/5.5-eval-call-func.k](spec-meta-k/al/5.5-eval-call-func.k)), with the
`elsclause` appended to the clause list so `-- otherwise` is just the last
clause. `evalTblRows`/`tryNextTblRow` likewise for table functions.

### The two stacks

The essential point is that **`<saves>` and `<callstack>` are separate, and
conflating them is a real hazard** — doing so once produced a bug where a
relation call's `ValList` met a continuation expecting a bare `Val`.

- **`<saves>`** holds `localSave(tdenv, fenv, venv)` snapshots for backtracking
  *within* one frame. `pushSave()`/`popSaveRestore()` bracket each clause or
  rule attempt, so a failed attempt's bindings do not leak into the next. The
  same mechanism serves iteration: each element of a `*`-iterated premise or
  expression is evaluated in a cleared `<lvenv>` so one element's bindings do
  not leak into the next, with the per-element results collected and bound into
  the restored environment afterwards
  ([al/5.2-eval-assign.k](spec-meta-k/al/5.2-eval-assign.k),
  [al/5.4-eval-prem.k](spec-meta-k/al/5.4-eval-prem.k)).
- **`<callstack>`** holds `callSave(localSave(...), cfenv)` full call frames.
  A callee *replaces* `<local>` wholesale rather than layering on it
  (`C_callee = C[ .LOCAL = $empty_layer ]`), so `pushCallFrame` clears the local
  cells and `popCallFrame` puts the caller's back. `<cfenv>` is saved and
  restored alongside `<local>` here, but *not* in `<saves>` — bindings roll back
  within a frame, where the caller snapshot cannot change.

A function call is `snapshotCaller() ~> pushCallFrame(tdenvOfTargs(...))` — the
snapshot runs first, while `<lfenv>` is still the caller's. A relation call needs
neither type parameters nor a caller snapshot, so it is just
`pushCallFrame(.Map)`.

Two places deliberately deviate from watsup to keep the port total: an
unresolved function name and an unresolved relation name both become `FAIL`
rather than sticking, since neither has a matching watsup clause.

## 5. Additions to the OCaml implementation

All in `p4spec/`, and all in service of K; nothing in the existing interpreter's
behaviour changed.

**Three new `spectec-boot` subcommands** ([p4spec/bin/boot.ml](p4spec/bin/boot.ml)):

| subcommand | purpose |
| --- | --- |
| `kast TARGET -o F` | boot a `.watsup` (or a spec directory) and emit it as KAST JSON of sort `Script` |
| `kast-p4 -p PROG -i INC -o F` | parse a P4 program and emit it as KAST JSON of sort `Val`, already wrapped as `someP4(...)` |
| `extern -spec SPEC -i REQ -o F` | evaluate one builtin, `extern dec` or `extern relation` call given as JSON, against SPEC, on the **P4 interface** |

The first two are what the parser wrappers (§6) invoke. The third is **no longer
on the K path** — external calls cross by FFI now (§7) — but it is kept: it is
the same dispatch, reachable from a shell, so a single request can be replayed
by hand against a `.json` file when debugging the wire.

**A fourth binary target**, [p4spec/bin/kffi.ml](p4spec/bin/kffi.ml), built by
dune as an *object* rather than an executable (`(modes object)`, which bundles
the OCaml runtime in). It is the in-process equivalent of `extern`: same wire,
same dispatch, but registered under the name `ml_eval` for the C shim to reach
via `caml_callback2`, and memoizing its runners since the process now outlives
the call (§7).

**Two new library modules**, exposed through `Interface.SpecTec_AL`
([p4spec/lib/interface/interface.ml](p4spec/lib/interface/interface.ml)):

- [`spectec/ali/kast.ml`](p4spec/lib/interface/spectec/ali/kast.ml) — the KAST
  emitter. Its constructor table is keyed by the pair *(sort, mixop)*, not by
  mixop alone, because notations are reused across sorts (`BOOL bool` is both
  `boolV` and `boolE`; `clause` and `tblrow` share a notation entirely). The
  mixops come from `Common.Mixops`/`Ali.Mixops` — the same constants `boot.ml`
  builds values with — so a renamed mixop yields an *unknown constructor* error
  rather than a silently wrong label. It also exposes `string_of_value` for the
  sort-independent structural emission `kast-p4` needs.
- [`spectec/ali/extern.ml`](p4spec/lib/interface/spectec/ali/extern.ml) — the
  wire codec (§7).

**Why builtins and externs need this at all.** `Call_builtin_func`,
`Call_extern_func` and `Call_extern_rel` are all `extern relation`s in watsup:
their meaning lives *outside* the meta-language. A builtin's lives in the host
registry (`p4spec/lib/interface/builtin/`); an extern's lives in the spec one
level *below* the one being run — in OCaml that is `Make_parametric`
(`backend-boot/spectec.ml`), routing into a lower runner's
`eval_func`/`eval_rel`. K implements **none** of them, and calls out instead, so
the OCaml stays the single authority. (An earlier revision reimplemented eight
builtins natively in K; those duplicated authority and were removed.)

**Two kinds of extern, and they are not the same mechanism.** The paragraph
above describes the *meta-language* extern, where a lower spec's ordinary
`dec`/`relation` definitions supply the meaning. The **P4 spec's own** externs —
`ExternFunctionCall_eval_lctk`, `ExternFunctionCall_eval`, `ExternMethodCall_eval`
— are the other kind: their meaning is the architecture model in
`p4spec/lib/backend-sim/`, reached through the P4 interface
(`P4.Make ()`/`Placeholder`, `backend-boot/p4.ml`), which is how `build_target`
wires the target level of a tower. There is no lower spec involved; `-spec` is
the P4 spec itself.

`extern` therefore builds its runner with **`build_target`** on
`P4_interface` — reusing the tower's target-level wiring rather than
duplicating it. That one runner serves both kinds, because the interface
governs only how *extern* names resolve: a spec declaring none of its own, like
`examples/lower`, resolves its `dec`s and `relation`s the same either way. Hence
the subcommand takes neither an interface nor a mode flag. (`build_null` cannot
serve the P4 kind at all: it wires `Spectec.Make_null`, whose `eval_extern_rel`
knows only `Call_builtin_func`.) `level.rel` is passed empty, since nothing on
this path runs a program, and `build_target`'s SL mode is unobservable, since an
`extern relation` dispatches to `Placeholder` from `invoke_rel` without any rule
running under an interpreter.

The two entries of §3 need one each, so `externArgs()` dispatches on `<p4prog>`
— the same cell that already tells those entries apart. `static_assert` is the
one such extern the type checker reaches (`Expr_eval_lctk`, in
[spec/5-typing/5.06.2-typing-expression-eval.watsup](spec/5-typing/5.06.2-typing-expression-eval.watsup)),
so before this dispatch existed, any program calling it type-checked as `FAIL`
under K while passing under the OCaml — silently, for the reason in §8.

**Builtins take that same subcommand.** There is no separate `builtin` command:
`extern` handles all three request kinds, told apart by the request's own key
(§7), so `builtinArgs()` is `extern -spec spec`. What kept them separate was
cost, not routing — a builtin is a static registry lookup, so **only the two
extern branches build a runner**, and the spec load rides along with it. That
matters because a type-check run is overwhelmingly builtins — 303 builtin calls
against 4 extern ones for `issue5231-const-int-concat.p4` — and building a
runner per call would take each from ~13 ms to ~1.2 s, roughly 90x on the
dominant path. The registry taken is the P4 one, a superset of the SpecTec one,
which is what lets one command serve both.

**`$print_` is the exception, and it is a quiet one.** It unparses a value back
to P4 source driven by the spec's hints, so it does need the spec — but only the
*unparser*, which `Interface.P4.init` installs from a parsed spec, far less than
a runner. Skip that and nothing errors: `unparser` keeps its initial
`fun _ -> ""` (`interface.ml:15`) and every `$print_` returns the empty string,
which the spec then builds names out of (`$name`; a table's default action name
in `5.14.1-typing-control-table.watsup:51`). The same program makes 90 `$print_`
calls, and without the init its type-check goes from passing to `FAIL` with no
diagnostic pointing anywhere near the cause.

This makes K the *broader* engine on builtins: the external route reaches all 44
entries of the OCaml registry, whereas the oracle `spectec-boot run spec-meta/al
... -ali` resolves builtins against the meta-spec's own functions and so only
runs the eight `spec-meta/common/0-stdlib.watsup` declares. A target calling
`$text_to_int` runs under K and fails under the oracle.

### The KAST JSON format

This is the `$PGM` wire — what `kast.ml` emits and `kast --input json` consumes.
It is KAST JSON version **4**, in which `label` and `sort` are objects (version 3
uses bare strings and is rejected):

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
- `String` tokens include their quotes *inside* the `token` field.
- Injections are not written out; `kast` infers `String -> Id`, `Num -> Exp` and
  so on.

The emitter can be exercised against the syntax modules alone, without any
semantics compiled:

```sh
kompile spec-meta-k/al/1-syntax.k --main-module AL-SYNTAX \
  --syntax-module AL-SYNTAX -o al-syntax-kompiled
kast -d al-syntax-kompiled --input json --output pretty add.json
```

which prints the script back as `typD("nat", .TParamList, aliasDT(natT())), ...`.

## 6. The two shell wrappers

`krun` has no `--input json`, so both configuration variables need a `--parser`,
and each parser is a shell script. Three constraints shape both:

- `--parser` takes **a single executable**, not a command string — K execs the
  flag's whole value as one filename, so `--parser "./x.sh arg"` fails.
- `krun` passes the parser **only the input file**. Anything else has to arrive
  through the environment: hence `$KDEF` for the definition, `$P4INCLUDE` for the
  preprocessor path, `$SPECTEC_BOOT` to override `./spectec-boot`.
- `krun` insists the input be a **file**.

**[scripts/kast-json.sh](spec-meta-k/scripts/kast-json.sh)** (`$PGM`, sort `Script`). Given a `.watsup` or a
spec directory it runs `spectec-boot kast` itself, so a target runs in one
command; anything else is assumed to be KAST JSON already. Because a whole spec
*directory* cannot be named as `$PGM`, `make k-typecheck` writes a one-line stub
file holding the path, `@`-prefixed, which the wrapper resolves and deletes.

**[scripts/kast-p4.sh](spec-meta-k/scripts/kast-p4.sh)** (`$P4`, sort `P4Opt`). `krun -cP4=VALUE` writes
VALUE to a temp file and passes *that file* to the parser, so its argument is a
file containing a P4 program *path*. Empty means `noP4()`; a path is booted with
`spectec-boot kast-p4`.

Neither wrapper can use `exec` together with an `EXIT` trap — `exec` replaces the
shell, so the trap never fires and scratch files accumulate one per run; `kast`
runs as a child instead, with the `rm` after it and its status passed on.

## 7. The K↔OCaml wire

The codec is [al/4-extern-json.k](spec-meta-k/al/4-extern-json.k) on the K side
and [extern.ml](p4spec/lib/interface/spectec/ali/extern.ml) on the OCaml side;
both document the format in full at their head. The *transport* — how a request
actually crosses — is [al/4-extern-ffi.k](spec-meta-k/al/4-extern-ffi.k),
[ffi/shim.c](spec-meta-k/ffi/shim.c) and [kffi.ml](p4spec/bin/kffi.ml).

```
request  ::= {"builtin":     <id>, "targs": [typ, ...], "args": [val, ...]}
           | {"extern-func": <id>, "targs": [typ, ...], "args": [val, ...]}
           | {"extern-rel":  <id>, "args": [val, ...]}

response ::= {"ok": val}         // builtin and extern-func
           | {"ok": [val, ...]}  // extern-rel: a relation yields val*
           | {"fail": null}      // recoverable failure; extern only

val   ::= ["boolV", <bool>] | ["natN", "<decimal>"] | ["intN", "<decimal>"]
        | ["textV", <string>] | ["strV", [[<atom>, val], ...]]
        | ["injV", mixop, [val, ...]] | ["tupV", [val, ...]]
        | ["optV", null] | ["optV", val] | ["listV", [val, ...]]
        | ["funcV", <id>] | ["extV", <json>]
mixop ::= [[<atom>, ...], ...]        // an atoms matrix, exactly K's MixOp
typ   ::= ["natT"] | ["intT"] | ["boolT"] | ["textT"]
        | ["varT", <id>, [typ, ...]] | ["tupT", [typ, ...]]
        | ["iterT", typ, "?"|"*"] | ["funcT"]
```

The three request kinds are told apart by **which key is present**, not by a
`"kind"` field, so a builtin request is byte-identical to what it was before
externs existed — and that is also what lets one *entry point* serve all three
(§5). One shared transport in `4-extern-ffi.k` serves them too, parameterized by
the spec path (`builtinSpec()` / `externSpec()`, which differ only in what they
name) and an `ExternKind` telling the last step which decoder to apply.

Format decisions worth knowing:

- **It is neither KAST JSON nor `Value.t`'s derived yojson.** KAST JSON cannot
  express it (`kast.ml`'s `sort_of_typ` accepts only a bare `VarT(id, [])`, but
  builtin results are noted with `IterT`/`VarT("map",[K;V])`). The derived yojson
  carries `vid`s, and importing vids minted by another process would collide with
  local ones — and `Value.compare` short-circuits on vid equality, so two
  structurally distinct values would compare *equal*. Everything decoded is
  rebuilt through `Value.Make.*`, minting fresh vids.
- **Numbers cross as decimal strings**, not JSON numbers: both sides hold
  arbitrary-precision integers and P4 routinely exceeds 64 bits.
- **`targs` are sent faithfully**, unlike type arguments elsewhere in the port: a
  builtin derives its result's `note` type from them. Argument values, by
  contrast, cross *without* their `note.typ`; `extern.ml` rebuilds a structural
  placeholder and records the invariant that no builtin or extern reads one.
- **`{"fail": null}` is a spec-level failure** and becomes K's `FAIL`, so it
  backtracks through `tryNextRul` like any failed rule. **"The wire broke" is a
  third reply shape**, `{"error": <diagnostic>}`, which has no K rule at all, so
  a genuine defect sticks visibly in `<k>` carrying the diagnostic — and, unlike
  the old non-zero-exit convention, that diagnostic is now in the configuration
  dump rather than on a lost child's stderr. `kffi.ml` also echoes it to stderr
  as it happens, since a K run can be minutes long. `Fail`'s region and message
  are still lost crossing the wire (K's `FAIL` is nullary) and go to stderr.

Transport mechanics:

The crossing is K's **FFI** (libffi), which can call any plain C ABI function.
OCaml cannot expose one directly, so a thin C shim sits between:

```
K rules  --#ffiCall-->  spec-meta-k/ffi/shim.c  --caml_callback2-->  p4spec/bin/kffi.ml
```

This replaced a `#system("./spectec-boot extern ...")` shell-out that forked a
fresh process per call. The shape is the one [examples/k/](examples/k/) proves
out and its README recommends for real use: **one** `ml_eval_c(spec, req) ->
char *` carrying a serialized request, with dispatch in OCaml.

- **The OCaml runtime lives inside the interpreter.** `kffi.exe.o` is built by
  dune with `(modes object)`, which bundles the whole runtime into one object,
  and `kompile -ccopt` links it and `shim.o` into `al-kompiled/interpreter`.
  `krun` is then a single long-lived process, which is what makes everything
  below possible — and what couples the interpreter to `p4spec/` (§3).
- **Initialization is explicit.** The shim assumes `ml_init_c` has run
  (`caml_startup` plus resolving the `ml_eval` closure) and, following the
  example, other entry points do not check. K guarantees it by *sequencing*:
  `initFFI()` sits at the head of `<k>`, so nothing can be touched until it
  reduces. Its result lands in `<ffiinit>` rather than being dropped — required,
  since `#ffiCall` is a `[function]` and an unconsumed pure term can simply be
  discarded, and useful, since `<ffiinit> 1 </ffiinit>` is then proof in the
  final configuration that the runtime came up. Every run pays this, including
  ones making no external call at all; `caml_startup` builds no spec, so it is
  small.
- **The transport is a chain of pure `[function]`s**, not `<k>`-cell rules. The
  old chain had to be cell rules — `#write`/`#close` have sort `K`, and
  `#system` returns a `#systemResult` needing a continuation — and neither
  constraint survives, since `#ffiCall` is a `[function]`. So an external call
  is one atomic rewrite step rather than six, and five intermediate `KItem`
  sorts, the `<builtinreq>` cell, its first-call/later-call split and
  `dropBuiltinReq()` are all gone.
- **Effect order is forced by threading**, the `#seqString`/`#seqBytes` trick
  from the example: `#nativeWrite`, `#nativeRead` and `#free` return `K` and are
  pure to K, so each is threaded through an argument position that must be
  evaluated before the result escapes. This is why the chain is several small
  helpers rather than one expression.
- **Buffers.** The spec path and the request are `#alloc`'d under *distinct*
  key constructors (`allocKeySpec`/`allocKeyReq`) so a request equal to its spec
  path cannot collide on one buffer; only addresses and keys travel down the
  chain, never the `Bytes` terms, since freeing a buffer a live `Bytes` still
  references is UB. All three buffers — the shim's `malloc`'d reply and the two
  inputs — are released before the decoded string escapes. Safety relies on
  there never being two crossings in flight, which holds because a `[function]`
  application reduces within one rewrite step and the LLVM backend is
  single-threaded: **`--enable-search` is unsupported.**
- **`-rdynamic` is mandatory** at kompile time. `#functionAddress` is `dlsym`,
  which searches only the *dynamic* symbol table, and nothing in the interpreter
  references these symbols, so without it the linker drops them and the run
  segfaults immediately. `make k-build` asserts `nm -D … | grep ml_eval_c` after
  kompiling for exactly this reason.
- **`ml_eval` is total.** An OCaml exception escaping through `caml_callback`
  has no handler in C and aborts the interpreter with no configuration dump, so
  `kffi.ml` mirrors every one of `boot.ml`'s handlers but returns the
  `{"error": …}` value described above instead of `exit 1`. The shim adds
  `caml_callback2_exn` + `Is_exception_result` as defence in depth.
- **Runners are memoized** per spec path in `kffi.ml`, so an extern call
  elaborates the lower spec once rather than per call. Building a runner also
  installs the `$print_` unparser (`Runner_target.init` → `Interface.init`), so
  no separate printer-init table is needed. Caveat: `Interface.P4.unparser` is a
  process-global ref, so if one run built runners for *two* paths the last would
  win; `kffi.ml` warns on stderr when that happens.
- The spec an extern resolves against is still **hardcoded**, for the same
  reason `entryRel()` is: K rules cannot read environment variables.
  `externSpec()` has two rules keyed on `<p4prog>`: `noP4()` gives
  `examples/lower` (the meta-language extern), `someP4(_)` gives `spec` (the P4
  spec's own). `builtinSpec()` is always `spec`. Only the path differs — the
  interface is always P4 — and that cell is the only channel that varies, so a
  third spec still has nowhere to come from.

## 8. Known problems

The first three problems here were properties of the old `#system` transport and
are **resolved** by the FFI crossing (§7). They are recorded, struck through,
because each shaped decisions elsewhere in the port.

- ~~**`#system` leaks a file descriptor per call**~~ (K 7.1.337, LLVM backend).
  After ~1024 external calls the interpreter aborted with `*** bit out of range 0 FD_SETSIZE on fd_set ***`, and `krun` then failed to parse an empty
  `result.kore`. **This, not correctness, was the ceiling on program size under
  K**: `action-bind.p4` type-checked, but `forloop1.p4` (which `#include`s
  `core.p4`, hence far more builtin calls) died here rather than on a spec
  error. The cause was in the K runtime and could not be fixed in this repo;
  memoizing pure builtin replies in a K cell would have cut the call count
  enough, but that was deliberately declined. **Resolved:** an FFI call consumes
  no file descriptor, so `/proc/<interp>/fd` stays flat rather than climbing
  4 → 1022.
- ~~**`$fresh_typeId` is wrong under K**~~. It closes over a per-process
  `int ref` (`builtin/call.ml`), and every `#system` call was a fresh process,
  so it returned the same value every time. **Resolved:** the OCaml runtime now
  lives for the whole run, so the counter advances. This is the cleanest
  observable proof that state persists across calls.
- ~~**One process spawn per call**~~ (~13 ms measured), with an **extern** call
  far worse — it parsed and elaborated the whole lower spec from scratch, and
  the lower runner's caches died with the process. **Resolved:** no `fork`/`exec`
  at all, and `kffi.ml` memoizes the runner per spec path, so the lower spec is
  elaborated once.

Standing problems:

- **An unknown extern name is indistinguishable from a spec-level failure.** The
  interpreter reports an undefined relation as a `Fail`, so a typo comes back as
  `{"fail": null}` and is silently recovered by an `otherwise` clause; stderr
  says ``relation `X` is undefined``. This is not
  hypothetical: it is what hid the missing P4-interface route (§5) — every
  `static_assert` came back as a recoverable failure, so the program merely
  failed to type-check rather than reporting a broken wire.
- **The spec an extern resolves against is hardcoded** per entry (§7), so it
  cannot vary per target beyond the two `<p4prog>` selects.

