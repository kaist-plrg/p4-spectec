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
| — | `al/0-config.k` (configuration), `al/4.1-extern-json.k` (external interface: codec), `al/4.2-extern-ffi.k` (external interface: transport) |

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
    <specdir> $SPEC:String </specdir>
    <global>  <gtdenv> .Map </gtdenv> <grenv> .Map </grenv> <gfenv> .Map </gfenv> </global>
    <local>   <ltdenv> .Map </ltdenv> <lfenv> .Map </lfenv> <lvenv> .Map </lvenv> </local>
    <caller>  <cfenv> .Map </cfenv> </caller>
    <saves>     .List </saves>
    <callstack> .List </callstack>
  </al>
```

- **`initFFI()`** — at the very *head* of `<k>`, ahead of `$PGM`, so the OCaml
  runtime behind the external interface is started, and its runner built for the
  target spec, before any rule that could reach a builtin (§7). Ordering is
  guaranteed by `<k>` sequencing, not a flag.
- **`<k>`** — the AL script arrives as `$PGM` and is *consumed* by loading:
  [al/3-context.k](spec-meta-k/al/3-context.k) peels definitions off its head
  into the global cells (this is `$load` / `$load_typdef` / `$load_reldef` /
  `$load_funcdef`), leaving `.K` and then `afterLoad()`.
- **`<p4prog>`** — the second krun input, `noP4()` or `someP4(val)`. It selects
  the entry (§3).
- **`<specdir>`** — the target spec path, from `-cSPEC` (§7).
- **`<global>` / `<local>`** — the two layers of the watsup `ctx`: type
  definitions, relations (global only, matching `$find_rel`), functions and
  value bindings. `$find_func` checks `<lfenv>` then `<gfenv>`.
- **`<caller>`/`<cfenv>`** — the caller's function environment, for
  `Assign_arg/fun`. In watsup this is `C_caller`, a *second context passed as an
  argument* to `Eval_clause`; K needs a cell for it, and that cell must be
  stacked per call frame (§4).
- **`<saves>` / `<callstack>`** — the two backtracking stacks (§4).

## 3. Running

```sh
make k-build                                # kompile -> al-kompiled/
make k-run TEC=examples/add.watsup          # a self-contained script
make k-typecheck P4=p4c/testdata/p4_16_samples/action-bind.p4
make k-clean                                # drop al-kompiled/ and scratch files
```

### Runtime dependency

**A K run must start from the repo root**, because the target spec path an
external call resolves against (`<specdir>`, from `-cSPEC`) is relative to it.

**`./spectec-boot` must exist**, not for external calls but because the two
parser wrappers (§6) invoke it to boot `$PGM` and parse `$P4`, entirely outside
the interpreter. Build it with `make boot` (never `dune build` directly; note
that `make clean` leaves a stale `./spectec-boot` behind).

**`al-kompiled/interpreter` embeds a snapshot of the OCaml implementation.**
`kffi.exe.o` — the whole of `p4spec/` as one object, OCaml runtime included — is
linked into it at kompile time (§7). So after editing `p4spec/`, `make boot`
alone leaves a **stale interpreter** that silently keeps using the old builtins.
The workflow is:

```sh
make boot && make k-build
```

Scratch files are `mktemp`'d straight into **`./spec-meta-k/`** (gitignored), by
the two parser wrappers, before K starts; there is no scratch directory to
create, and nothing writes there from inside K.

### A simple AL program with a `main`

`make k-run TEC=examples/add.watsup` expands to

```sh
KDEF=al-kompiled krun -d al-kompiled \
  --parser ./spec-meta-k/scripts/kast-json.sh examples/add.watsup \
  -cP4= -pP4=./spec-meta-k/scripts/kast-p4.sh \
  -cSPEC='"examples/add.watsup"'
```

The `.watsup` is booted to a term by the parser wrapper; `-cP4=` is empty, so
`<p4prog>` is `noP4()` and `afterLoad()` fires the first entry rule:

```k
rule <k> afterLoad() => ... ~> callFunc("main", .TypList, .ValList) ~> finishMain() ... </k>
     <p4prog> noP4() </p4prog>
```

This is exactly `spec-meta/al/6-entry.watsup`'s `Entry`. Output is the printed
log followed by `$main()`'s return value, rendered by the same `jsonOfVal` the
log uses:

```
["textV","entry-al"]
["textV","load complete"]
["textV","into call"]
["textV","Add"]          <- the `-- debug "Add"` premise in add.watsup
["intN","119"]           <- what `$main()` returned
```

A failing run prints `fail` in place of the value.

To inspect the final configuration — to check that `<k>`, `<saves>` and
`<callstack>` all end empty, since a leftover frame in either stack means a
save/restore path returned without popping — drop `--output none` from the
`make` line and read the dump. Expect megabytes: `<global>` holds the whole
loaded spec.

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

Use `krun --output json` (in place of `--output none`) to diff whole
configurations mechanically rather than by eye.

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
make k-typecheck P4=p4c/testdata/p4_16_samples/action-bind.p4   # -> passed
make k-typecheck P4=p4c/testdata/p4_16_errors/action-bind.p4    # -> fail
```

`passed` means `Program_ok` held; `fail` means it did not, i.e. a type error in
the program under test. The verdict is the last line of stdout, after the log,
so a batch runner can read it with `tail -1`. Note that `fail` does not say
*why*: a diagnostic needs the OCaml implementation, or `debug` premises in the
spec.

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

**`<saves>` and `<callstack>` are separate, and conflating them is a real
hazard** — doing so once produced a bug where a relation call's `ValList` met a
continuation expecting a bare `Val`.

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

The first two are what the parser wrappers (§6) invoke. The third is **not on
the K path** — external calls cross by FFI (§7) — but it is kept: it is the same
dispatch, reachable from a shell, so a single request can be replayed by hand
against a `.json` file when debugging the wire.

**A fourth binary target**, [p4spec/bin/kffi.ml](p4spec/bin/kffi.ml), built by
dune as an *object* rather than an executable (`(modes object)`, which bundles
the OCaml runtime in). It is the in-process equivalent of `extern`: same wire,
same dispatch, but registered under the names `ml_init`/`ml_eval` for the C
shim to reach via `caml_callback`, and holding its runner across calls (§7).

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
the OCaml stays the single authority.

**Two kinds of extern, and they are not the same mechanism.** The paragraph
above describes the *meta-language* extern, where a lower spec's ordinary
`dec`/`relation` definitions supply the meaning. The **P4 spec's own** externs —
`ExternFunctionCall_eval_lctk`, `ExternFunctionCall_eval`, `ExternMethodCall_eval`
— are the other kind: their meaning is the architecture model in
`p4spec/lib/backend-sim/`, reached through the P4 interface
(`P4.Make ()`/`Placeholder`, `backend-boot/p4.ml`), which is how `build_target`
wires the target level of a tower. There is no lower spec involved; `-spec` is
the P4 spec itself.

`extern` therefore builds its runner with **`build_target`** on `P4_interface` —
reusing the tower's target-level wiring rather than duplicating it. That one
runner serves both kinds, because the interface governs only how *extern* names
resolve: a spec declaring none of its own resolves its `dec`s and `relation`s
the same either way. Hence the subcommand takes neither an interface nor a mode
flag. (`build_null` cannot serve the P4 kind at all: it wires
`Spectec.Make_null`, whose `eval_extern_rel` knows only `Call_builtin_func`.)
`level.rel` is passed empty, since nothing on this path runs a program, and
`build_target`'s SL mode is unobservable, since an `extern relation` dispatches
to `Placeholder` from `invoke_rel` without any rule running under an
interpreter.
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

`krun` has no `--input json`, so the two *term-valued* configuration variables
need a `--parser`, and each parser is a shell script. (`$SPEC`, the third, is a
plain `String`: `krun` parses `-cSPEC='"spec"'` directly, so it has no wrapper.)
Three constraints shape both:

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

The codec is [al/4.1-extern-json.k](spec-meta-k/al/4.1-extern-json.k) on the K
side and [extern.ml](p4spec/lib/interface/spectec/ali/extern.ml) on the OCaml
side; both document the format in full at their head. The *transport* — how a
request actually crosses — is
[al/4.2-extern-ffi.k](spec-meta-k/al/4.2-extern-ffi.k),
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
  a genuine defect sticks visibly in `<k>` carrying the diagnostic. `kffi.ml`
  also echoes it to stderr as it happens, since a K run can be minutes long.
  `Fail`'s region and message are lost crossing the wire (K's `FAIL` is nullary)
  and go to stderr.

Transport mechanics:

The crossing is K's **FFI** (libffi), which can call any plain C ABI function.
OCaml cannot expose one directly, so a thin C shim sits between:

```
K rules  --#ffiCall-->  spec-meta-k/ffi/shim.c  --caml_callback-->  p4spec/bin/kffi.ml
```

The shape is the one [examples/k/](examples/k/) proves out and its README
recommends for real use: **one** `ml_eval_c(req) -> char *` carrying a
serialized request, with dispatch in OCaml — preceded by a single
`ml_init_c(spec)` that names the target spec once and for all.

- **The OCaml runtime lives inside the interpreter.** `kffi.exe.o` is built by
  dune with `(modes object)`, which bundles the whole runtime into one object,
  and `kompile -ccopt` links it and `shim.o` into `al-kompiled/interpreter`.
  `krun` is then a single long-lived process, which is what makes everything
  below possible — and what couples the interpreter to `p4spec/` (§3).
- **Initialization is explicit, and eager.** The shim assumes `ml_init_c(spec)`
  has run — `caml_startup`, resolving the `ml_init`/`ml_eval` closures, and
  building the runner for the target spec — and, following the example, other
  entry points do not check. K guarantees it by *sequencing*: `initFFI()` sits at
  the head of `<k>`, reads the target spec out of `<specdir>`, and nothing can be
  touched until it reduces. The `Int` the shim returns is discarded mid-chain;
  since K evaluates a function's arguments before applying its rule, discarding
  a result never discards the effect that produced it. Every run pays the
  runner build, including ones making no external call at all
  (`examples/add.watsup`); that is the cost of eager init, and it buys a wire on
  which no individual call carries a spec path.
