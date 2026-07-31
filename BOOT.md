# Meta-circular interpretation

This document explains the meta-circular interpretation feature of P4-SpecTec,
alongside the **tower abstraction** that builds the meta-circular stack of interpreters.

---

## Motivation

### Background: type-checking P4 programs with P4-SpecTec

The standard use of P4-SpecTec is a two-level stack:

```
OCaml AL/SL interpreter  ──runs──►  P4 static semantics spec
                                            │
                                            └──type-checks──►  P4 program
```

The OCaml AL/SL interpreter is a general meta-interpreter: given any spec
written in the P4-SpecTec DSL, it can execute that spec.The P4 static semantics
spec is simply one particular spec in this meta-language.

### Meta-circular interpretation: P4-SpecTec²

Because the OCaml AL/SL interpreter is general-purpose, it can equally well
interpret a spec that describes the *dynamic semantics of P4-SpecTec itself*
— a spec we call **P4-SpecTec²** (read: P4-SpecTec-squared, or
P4-SpecTec-in-P4-SpecTec). This gives a three-level stack:

```
OCaml AL/SL interpreter  ──runs──►  P4-SpecTec² spec
                                            │
                                       ──runs──►  P4 static semantics spec
                                                        │
                                                   ──type-checks──►  P4 program
```

This is **meta-circular interpretation**: the interpreter spec is written in
the same language that the interpreter interprets.Note that this is distinct
from *bootstrapping* — we are chaining interpreters, not compiling the defined
language with itself.

Stacking one more P4-SpecTec² layer in between yields the second tower:

```
OCaml  ──►  P4-SpecTec²  ──►  P4-SpecTec²  ──►  P4 static semantics  ──►  P4 program
```

In principle, any number of intermediate layers can be inserted. Also note that
we have multiple P4-SpecTec² specs. Because P4-SpecTec adopts a multi-stage
compilation approach, each stage has its own spec. Currently, we have a spec
for the AL stage and a spec for the SL stage. Thus, we may stack multiple
versions of P4-SpecTec², each running a different stage's spec.

```
OCaml  ──►  P4-SpecTec² (of SL)  ──►  P4-SpecTec² (of AL)  ──►  P4 static semantics  ──►  P4 program
```

## The Tower Abstraction

A **tower** is a stack of interpreter *levels* where each level runs the spec
of the level below it.  The outermost (topmost) level is the **booter**; the
innermost (bottommost) level is the **target**.  Zero or more **intermediate**
levels sit in between.

```
  ┌──────────────────────────────────┐
  │  Booter  (runs Entry)            │  ← run on OCaml AL/SL interpreter
  ├──────────────────────────────────┤
  │  Intermediate  (optional, ...)   │
  ├──────────────────────────────────┤
  │  Target  (runs Program_ok / ...) │
  └──────────────────────────────────┘
```

Concretely, a tower is represented by the `Config.tower` record:

```ocaml
type tower = {
  mode        : mode;          (* AL_mode or SL_mode — how the booter's spec is loaded *)
  level_boot  : level;         (* outermost level, run directly on the OCaml interpreter *)
  levels_interm : level list;  (* zero or more intermediate levels, top to bottom *)
  level_target : level;        (* innermost level, runs the object-language spec *)
  target      : target;        (* the object-language program to check *)
}

type level  = { layer : layer; interface : interface }
type layer  = { specdir : string; rel : string }
type target = { includes : string list; path : string }
```

The `target` field carries the P4 program path and include directories supplied
on the command line (`-p` / `-i`).

Each level is described by:

| Field       | Meaning                                             |
|-------------|-----------------------------------------------------|
| `specdir`   | Directory whose `.watsup` files define this level   |
| `rel`       | Entry relation to evaluate at this level            |
| `interface` | How programs at this level are parsed (`al`/`sl`/`p4`) |

A tower also carries a global `mode` (`al` or `sl`) that determines whether the
booter's own spec is loaded as an AL or SL spec. If the `mode` is set as `al`,
the AL interpreter (in OCaml) runs the tower's boot level; if `mode` is `sl`,
the SL interpreter runs the boot level.

### Runner modules

Each level in the tower is realized as an OCaml `Runner` module with three
sub-modules:

```
Runner
  ├── Interface   — language-specific I/O: parses and unparses programs,
  │                 provides native builtins (e.g. map lookup for P4,
  │                 boot/unboot conversions for AL/SL specs)
  ├── Interp      — the AL or SL interpreter that evaluates the level's spec
  └── Extern      — supplies semantics for `extern` declarations in the spec
```

The `Interface` module determines what *language* the level speaks. For a P4
level it parses P4 programs; for an AL or SL level it parses SpecTec scripts
and provides the boot/unboot functions that convert between OCaml runtime values
and their spec-level representations.

The `Extern` module supplies the semantics for `extern` declarations in the spec.
In P4-SpecTec, a spec may declare relations or functions as `extern`, meaning
their implementation is provided from the outside rather than specified in the
DSL itself. Normally this is how native OCaml primitives (e.g. map lookup) are
hooked in. In the tower, we reuse this same mechanism to wire levels together.

Each intermediate or boot level is given a `Make_parametric` extern, constructed
with the `Runner` of the level immediately above it:

```ocaml
module Make_parametric
    (Runner_above : Run.RUNNER)
    (Interface_SpecTec : INTERFACE_SPECTEC)
    () : Run.EXTERN
```

When the level's interpreter encounters an `extern` call, `Make_parametric`
handles it by translating the call across the level boundary:

1. **Unboot** — convert the meta-level value representation of the arguments
   to the spec-level representation using `Interface_SpecTec.unboot_*`.
2. **Relay** — dispatch to `Runner_above.Interp.eval_rel` or `eval_func`,
   passing the spec-level values.
3. **Boot** — convert the results back into the meta-level representation
   using `Interface_SpecTec.boot_*` and return them to the caller.

The three `extern` relations that flow across level boundaries are:

| Extern name         | Relay target                       | Meaning                                    |
|---------------------|------------------------------------|--------------------------------------------|
| `Call_builtin_func` | `Runner_above.Interp.eval_func`    | Call a builtin function one level up       |
| `Call_extern_func`  | `Runner_above.Interp.eval_func`    | Call an extern function one level up       |
| `Call_extern_rel`   | `Runner_above.Interp.eval_rel`     | Invoke an extern relation one level up     |

### JSON schema for defining a tower

```json
{
  "mode": "al" | "sl",
  "levels": [
    { "specdir": "<dir>", "rel": "<rel>", "interface": "al" | "sl" | "p4" },
    ...
  ]
}
```

We supply the tower definition as a JSON file to ease command-line parsing. The
first entry in `levels` is the boot level; the last entry is the target level;
everything in between is intermediate. At least two levels (for boot and
target) are required.

---

### Example Towers

In `towers`, we define several example towers.

| File          | Mode  | Boot interface | Intermediates | Target | Description                           |
|---------------|-------|----------------|---------------|--------|---------------------------------------|
| `al.json`     | `sl`  | `al`           | —             | `p4`   | Single AL meta-interpreter over P4    |
| `sl.json`     | `sl`  | `sl`           | —             | `p4`   | Single SL meta-interpreter over P4    |
| `al-al.json`  | `sl`  | `al`           | AL            | `p4`   | AL meta-interpreter over AL-over-P4   |
| `al-sl.json`  | `sl`  | `al`           | SL            | `p4`   | AL meta-interpreter over SL-over-P4   |
| `sl-al.json`  | `sl`  | `sl`           | AL            | `p4`   | SL meta-interpreter over AL-over-P4   |
| `sl-sl.json`  | `sl`  | `sl`           | SL            | `p4`   | SL meta-interpreter over SL-over-P4   |

---

## How a Tower Is Built

`build_tower` assembles the runner stack from target to booter:

```
1.  build_target (level_target)
      → creates a Runner for the target, either P4 or AL/SL depending on the target's interface
      → loads the target spec to the Runner's interpreter module for relays

2.  for each intermediate level (target → boot order):
      build_interm (Runner_above, level)
        → creates a Runner with Make_parametric(Runner_above) Extern
        → loads the level's spec to the Runner's interpreter module for relays

3.  build_boot (Runner_above, mode, level_boot)
      → creates a Runner with Make_parametric(Runner_above) Extern
      → loads the boot level's spec as AL (if mode=al) or SL (if mode=sl)
```

The result is a chain of runners where each runner's `Extern` module holds a
reference to the runner above it. `extern` meta-function or relation calls
are relayed upwards from the booter to the appropriate level where they are defined.

---

## The `al-sl.json` Tower

```json
{
  "mode": "sl",
  "levels": [
    { "specdir": "spec-meta/al", "rel": "Entry", "interface": "al" },
    { "specdir": "spec-meta/sl", "rel": "Entry", "interface": "sl" },
    { "specdir": "spec",      "rel": "Program_ok", "interface": "p4" }
  ]
}
```

This builds a three-level tower:

```
Level 0 (boot):   spec-meta/al Entry    — AL interface, loaded as SL spec
Level 1 (interm): spec-meta/sl Entry    — SL interface, loaded as SL spec
Level 2 (target): spec/Program_ok       — P4 interface, loaded as SL spec
```

### Extern relay

What happens when "spec" (the P4 spec) calls a builtin function?

```
;; spec/5-typing/5.02.1-typing-context.watsup
def $add_var_t(GLOBAL, TC, id, varTypeIR) = TC'
  -- ...
  -- if typeFrame_update
      = $add_map<nameIR, varTypeIR>(typeFrame, id, varTypeIR)
```

The semantics of a builtin function is interpreted at the intermediate level,
running "spec-meta/sl" in "sl" interface.

```
;; spec-meta/sl/5.6-eval-call-func.watsup
rule Call_func_dispatch/builtin:
  C |- builtinFuncDef typ* val* : valres
  -- if BUILTIN id _ _ = builtinFuncDef
  -- Call_builtin_func:
      |- id `@ `< typ* > `( val* ) : valres

;; spec-meta/common/4-relation.watsup
extern relation Call_builtin_func:
  |- id `@ `< typ* > `( val* ) : res<val>
  hint(input %0 %1 %2)
```

So, the builtin meta-function call at the target spec layer is relayed as an
extern relation call in the intermediate layer. Now, the semantics of an extern
relation is interpreted at the layer below, the boot level, running "spec-meta/al"
in "al" interface.

```
;; spec-meta/al/5.7-eval-call-rel.watsup
rule Call_rel_dispatch/ext:
  C |- (EXT id) val* : valsres
  -- Call_extern_rel:
      |- id val* : valsres

;; spec-meta/common/4-relation.watsup
extern relation Call_extern_rel:
  |- id val* : res<val*>
  hint(input %0 %1)
```

Below the boot level, is the SL interpreter in OCaml. When it receives the extern relation
call to `Call_extern_rel`, it dispatches to its extern module.

```ocaml
(* p4spec/lib/interp/interp-sl/interp.ml *)
and invoke_extern_rel (ctx : Ctx.t) (nottyp : nottyp) (inputs : Hints.Input.t)
    (id : id) (values_input : value list) : value list =
  let values_output =
    match Extern.eval_extern_rel id.it values_input with
    | Pass values -> values
    | Fail (at, msg) -> back_unmatch at msg
  in
```

This extern call is handled by the `Make_parametric` extern module (the one
constructed during tower building).

```ocaml
(* p4spec/lib/backend-boot/spectec.ml *)
module Make_parametric
    (Runner : Run.RUNNER)
    (Interface_SpecTec : INTERFACE_SPECTEC)
    () : Run.EXTERN = struct

  let call_extern_rel (values_input : Value.t list) : Value.t list =
    let value_id, value_values =
      match values_input with
      | [ value_id; value_values ] -> (value_id, value_values)
      | _ -> error_no_region "unexpected number of arguments to call_extern_rel"
    in
    ...
    let id = value_id |> Interface_SpecTec.unboot_id in
    let values = value_values |> Interface_SpecTec.unboot_values in
    let values_output =
      match Runner.Interp.eval_rel id.it values with
      | Pass values_output -> values_output
      | Fail (at, msg) -> error at msg
    in
    let value_values_output = Interface_SpecTec.boot_values values_output in
    let value_values_output_res =
      Value.Make.("OK val*" <| [ value_values_output ] <<| "valsres")
    in
    ...
    [ value_values_output_res ]

  let eval_extern_rel (name : string) (values_input : Value.t list) :
      Run.rel_result =
    try
      Run.Pass
        (match name with
        | "Call_extern_rel" -> call_extern_rel values_input
        | ...
    with ...

end
```

And `Runner.Interp.eval_rel` in `call_extern_rel` would relay the call to the
intermediate level's interpreter, which would dispatch `Call_builtin_func` to the
intermediate level's extern module.

```ocaml
(* p4spec/lib/backend-boot/spectec.ml *)
module Make_parametric
    (Runner : Run.RUNNER)
    (Interface_SpecTec : INTERFACE_SPECTEC)
    () : Run.EXTERN = struct

  let call_builtin_func (values_input : Value.t list) : Value.t list =
    let value_id, value_typs, value_values =
      match values_input with
      | [ value_id; value_typs; value_values ] ->
          (value_id, value_typs, value_values)
      | _ ->
          error_no_region "unexpected number of arguments to call_builtin_func"
    in
    ...
    let id = value_id |> Interface_SpecTec.unboot_id in
    let typs = value_typs |> Interface_SpecTec.unboot_typs in
    let values = value_values |> Interface_SpecTec.unboot_values in
    let value_output =
      match Runner.Interp.eval_func id.it typs values with
      | Pass value_output -> value_output
      | Fail (at, msg) -> error at msg
    in
    let value_value_output = Interface_SpecTec.boot_value value_output in
    let value_value_output_res =
      Value.Make.("OK val" <| [ value_value_output ] <<| "valres")
    in
    ...
    [ value_value_output_res ]

  let eval_extern_rel (name : string) (values_input : Value.t list) :
      Run.rel_result =
    try
      Run.Pass
        (match name with
        | "Call_builtin_func" -> call_builtin_func values_input
        | ...
    with ...

end
```

Now, the `Call_builtin_func` extern relation call is handled by the intermediate level's
extern module. It is relayed to the target level's interpreter, which evaluates the builtin function
call (according to the semantics of the P4 interface) and returns the result.

Now the result travels back as follows:

* The intermediate level's extern module receives the result.
* The intermediate level's extern module wraps the result as a intermediate-level spec value and returns it to the boot level's interpreter.
* The boot level's extern module receives the result.
* The boot level's extern module wraps the result again as a boot-level spec value and returns it to the SL interpreter.
* The SL interpreter receives the value.

Notice that this call path is very costly, as it traverses the whole tower.
Thus, caches are inserted at each relay point to avoid redundant calls and
wrap-unwraps.

## The Patch Mechanism

Before `eval_rel` is called on the booter, `apply_tower` synthesizes a `main()`
function and splices it into each level's spec.  The synthetic `main()` embeds
the parsed program (and each intermediate spec) as a literal value so that the
boot relation finds them at runtime without any I/O.

The patch proceeds from the target upward:

```
1. parse_target (prog.p4)         → value_target
2. apply_target (level_target, value_target, level_interm_spec)
     → synthetic main() in interm spec that calls Entry on value_target
3. apply_interm (level_interm, value_interm_script, level_boot_spec)
     → synthetic main() in boot spec that calls Entry on value_interm_script
```

The booter's `eval_rel "Entry"` receives the fully patched script as input
and evaluates the whole tower without any further file I/O.

---

## Invoking a Tower

```bash
spectec-boot boot-n \
  -tower towers/al-sl.json \
  -p     prog.p4 \
  -i     p4c/p4include
```

Optional flags: `-no-cache`, `-det`, `-guard`, `-trace`, `-trace-full`, `-profile`.
