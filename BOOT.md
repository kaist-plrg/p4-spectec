# Bootstrap Harness — Module & Functor Structure

This document describes the layered functor architecture used by the bootstrap harness.
The design relies on OCaml's **generative functors** (`Make ()`) to give each simulator
instance its own isolated mutable state, so that multiple SIM runners can coexist without
interfering with one another.

---

## Layer overview

```
┌─────────────────────────────────────────────────────────┐
│  backend-boot/      boot-time runners                   │
│    gen.ml           gen_boot_zero / gen_boot_one        │
│    p4.ml            P4.Make () : RUNNER                 │
├─────────────────────────────────────────────────────────┤
│  backend-sim/       simulation runners                  │
│    gen.ml           gen_p4 "v1model" / "ebpf" / "psa"   │
│    make.ml          Make.Make (...) : SIM               │
│    spec.ml          Spec.Make () : Spec.S               │
│    {v1model,ebpf,psa}/pipe.ml   Pipe.Make (Spec) : ARCH │
├─────────────────────────────────────────────────────────┤
│  runner/            generic runner (arch-agnostic)      │
│    make.ml          Make.Make (...) : RUNNER            │
├─────────────────────────────────────────────────────────┤
│  backend-sim/spec_impl/         per-SIM call refs       │
│    func.ml  rel.ml  pgm.ml      Make () + register      │
└─────────────────────────────────────────────────────────┘
```

---

## `runner/make.ml` — the recursive knot

`Runner.Make.Make` is the lowest-level combinator.  It ties the `Extern ↔ Interp`
circular dependency using OCaml's `module rec`:

```
Runner.Make.Make
  (Interface   : INTERFACE)
  (MakeExtern  : functor (INTERP_IL) (INTERP_SL) -> EXTERN)
  (MakeInterp_IL : functor (INTERFACE) (EXTERN) () -> INTERP_IL)
  (MakeInterp_SL : functor (INTERFACE) (EXTERN) () -> INTERP_SL)
  : RUNNER
```

Inside the functor body, the three modules are co-defined:

```ocaml
module rec Extern   : EXTERN   = MakeExtern   (Interp_IL) (Interp_SL)
       and Interp_IL : INTERP_IL = MakeInterp_IL (Interface) (Extern) ()
       and Interp_SL : INTERP_SL = MakeInterp_SL (Interface) (Extern) ()
```

On `init`, after loading the spec into one of the interpreters, the runner calls:

```
RUNNER.init spec
  ├── Interface.init spec
  ├── Interp_{IL,SL}.init spec     ← loads and caches the spec AST
  └── Extern.init_mode mode        ← wires Spec_.*.call (see below)
```

---

## `backend-sim/make.ml` — the SIM functor

`Backend_sim.Make.Make` wraps a runner with P4-specific concerns:

```
Backend_sim.Make.Make
  (Interface : INTERFACE)
  (MakeArch  : functor (Spec : Spec.S) -> ARCH)
  (MakeInterp_IL : ...)
  (MakeInterp_SL : ...)
  : SIM
```

Key steps inside the body:

```ocaml
module Spec_  = Spec.Make ()                  (* fresh per-SIM spec bundle *)
module Arch   = MakeArch (Spec_)              (* e.g. V1model.Pipe.Make    *)
module Table  = Table.Make (Spec_.Func)
```

`MakeExtern` closes over `Spec_` so that `init_mode` can register the
right interpreter callbacks:

```ocaml
module MakeExtern (Interp_IL : INTERP_IL) (Interp_SL : INTERP_SL) : EXTERN = struct
  let init_mode mode_ =
    Spec_.Func.register (fun name typs values -> ...dispatch to IL/SL...);
    Spec_.Rel.register  (fun name values      -> ...);
    Spec_.Pgm.register  (fun rel inc file     -> ...)
  let eval_extern_rel = Arch.eval_extern_rel
  let eval_extern_func = Arch.eval_extern_func
end
```

The resulting module is handed to `Runner.Make.Make`, which handles the
`module rec` knot and `init` sequencing.

---

## `spec.ml` / `spec_impl/` — per-SIM isolation

### The problem (before this design)

The original code had a single global trampoline per family:

```ocaml
(* old — shared across all runners *)
let call : call_func ref = Runner.Spec.Func.call
```

Initialising any runner would overwrite `Runner.Spec.Func.call`, silently
breaking any concurrently active runner.

### The fix — generative functors

Each file in `spec_impl/` follows the same pattern:

```ocaml
(* spec_impl/func.ml *)
module Make () = struct
  let call : call_func ref = ref (fun _ _ _ -> assert false)
  let register (f : call_func) = call := f
  (* helper functions that invoke !call *)
  let find_var_e_local ctx name = !call "find_var_e_local" [] [ctx; ...]
  ...
end
module type S = module type of Make ()
```

`spec.ml` bundles the three families and exposes the aggregate type:

```ocaml
module type S = sig
  module Func : Func.S
  module Rel  : Rel.S
  module Pgm  : Pgm.S
end

module Make () : S = struct          (* generative *)
  module Func = Func.Make ()         (* fresh ref *)
  module Rel  = Rel.Make  ()         (* fresh ref *)
  module Pgm  = Pgm.Make  ()         (* fresh ref *)
end
```

Every call to `Spec.Make ()` produces a **new, independent** triple of refs.

---

## Arch functors — `Pipe.Make (Spec : Spec.S) : ARCH`

Each architecture entry point takes the bundled spec and wires its
sub-components:

```
V1model.Pipe.Make (Spec : Spec.S) : ARCH
  ├── Core.Func   = Core.Func.Make   (Spec.Func)
  ├── Core.Object = Core.Object.Make (Spec.Func) (Spec.Rel)
  ├── Func        = V1model.Func.Make   (Spec)
  └── Object      = V1model.Object.Make (Spec)
```

```
Ebpf.Pipe.Make (Spec : Spec.S) : ARCH
  ├── Core.Func   = Core.Func.Make   (Spec.Func)
  ├── Core.Object = Core.Object.Make (Spec.Func) (Spec.Rel)
  └── Object      = Ebpf.Object.Make (Spec.Func)

Psa.Pipe.Make  (Spec : Spec.S) : ARCH    (same shape as ebpf)
```

Sub-functors that only need a subset of the spec keep narrower parameters
(`Spec.Func.S` only) for precision; the arch pipe functor threads the right
sub-module through each application.

---

## `backend-boot/` — the boot path

The boot path uses a **placeholder** arch (no real packet pipeline) purely to
run `static_assert` and other compile-time externs during P4 type-checking.

### `p4.ml`

```
P4.Make () : RUNNER
  ├── Spec_       = Spec.Make ()
  ├── Placeholder = Placeholder.Make (Spec_)   ← stub arch
  ├── MakeExtern  = { init_mode registers Spec_.*.call;
  │                   delegates eval_extern_{rel,func} to Placeholder }
  └── include Runner.Make.Make (Interface.P4) (MakeExtern) (...)
```

Each call to `P4.Make ()` is fully isolated: it carries its own `Spec_`
instance and its own `Placeholder`.

### `gen.ml`

```ocaml
(* Zero-boot: SpecTec runner with no P4 sub-runner *)
gen_boot_zero () =
  Runner.Make.Make (Interface.SpecTec) (Spectec.Make_zero) (...)

(* One-boot: SpecTec runner that drives a P4 sub-runner *)
gen_boot_one () =
  let Runner_P4 = P4.Make () in
  Runner.Make.Make (Interface.SpecTec) (Spectec.Make_one (Runner_P4)) (...)
```

---

## Builtin call flow in boot-2-p4

Builtin functions (e.g., `$find_map`) are declared in the P4 spec and called
by the SpecTec interpreter via an `extern relation Call_builtin_func`.  The
call crosses two interpreter layers — SpecTec then P4 — before resolving in
native OCaml.

### Spec sources

```
┌─────────────────────────────────┐  ┌──────────────────────────────┐  ┌──────────────────┐
│ spec^IL_src                     │  │ spec^P4_src                  │  │ pgm^P4_src       │
│                                 │  │                              │  │                  │
│ ;; 4.4.1-eval-cal-func.watsup   │  │ ;; 0.0-stdlib.watsup (P4)    │  │ bit<32> x = 32w1;│
│ extern relation Call_builtin_   │  │ builtin dec                  │  │ bit<32> y = x;   │
│   func                          │  │   $find_map<K,V>             │  │                  │
│                                 │  │   (map<K,V>, K) : V?         │  └──────────────────┘
│ rule Call_func/builtin:         │  │                              │
│   C |- builtinFuncDef           │  │ ;; 5.02.1-typing-            │
│     typ* val* : OK val_output   │  │    context.watsup (P4)       │
│   -- if BUILTIN id _ _ =        │  │ -- if varTypeIR =            │
│        builtinFuncDef           │  │      $find_map<id,           │
│   -- Call_builtin_func:         │  │      varTypeIR>              │
│      C |- id @`<typ*>`          │  │      (typeFrame, id)         │
│         `(val*)` : OK val_out   │  └──────────────────────────────┘
│                                 │
│ ;; 4.1-eval-exp.watsup (SpecTec)│
│ rule Eval_exp/call:             │
│   C |- CALL id targ* arg*       │
│     : valres                    │
│   -- Call_func_cached:          │
│      C |- id targ* arg* : valres│
└─────────────────────────────────┘
```

### Runtime layers

```
┌─────────────────────────────────────────────────────────────────────────┐
│  P4 interpreter layer  (Runner_P4 = P4.Make ())                         │
│                                                                         │
│  ┌──────────────────────────┬──────────────────────────────────────┐    │
│  │  Builtin^P4              │  Extern^P4  (Placeholder)            │    │
│  ├──────────────────────────┴──────────────────────────────────────┤    │
│  │  Interp^P4_OCaml                                  ↺ recursive   │    │
│  └──────────────────────────────────────────────────────────────── ┘    │
│                                                                         │
│  invoke_builtin_func (...) =       eval_extern_rel (...) =              │
│    let value_output =                (match name with                   │
│      try                            | "Call_builtin_func" ->            │
│        Interface.call_builtin           call_builtin_func values_input) │
│          Hook.on_value id           call_builtin_func (...) =           │
│          targs values_input           let value_output =                │
│                            (4)          match Runner.run_func           │
│                              ◄────────    id.it typs values with ...    │
└──────────────────────────────────────────────────────────────────────── ┘
                                ▲
                                │  (3) Runner_P4.run_func
                                │
┌─────────────────────────────────────────────────────────────────────────┐
│  SpecTec interpreter layer  (Runner_SpecTec = gen_boot_one ())          │
│                                                                         │
│  ┌──────────────────────────┬──────────────────────────────────────┐    │
│  │  Builtin^SpecTec         │  Extern^SpecTec  (Spectec_one)       │◄── (2)
│  ├──────────────────────────┴──────────────────────────────────────┤    │
│  │  Interp^SpecTec_OCaml                                           │    │
│  └──────────────────────────────────────────────────────────────── ┘    │
│                                                                         │
│  (1) rule Eval_exp/call fires; spec IL derives Call_builtin_func        │
│  (2) invoke_extern_rel:                                                 │
│        let values_output =                                              │
│          match Extern.eval_extern_rel id.it values_input with ...       │
└─────────────────────────────────────────────────────────────────────────┘
```

### Call sequence

```
(1) SpecTec interp hits Eval_exp/call; spec IL fires rule Call_func/builtin
(2) invoke_extern_rel → Extern^SpecTec.eval_extern_rel "Call_builtin_func"
        dispatches to call_builtin_func in Placeholder (Extern^P4)
(3) call_builtin_func → Runner_P4.run_func id.it typs values
        (crosses from SpecTec layer up to P4 layer)
(4) P4 interp → invoke_builtin_func
        → Interface.call_builtin  (native OCaml)
```

---

## Instance topology — no shared state

Two independent SIM instances after `init`:

```
SIM_v1model = Backend_sim.Make.Make (Interface.P4) (V1model.Pipe.Make) (...)
  │
  ├── Spec_v1        = Spec.Make ()
  │     Func.call ──────────────────► Interp_SL_v1.eval_func
  │     Rel.call  ──────────────────► Interp_SL_v1.eval_rel
  │     Pgm.call  ──────────────────► Interp_SL_v1.eval_program
  └── Arch_v1        = V1model.Pipe.Make (Spec_v1)

SIM_ebpf = Backend_sim.Make.Make (Interface.P4) (Ebpf.Pipe.Make) (...)
  │
  ├── Spec_ebpf      = Spec.Make ()   ← entirely separate refs
  │     Func.call ──────────────────► Interp_SL_ebpf.eval_func
  │     Rel.call  ──────────────────► Interp_SL_ebpf.eval_rel
  │     Pgm.call  ──────────────────► Interp_SL_ebpf.eval_program
  └── Arch_ebpf      = Ebpf.Pipe.Make (Spec_ebpf)
```

No ref is shared between the two instances.

---

## Init sequence (step by step)

Given `(module SIM) = Backend_sim.Make.Make (Interface.P4) (V1model.Pipe.Make) (...)`:

```
1.  SIM.init (SL spec_sl)
2.    Interface.P4.init (SL spec_sl)          ← parse context reset
3.    Interp_SL.init ~cache ~det spec_sl      ← spec AST loaded & cached
4.    Extern.init_mode SL_mode
5.      Spec_.Func.register call_func         ← call_func dispatches to Interp_SL
6.      Spec_.Rel.register  call_rel
7.      Spec_.Pgm.register  call_pgm
        (Spec_.Func.call now points to Interp_SL.eval_func)
8.  SIM.init_pipe includes filename
9.    Spec_.Pgm.call "V1Model_init" includes filename
10.     Interp_SL.eval_program "V1Model_init" includes filename
           ↳  interpreter executes spec relation, returns (ctx, arch)
```

From step 7 onward, every call through `Spec_.Func.*` / `Spec_.Rel.*` /
`Spec_.Pgm.*` reaches this SIM's own interpreter — never another SIM's.
