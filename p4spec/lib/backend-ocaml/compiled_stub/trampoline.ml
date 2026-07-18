(* Stub Trampoline for the [spec_parts] library — copied into [compiled/] by
   `make restore-stub` so plain `make build` never compiles the heavy generated
   parts. Overwritten by `make gen-ocaml`.

   Mirrors the type surface of the generated [Trampoline] (same
   [interface__]/[extern__] field names) so the thin [interp_ml_stub.ml] shell
   typechecks against it. *)

[@@@warning "-8-11-26-27-30-32-33-39"]

module Value = Runtime.Value
module Typ = Runtime.Type.Typ
module Run = Runtime.Dynamic_Runner.Signature

type interface__ = {
  call_builtin  : (Value.t -> unit) -> Domain.Lib.Id.t -> Typ.t list -> Value.t list -> Value.t;
  parse_program : string list -> string list -> Run.parse_result;
}

type extern__ = {
  eval_extern_rel  : string -> Value.t list -> Run.rel_result;
  eval_extern_func : string -> Typ.t list -> Value.t list -> Run.func_result;
}

type trampoline__ = {
  interface : interface__;
  extern : extern__;
}

let dummy_interface__ : interface__ = {
  call_builtin = (fun _ _ _ _ -> failwith "interp_ml: trampoline not initialized");
  parse_program = (fun _ _ -> failwith "interp_ml: trampoline not initialized");
}

let dummy_extern__ : extern__ = {
  eval_extern_rel = (fun _ _ -> failwith "interp_ml: trampoline not initialized");
  eval_extern_func = (fun _ _ _ -> failwith "interp_ml: trampoline not initialized");
}

let dummy__ : trampoline__ = {
  interface = dummy_interface__;
  extern = dummy_extern__;
}

let trampoline_cur__ : trampoline__ ref = ref dummy__

let with_trampoline (c : trampoline__) (f : unit -> 'a) : 'a =
  let saved = !trampoline_cur__ in
  trampoline_cur__ := c;
  Fun.protect ~finally:(fun () -> trampoline_cur__ := saved) f
