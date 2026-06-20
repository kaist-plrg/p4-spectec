(* Stub Ctx for the [spec_parts] library — copied into [compiled/] by
   `make restore-stub` so plain `make build` never compiles the heavy generated
   parts. Overwritten by `make gen-ocaml`.

   Mirrors the type surface of the generated [Ctx] (same [iface__]/[extern__]
   field names) so the thin [spec_compiled_stub.ml] shell typechecks against it. *)

[@@@warning "-8-11-26-27-30-32-33-39"]

module Value = Runtime.Value
module Typ = Runtime.Type.Typ
module Run = Runtime.Dynamic_Runner.Signature

type iface__ = {
  parse_program : string list -> string list -> Run.parse_result;
  call_builtin  : (Value.t -> unit) -> Domain.Lib.Id.t -> Typ.t list -> Value.t list -> Value.t;
}

type extern__ = {
  eval_extern_rel  : string -> Value.t list -> Run.rel_result;
  eval_extern_func : string -> Typ.t list -> Value.t list -> Run.func_result;
}

type ctx__ = {
  iface : iface__;
  extern : extern__;
}

let dummy_iface__ : iface__ = {
  parse_program = (fun _ _ -> failwith "spec_compiled: ctx not initialized");
  call_builtin = (fun _ _ _ _ -> failwith "spec_compiled: ctx not initialized");
}

let dummy_extern__ : extern__ = {
  eval_extern_rel = (fun _ _ -> failwith "spec_compiled: ctx not initialized");
  eval_extern_func = (fun _ _ _ -> failwith "spec_compiled: ctx not initialized");
}

let dummy__ : ctx__ = {
  iface = dummy_iface__;
  extern = dummy_extern__;
}

let cur__ : ctx__ ref = ref dummy__

let with_ctx (c : ctx__) (f : unit -> 'a) : 'a =
  let saved = !cur__ in
  cur__ := c;
  Fun.protect ~finally:(fun () -> cur__ := saved) f
