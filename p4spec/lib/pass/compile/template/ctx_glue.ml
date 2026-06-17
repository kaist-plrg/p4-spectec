(* Per-instance context glue for generated OCaml.

   Replaces the old functor-internal state. The [iface]/[extern] state of the
   current interpreter instance lives in a module-global [cur__], switched only
   at dispatch boundaries by [with_ctx] (save / set / restore), so towers of
   nested runners each keep their own context. *)

let static =
  {|
type iface__ = {
  parse_program : string list -> string list -> Run.parse_result;
}

type extern__ = {
  eval_extern_rel  : string -> Value.t list -> Run.rel_result;
  eval_extern_func : string -> Typ.t list -> Value.t list -> Run.func_result;
  call_builtin     : (Value.t -> unit) -> Domain.Lib.Id.t -> Typ.t list -> Value.t list -> Value.t;
}

type ctx__ = {
  iface : iface__;
  extern : extern__;
}

let dummy_iface__ : iface__ = {
  parse_program = (fun _ _ -> failwith "spec_compiled: ctx not initialized");
}

let dummy_extern__ : extern__ = {
  eval_extern_rel = (fun _ _ -> failwith "spec_compiled: ctx not initialized");
  eval_extern_func = (fun _ _ _ -> failwith "spec_compiled: ctx not initialized");
  call_builtin = (fun _ _ _ _ -> failwith "spec_compiled: ctx not initialized");
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
|}

let glue () : string = static
