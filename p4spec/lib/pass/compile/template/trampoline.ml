(* Trampoline glue for generated OCaml

   A builtin like [builtin dec $sum_nat(nat* ) : nat] compiles to a top-level
   [f__sum_nat] that still needs the host's [call_builtin].

   [call_builtin] is called through a [trampoline], holding function pointers
   to the actual builtin implementation.

     [let f__sum_nat (p__0 : Bigint.t list) : Bigint.t =
        let trampoline__ = !trampoline_cur__ in
        ..
        trampoline__.interface.call_builtin (fun _ -> ()) ("sum_nat" $ no_region) [] [v__0]
        ..]

   Similar idea applies for extern calls and program parsing. *)

let glue () : string =
  {|
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
|}
