(* Re-export of the boundary contracts and the interpreter instance so consumers
   keep using [Valrep.SAFE] / [Valrep.UNSAFE] / [Valrep.VAL] and [Valrep.V_value].
   The contracts live in [sig.ml] and the [V_value] instance in [val_value.ml];
   [V_native] (the native-OCaml instance) lives in [backend-ocaml], where the
   generated symbols are in scope. *)

module type SAFE = Sig.SAFE
module type UNSAFE = Sig.UNSAFE
module type VAL = Sig.VAL

module V_value = Val_value.V_value
