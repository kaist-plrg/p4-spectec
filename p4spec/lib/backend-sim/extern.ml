module Value = Runtime.Value
module Typ = Runtime.Type.Typ
module Run = Runtime.Dynamic_Runner.Signature
open Error
open Util.Source

module type IMPL = sig
  type vt

  val eval_extern_init : vt list -> vt
  val eval_extern_func_lctk_call : vt list -> vt list
  val eval_extern_func_call : vt list -> vt list
  val eval_extern_method_call : vt list -> vt list
  val init_arch_state : vt
end

(* Bridge the [vt]-typed extern implementation to the [Value.t]-typed dynamic
   runner. The (un)wrapping via [R.to_value]/[R.of_value] is a zero-cost
   reinterpret across the runner edge: the value rides through the [Value.t]-
   typed signature and is handed back to compiled code undecoded (identity under
   [V_value]; bare [Obj.obj]/[Obj.repr] under [V_native]).

   This bridge needs nothing but that cast, so it takes [Valrep.RAW] rather than
   the structural [SAFE] — making explicit that it touches no value structure.

   This is the per-arch extern bridge: it only produces [eval_extern_rel]/
   [eval_extern_func] (consumed by [ARCH]). Builtins are arch-independent, so
   [Run.EXTERN.call_builtin] lives in [make.ml]'s [MakeExtern], not here — hence
   this is no longer sealed to the full [Run.EXTERN]. *)

module Make (R : Valrep.RAW) (A : IMPL with type vt = R.t) = struct
  let init_mode _ = ()
  let checkpoint () : int = 0
  let seff (before : int) (after : int) : bool = before <> after
  let clear () = ()

  module Cache = struct
    let cache_on () = ()
    let cache_off () = ()
  end

  let eval_extern_rel (name : string) (values_input : Value.t list) :
      Run.rel_result =
    let values_input = List.map R.of_value values_input in
    try
      Run.Pass
        (List.map R.to_value
           (match name with
           | "ExternFunctionCall_eval_lctk" ->
               A.eval_extern_func_lctk_call values_input
           | "ExternFunctionCall_eval" -> A.eval_extern_func_call values_input
           | "ExternMethodCall_eval" -> A.eval_extern_method_call values_input
           | _ ->
               error no_region
                 (Format.asprintf "unimplemented extern relation: %s" name)))
    with Util.Error.ExternError (at, msg) -> Run.Fail (at, msg)

  let eval_extern_func (name : string) (_typs : Typ.t list)
      (values_input : Value.t list) : Run.func_result =
    let values_input = List.map R.of_value values_input in
    try
      Run.Pass
        (R.to_value
           (match name with
           | "init_objectState" -> A.eval_extern_init values_input
           | "init_archState" -> A.init_arch_state
           | _ ->
               error no_region
                 (Format.asprintf "unimplemented extern function: %s" name)))
    with Util.Error.ExternError (at, msg) -> Run.Fail (at, msg)
end
