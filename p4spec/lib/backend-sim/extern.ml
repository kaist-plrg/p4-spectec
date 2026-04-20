module Sim = Runtime.Sim.Simulator
module Value = Runtime.Value
module Typ = Runtime.Type.Typ
open Error
open Util.Source

module type IMPL = sig
  val eval_extern_init : Value.t list -> Value.t
  val eval_extern_func_lctk_call : Value.t list -> Value.t list
  val eval_extern_func_call : Value.t list -> Value.t list
  val eval_extern_method_call : Value.t list -> Value.t list
  val init_arch_state : Value.t
end

module Make (A : IMPL) : Sim.EXTERN = struct
  let eval_extern_rel (name : string) (values_input : Value.t list) :
      Sim.rel_result =
    try
      Sim.Pass
        (match name with
        | "ExternFunctionCall_eval_lctk" ->
            A.eval_extern_func_lctk_call values_input
        | "ExternFunctionCall_eval" -> A.eval_extern_func_call values_input
        | "ExternMethodCall_eval" -> A.eval_extern_method_call values_input
        | _ ->
            error no_region
              (Format.asprintf "unimplemented extern relation: %s" name))
    with Util.Error.ArchError (at, msg) -> Sim.Fail (at, msg)

  let eval_extern_func (name : string) (_typs : Typ.t list)
      (values_input : Value.t list) : Sim.func_result =
    try
      Sim.Pass
        (match name with
        | "init_objectState" -> A.eval_extern_init values_input
        | "init_archState" -> A.init_arch_state
        | _ ->
            error no_region
              (Format.asprintf "unimplemented extern function: %s" name))
    with Util.Error.ArchError (at, msg) -> Sim.Fail (at, msg)
end
