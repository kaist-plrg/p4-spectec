module Typ = Runtime.Type.Typ
module Value = Runtime.Value
module Run = Runtime.Dynamic_Runner.Signature
open Error
open Util.Source

(* The bottom layer *)

module Make_null (_ : Run.INTERP_IL) (_ : Run.INTERP_SL) : Run.EXTERN = struct
  (* Mode initialization *)

  let init_mode _ = ()

  (* Externs *)

  let eval_extern_rel (name : string) (_values_input : Value.t list) :
      Run.rel_result =
    try
      error no_region (Format.asprintf "unimplemented extern relation: %s" name)
    with Util.Error.ExternError (at, msg) -> Run.Fail (at, msg)

  let eval_extern_func (name : string) (_typs : Typ.t list)
      (_values_input : Value.t list) : Run.func_result =
    try
      error no_region (Format.asprintf "unimplemented extern function: %s" name)
    with Util.Error.ExternError (at, msg) -> Run.Fail (at, msg)
end

(* The intermediate layer *)

module Make_parametric
    (Runner : Run.RUNNER)
    (_ : Run.INTERP_IL)
    (_ : Run.INTERP_SL) : Run.EXTERN = struct
  (* Mode initialization *)

  let init_mode _ = ()

  (* Externs - threading extern calls to the runner *)

  let call_builtin_func (values_input : Value.t list) : Value.t list =
    let _value_ctx, value_id, _value_builtinFuncDef, value_typs, value_values =
      match values_input with
      | [ value_ctx; value_id; value_builtinFuncDef; value_typs; value_values ]
        ->
          (value_ctx, value_id, value_builtinFuncDef, value_typs, value_values)
      | _ ->
          error_no_region "unexpected number of arguments to call_builtin_func"
    in
    let id = value_id |> Interface.SpecTec.unboot_id in
    let typs = value_typs |> Interface.SpecTec.unboot_typs in
    let values = value_values |> Interface.SpecTec.unboot_values in
    let value_output =
      match Runner.run_func id.it typs values with
      | Pass value_output -> value_output
      | Fail (at, msg) -> error at msg
    in
    let value_value_output = Interface.SpecTec.boot_value value_output in
    let value_value_output_res =
      Value.Make.("OK val" <| [ value_value_output ] <<| "valres")
    in
    [ value_value_output_res ]

  let call_extern_rel (values_input : Value.t list) : Value.t list =
    let _value_ctx, value_id, value_values =
      match values_input with
      | [ value_ctx; value_id; value_values ] ->
          (value_ctx, value_id, value_values)
      | _ -> error_no_region "unexpected number of arguments to call_extern_rel"
    in
    let id = value_id |> Interface.SpecTec.unboot_id in
    let values = value_values |> Interface.SpecTec.unboot_values in
    let values_output =
      match Runner.run_rel id.it values with
      | Pass values_output -> values_output
      | Fail (at, msg) -> error at msg
    in
    let value_values_output = Interface.SpecTec.boot_values values_output in
    let value_values_output_res =
      Value.Make.("OK val*" <| [ value_values_output ] <<| "valsres")
    in
    [ value_values_output_res ]

  let eval_extern_rel (name : string) (values_input : Value.t list) :
      Run.rel_result =
    try
      Run.Pass
        (match name with
        | "Call_builtin_func" -> call_builtin_func values_input
        | "Call_extern_rel" -> call_extern_rel values_input
        | _ ->
            failwith (Format.asprintf "unimplemented extern relation: %s" name))
    with Util.Error.ExternError (at, msg) -> Run.Fail (at, msg)

  let eval_extern_func (name : string) (_typs : Typ.t list)
      (_values_input : Value.t list) : Run.func_result =
    try
      error no_region (Format.asprintf "unimplemented extern function: %s" name)
    with Util.Error.ExternError (at, msg) -> Run.Fail (at, msg)
end
