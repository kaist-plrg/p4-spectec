module Typ = Runtime.Type.Typ
module Value = Runtime.Value
module Run = Runtime.Dynamic_Runner.Signature
open Error
open Util.Source

(* The bottom layer *)

module Make_zero (Interp_IL : Run.INTERP_IL) (Interp_SL : Run.INTERP_SL) :
  Run.EXTERN = struct
  (* Mode initialization *)

  let mode : Run.mode ref = ref Run.Empty_mode
  let init_mode mode_ = mode := mode_

  (* Calling SpecTec spec's built-in functions *)

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
      match !mode with
      | Run.IL_mode -> (
          match Interp_IL.eval_func id.it typs values with
          | Run.Pass v -> v
          | Run.Fail (at, msg) -> error at msg)
      | Run.SL_mode -> (
          match Interp_SL.eval_func id.it typs values with
          | Run.Pass v -> v
          | Run.Fail (at, msg) -> error at msg)
      | Run.Empty_mode -> assert false
    in
    let value_value_output = Interface.SpecTec.boot_value value_output in
    let value_value_output_res =
      Value.Make.("OK val" <| [ value_value_output ] <<| "valres")
    in
    [ value_value_output_res ]

  let eval_extern_rel (name : string) (values_input : Value.t list) :
      Run.rel_result =
    try
      Run.Pass
        (match name with
        | "Call_builtin_func" -> call_builtin_func values_input
        | _ ->
            error no_region
              (Format.asprintf "unimplemented extern relation: %s" name))
    with Util.Error.ExternError (at, msg) -> Run.Fail (at, msg)

  let eval_extern_func (name : string) (_typs : Typ.t list)
      (_values_input : Value.t list) : Run.func_result =
    try
      error no_region (Format.asprintf "unimplemented extern function: %s" name)
    with Util.Error.ExternError (at, msg) -> Run.Fail (at, msg)
end

(* The intermediate layer *)

module Make_interm
    (Runner_SpecTec : Run.RUNNER)
    (_ : Run.INTERP_IL)
    (_ : Run.INTERP_SL) : Run.EXTERN = struct
  (* Mode initialization *)

  let init_mode _ = ()

  (* Calling SpecTec's built-in functions *)

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
      match Runner_SpecTec.run_func id.it typs values with
      | Pass value_output -> value_output
      | Fail (at, msg) -> error at msg
    in
    let value_value_output = Interface.SpecTec.boot_value value_output in
    let value_value_output_res =
      Value.Make.("OK val" <| [ value_value_output ] <<| "valres")
    in
    [ value_value_output_res ]

  let eval_extern_rel (name : string) (values_input : Value.t list) :
      Run.rel_result =
    try
      Run.Pass
        (match name with
        | "Call_builtin_func" -> call_builtin_func values_input
        | _ ->
            error no_region
              (Format.asprintf "unimplemented extern relation: %s" name))
    with Util.Error.ExternError (at, msg) -> Run.Fail (at, msg)

  let eval_extern_func (name : string) (_typs : Typ.t list)
      (_values_input : Value.t list) : Run.func_result =
    try
      error no_region (Format.asprintf "unimplemented extern function: %s" name)
    with Util.Error.ExternError (at, msg) -> Run.Fail (at, msg)
end

(* The top layer *)

module Make_top
    (Runner_P4 : Run.RUNNER)
    (_ : Run.INTERP_IL)
    (_ : Run.INTERP_SL) : Run.EXTERN = struct
  (* Mode initialization *)

  let init_mode _ = ()

  (* Calling P4 spec's built-in functions *)

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
      match Runner_P4.run_func id.it typs values with
      | Pass value_output -> value_output
      | Fail (at, msg) -> error at msg
    in
    let value_value_output = Interface.SpecTec.boot_value value_output in
    let value_value_output_res =
      Value.Make.("OK val" <| [ value_value_output ] <<| "valres")
    in
    [ value_value_output_res ]

  let eval_extern_rel (name : string) (values_input : Value.t list) :
      Run.rel_result =
    try
      Run.Pass
        (match name with
        | "Call_builtin_func" -> call_builtin_func values_input
        | _ ->
            error no_region
              (Format.asprintf "unimplemented extern relation: %s" name))
    with Util.Error.ExternError (at, msg) -> Run.Fail (at, msg)

  let eval_extern_func (name : string) (_typs : Typ.t list)
      (_values_input : Value.t list) : Run.func_result =
    try
      error no_region (Format.asprintf "unimplemented extern function: %s" name)
    with Util.Error.ExternError (at, msg) -> Run.Fail (at, msg)
end
