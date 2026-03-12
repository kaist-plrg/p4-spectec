open Lang
open Interface.Wrap
open Interface.Unwrap
module Value = Runtime.Sim.Value
module IO = Runtime.Sim.Io
module Sim = Runtime.Sim.Simulator
open Error

module Make (Interp_IL : Sim.INTERP_IL) (Interp_SL : Sim.INTERP_SL) : Sim.ARCH =
struct
  let transform_stf_stmt = Fun.id

  (* Mode *)

  let mode : Sim.mode ref = ref (Sim.Empty_mode : Sim.mode)
  let init_mode (mode_ : Sim.mode) : unit = mode := mode_

  (* Call entry points *)

  let call_rel (relname : string) (values_input : Value.t list) : Value.t list =
    match !mode with
    | IL_mode -> (
        let rel_result_il = Interp_IL.eval_rel relname values_input in
        match rel_result_il with
        | Pass values_output -> values_output
        | Fail (at, msg) -> error at msg)
    | SL_mode -> (
        let rel_result_sl = Interp_SL.eval_rel relname values_input in
        match rel_result_sl with
        | Pass values_output -> values_output
        | Fail (at, msg) -> error at msg)
    | Empty_mode -> assert false

  let init_call_rel () = Spec.Rel.register call_rel

  let call_func (funcname : string) (typs_input : Sl.typ list)
      (values_input : Value.t list) : Value.t =
    match !mode with
    | IL_mode -> (
        let func_result_il =
          Interp_IL.eval_func funcname typs_input values_input
        in
        match func_result_il with
        | Pass value_output -> value_output
        | Fail (at, msg) -> error at msg)
    | SL_mode -> (
        let func_result_sl =
          Interp_SL.eval_func funcname typs_input values_input
        in
        match func_result_sl with
        | Pass value_output -> value_output
        | Fail (at, msg) -> error at msg)
    | Empty_mode -> assert false

  let init_call_func () = Spec.Func.register call_func

  (* Extern calls *)

  type arch_state = unit [@@deriving yojson]

  let init_arch_state = () |> arch_state_to_yojson |> wrap_extern_v "archState"

  let eval_extern_init (_values_input : Value.t list) : Value.t =
    wrap_extern_v "objectState" `Null

  let eval_extern_func_lctk_call (values_input : Value.t list) : Value.t list =
    let value_ctx, value_name_func, value_names_param =
      match values_input with
      | [ value_ctx; value_name_func; value_names_param ] ->
          (value_ctx, value_name_func, value_names_param)
      | _ ->
          error_no_region
            "unexpected number of arguments to local compile-time known extern \
             function call"
    in
    let name_func = unwrap_text_v value_name_func in
    let names_param =
      value_names_param |> unwrap_list_v |> List.map unwrap_text_v
    in
    match (name_func, names_param) with
    | "static_assert", [ "check"; "message" ] ->
        [ Core.Func.static_assert ~message:true value_ctx ]
    | "static_assert", [ "check" ] ->
        [ Core.Func.static_assert ~message:false value_ctx ]
    | _ ->
        error_no_region
          ("unsupported local compile-time known extern function call: "
         ^ name_func ^ "("
          ^ String.concat ", " names_param
          ^ ")")

  let eval_extern_func_call (_values_input : Value.t list) : Value.t list =
    error_no_region
      "eval_extern_func_call not implemented for the placeholder simulator"

  let eval_extern_method_call (_values_input : Value.t list) : Value.t list =
    error_no_region
      "eval_extern_method_call not implemented for the placeholder simulator"

  (* Match-action table interface *)

  let table_add_entry (_value_ctx : Value.t) (_value_arch : Value.t)
      (_value_tableName : Value.t)
      (_value_tableEntryPriorityInterface : Value.t)
      (_value_tableKeysetInterface : Value.t)
      (_value_tableActionInterface : Value.t) : Value.t =
    error_no_region
      "table_add_entry not implemented for the placeholder simulator"

  let table_add_default_action (_value_ctx : Value.t) (_value_arch : Value.t)
      (_value_tableName : Value.t) (_value_tableActionInterface : Value.t) :
      Value.t =
    error_no_region
      "table_add_default_action not implemented for the placeholder simulator"

  (* Mirror session interface *)

  let add_mirror_session _session _port =
    error_no_region
      "add_mirror_session is not implemented for the placeholder simulator"

  let add_mirror_session_mc _session _multicast_group =
    error_no_region
      "add_mirror_session_mc is not implemented for the placeholder simulator"

  (* Multicast interface *)

  let mc_mgrp_create (_value_arch : Value.t) (_mgid : int) : Value.t =
    error_no_region
      "mc_mgrp_create is not implemented for the placeholder simulator"

  let mc_node_create (_value_arch : Value.t) (_rid : int) (_ports : int list) :
      Value.t =
    error_no_region
      "mc_node_create is not implemented for the placeholder simulator"

  let mc_node_associate (_value_arch : Value.t) (_mgid : int) (_handle : int) :
      Value.t =
    error_no_region
      "mc_node_associate is not implemented for the placeholder simulator"

  (* Register interface *)

  let register_read (_value_arch : Value.t) (_reg_name : string) (_index : int)
      : Value.t =
    error_no_region
      "register_read is not implemented for the placeholder simulator"

  let register_write (_value_arch : Value.t) (_reg_name : string) (_index : int)
      (_value : int) : Value.t =
    error_no_region
      "register_write is not implemented for the placeholder simulator"

  let register_reset (_value_arch : Value.t) (_reg_name : string) : Value.t =
    error_no_region
      "register_reset is not implemented for the placeholder simulator"

  (* Pipeline initializer *)

  let init_pipe (_includes_p4 : string list) (_filename_p4 : string) :
      Value.t * Value.t =
    error_no_region "init_pipe not implemented for the placeholder simulator"

  (* Pipeline driver *)

  let drive_pipe (_value_ctx : Value.t) (_value_arch : Value.t) (_rx : IO.rx) :
      Value.t * Value.t * IO.tx list =
    error_no_region "drive_pipe not implemented for the placeholder simulator"

  (* Initializer *)

  let init (mode_ : Sim.mode) : unit =
    init_mode mode_;
    init_call_rel ();
    init_call_func ()
end
