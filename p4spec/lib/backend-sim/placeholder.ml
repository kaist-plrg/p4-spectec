module Typ = Runtime.Type.Typ
module Value = Runtime.Value
module IO = Runtime.Sim.Io
module Sim = Runtime.Sim.Simulator
open Error
open Util.Source

module Make (Interp_AL : Sim.INTERP_AL) (Interp_SL : Sim.INTERP_SL) : Sim.ARCH =
struct
  let transform_stf_stmt = Fun.id

  (* Extern calls *)

  type arch_state = unit [@@deriving yojson]

  let init_arch_state =
    () |> arch_state_to_yojson
    |> Value.Make.extern (Typ.Make.var ("archState" $ no_region) [])

  let eval_extern_init (_values_input : Value.t list) : Value.t =
    Value.Make.extern (Typ.Make.var ("objectState" $ no_region) []) `Null

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
    let name_func = Value.Get.text value_name_func in
    let names_param =
      value_names_param |> Value.Get.list |> List.map Value.Get.text
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
end
