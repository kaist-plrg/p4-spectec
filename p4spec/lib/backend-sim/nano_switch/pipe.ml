module Typ = Runtime.Type.Typ
module Value = Runtime.Value
module IO = Runtime.Sim.Io
module Sim = Runtime.Sim.Signature
open Spec.Unpack
open Util.Source
open Error

module Make (Spec : Spec.S) : Sim.ARCH = struct
  (* Core externs *)

  module Core = struct
    module Func = Core.Func.Make (Spec.Func)
    module Object = Core.Object.Make (Spec.Func) (Spec.Rel)
  end

  (* STF transformation *)

  let transform_stf_stmt = Fun.id

  (* Extern objects *)

  type arch_state = unit [@@deriving yojson]

  let init_arch_state =
    () |> arch_state_to_yojson
    |> Value.Make.extern (Typ.Make.var ("archState" $ no_region) [])

  type extern = PacketIn of Core.Object.PacketIn.t [@@deriving yojson]

  let get_extern (value_arch : Value.t) (value_oid : Value.t) : extern =
    Spec.Func.find_objectState_e value_arch value_oid
    |> Value.Get.extern |> extern_of_yojson |> Result.get_ok

  (* Extern calls *)

  let eval_extern_init (values_input : Value.t list) : Value.t =
    let _value_name_extern, _value_type_args, _value_args =
      match values_input with
      | [ value_name; value_type_args; value_args ] ->
          (value_name, value_type_args, value_args)
      | _ -> error_no_region "unexpected number of arguments to extern init"
    in
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

  let eval_extern_func_call (values_input : Value.t list) : Value.t list =
    let value_ctx, value_arch, value_name_func, value_names_param =
      match values_input with
      | [ value_ctx; value_arch; value_name_func; value_names_param ] ->
          (value_ctx, value_arch, value_name_func, value_names_param)
      | _ ->
          error_no_region
            "unexpected number of arguments to extern function call"
    in
    let name_func = Value.Get.text value_name_func in
    let names_param =
      value_names_param |> Value.Get.list |> List.map Value.Get.text
    in
    let value_ctx, value_arch, value_callResult =
      match (name_func, names_param) with
      | "verify", [ "check"; "toSignal" ] ->
          Core.Func.verify value_ctx value_arch
      | _ ->
          error_no_region
            ("unsupported extern function call: " ^ name_func ^ "("
            ^ String.concat ", " names_param
            ^ ")")
    in
    [ value_ctx; value_arch; value_callResult ]

  let eval_extern_method_call (values_input : Value.t list) : Value.t list =
    let value_ctx, value_extern, value_name_method, value_names_param =
      match values_input with
      | [ value_ctx; value_extern; value_name_method; value_names_param ] ->
          (value_ctx, value_extern, value_name_method, value_names_param)
      | _ ->
          error_no_region "unexpected number of arguments to extern method call"
    in
    let extern =
      value_extern |> Value.Get.extern |> extern_of_yojson |> Result.get_ok
    in
    let name_method = Value.Get.text value_name_method in
    let names_param =
      value_names_param |> Value.Get.list |> List.map Value.Get.text
    in
    let extern, value_ctx =
      match (extern, name_method, names_param) with
      | PacketIn pkt, "extract", [ "hdr" ] ->
          let nano_switch_header_size = 24 in
          if pkt.idx + nano_switch_header_size > pkt.len then
            (PacketIn pkt, value_ctx)
          else
            let pkt, bits =
              Core.Object.PacketIn.parse pkt nano_switch_header_size
            in
            (* Use $find_var_e(LOCAL, EC, nameIR) and $update_var_e(LOCAL, EC, nameIR, value)
               from the nano spec directly via the Spec.Func trampoline.
               These have nano-specific argument shapes, distinct from the main spec wrappers. *)
            let value_scope_local = Value.Make.("LOCAL" <| [] <<| "scope") in
            let find_var_local (ec : Value.t) (name : string) : Value.t =
              !Spec.Func.call "find_var_e" []
                [ value_scope_local; ec; Value.Make.text name ]
            in
            let update_var_local (ec : Value.t) (name : string) (v : Value.t) :
                Value.t =
              !Spec.Func.call "update_var_e" []
                [ value_scope_local; ec; Value.Make.text name; v ]
            in
            let value_hdr = find_var_local value_ctx "hdr" in
            let value_bits =
              let typ = Typ.Make.var ("bit" $ no_region) [] |> Typ.Make.list in
              bits |> Array.to_list |> List.map Value.Make.bool
              |> Value.Make.list typ
            in
            let value_hdr' =
              !Spec.Func.call "write_value_from_bits" []
                [ value_hdr; value_bits ]
            in
            let value_ctx = update_var_local value_ctx "hdr" value_hdr' in
            (PacketIn pkt, value_ctx)
      | _ -> error_no_region ("Unsupported extern method call: " ^ name_method)
    in
    let value_extern =
      extern |> extern_to_yojson
      |> Value.Make.extern (Typ.Make.var ("objectState" $ no_region) [])
    in
    [ value_extern; value_ctx ]

  (* Mirror session interface *)

  let add_mirror_session _session _port =
    error_no_region
      "add_mirror_session is not implemented for the nano-switch simulator"

  let add_mirror_session_mc _session _multicast_group =
    error_no_region
      "add_mirror_session_mc is not implemented for the nano-switch simulator"

  let mc_mgrp_create (_value_arch : Value.t) (_mgid : int) : Value.t =
    error_no_region
      "mc_mgrp_create is not implemented for the nano-switch simulator"

  let mc_node_create (_value_arch : Value.t) (_rid : int) (_port : int list) :
      Value.t =
    error_no_region
      "mc_node_create is not implemented for the nano-switch simulator"

  let mc_node_associate (_value_arch : Value.t) (_mgid : int) (_handle : int) :
      Value.t =
    error_no_region
      "mc_node_associate is not implemented for the nano-switch simulator"

  (* Register interface *)

  let register_read (_value_arch : Value.t) (_reg_name : string) (_index : int)
      : Value.t =
    error_no_region
      "register_read is not implemented for the nano-switch simulator"

  let register_write (_value_arch : Value.t) (_reg_name : string) (_index : int)
      (_value : int) : Value.t =
    error_no_region
      "register_write is not implemented for the nano-switch simulator"

  let register_reset (_value_arch : Value.t) (_reg_name : string) : Value.t =
    error_no_region
      "register_reset is not implemented for the nano-switch simulator"

  (* Pipeline initializer *)

  let init_pipe (includes_p4 : string list) (filename_p4 : string) :
      Value.t * Value.t =
    Spec.Pgm.nanoswitch_init includes_p4 filename_p4

  (* Pipeline driver *)

  let drive_pipe (value_ctx : Value.t) (value_arch : Value.t) (rx : IO.rx) :
      Value.t * Value.t * IO.tx list =
    let port_in, packet_bytes = rx in
    let packet_in = PacketIn (Core.Object.PacketIn.init packet_bytes) in
    let value_packet_in_state =
      extern_to_yojson packet_in
      |> Value.Make.extern (Typ.Make.var ("objectState" $ no_region) [])
    in
    let value_ctx, value_forwarding_decision =
      Spec.Rel.nanoswitch_drive value_ctx value_packet_in_state
    in
    let forward =
      Value.Get.(value_forwarding_decision |>>? "FORWARD" |> Option.is_some)
    in
    if forward then (value_ctx, value_arch, [ (port_in, packet_bytes) ])
    else (value_ctx, value_arch, [])

  include Extern.Make (struct
    let eval_extern_init = eval_extern_init
    let eval_extern_func_lctk_call = eval_extern_func_lctk_call
    let eval_extern_func_call = eval_extern_func_call
    let eval_extern_method_call = eval_extern_method_call
    let init_arch_state = init_arch_state
  end)
end
