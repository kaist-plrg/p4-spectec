open Lang
open Interface.Wrap
open Interface.Unwrap
open Interface.Pack
open Interface.Unpack
open Interface.Flatten
module Value = Runtime.Sim.Value
module IO = Runtime.Sim.Io
module Sim = Runtime.Sim.Simulator
open State
open Error

module Make (Interp_IL : Sim.INTERP_IL) (Interp_SL : Sim.INTERP_SL) : Sim.ARCH =
struct
  let transform_stf_stmt (stmt : Stf.Ast.stmt) : Stf.Ast.stmt =
    let transform_name name =
      Stf.Transform.Name.(
        name
        |> rewrite_substring ~substrings:[ "ingress"; "preqos" ]
             ~replacement:"main.ig"
        |> rewrite_substring
             ~substrings:[ "egress"; "postqos"; "c3" ]
             ~replacement:"main.eg")
    in
    let transform_matches = List.map Stf.Transform.Match.rewrite_valid in
    let transform_action (name, args) = (transform_name name, args) in
    match stmt with
    | Add (name, priority_opt, mtches, action, id_opt) ->
        let name = transform_name name in
        let mtches = transform_matches mtches in
        let action = transform_action action in
        Add (name, priority_opt, mtches, action, id_opt)
    | SetDefault (name, action) ->
        let name = transform_name name in
        let action = transform_action action in
        SetDefault (name, action)
    | _ -> stmt

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

  (* Architectural state *)

  let init_arch_state = Arch.empty |> Arch.to_value

  let get_arch_state : Arch.t state =
    let+ _, value_arch, _ = get in
    value_arch |> Spec.Func.find_archState_e |> Arch.of_value

  let put_arch_state (arch_state : Arch.t) : unit state =
    modify (fun (value_ctx, value_arch, txs) ->
        let value_arch =
          arch_state |> Arch.to_value |> Spec.Func.update_archState_e value_arch
        in
        (value_ctx, value_arch, txs))

  (* Extern objects *)

  type object_state =
    | PacketIn of Core.Object.PacketIn.t
    | PacketOut of Core.Object.PacketOut.t
    | Counter of Object.Counter.t
    | Register of Object.Register.t
    | DirectCounter of Object.DirectCounter.t
    | DirectMeter of Object.DirectMeter.t
  [@@deriving yojson]

  let get_object_state (value_arch : Value.t) (value_objectId : Value.t) :
      object_state =
    Spec.Func.find_objectState_e value_arch value_objectId
    |> unwrap_extern_v |> object_state_of_yojson |> Result.get_ok

  let get_packet_in (value_arch : Value.t) : Core.Object.PacketIn.t =
    let value_objectId = wrap_list_v "id" [ wrap_text_v "packet_in" ] in
    match get_object_state value_arch value_objectId with
    | PacketIn packet_in -> packet_in
    | _ -> error_no_region "packet_in object not found"

  let get_packet_out (value_arch : Value.t) : Core.Object.PacketOut.t =
    let value_objectId = wrap_list_v "id" [ wrap_text_v "packet_out" ] in
    match get_object_state value_arch value_objectId with
    | PacketOut packet_out -> packet_out
    | _ -> error_no_region "packet_out object not found"

  (* Extern calls *)

  let eval_extern_init (values_input : Value.t list) : Value.t =
    let value_name_extern, value_type_args, value_args =
      match values_input with
      | [ value_name; value_type_args; value_args ] ->
          (value_name, value_type_args, value_args)
      | _ -> error_no_region "unexpected number of arguments to extern init"
    in
    let name_extern = unwrap_text_v value_name_extern in
    match name_extern with
    | "counter" ->
        let counter = Object.Counter.init value_type_args value_args in
        let counter = Counter counter in
        counter |> object_state_to_yojson |> wrap_extern_v "objectState"
    | "register" ->
        let register = Object.Register.init value_type_args value_args in
        let register = Register register in
        register |> object_state_to_yojson |> wrap_extern_v "objectState"
    | "direct_counter" ->
        let direct_counter =
          Object.DirectCounter.init value_type_args value_args
        in
        let direct_counter = DirectCounter direct_counter in
        direct_counter |> object_state_to_yojson |> wrap_extern_v "objectState"
    | "direct_meter" ->
        let direct_meter = Object.DirectMeter.init value_type_args value_args in
        let direct_meter = DirectMeter direct_meter in
        direct_meter |> object_state_to_yojson |> wrap_extern_v "objectState"
    | _ -> wrap_extern_v "objectState" `Null

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

  let eval_extern_func_call (values_input : Value.t list) : Value.t list =
    let value_ctx, value_arch, value_name_func, value_names_param =
      match values_input with
      | [ value_ctx; value_arch; value_name_func; value_names_param ] ->
          (value_ctx, value_arch, value_name_func, value_names_param)
      | _ ->
          error_no_region
            "unexpected number of arguments to extern function call"
    in
    let name_func = unwrap_text_v value_name_func in
    let names_param =
      value_names_param |> unwrap_list_v |> List.map unwrap_text_v
    in
    let value_ctx, value_arch, value_callResult =
      match (name_func, names_param) with
      | "verify", [ "check"; "toSignal" ] ->
          Core.Func.verify value_ctx value_arch
      | "digest", [ "receiver"; "data" ] -> Func.digest value_ctx value_arch
      | "mark_to_drop", [ "standard_metadata" ] ->
          Func.mark_to_drop value_ctx value_arch
      | "verify_checksum", [ "condition"; "data"; "checksum"; "algo" ] ->
          Func.verify_checksum value_ctx value_arch
      | ( "verify_checksum_with_payload",
          [ "condition"; "data"; "checksum"; "algo" ] ) ->
          let packet_in = get_packet_in value_arch in
          Func.verify_checksum_with_payload value_ctx value_arch packet_in
      | "update_checksum", [ "condition"; "data"; "checksum"; "algo" ] ->
          Func.update_checksum value_ctx value_arch
      | ( "update_checksum_with_payload",
          [ "condition"; "data"; "checksum"; "algo" ] ) ->
          let packet_in = get_packet_in value_arch in
          Func.update_checksum_with_payload value_ctx value_arch packet_in
      | "clone_preserving_field_list", [ "type"; "session"; "index" ] ->
          Func.clone_preserving_field_list value_ctx value_arch
          (* TODO: when to resolve port id? *)
      | "resubmit_preserving_field_list", [ "index" ] ->
          Func.resubmit_preserving_field_list value_ctx value_arch
      | "recirculate_preserving_field_list", [ "index" ] ->
          Func.recirculate_preserving_field_list value_ctx value_arch
      | "hash", [ "result"; "algo"; "base"; "data"; "max" ] ->
          Func.hash value_ctx value_arch
      | "log_msg", [ "msg" ] -> Func.log_msg value_ctx value_arch
      | "log_msg", [ "msg"; "data" ] -> Func.log_msg_format value_ctx value_arch
      | _ ->
          error_no_region
            ("unsupported extern function call: " ^ name_func ^ "("
            ^ String.concat ", " names_param
            ^ ")")
    in
    [ value_ctx; value_arch; value_callResult ]

  let eval_extern_method_call (values_input : Value.t list) : Value.t list =
    let ( value_ctx,
          value_arch,
          value_objectId,
          value_name_method,
          value_names_param ) =
      match values_input with
      | [
       value_ctx;
       value_arch;
       value_objectId;
       value_name_method;
       value_names_param;
      ] ->
          ( value_ctx,
            value_arch,
            value_objectId,
            value_name_method,
            value_names_param )
      | _ ->
          error_no_region "unexpected number of arguments to extern method call"
    in
    let obj = get_object_state value_arch value_objectId in
    let name_method = unwrap_text_v value_name_method in
    let names_param =
      value_names_param |> unwrap_list_v |> List.map unwrap_text_v
    in
    let obj, value_ctx, value_arch, value_callResult =
      match (obj, name_method, names_param) with
      | PacketIn packet_in, "extract", [ "hdr" ] ->
          let packet_in, value_ctx, value_arch, value_callResult =
            Core.Object.PacketIn.extract value_ctx value_arch packet_in
          in
          let packet_in = PacketIn packet_in in
          (packet_in, value_ctx, value_arch, value_callResult)
      | ( PacketIn packet_in,
          "extract",
          [ "variableSizeHeader"; "variableFieldSizeInBits" ] ) ->
          let packet_in, value_ctx, value_arch, value_callResult =
            Core.Object.PacketIn.extract_varsize value_ctx value_arch packet_in
          in
          let packet_in = PacketIn packet_in in
          (packet_in, value_ctx, value_arch, value_callResult)
      | PacketIn packet_in, "lookahead", [] ->
          let packet_in, value_ctx, value_arch, value_callResult =
            Core.Object.PacketIn.lookahead value_ctx value_arch packet_in
          in
          let packet_in = PacketIn packet_in in
          (packet_in, value_ctx, value_arch, value_callResult)
      | PacketIn packet_in, "advance", [ "sizeInBits" ] ->
          let packet_in, value_ctx, value_arch, value_callResult =
            Core.Object.PacketIn.advance value_ctx value_arch packet_in
          in
          let packet_in = PacketIn packet_in in
          (packet_in, value_ctx, value_arch, value_callResult)
      | PacketIn packet_in, "length", [] ->
          let packet_in, value_ctx, value_arch, value_callResult =
            Core.Object.PacketIn.length value_ctx value_arch packet_in
          in
          let packet_in = PacketIn packet_in in
          (packet_in, value_ctx, value_arch, value_callResult)
      | PacketOut packet_out, "emit", [ "hdr" ] ->
          let packet_out, value_ctx, value_arch, value_callResult =
            Core.Object.PacketOut.emit value_ctx value_arch packet_out
          in
          let packet_out = PacketOut packet_out in
          (packet_out, value_ctx, value_arch, value_callResult)
      | Counter counter, "count", [ "index" ] ->
          let packet_in = get_packet_in value_arch in
          let counter, value_ctx, value_arch, value_callResult =
            Object.Counter.count value_ctx value_arch packet_in counter
          in
          let counter = Counter counter in
          (counter, value_ctx, value_arch, value_callResult)
      | Register register, "read", [ "result"; "index" ] ->
          let register, value_ctx, value_arch, value_callResult =
            Object.Register.read value_ctx value_arch register
          in
          let register = Register register in
          (register, value_ctx, value_arch, value_callResult)
      | Register register, "write", [ "index"; "value" ] ->
          let register, value_ctx, value_arch, value_callResult =
            Object.Register.write value_ctx value_arch register
          in
          let register = Register register in
          (register, value_ctx, value_arch, value_callResult)
      | DirectCounter direct_counter, "count", [] ->
          let packet_in = get_packet_in value_arch in
          let direct_counter, value_ctx, value_arch, value_callResult =
            Object.DirectCounter.count value_ctx value_arch packet_in
              direct_counter
          in
          let direct_counter = DirectCounter direct_counter in
          (direct_counter, value_ctx, value_arch, value_callResult)
      | DirectMeter direct_meter, "read", [ "result" ] ->
          let packet_in = get_packet_in value_arch in
          let direct_meter, value_ctx, value_arch, value_callResult =
            Object.DirectMeter.read value_ctx value_arch packet_in direct_meter
          in
          let direct_meter = DirectMeter direct_meter in
          (direct_meter, value_ctx, value_arch, value_callResult)
      | _ ->
          let oid =
            value_objectId |> unwrap_list_v |> List.map unwrap_text_v
            |> String.concat "."
          in
          error_no_region
            ("unsupported extern method call: " ^ oid ^ "." ^ name_method ^ "("
            ^ String.concat ", " names_param
            ^ ")")
    in
    let value_obj =
      obj |> object_state_to_yojson |> wrap_extern_v "objectState"
    in
    let value_arch =
      Spec.Func.update_objectState_e value_arch value_objectId value_obj
    in
    [ value_ctx; value_arch; value_callResult ]

  (* Match-action table interface *)

  let find_table (value_arch : Value.t) (value_tableName : Value.t) : Value.t =
    let find_table_unqualified table_name_unqualified =
      let value_tableName_unqualified = wrap_text_v table_name_unqualified in
      Spec.Func.find_object_unqualified_e value_arch value_tableName_unqualified
      |> Option.get
    in
    let table_name = unwrap_text_v value_tableName in
    match String.split_on_char '.' table_name with
    | [] -> assert false
    | [ table_name_unqualified ] ->
        find_table_unqualified table_name_unqualified
    | names -> (
        let values_name = List.map wrap_text_v names in
        let value_objectId = wrap_list_v "nameIR" values_name in
        match Spec.Func.find_object_qualified_e value_arch value_objectId with
        | Some value_table -> value_table
        | None ->
            let table_name_unqualified = names |> List.rev |> List.hd in
            find_table_unqualified table_name_unqualified)

  let update_table (value_arch : Value.t) (value_tableName : Value.t)
      (value_tableObject : Value.t) : Value.t =
    let update_table_unqualified table_name_unqualified =
      let value_tableName_unqualified = wrap_text_v table_name_unqualified in
      Spec.Func.update_object_unqualified_e value_arch
        value_tableName_unqualified value_tableObject
    in
    let table_name = unwrap_text_v value_tableName in
    match String.split_on_char '.' table_name with
    | [] -> assert false
    | [ table_name_unqualified ] ->
        update_table_unqualified table_name_unqualified
    | names ->
        let values_name = List.map wrap_text_v names in
        let value_objectId = wrap_list_v "nameIR" values_name in
        if
          Spec.Func.find_object_qualified_e value_arch value_objectId
          |> Option.is_some
        then
          Spec.Func.update_object_qualified_e value_arch value_objectId
            value_tableObject
        else
          let table_name_unqualified = names |> List.rev |> List.hd in
          update_table_unqualified table_name_unqualified

  let table_add_entry (value_ctx : Value.t) (value_arch : Value.t)
      (value_tableName : Value.t) (value_tableEntryPriorityInterface : Value.t)
      (value_tableKeysetInterface : Value.t)
      (value_tableActionInterface : Value.t) : Value.t =
    (* Lookup table object *)
    let value_tableObject = find_table value_arch value_tableName in
    (* Add entry to table object *)
    let value_tableObject =
      Spec.Func.tableObject_add_entry value_ctx value_tableObject
        value_tableEntryPriorityInterface value_tableKeysetInterface
        value_tableActionInterface
    in
    (* Update arch with modified table object *)
    update_table value_arch value_tableName value_tableObject

  let table_add_default_action (value_ctx : Value.t) (value_arch : Value.t)
      (value_tableName : Value.t) (value_tableActionInterface : Value.t) :
      Value.t =
    (* Lookup table object *)
    let value_tableObject = find_table value_arch value_tableName in
    (* Add entry to table object *)
    let value_tableObject =
      Spec.Func.tableObject_add_default_action value_ctx value_tableObject
        value_tableActionInterface
    in
    (* Update arch with modified table object *)
    update_table value_arch value_tableName value_tableObject

  (* Mirror table interface *)

  let add_mirror_session (value_arch : Value.t) (session : int) (port : int) :
      Value.t =
    let arch_state =
      value_arch |> Spec.Func.find_archState_e |> Arch.of_value
    in
    let mirrortable = Mirror.Table.add session port arch_state.mirrortable in
    arch_state
    |> Arch.with_mirrortable mirrortable
    |> Arch.to_value
    |> Spec.Func.update_archState_e value_arch

  let add_mirror_session_mc _session _multicast_group =
    error_no_region
      "add_mirror_session_mc is not implemented for the v1model simulator"

  (* Multicast interface *)

  let mc_mgrp_create (value_arch : Value.t) (mgid : int) : Value.t =
    let arch_state =
      value_arch |> Spec.Func.find_archState_e |> Arch.of_value
    in
    let multicast = Multicast.State.group_create mgid arch_state.multicast in
    arch_state
    |> Arch.with_multicast multicast
    |> Arch.to_value
    |> Spec.Func.update_archState_e value_arch

  let mc_node_create (value_arch : Value.t) (rid : int) (ports : int list) :
      Value.t =
    let arch_state =
      value_arch |> Spec.Func.find_archState_e |> Arch.of_value
    in
    let multicast =
      Multicast.State.node_create rid ports arch_state.multicast
    in
    arch_state
    |> Arch.with_multicast multicast
    |> Arch.to_value
    |> Spec.Func.update_archState_e value_arch

  let mc_node_associate (value_arch : Value.t) (mgid : int) (handle : int) :
      Value.t =
    let arch_state =
      value_arch |> Spec.Func.find_archState_e |> Arch.of_value
    in
    let multicast =
      Multicast.State.node_associate mgid handle arch_state.multicast
    in
    arch_state
    |> Arch.with_multicast multicast
    |> Arch.to_value
    |> Spec.Func.update_archState_e value_arch

  (* Register interface *)

  let register_read (_value_arch : Value.t) (_reg_name : string) (_index : int)
      : Value.t =
    error_no_region "register_read is not implemented for the v1model simulator"

  let register_write (_value_arch : Value.t) (_reg_name : string) (_index : int)
      (_value : int) : Value.t =
    error_no_region
      "register_write is not implemented for the v1model simulator"

  let register_reset (_value_arch : Value.t) (_reg_name : string) : Value.t =
    error_no_region
      "register_reset is not implemented for the v1model simulator"

  (* Packet state *)

  let insert_packet (packet : Packet.t) : unit state =
    let { packet_in; value_ctx; _ } : Packet.t = packet in
    let packet_in = PacketIn packet_in in
    let value_objectId = wrap_list_v "id" [ wrap_text_v "packet_in" ] in
    let value_packet_in =
      packet_in |> object_state_to_yojson |> wrap_extern_v "objectState"
    in
    modify (fun (_, value_arch, txs) ->
        let value_arch =
          Spec.Func.update_objectState_e value_arch value_objectId
            value_packet_in
        in
        (value_ctx, value_arch, txs))

  let remove_packet_in : unit state =
    let* _, value_arch, _ = get in
    let value_arch =
      let packet_in =
        value_arch |> get_packet_in |> Core.Object.PacketIn.reset
      in
      let packet_in = PacketIn packet_in in
      let value_objectId = wrap_list_v "id" [ wrap_text_v "packet_in" ] in
      let value_packet_in =
        packet_in |> object_state_to_yojson |> wrap_extern_v "objectState"
      in
      Spec.Func.update_objectState_e value_arch value_objectId value_packet_in
    in
    modify (fun (value_ctx, _, txs) -> (value_ctx, value_arch, txs))

  let remove_packet_out : unit state =
    let* _, value_arch, _ = get in
    let value_arch =
      let packet_out = Core.Object.PacketOut.init () in
      let packet_out = PacketOut packet_out in
      let value_objectId = wrap_list_v "id" [ wrap_text_v "packet_out" ] in
      let value_packet_out =
        packet_out |> object_state_to_yojson |> wrap_extern_v "objectState"
      in
      Spec.Func.update_objectState_e value_arch value_objectId value_packet_out
    in
    modify (fun (value_ctx, _, txs) -> (value_ctx, value_arch, txs))

  let is_dropped : bool state =
    let+ value_ctx, value_arch, _ = get in
    let value_egress_spec =
      Spec.Rel.lvalue_read_dot_global value_ctx value_arch "standard_metadata"
        "egress_spec"
    in
    let width_egress_spec, int_egress_spec =
      unpack_p4_fixedBit value_egress_spec
    in
    Bigint.(width_egress_spec = of_int 9 && int_egress_spec = of_int 511)

  let get_mcast_grp : int state =
    let+ value_ctx, value_arch, _ = get in
    let value_mcast_grp =
      Spec.Rel.lvalue_read_dot_global value_ctx value_arch "standard_metadata"
        "mcast_grp"
    in
    let _, int_mcast_grp = unpack_p4_fixedBit value_mcast_grp in
    Bigint.to_int_exn int_mcast_grp

  (* Pipeline initializer *)

  let init_pipe (includes_p4 : string list) (filename_p4 : string) :
      Value.t * Value.t =
    let program_result =
      match !mode with
      | IL_mode -> Interp_IL.eval_program "V1Model_init" includes_p4 filename_p4
      | SL_mode -> Interp_SL.eval_program "V1Model_init" includes_p4 filename_p4
      | Empty_mode -> assert false
    in
    match program_result with
    | Pass [ value_ctx; value_arch ] -> (value_ctx, value_arch)
    | Pass _ -> error_no_region "unexpected return from V1Model_init"
    | Fail (`Syntax (at, msg)) | Fail (`Runtime (at, msg)) -> error at msg

  (* Pipeline driver *)

  let setup_rx (rx : IO.rx) : unit state =
    (* Setup packet_in object *)
    let port_in, packet_in = rx in
    let packet_in = PacketIn (Core.Object.PacketIn.init packet_in) in
    let packet_in_state = object_state_to_yojson packet_in in
    let value_packet_in_state = wrap_extern_v "objectState" packet_in_state in
    let* value_ctx, value_arch, _ = get in
    let value_ctx, value_arch =
      Spec.Rel.v1model_init_packet_in value_ctx value_arch value_packet_in_state
    in
    (* Setup packet_out object *)
    let packet_out = PacketOut (Core.Object.PacketOut.init ()) in
    let packet_out_state = object_state_to_yojson packet_out in
    let value_packet_out_state = wrap_extern_v "objectState" packet_out_state in
    let value_ctx, value_arch =
      Spec.Rel.v1model_init_packet_out value_ctx value_arch
        value_packet_out_state
    in
    (* Setup global variables *)
    let value_ctx =
      Spec.Rel.v1model_init_globals value_ctx value_arch port_in
    in
    modify (fun (_, _, txs) -> (value_ctx, value_arch, txs))

  (* Parser + Verify *)

  let drive_p : unit state =
    let* value_parser_result = apply Spec.Rel.v1model_parser in
    let* value_ctx, value_arch, _ = get in
    let value_ctx =
      match flatten_case_v_opt value_parser_result with
      | Some (_, [ [ "REJECT" ]; [] ], [ value_error ]) ->
          Spec.Rel.lvalue_write_dot_global value_ctx value_arch
            "standard_metadata" "parser_error" value_error
      | Some _ -> value_ctx
      | None -> assert false
    in
    modify (fun (_, _, txs) -> (value_ctx, value_arch, txs))

  let drive_vr : Value.t state = apply Spec.Rel.v1model_verify

  let drive_pipe_pre : Value.t state =
    let* arch_state = get_arch_state in
    put_arch_state (Arch.reset arch_state)
    >> remove_packet_in >> drive_p >> drive_vr

  (* Checksum + Deparser *)

  let drive_ck : Value.t state = apply Spec.Rel.v1model_check
  let drive_dep : Value.t state = apply Spec.Rel.v1model_deparse

  let drive_pipe_post : Value.t state =
    let* result = drive_ck >> remove_packet_out >> drive_dep in
    let* value_ctx, value_arch, _ = get in
    let value_egress_spec =
      Spec.Rel.lvalue_read_dot_global value_ctx value_arch "standard_metadata"
        "egress_spec"
    in
    let _, int_egress_spec = unpack_p4_fixedBit value_egress_spec in
    (* Get egress port *)
    let port = Bigint.to_int_exn int_egress_spec in
    (* Get input packet *)
    let packet_in = get_packet_in value_arch in
    (* Get output packet *)
    let packet_out = get_packet_out value_arch in
    let packet =
      Format.asprintf "%a" Core.Object.Packet.pp (packet_in, packet_out)
    in
    (* Return port and packet *)
    let tx = (port, packet) in
    modify (fun (value_ctx, value_arch, txs) ->
        (value_ctx, value_arch, tx :: txs))
    >> return result

  (* Prepare context for resubmit/clone/recirculate/multicast *)

  let prepare_resubmit_ctx (index : int) : unit state =
    let* value_ctx, value_arch, _ = get in
    let value_ctx =
      Spec.Rel.v1model_setup_preserved_meta_fields value_ctx value_arch
        (Packet.ResubmitInfo.to_value index)
    in
    (* Set standard_metadata.instance_type as PKT_INSTANCE_TYPE_RESUBMIT *)
    let value_ctx =
      let value_instance_type =
        Interface.Pack.pack_p4_fixedBit (Bigint.of_int 32) (Bigint.of_int 6)
      in
      Spec.Rel.lvalue_write_dot_global value_ctx value_arch "standard_metadata"
        "instance_type" value_instance_type
    in
    modify (fun (_, value_arch, txs) -> (value_ctx, value_arch, txs))

  let prepare_clone_ctx (clone_type : Packet.CloneInfo.clone_type) (port : int)
      (index : int) : unit state =
    let* value_ctx, value_arch, _ = get in
    let value_index =
      pack_p4_fixedBit (Bigint.of_int 8) (Bigint.of_int index)
    in
    let value_ctx =
      Spec.Rel.v1model_setup_preserved_meta_fields value_ctx value_arch
        value_index
    in
    (* Set standard_metadata.instance_type according to clone type *)
    let value_ctx =
      let instance_type = match clone_type with I2E -> 1 | E2E -> 2 in
      let value_instance_type =
        Interface.Pack.pack_p4_fixedBit (Bigint.of_int 32)
          (Bigint.of_int instance_type)
      in
      Spec.Rel.lvalue_write_dot_global value_ctx value_arch "standard_metadata"
        "instance_type" value_instance_type
    in
    let value_ctx =
      let value_egress_spec =
        Interface.Pack.pack_p4_fixedBit (Bigint.of_int 9) (Bigint.of_int port)
      in
      Spec.Rel.lvalue_write_dot_global value_ctx value_arch "standard_metadata"
        "egress_spec" value_egress_spec
    in
    modify (fun (_, value_arch, txs) -> (value_ctx, value_arch, txs))

  let prepare_recirculate_ctx (index : int) : unit state =
    let* value_ctx, value_arch, _ = get in
    let value_ctx =
      Spec.Rel.v1model_setup_preserved_meta_fields value_ctx value_arch
        (Packet.RecirculateInfo.to_value index)
    in
    (* Set standard_metadata.instance_type as PKT_INSTANCE_TYPE_RECIRC *)
    let value_ctx =
      let value_instance_type =
        Interface.Pack.pack_p4_fixedBit (Bigint.of_int 32) (Bigint.of_int 4)
      in
      Spec.Rel.lvalue_write_dot_global value_ctx value_arch "standard_metadata"
        "instance_type" value_instance_type
    in
    modify (fun (_, value_arch, txs) -> (value_ctx, value_arch, txs))

  let prepare_multicast_ctx (rid : Multicast.rid) (port : Multicast.port) :
      unit state =
    let* value_ctx, value_arch, _ = get in
    let value_ctx =
      let value_egress_rid =
        pack_p4_fixedBit (Bigint.of_int 16) (Bigint.of_int rid)
      in
      Spec.Rel.lvalue_write_dot_global value_ctx value_arch "standard_metadata"
        "egress_rid" value_egress_rid
    in
    let value_ctx =
      let value_egress_spec =
        Interface.Pack.pack_p4_fixedBit (Bigint.of_int 9) (Bigint.of_int port)
      in
      Spec.Rel.lvalue_write_dot_global value_ctx value_arch "standard_metadata"
        "egress_spec" value_egress_spec
    in
    (* Set standard_metadata.instance_type as PKT_INSTANCE_TYPE_REPLICATION *)
    let value_ctx =
      let value_instance_type =
        pack_p4_fixedBit (Bigint.of_int 32) (Bigint.of_int 5)
      in
      Spec.Rel.lvalue_write_dot_global value_ctx value_arch "standard_metadata"
        "instance_type" value_instance_type
    in
    modify (fun (_, value_arch, txs) -> (value_ctx, value_arch, txs))

  (* Schedule resubmit/clone/recirculate/multicast if needed *)

  let schedule_packet (entrypoint : Packet.entrypoint) : unit state =
    let* value_ctx, value_arch, _ = get in
    let packet_in = get_packet_in value_arch in
    let packet = Packet.{ value_ctx; packet_in; entrypoint } in
    let* arch_state = get_arch_state in
    let queue =
      match entrypoint with
      | Ingress -> Scheduler.push_front packet arch_state.queue
      | Egress -> Scheduler.push_back packet arch_state.queue
    in
    arch_state |> Arch.with_queue queue |> put_arch_state

  let schedule_resubmit (arch_state : Arch.t) : bool state =
    let open Arch in
    match arch_state.action.resubmit_opt with
    | None -> return false
    | Some field_index ->
        let* value_ctx_original, _, _ = get in
        prepare_resubmit_ctx field_index
        >> drive_pipe_pre >> schedule_packet Ingress
        >> modify (fun (_, value_arch, txs) ->
               (value_ctx_original, value_arch, txs))
        >> return true

  let schedule_clone (arch_state : Arch.t) : bool state =
    let open Arch in
    match arch_state.action.clone_opt with
    | None -> return false
    | Some (clone_type, session, field_index) -> (
        match Mirror.Table.find_opt session arch_state.mirrortable with
        | Some port ->
            let* value_ctx_original, _, _ = get in
            prepare_clone_ctx clone_type port field_index
            >> (match clone_type with
               | I2E -> drive_pipe_pre >> return ()
               | E2E -> return ())
            >> schedule_packet Egress
            >> modify (fun (_, value_arch, txs) ->
                   (value_ctx_original, value_arch, txs))
            >> return true
        | None -> return false)

  let schedule_recirculate (arch_state : Arch.t) : bool state =
    let open Arch in
    match arch_state.action.recirculate_opt with
    | None -> return false
    | Some field_index ->
        let* value_ctx_original, _, _ = get in
        (* run ck and dep block *)
        let* _ =
          prepare_recirculate_ctx field_index
          >> drive_ck >> remove_packet_out >> drive_dep
        in
        (* take output packet from deparser and feed it back to pipeline *)
        let* value_ctx, value_arch, _ = get in
        let packet_in =
          let packet_in = get_packet_in value_arch in
          let packet_out = get_packet_out value_arch in
          Format.asprintf "%a" Core.Object.Packet.pp (packet_in, packet_out)
        in
        let value_objectId = wrap_list_v "id" [ wrap_text_v "packet_in" ] in
        let value_packet_in =
          PacketIn (Core.Object.PacketIn.init packet_in)
          |> object_state_to_yojson
          |> wrap_extern_v "objectState"
        in
        let* _ =
          modify (fun (_, value_arch, txs) ->
              let value_arch =
                Spec.Func.update_objectState_e value_arch value_objectId
                  value_packet_in
              in
              (value_ctx, value_arch, txs))
        in
        (* run p + vr block before scheduling packet *)
        drive_pipe_pre >> schedule_packet Ingress
        >> modify (fun (_, value_arch, txs) ->
               (value_ctx_original, value_arch, txs))
        >> return true

  let schedule_multicast (arch_state : Arch.t) (mcast_grp : int) : bool state =
    let open Arch in
    let open Multicast in
    match GroupMap.find_opt mcast_grp arch_state.multicast.groups with
    | Some { node_handles; _ } ->
        let actions =
          node_handles
          |> List.filter_map (fun handle ->
                 NodeMap.find_opt handle arch_state.multicast.nodes)
          |> List.flatten
          |> List.map (fun node ->
                 prepare_multicast_ctx node.rid node.port
                 >> schedule_packet Egress)
        in
        sequence actions >> return true
    | None -> return false

  (* Ingress + Handle clone, resubmit, drop *)

  let drive_ig : Value.t state =
    let* result = apply Spec.Rel.v1model_ingress in
    let* arch_state = get_arch_state in
    let* _cloned = schedule_clone arch_state in
    let* resubmitted = schedule_resubmit arch_state in
    if resubmitted then return result
    else
      let* mcast_grp = get_mcast_grp in
      if mcast_grp <> 0 then
        schedule_multicast arch_state mcast_grp >> return result
      else
        let* drop = is_dropped in
        if drop then return result else schedule_packet Egress >> return result

  (* Egress + Handle clone

     This field is assigned a predictable value just before the packet begins
     egress processing, equal to the output port that this packet is destined
     to

     - 'Standard metadata' at
     https://github.com/p4lang/behavioral-model/blob/168eca/docs/simple_switch.md *)

  let prepare_egress_ctx : unit state =
    let* value_ctx, value_arch, _ = get in
    let value_egress_spec =
      Spec.Rel.lvalue_read_dot_global value_ctx value_arch "standard_metadata"
        "egress_spec"
    in
    let value_ctx =
      Spec.Rel.lvalue_write_dot_global value_ctx value_arch "standard_metadata"
        "egress_port" value_egress_spec
    in
    modify (fun (_, _, txs) -> (value_ctx, value_arch, txs))

  let drive_eg : Value.t state =
    let* () = prepare_egress_ctx in
    let* result = apply Spec.Rel.v1model_egress in
    let* arch_state = get_arch_state in
    let* _cloned = schedule_clone arch_state in
    let* drop = is_dropped in
    let* () = guard (not drop) in
    let* arch_state = get_arch_state in
    let* recirculated = schedule_recirculate arch_state in
    if recirculated then empty else return result

  (* Scheduling packets *)

  let drive_packet (packet : Packet.t) : unit state =
    match packet.entrypoint with
    | Ingress -> insert_packet packet >> drive_ig >> return ()
    | Egress ->
        insert_packet packet
        >> on_result drive_eg
             ~some:(fun _ -> drive_pipe_post >> return ())
             ~none:(fun () -> return ())

  let rec run_scheduler () : unit state =
    let* arch_state = get_arch_state in
    match Scheduler.pop_front_opt arch_state.queue with
    | None -> empty
    | Some (packet, queue) ->
        Arch.(arch_state |> reset |> with_queue queue)
        |> put_arch_state >> drive_packet packet >> run_scheduler ()

  let drive_pipe (value_ctx : Value.t) (value_arch : Value.t) (rx : IO.rx) :
      Value.t * Value.t * IO.tx list =
    let pipe : unit state =
      (* Setup port and packet *)
      setup_rx rx >> drive_pipe_pre >> schedule_packet Ingress
      >> run_scheduler ()
    in
    let state_init = (value_ctx, value_arch, []) in
    let _, (value_ctx, value_arch, txs) = State.run pipe state_init in
    (value_ctx, value_arch, List.rev txs)

  (* Initializer *)

  let init (mode_ : Sim.mode) : unit =
    init_mode mode_;
    init_call_rel ();
    init_call_func ()
end
