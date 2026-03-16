open Lang
open Interface.Wrap
open Interface.Unwrap
open Interface.Unpack
open Interface.Pack
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
        name |> rewrite_substring ~substrings:[ "ingress" ] ~replacement:"ip.ig")
    in
    match stmt with
    | RegisterRead (name, number) ->
        let name = transform_name name in
        RegisterRead (name, number)
    | RegisterWrite (name, number_reg, number_index) ->
        let name = transform_name name in
        RegisterWrite (name, number_reg, number_index)
    | RegisterReset name ->
        let name = transform_name name in
        RegisterReset name
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
    | Hash of Object.HashExtern.t
    | InternetChecksum of Object.InternetChecksum.t
    | Meter of Object.Meter.t
  [@@deriving yojson]

  let get_object_state (value_arch : Value.t) (value_objectId : Value.t) :
      object_state =
    Spec.Func.find_objectState_e value_arch value_objectId
    |> unwrap_extern_v |> object_state_of_yojson |> Result.get_ok

  let get_ingress_packet_in (value_arch : Value.t) : Core.Object.PacketIn.t =
    let value_objectId = wrap_list_v "id" [ wrap_text_v "ingress_packet_in" ] in
    match get_object_state value_arch value_objectId with
    | PacketIn packet_in -> packet_in
    | _ -> error_no_region "ingress_packet_in extern not found"

  let get_ingress_packet_out (value_arch : Value.t) : Core.Object.PacketOut.t =
    let value_objectId =
      wrap_list_v "id" [ wrap_text_v "ingress_packet_out" ]
    in
    match get_object_state value_arch value_objectId with
    | PacketOut packet_out -> packet_out
    | _ -> error_no_region "ingress_packet_out extern not found"

  let get_egress_packet_in (value_arch : Value.t) : Core.Object.PacketIn.t =
    let value_objectId = wrap_list_v "id" [ wrap_text_v "egress_packet_in" ] in
    match get_object_state value_arch value_objectId with
    | PacketIn packet_in -> packet_in
    | _ -> error_no_region "egress_packet_in extern not found"

  let get_egress_packet_out (value_arch : Value.t) : Core.Object.PacketOut.t =
    let value_objectId = wrap_list_v "id" [ wrap_text_v "egress_packet_out" ] in
    match get_object_state value_arch value_objectId with
    | PacketOut packet_out -> packet_out
    | _ -> error_no_region "egress_packet_out extern not found"

  let get_register (value_arch : Value.t) (reg_name : string) :
      Object.Register.t =
    let names = String.split_on_char '.' reg_name in
    let values_name = List.map wrap_text_v names in
    let value_objectId = wrap_list_v "id" values_name in
    match get_object_state value_arch value_objectId with
    | Register register -> register
    | _ -> error_no_region ("Register extern " ^ reg_name ^ " not found")

  let put_register (value_arch : Value.t) (reg_name : string)
      (reg : Object.Register.t) : Value.t =
    let names = String.split_on_char '.' reg_name in
    let values_name = List.map wrap_text_v names in
    let value_objectId = wrap_list_v "id" values_name in
    let value_reg =
      Register reg |> object_state_to_yojson |> wrap_extern_v "objectState"
    in
    Spec.Func.update_objectState_e value_arch value_objectId value_reg

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
    | "Counter" ->
        let counter = Object.Counter.init value_type_args value_args in
        let counter = Counter counter in
        counter |> object_state_to_yojson |> wrap_extern_v "objectState"
    | "Register" ->
        let register = Object.Register.init value_type_args value_args in
        let register = Register register in
        register |> object_state_to_yojson |> wrap_extern_v "objectState"
    | "Hash" ->
        let hash = Object.HashExtern.init value_type_args value_args in
        let hash = Hash hash in
        hash |> object_state_to_yojson |> wrap_extern_v "objectState"
    | "InternetChecksum" ->
        let checksum =
          Object.InternetChecksum.init value_type_args value_args
        in
        let checksum = InternetChecksum checksum in
        checksum |> object_state_to_yojson |> wrap_extern_v "objectState"
    | "Meter" ->
        let meter = Object.Meter.init value_type_args value_args in
        let meter = Meter meter in
        meter |> object_state_to_yojson |> wrap_extern_v "objectState"
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
    let extern, value_ctx, value_arch, value_callResult =
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
          let counter, value_ctx, value_arch, value_callResult =
            Object.Counter.count value_ctx value_arch counter
          in
          let counter = Counter counter in
          (counter, value_ctx, value_arch, value_callResult)
      | Register register, "read", [ "index" ] ->
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
      | Hash hash, "get_hash", [ "data" ] ->
          let hash, value_ctx, value_arch, value_callResult =
            Object.HashExtern.get_hash value_ctx value_arch hash
          in
          let hash = Hash hash in
          (hash, value_ctx, value_arch, value_callResult)
      | Hash hash, "get_hash", [ "base"; "data"; "max" ] ->
          let hash, value_ctx, value_arch, value_callResult =
            Object.HashExtern.get_hash_adjust value_ctx value_arch hash
          in
          let hash = Hash hash in
          (hash, value_ctx, value_arch, value_callResult)
      | InternetChecksum checksum, "clear", [] ->
          let checksum, value_ctx, value_arch, value_callResult =
            Object.InternetChecksum.clear value_ctx value_arch checksum
          in
          let checksum = InternetChecksum checksum in
          (checksum, value_ctx, value_arch, value_callResult)
      | InternetChecksum checksum, "add", [ "data" ] ->
          let checksum, value_ctx, value_arch, value_callResult =
            Object.InternetChecksum.add value_ctx value_arch checksum
          in
          let checksum = InternetChecksum checksum in
          (checksum, value_ctx, value_arch, value_callResult)
      | InternetChecksum checksum, "subtract", [ "data" ] ->
          let checksum, value_ctx, value_arch, value_callResult =
            Object.InternetChecksum.subtract value_ctx value_arch checksum
          in
          let checksum = InternetChecksum checksum in
          (checksum, value_ctx, value_arch, value_callResult)
      | InternetChecksum checksum, "get", [] ->
          let checksum, value_ctx, value_arch, value_callResult =
            Object.InternetChecksum.get value_ctx value_arch checksum
          in
          let checksum = InternetChecksum checksum in
          (checksum, value_ctx, value_arch, value_callResult)
      | InternetChecksum checksum, "get_state", [] ->
          let checksum, value_ctx, value_arch, value_callResult =
            Object.InternetChecksum.get_state value_ctx value_arch checksum
          in
          let checksum = InternetChecksum checksum in
          (checksum, value_ctx, value_arch, value_callResult)
      | InternetChecksum checksum, "set_state", [ "checksum_state" ] ->
          let checksum, value_ctx, value_arch, value_callResult =
            Object.InternetChecksum.set_state value_ctx value_arch checksum
          in
          let checksum = InternetChecksum checksum in
          (checksum, value_ctx, value_arch, value_callResult)
      | Meter meter, "execute", [ "index"; "color" ] ->
          let meter, value_ctx, value_arch, value_callResult =
            Object.Meter.execute_color_aware value_ctx value_arch meter
          in
          let meter = Meter meter in
          (meter, value_ctx, value_arch, value_callResult)
      | Meter meter, "execute", [ "index" ] ->
          let meter, value_ctx, value_arch, value_callResult =
            Object.Meter.execute_color_blind value_ctx value_arch meter
          in
          let meter = Meter meter in
          (meter, value_ctx, value_arch, value_callResult)
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
      extern |> object_state_to_yojson |> wrap_extern_v "objectState"
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
    (* Update store with modified table object *)
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
    (* Update store with modified table object *)
    update_table value_arch value_tableName value_tableObject

  (* Mirror session interface *)

  let add_mirror_session _session _port =
    error_no_region
      "add_mirror_session is not implemented for the psa simulator"

  let add_mirror_session_mc (value_arch : Value.t) (session : int)
      (multicast_group : int) : Value.t =
    let arch_state =
      value_arch |> Spec.Func.find_archState_e |> Arch.of_value
    in
    let mirrortable =
      Mirror.Table.add session multicast_group arch_state.mirrortable
    in
    arch_state
    |> Arch.with_mirrortable mirrortable
    |> Arch.to_value
    |> Spec.Func.update_archState_e value_arch

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

  let mc_node_create (value_arch : Value.t) (instance : int) (ports : int list)
      : Value.t =
    let arch_state =
      value_arch |> Spec.Func.find_archState_e |> Arch.of_value
    in
    let multicast =
      Multicast.State.node_create instance ports arch_state.multicast
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

  let register_read (value_arch : Value.t) (reg_name : string) (index : int) :
      Value.t =
    let reg = get_register value_arch reg_name in
    let _value =
      if index < List.length reg.values then List.nth reg.values index
      else Spec.Func.default reg.typ
    in
    (* Print register value *)
    (* Format.printf "%s[%d] = %s\n" reg_name index (Value.to_string value); *)
    value_arch

  let register_write (value_arch : Value.t) (reg_name : string) (index : int)
      (value : int) : Value.t =
    let reg = get_register value_arch reg_name in
    let value_value = Bigint.of_int value |> pack_p4_arbitraryInt in
    let value_value = Spec.Func.cast_op reg.typ value_value in
    let values =
      List.mapi
        (fun idx value -> if idx = index then value_value else value)
        reg.values
    in
    let reg = { reg with values } in
    put_register value_arch reg_name reg

  let register_reset (value_arch : Value.t) (reg_name : string) : Value.t =
    let reg = get_register value_arch reg_name in
    let value_default = Spec.Func.default reg.typ in
    let values = List.map (fun _ -> value_default) reg.values in
    let reg = { reg with values } in
    put_register value_arch reg_name reg

  (* Packet state *)

  let insert_packet (packet : Packet.t) : unit state =
    let { packet_in; value_ctx; entrypoint } : Packet.t = packet in
    let id_packet_in =
      match entrypoint with
      | Ingress -> "ingress_packet_in"
      | Egress -> "egress_packet_in"
    in
    let value_objectId = wrap_list_v "id" [ wrap_text_v id_packet_in ] in
    let value_packet_in =
      let packet_in = PacketIn packet_in in
      packet_in |> object_state_to_yojson |> wrap_extern_v "objectState"
    in
    modify (fun (_, value_arch, txs) ->
        let value_arch =
          Spec.Func.update_objectState_e value_arch value_objectId
            value_packet_in
        in
        (value_ctx, value_arch, txs))

  let remove_ingress_packet_in : unit state =
    let* _, value_arch, _ = get in
    let value_arch =
      let packet_in =
        value_arch |> get_ingress_packet_in |> Core.Object.PacketIn.reset
      in
      let packet_in = PacketIn packet_in in
      let value_objectId =
        wrap_list_v "id" [ wrap_text_v "ingress_packet_in" ]
      in
      let value_packet_in =
        packet_in |> object_state_to_yojson |> wrap_extern_v "objectState"
      in
      Spec.Func.update_objectState_e value_arch value_objectId value_packet_in
    in
    modify (fun (value_ctx, _, txs) -> (value_ctx, value_arch, txs))

  let remove_ingress_packet_out : unit state =
    let* _, value_arch, _ = get in
    let value_arch =
      let packet_out = Core.Object.PacketOut.init () in
      let packet_out = PacketOut packet_out in
      let value_objectId =
        wrap_list_v "id" [ wrap_text_v "ingress_packet_out" ]
      in
      let value_packet_out =
        packet_out |> object_state_to_yojson |> wrap_extern_v "objectState"
      in
      Spec.Func.update_objectState_e value_arch value_objectId value_packet_out
    in
    modify (fun (value_ctx, _, txs) -> (value_ctx, value_arch, txs))

  let remove_egress_packet_out : unit state =
    let* _, value_arch, _ = get in
    let value_arch =
      let packet_out = Core.Object.PacketOut.init () in
      let packet_out = PacketOut packet_out in
      let value_objectId =
        wrap_list_v "id" [ wrap_text_v "egress_packet_out" ]
      in
      let value_packet_out =
        packet_out |> object_state_to_yojson |> wrap_extern_v "objectState"
      in
      Spec.Func.update_objectState_e value_arch value_objectId value_packet_out
    in
    modify (fun (value_ctx, _, txs) -> (value_ctx, value_arch, txs))

  let is_ingress_clone : bool state =
    let+ value_ctx, value_arch, _ = get in
    let value_clone =
      Spec.Rel.lvalue_read_dot_global value_ctx value_arch
        "ingress_output_metadata" "clone"
    in
    unpack_p4_bool value_clone

  let is_ingress_drop : bool state =
    let+ value_ctx, value_arch, _ = get in
    let value_drop =
      Spec.Rel.lvalue_read_dot_global value_ctx value_arch
        "ingress_output_metadata" "drop"
    in
    unpack_p4_bool value_drop

  let is_ingress_resubmit : bool state =
    let+ value_ctx, value_arch, _ = get in
    let value_drop =
      Spec.Rel.lvalue_read_dot_global value_ctx value_arch
        "ingress_output_metadata" "resubmit"
    in
    unpack_p4_bool value_drop

  let get_ingress_clone_session_id : int state =
    let+ value_ctx, value_arch, _ = get in
    let value_session =
      Spec.Rel.lvalue_read_dot_global value_ctx value_arch
        "ingress_output_metadata" "clone_session_id"
    in
    let _, int_session = unpack_p4_fixedBit value_session in
    Bigint.to_int_exn int_session

  let get_multicast_group : int state =
    let+ value_ctx, value_arch, _ = get in
    let value_mcast_grp =
      Spec.Rel.lvalue_read_dot_global value_ctx value_arch
        "ingress_output_metadata" "multicast_group"
    in
    let _, int_mcast_grp = unpack_p4_fixedBit value_mcast_grp in
    Bigint.to_int_exn int_mcast_grp

  let is_egress_clone : bool state =
    let+ value_ctx, value_arch, _ = get in
    let value_clone =
      Spec.Rel.lvalue_read_dot_global value_ctx value_arch
        "egress_output_metadata" "clone"
    in
    unpack_p4_bool value_clone

  let is_egress_drop : bool state =
    let+ value_ctx, value_arch, _ = get in
    let value_drop =
      Spec.Rel.lvalue_read_dot_global value_ctx value_arch
        "egress_output_metadata" "drop"
    in
    unpack_p4_bool value_drop

  let is_egress_recirculate : bool state =
    let+ value_ctx, value_arch, _ = get in
    let value_port =
      Spec.Rel.lvalue_read_dot_global value_ctx value_arch
        "egress_input_metadata" "egress_port"
    in
    let width_port, int_port = unpack_p4_fixedBit value_port in
    Bigint.(width_port = of_int 32 && int_port = of_int 0xfffffffa)

  let get_egress_clone_session_id : int state =
    let+ value_ctx, value_arch, _ = get in
    let value_session =
      Spec.Rel.lvalue_read_dot_global value_ctx value_arch
        "egress_output_metadata" "clone_session_id"
    in
    let _, int_session = unpack_p4_fixedBit value_session in
    Bigint.to_int_exn int_session

  (* Pipeline initializer *)

  let init_pipe (includes_p4 : string list) (filename_p4 : string) :
      Value.t * Value.t =
    let program_result =
      match !mode with
      | IL_mode -> Interp_IL.eval_program "PSA_init" includes_p4 filename_p4
      | SL_mode -> Interp_SL.eval_program "PSA_init" includes_p4 filename_p4
      | Empty_mode -> assert false
    in
    match program_result with
    | Pass [ value_ctx; value_arch ] -> (value_ctx, value_arch)
    | Pass _ -> error_no_region "unexpected return from PSA_init"
    | Fail (`Syntax (at, msg)) | Fail (`Runtime (at, msg)) -> error at msg

  (* Prepare context *)

  let prepare_unicast_ctx : unit state =
    let* value_ctx, value_arch, txs = get in
    (* Prepare egress port *)
    let value_egress_port =
      Spec.Rel.lvalue_read_dot_global value_ctx value_arch
        "ingress_output_metadata" "egress_port"
    in
    let egress_port =
      unpack_p4_fixedBit value_egress_port |> snd |> Bigint.to_int |> Option.get
    in
    (* Prepare class of service *)
    let value_cos =
      Spec.Rel.lvalue_read_dot_global value_ctx value_arch
        "ingress_output_metadata" "class_of_service"
    in
    let cos =
      unpack_p4_fixedBit value_cos |> snd |> Bigint.to_int |> Option.get
    in
    (* Init egress metadata *)
    let value_ctx =
      Spec.Rel.psa_egress_init_metadata value_ctx value_arch egress_port
        "NORMAL_UNICAST" cos 0
    in
    put (value_ctx, value_arch, txs)

  let prepare_multicast_ctx (instance : Multicast.instance)
      (port : Multicast.port) : unit state =
    let* value_ctx, value_arch, txs = get in
    (* Prepare class of service *)
    let value_cos =
      Spec.Rel.lvalue_read_dot_global value_ctx value_arch
        "ingress_output_metadata" "class_of_service"
    in
    let cos =
      unpack_p4_fixedBit value_cos |> snd |> Bigint.to_int |> Option.get
    in
    (* Init egress metadata *)
    let value_ctx =
      Spec.Rel.psa_egress_init_metadata value_ctx value_arch port
        "NORMAL_MULTICAST" cos instance
    in
    put (value_ctx, value_arch, txs)

  let prepare_clone_i2e_ctx (instance : Multicast.instance)
      (port : Multicast.port) : unit state =
    let* value_ctx, value_arch, txs = get in
    (* Prepare class of service *)
    let value_cos =
      Spec.Rel.lvalue_read_dot_global value_ctx value_arch
        "ingress_output_metadata" "class_of_service"
    in
    let cos =
      unpack_p4_fixedBit value_cos |> snd |> Bigint.to_int |> Option.get
    in
    (* Init egress metadata *)
    let value_ctx =
      Spec.Rel.psa_egress_init_metadata value_ctx value_arch port "CLONE_I2E"
        cos instance
    in
    put (value_ctx, value_arch, txs)

  let prepare_clone_e2e_ctx (instance : Multicast.instance)
      (port : Multicast.port) : unit state =
    let* value_ctx, value_arch, txs = get in
    (* Prepare class of service *)
    let value_cos =
      Spec.Rel.lvalue_read_dot_global value_ctx value_arch
        "egress_input_metadata" "class_of_service"
    in
    let cos =
      unpack_p4_fixedBit value_cos |> snd |> Bigint.to_int |> Option.get
    in
    (* Init egress metadata *)
    let value_ctx =
      Spec.Rel.psa_egress_init_metadata value_ctx value_arch port "CLONE_E2E"
        cos instance
    in
    put (value_ctx, value_arch, txs)

  let prepare_resubmit_ctx : unit state =
    let* value_ctx, value_arch, txs = get in
    (* Prepare ingress port *)
    let value_ingress_port =
      Spec.Rel.lvalue_read_dot_global value_ctx value_arch
        "ingress_input_metadata" "ingress_port"
    in
    let ingress_port =
      unpack_p4_fixedBit value_ingress_port
      |> snd |> Bigint.to_int |> Option.get
    in
    (* Init ingress metadata *)
    let value_ctx =
      Spec.Rel.psa_ingress_init_metadata value_ctx value_arch ingress_port
        "RESUBMIT"
    in
    put (value_ctx, value_arch, txs)

  let prepare_recirculate_ctx : unit state =
    (* Init ingress metadata *)
    modify (fun (value_ctx, value_arch, txs) ->
        let value_ctx =
          Spec.Rel.psa_ingress_init_metadata value_ctx value_arch 0xfffffffa
            "RECIRCULATE"
        in
        (value_ctx, value_arch, txs))

  (* Schedule packet *)

  let schedule_packet (entrypoint : Packet.entrypoint) : unit state =
    let* value_ctx, value_arch, _ = get in
    let packet_in =
      match entrypoint with
      | Ingress -> get_ingress_packet_in value_arch
      | Egress -> get_egress_packet_in value_arch
    in
    let packet = Packet.{ value_ctx; packet_in; entrypoint } in
    let* arch_state = get_arch_state in
    let queue = Scheduler.push_back packet arch_state.queue in
    arch_state |> Arch.with_queue queue |> put_arch_state

  let schedule_unicast : unit state =
    let* value_ctx, value_arch, txs = get in
    (* Prepare egress packet *)
    let egress_packet_in =
      let packet_in = get_ingress_packet_in value_arch in
      let packet_out = get_ingress_packet_out value_arch in
      Format.asprintf "%a" Core.Object.Packet.pp (packet_in, packet_out)
    in
    let value_egress_packet_in =
      PacketIn (Core.Object.PacketIn.init egress_packet_in)
      |> object_state_to_yojson
      |> wrap_extern_v "objectState"
    in
    let value_objectId = wrap_list_v "id" [ wrap_text_v "egress_packet_in" ] in
    let value_arch =
      Spec.Func.update_objectState_e value_arch value_objectId
        value_egress_packet_in
    in
    (* Schedule packet *)
    put (value_ctx, value_arch, txs)
    >> prepare_unicast_ctx >> schedule_packet Egress

  let schedule_multicast (multicast_group : int) : unit state =
    let open Multicast in
    let* arch_state = get_arch_state in
    match GroupMap.find_opt multicast_group arch_state.multicast.groups with
    | Some node_handles ->
        (* Prepare egress packet *)
        let* value_ctx, value_arch, txs = get in
        let value_arch =
          let egress_packet_in =
            let packet_in = get_ingress_packet_in value_arch in
            let packet_out = get_ingress_packet_out value_arch in
            Format.asprintf "%a" Core.Object.Packet.pp (packet_in, packet_out)
          in
          let value_egress_packet_in =
            PacketIn (Core.Object.PacketIn.init egress_packet_in)
            |> object_state_to_yojson
            |> wrap_extern_v "objectState"
          in
          let value_objectId =
            wrap_list_v "id" [ wrap_text_v "egress_packet_in" ]
          in
          Spec.Func.update_objectState_e value_arch value_objectId
            value_egress_packet_in
        in
        let* () = put (value_ctx, value_arch, txs) in
        (* Prepare multicast actions *)
        let* arch_state = get_arch_state in
        let actions =
          node_handles
          |> List.filter_map (fun handle ->
                 NodeMap.find_opt handle arch_state.multicast.nodes)
          |> List.flatten
          |> List.map (fun node ->
                 let* value_ctx_original, _, _ = get in
                 prepare_multicast_ctx node.instance node.port
                 >> schedule_packet Egress
                 >> modify (fun (_, value_arch, txs) ->
                        (value_ctx_original, value_arch, txs)))
        in
        sequence actions >> return ()
    | None -> return ()

  let schedule_clone_i2e (session : int) : unit state =
    let open Multicast in
    let* arch_state = get_arch_state in
    match Mirror.Table.find_opt session arch_state.mirrortable with
    | Some multicast_group -> (
        match GroupMap.find_opt multicast_group arch_state.multicast.groups with
        | Some node_handles ->
            (* Preserve original store for ingress packet_in *)
            let* _, value_arch_original, _ = get in
            (* Prepare egress packet *)
            let* value_ctx, value_arch, txs = remove_ingress_packet_in >> get in
            let value_arch =
              let egress_packet_in = get_ingress_packet_in value_arch in
              let value_egress_packet_in =
                PacketIn egress_packet_in |> object_state_to_yojson
                |> wrap_extern_v "objectState"
              in
              let value_objectId =
                wrap_list_v "id" [ wrap_text_v "egress_packet_in" ]
              in
              Spec.Func.update_objectState_e value_arch value_objectId
                value_egress_packet_in
            in
            let* () = put (value_ctx, value_arch, txs) in
            (* Prepare clone actions *)
            let* arch_state = get_arch_state in
            let actions =
              node_handles
              |> List.filter_map (fun handle ->
                     NodeMap.find_opt handle arch_state.multicast.nodes)
              |> List.flatten
              |> List.map (fun node ->
                     let* value_ctx_original, _, _ = get in
                     prepare_clone_i2e_ctx node.instance node.port
                     >> schedule_packet Egress
                     >> modify (fun (_, value_arch, txs) ->
                            (value_ctx_original, value_arch, txs)))
            in
            let* arch_state = sequence actions >> get_arch_state in
            (* Restore original store *)
            modify (fun (value_ctx, _, txs) ->
                (value_ctx, value_arch_original, txs))
            >> put_arch_state arch_state >> return ()
        | None -> return ())
    | None -> return ()

  let schedule_clone_e2e (session : int) : unit state =
    let open Multicast in
    let* arch_state = get_arch_state in
    match Mirror.Table.find_opt session arch_state.mirrortable with
    | Some multicast_group -> (
        match GroupMap.find_opt multicast_group arch_state.multicast.groups with
        | Some node_handles ->
            (* Preserve original store for egress packet_in *)
            let* value_ctx, value_arch_original, txs = get in
            (* Prepare egress packet *)
            let value_arch =
              let egress_packet_in =
                let packet_in = get_egress_packet_in value_arch_original in
                let packet_out = get_egress_packet_out value_arch_original in
                Format.asprintf "%a" Core.Object.Packet.pp
                  (packet_in, packet_out)
              in
              let value_egress_packet_in =
                PacketIn (Core.Object.PacketIn.init egress_packet_in)
                |> object_state_to_yojson
                |> wrap_extern_v "objectState"
              in
              let value_objectId =
                wrap_list_v "id" [ wrap_text_v "egress_packet_in" ]
              in
              Spec.Func.update_objectState_e value_arch_original value_objectId
                value_egress_packet_in
            in
            let* () = put (value_ctx, value_arch, txs) in
            (* Prepare clone actions *)
            let* arch_state = get_arch_state in
            let actions =
              node_handles
              |> List.filter_map (fun handle ->
                     NodeMap.find_opt handle arch_state.multicast.nodes)
              |> List.flatten
              |> List.map (fun node ->
                     let* value_ctx_original, _, _ = get in
                     prepare_clone_e2e_ctx node.instance node.port
                     >> schedule_packet Egress
                     >> modify (fun (_, value_arch, txs) ->
                            (value_ctx_original, value_arch, txs)))
            in
            let* arch_state = sequence actions >> get_arch_state in
            (* Restore original store *)
            modify (fun (value_ctx, _, txs) ->
                (value_ctx, value_arch_original, txs))
            >> put_arch_state arch_state >> return ()
        | None -> return ())
    | None -> return ()

  let schedule_resubmit : unit state =
    prepare_resubmit_ctx >> remove_ingress_packet_in >> schedule_packet Ingress

  let schedule_recirculate : unit state =
    let* value_ctx, value_arch, txs = get in
    (* Prepare ingress packet *)
    let ingress_packet_in =
      let packet_in = get_egress_packet_in value_arch in
      let packet_out = get_egress_packet_out value_arch in
      Format.asprintf "%a" Core.Object.Packet.pp (packet_in, packet_out)
    in
    let value_ingress_packet_in =
      PacketIn (Core.Object.PacketIn.init ingress_packet_in)
      |> object_state_to_yojson
      |> wrap_extern_v "objectState"
    in
    let value_objectId = wrap_list_v "id" [ wrap_text_v "ingress_packet_in" ] in
    let value_arch =
      Spec.Func.update_objectState_e value_arch value_objectId
        value_ingress_packet_in
    in
    (* Schedule packet *)
    put (value_ctx, value_arch, txs)
    >> prepare_recirculate_ctx >> schedule_packet Ingress

  let transfer_packet : unit state =
    let* value_ctx, value_arch, txs = get in
    (* Get egress port *)
    let port =
      let value_egress_port =
        Spec.Rel.lvalue_read_dot_global value_ctx value_arch
          "egress_input_metadata" "egress_port"
      in
      let _, int_egress_port = unpack_p4_fixedBit value_egress_port in
      Bigint.to_int_exn int_egress_port
    in
    (* Get packet *)
    let packet =
      let packet_in = get_egress_packet_in value_arch in
      let packet_out = get_egress_packet_out value_arch in
      Format.asprintf "%a" Core.Object.Packet.pp (packet_in, packet_out)
    in
    (* Transfer packet to port *)
    let tx = (port, packet) in
    put (value_ctx, value_arch, tx :: txs)

  (* Setup packets and globals *)

  let setup_rx (rx : IO.rx) : unit state =
    let port_in, packet_in = rx in
    (* Setup packet_in objects *)
    let packet_in = PacketIn (Core.Object.PacketIn.init packet_in) in
    let packet_in_state = object_state_to_yojson packet_in in
    let value_packet_in_state = wrap_extern_v "objectState" packet_in_state in
    let* value_ctx, value_arch, txs = get in
    let value_ctx, value_arch =
      Spec.Rel.psa_ingress_init_packet_in value_ctx value_arch
        value_packet_in_state
    in
    let value_ctx, value_arch =
      Spec.Rel.psa_egress_init_packet_in value_ctx value_arch
        value_packet_in_state
    in
    (* Setup packet_out objects *)
    let packet_out = PacketOut (Core.Object.PacketOut.init ()) in
    let packet_out_state = object_state_to_yojson packet_out in
    let value_packet_out_state = wrap_extern_v "objectState" packet_out_state in
    let value_ctx, value_arch =
      Spec.Rel.psa_ingress_init_packet_out value_ctx value_arch
        value_packet_out_state
    in
    let value_ctx, value_arch =
      Spec.Rel.psa_egress_init_packet_out value_ctx value_arch
        value_packet_out_state
    in
    (* Setup global variables *)
    let value_ctx =
      Spec.Rel.psa_ingress_init_globals value_ctx value_arch port_in
    in
    let value_ctx =
      Spec.Rel.psa_egress_init_globals value_ctx value_arch port_in
    in
    put (value_ctx, value_arch, txs)

  (* Ingress pipeline driver *)

  let drive_ip : unit state =
    let* value_parser_result = apply Spec.Rel.psa_ingress_parser in
    let* value_ctx, value_arch, txs = get in
    let value_ctx =
      match flatten_case_v_opt value_parser_result with
      | Some (_, [ [ "REJECT" ]; [] ], [ value_error ]) ->
          Spec.Rel.lvalue_write_dot_global value_ctx value_arch
            "ingress_input_metadata" "parser_error" value_error
      | Some _ -> value_ctx
      | None -> assert false
    in
    put (value_ctx, value_arch, txs)

  let drive_ig : Value.t state = apply Spec.Rel.psa_ingress
  let drive_id : Value.t state = apply Spec.Rel.psa_ingress_deparser

  let drive_ingress_pipe : Value.t state =
    drive_ip >> drive_ig >> remove_ingress_packet_out >> drive_id

  (* Packet replication engine *)

  let run_pre : unit state =
    let* clone = is_ingress_clone in
    let* () =
      if clone then
        let* session = get_ingress_clone_session_id in
        schedule_clone_i2e session
      else return ()
    in
    let* drop = is_ingress_drop in
    if drop then return ()
    else
      let* resubmit = is_ingress_resubmit in
      if resubmit then schedule_resubmit
      else
        let* multicast_group = get_multicast_group in
        if multicast_group <> 0 then schedule_multicast multicast_group
        else schedule_unicast

  (* Egress pipeline driver *)

  let drive_ep : unit state =
    let* value_parser_result = apply Spec.Rel.psa_egress_parser in
    let* value_ctx, value_arch, txs = get in
    let value_ctx =
      match flatten_case_v_opt value_parser_result with
      | Some (_, [ [ "REJECT" ]; [] ], [ value_error ]) ->
          Spec.Rel.lvalue_write_dot_global value_ctx value_arch
            "egress_input_metadata" "parser_error" value_error
      | Some _ -> value_ctx
      | None -> assert false
    in
    put (value_ctx, value_arch, txs)

  let drive_eg : Value.t state = apply Spec.Rel.psa_egress
  let drive_ed : Value.t state = apply Spec.Rel.psa_egress_deparser

  let drive_egress_pipe : Value.t state =
    drive_ep >> drive_eg >> remove_egress_packet_out >> drive_ed

  (* Buffering queueing engine *)

  let run_bqe : unit state =
    let* clone = is_egress_clone in
    let* () =
      if clone then
        let* session = get_egress_clone_session_id in
        schedule_clone_e2e session
      else return ()
    in
    let* drop = is_egress_drop in
    if drop then return ()
    else
      let* recirculate = is_egress_recirculate in
      if recirculate then schedule_recirculate else transfer_packet

  (* Scheduling packets *)

  let drive_packet (packet : Packet.t) : unit state =
    match packet.entrypoint with
    | Ingress -> insert_packet packet >> drive_ingress_pipe >> run_pre
    | Egress -> insert_packet packet >> drive_egress_pipe >> run_bqe

  let rec run_scheduler () : unit state =
    let* arch_state = get_arch_state in
    match Scheduler.pop_front_opt arch_state.queue with
    | None -> empty
    | Some (packet, queue) ->
        Arch.(arch_state |> with_queue queue)
        |> put_arch_state >> drive_packet packet >> run_scheduler ()

  let drive_pipe (value_ctx : Value.t) (value_arch : Value.t) (rx : IO.rx) :
      Value.t * Value.t * IO.tx list =
    let pipe : unit state =
      (* Setup port and packet *)
      setup_rx rx >> schedule_packet Ingress >> run_scheduler ()
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
