open Lang
open Interface.Wrap
open Interface.Unwrap
open Interface.Pack
open Interface.Unpack
open Interface.Flatten
open State
module Deque = Util.Deque
module OptionState = Util.Option_state
module Value = Runtime.Sim.Value
module IO = Runtime.Sim.Io
module Sim = Runtime.Sim.Simulator
open Error
open OptionState

module Make (Interp_IL : Sim.INTERP_IL) (Interp_SL : Sim.INTERP_SL) : Sim.ARCH =
struct
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

  (* Extern objects *)

  let empty_arch_state = ArchState.empty |> ArchState.to_value

  type object_state =
    | PacketIn of Core.Object.PacketIn.t
    | PacketOut of Core.Object.PacketOut.t
    | Counter of Object.Counter.t
    | Register of Object.Register.t
  [@@deriving yojson]

  let get_object_state (value_sto : Value.t) (value_objectId : Value.t) : object_state
      =
    Spec.Func.find_store_objectState value_sto value_objectId
    |> unwrap_extern_v |> object_state_of_yojson |> Result.get_ok

  let get_packet_in (value_sto : Value.t) : Core.Object.PacketIn.t =
    let value_objectId = wrap_list_v "id" [ wrap_text_v "packet_in" ] in
    match get_object_state value_sto value_objectId with
    | PacketIn packet_in -> packet_in
    | _ -> error_no_region "packet_in object not found"

  let get_packet_out (value_sto : Value.t) : Core.Object.PacketOut.t =
    let value_objectId = wrap_list_v "id" [ wrap_text_v "packet_out" ] in
    match get_object_state value_sto value_objectId with
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
    let value_ctx, value_sto, value_name_func, value_names_param =
      match values_input with
      | [ value_ctx; value_sto; value_name_func; value_names_param ] ->
          (value_ctx, value_sto, value_name_func, value_names_param)
      | _ ->
          error_no_region
            "unexpected number of arguments to extern function call"
    in
    let name_func = unwrap_text_v value_name_func in
    let names_param =
      value_names_param |> unwrap_list_v |> List.map unwrap_text_v
    in
    let value_ctx, value_sto, value_callResult =
      match (name_func, names_param) with
      | "verify", [ "check"; "toSignal" ] ->
          Core.Func.verify value_ctx value_sto
      | "digest", [ "receiver"; "data" ] -> Func.digest value_ctx value_sto
      | "mark_to_drop", [ "standard_metadata" ] ->
          Func.mark_to_drop value_ctx value_sto
      | "verify_checksum", [ "condition"; "data"; "checksum"; "algo" ] ->
          Func.verify_checksum value_ctx value_sto
      | ( "verify_checksum_with_payload",
          [ "condition"; "data"; "checksum"; "algo" ] ) ->
          let packet_in = get_packet_in value_sto in
          Func.verify_checksum_with_payload value_ctx value_sto packet_in
      | "update_checksum", [ "condition"; "data"; "checksum"; "algo" ] ->
          Func.update_checksum value_ctx value_sto
      | ( "update_checksum_with_payload",
          [ "condition"; "data"; "checksum"; "algo" ] ) ->
          let packet_in = get_packet_in value_sto in
          Func.update_checksum_with_payload value_ctx value_sto packet_in
      | "clone_preserving_field_list", [ "type"; "session"; "index" ] ->
          Func.clone_preserving_field_list value_ctx value_sto
          (* TODO: when to resolve port id? *)
      | "resubmit_preserving_field_list", [ "index" ] ->
          Func.resubmit_preserving_field_list value_ctx value_sto
      | "hash", [ "result"; "algo"; "base"; "data"; "max" ] ->
          Func.hash value_ctx value_sto
      | "log_msg", [ "msg" ] -> Func.log_msg value_ctx value_sto
      | "log_msg", [ "msg"; "data" ] -> Func.log_msg_format value_ctx value_sto
      | _ ->
          error_no_region
            ("unsupported extern function call: " ^ name_func ^ "("
            ^ String.concat ", " names_param
            ^ ")")
    in
    [ value_ctx; value_sto; value_callResult ]

  let eval_extern_method_call (values_input : Value.t list) : Value.t list =
    let ( value_ctx,
          value_sto,
          value_objectId,
          value_name_method,
          value_names_param ) =
      match values_input with
      | [
       value_ctx;
       value_sto;
       value_objectId;
       value_name_method;
       value_names_param;
      ] ->
          ( value_ctx,
            value_sto,
            value_objectId,
            value_name_method,
            value_names_param )
      | _ ->
          error_no_region "unexpected number of arguments to extern method call"
    in
    let obj = get_object_state value_sto value_objectId in
    let name_method = unwrap_text_v value_name_method in
    let names_param =
      value_names_param |> unwrap_list_v |> List.map unwrap_text_v
    in
    let obj, value_ctx, value_sto, value_callResult =
      match (obj, name_method, names_param) with
      | PacketIn packet_in, "extract", [ "hdr" ] ->
          let packet_in, value_ctx, value_sto, value_callResult =
            Core.Object.PacketIn.extract value_ctx value_sto packet_in
          in
          let packet_in = PacketIn packet_in in
          (packet_in, value_ctx, value_sto, value_callResult)
      | ( PacketIn packet_in,
          "extract",
          [ "variableSizeHeader"; "variableFieldSizeInBits" ] ) ->
          let packet_in, value_ctx, value_sto, value_callResult =
            Core.Object.PacketIn.extract_varsize value_ctx value_sto packet_in
          in
          let packet_in = PacketIn packet_in in
          (packet_in, value_ctx, value_sto, value_callResult)
      | PacketIn packet_in, "lookahead", [] ->
          let packet_in, value_ctx, value_sto, value_callResult =
            Core.Object.PacketIn.lookahead value_ctx value_sto packet_in
          in
          let packet_in = PacketIn packet_in in
          (packet_in, value_ctx, value_sto, value_callResult)
      | PacketIn packet_in, "advance", [ "sizeInBits" ] ->
          let packet_in, value_ctx, value_sto, value_callResult =
            Core.Object.PacketIn.advance value_ctx value_sto packet_in
          in
          let packet_in = PacketIn packet_in in
          (packet_in, value_ctx, value_sto, value_callResult)
      | PacketIn packet_in, "length", [] ->
          let packet_in, value_ctx, value_sto, value_callResult =
            Core.Object.PacketIn.length value_ctx value_sto packet_in
          in
          let packet_in = PacketIn packet_in in
          (packet_in, value_ctx, value_sto, value_callResult)
      | PacketOut packet_out, "emit", [ "hdr" ] ->
          let packet_out, value_ctx, value_sto, value_callResult =
            Core.Object.PacketOut.emit value_ctx value_sto packet_out
          in
          let packet_out = PacketOut packet_out in
          (packet_out, value_ctx, value_sto, value_callResult)
      | Counter counter, "count", [ "index" ] ->
          let packet_in = get_packet_in value_sto in
          let counter, value_ctx, value_sto, value_callResult =
            Object.Counter.count value_ctx value_sto packet_in counter
          in
          let counter = Counter counter in
          (counter, value_ctx, value_sto, value_callResult)
      | Register register, "read", [ "result"; "index" ] ->
          let register, value_ctx, value_sto, value_callResult =
            Object.Register.read value_ctx value_sto register
          in
          let register = Register register in
          (register, value_ctx, value_sto, value_callResult)
      | Register register, "write", [ "index"; "value" ] ->
          let register, value_ctx, value_sto, value_callResult =
            Object.Register.write value_ctx value_sto register
          in
          let register = Register register in
          (register, value_ctx, value_sto, value_callResult)
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
    let value_sto =
      Spec.Func.update_store_objectState value_sto value_objectId value_obj
    in
    [ value_ctx; value_sto; value_callResult ]

  (* Match-action table interface *)

  let find_table (value_sto : Value.t) (value_tableName : Value.t) : Value.t =
    let table_name = unwrap_text_v value_tableName in
    match String.split_on_char '.' table_name with
    | [] -> assert false
    | [ table_name_unqualified ] ->
        let value_tableName_unqualified = wrap_text_v table_name_unqualified in
        Spec.Func.find_store_unqualified value_sto value_tableName_unqualified
    | names ->
        let values_name = List.map wrap_text_v names in
        let value_objectId = wrap_list_v "nameIR" values_name in
        Spec.Func.find_store_qualified value_sto value_objectId

  let update_table (value_sto : Value.t) (value_tableName : Value.t)
      (value_tableObject : Value.t) : Value.t =
    let table_name = unwrap_text_v value_tableName in
    match String.split_on_char '.' table_name with
    | [] -> assert false
    | [ table_name_unqualified ] ->
        let value_tableName_unqualified = wrap_text_v table_name_unqualified in
        Spec.Func.update_store_unqualified value_sto value_tableName_unqualified
          value_tableObject
    | names ->
        let values_name = List.map wrap_text_v names in
        let value_objectId = wrap_list_v "nameIR" values_name in
        Spec.Func.update_store_qualified value_sto value_objectId
          value_tableObject

  let table_add_entry (value_sto : Value.t) (value_tableName : Value.t)
      (value_tableEntryPriorityInterface : Value.t)
      (value_tableKeysetInterface : Value.t)
      (value_tableActionInterface : Value.t) : Value.t =
    (* Lookup table object *)
    let value_tableObject = find_table value_sto value_tableName in
    (* Add entry to table object *)
    let value_tableObject =
      Spec.Func.tableObject_add_entry value_tableObject
        value_tableEntryPriorityInterface value_tableKeysetInterface
        value_tableActionInterface
    in
    (* Update store with modified table object *)
    update_table value_sto value_tableName value_tableObject

  let table_add_default_action (value_sto : Value.t) (value_tableName : Value.t)
      (value_tableActionInterface : Value.t) : Value.t =
    (* Lookup table object *)
    let value_tableObject = find_table value_sto value_tableName in
    (* Add entry to table object *)
    let value_tableObject =
      Spec.Func.tableObject_add_default_action value_tableObject
        value_tableActionInterface
    in
    (* Update store with modified table object *)
    update_table value_sto value_tableName value_tableObject

  let add_mirror_session (value_sto : Value.t) (session : int) (port : int) :
      Value.t =
    let arch_state =
      value_sto |> Spec.Func.find_store_archState |> ArchState.of_value
    in
    let mirror_tbl = MirrorTable.add session port arch_state.mirror_tbl in
    arch_state
    |> ArchState.with_mirror_tbl mirror_tbl
    |> ArchState.to_value
    |> Spec.Func.update_store_archState value_sto

  (* pipe_ctx helpers *)

  type 'a pipe_ctx = (Value.t * Value.t * IO.tx list, 'a) OptionState.t

  let get_arch_state : ArchState.t pipe_ctx =
    let+ _, value_sto, _ = get in
    value_sto |> Spec.Func.find_store_archState |> ArchState.of_value

  let get_ctx : Value.t pipe_ctx =
    let+ value_ctx, _, _ = get in
    value_ctx

  let get_sto : Value.t pipe_ctx =
    let+ _, value_sto, _ = get in
    value_sto

  let put_ctx (ctx : Value.t) : unit pipe_ctx =
    modify (fun (_, sto, txs) -> (ctx, sto, txs))

  let put_sto (sto : Value.t) : unit pipe_ctx =
    modify (fun (ctx, _, txs) -> (ctx, sto, txs))

  let get_ctx_sto : (Value.t * Value.t) pipe_ctx =
    let+ value_ctx, value_sto, _ = get in
    (value_ctx, value_sto)

  let put_ctx_sto (ctx : Value.t) (sto : Value.t) : unit pipe_ctx =
    put_ctx ctx >> put_sto sto

  let modify_sto (f : Value.t -> Value.t) : unit pipe_ctx =
    modify (fun (ctx, sto, txs) -> (ctx, f sto, txs))

  let put_arch_state (arch_state : ArchState.t) : unit pipe_ctx =
    modify_sto (fun value_sto ->
        arch_state |> ArchState.to_value
        |> Spec.Func.update_store_archState value_sto)

  let produce_tx (tx : IO.tx) : unit pipe_ctx =
    modify (fun (ctx, sto, txs) -> (ctx, sto, tx :: txs))

  let get_drop : bool pipe_ctx =
    let+ value_ctx, value_sto = get_ctx_sto in
    let value_egress_spec =
      Spec.Rel.lvalue_read_dot_global value_ctx value_sto "standard_metadata"
        "egress_spec"
    in
    let width_egress_spec, int_egress_spec =
      unpack_p4_fixedBit value_egress_spec
    in
    Bigint.(width_egress_spec = of_int 9 && int_egress_spec = of_int 511)

  let with_pipe_ctx (f : Value.t -> Value.t -> Value.t * Value.t * Value.t) :
      Value.t pipe_ctx =
    let* value_ctx, value_sto = get_ctx_sto in
    let value_ctx, value_sto, value_callResult = f value_ctx value_sto in
    let+ _ = put_ctx_sto value_ctx value_sto in
    value_callResult

  let with_packet (packet : Packet.t) : unit pipe_ctx =
    let { packet_in; value_ctx; _ } : Packet.t = packet in
    let packet_in = PacketIn packet_in in
    let value_objectId = wrap_list_v "id" [ wrap_text_v "packet_in" ] in
    let value_packet_in =
      packet_in |> object_state_to_yojson |> wrap_extern_v "objectState"
    in
    put_ctx value_ctx
    >> modify_sto (fun value_sto ->
        Spec.Func.update_store_objectState value_sto value_objectId
          value_packet_in)

  let reset_packet_in : unit pipe_ctx =
    let* value_sto = get_sto in
    let value_sto =
      let packet_in =
        value_sto |> get_packet_in |> Core.Object.PacketIn.reset
      in
      let packet_in = PacketIn packet_in in
      let value_objectId = wrap_list_v "id" [ wrap_text_v "packet_in" ] in
      let value_packet_in =
        packet_in |> object_state_to_yojson |> wrap_extern_v "objectState"
      in
      Spec.Func.update_store_objectState value_sto value_objectId
        value_packet_in
    in
    put_sto value_sto

  let reset_packet_out : unit pipe_ctx =
    let* value_sto = get_sto in
    let value_sto =
      let packet_out = Core.Object.PacketOut.init () in
      let packet_out = PacketOut packet_out in
      let value_objectId = wrap_list_v "id" [ wrap_text_v "packet_out" ] in
      let value_packet_out =
        packet_out |> object_state_to_yojson |> wrap_extern_v "objectState"
      in
      Spec.Func.update_store_objectState value_sto value_objectId
        value_packet_out
    in
    put_sto value_sto

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
    | Pass [ value_ctx; value_sto ] -> (value_ctx, value_sto)
    | Pass _ -> error_no_region "unexpected return from V1Model_init"
    | Fail (`Syntax (at, msg)) | Fail (`Runtime (at, msg)) -> error at msg

  (* Pipeline driver *)

  let setup_rx (rx : IO.rx) : unit pipe_ctx =
    let port_in, packet_in = rx in
    (* Setup packet_in object *)
    let packet_in = PacketIn (Core.Object.PacketIn.init packet_in) in
    let packet_in_state = object_state_to_yojson packet_in in
    let value_packet_in_state = wrap_extern_v "objectState" packet_in_state in
    let* value_ctx, value_sto = get_ctx_sto in
    let value_ctx, value_sto =
      Spec.Rel.v1model_init_packet_in value_ctx value_sto value_packet_in_state
    in
    (* Setup packet_out object *)
    let packet_out = PacketOut (Core.Object.PacketOut.init ()) in
    let packet_out_state = object_state_to_yojson packet_out in
    let value_packet_out_state = wrap_extern_v "objectState" packet_out_state in
    let value_ctx, value_sto =
      Spec.Rel.v1model_init_packet_out value_ctx value_sto
        value_packet_out_state
    in
    (* Setup global variables *)
    let value_ctx = Spec.Rel.v1model_init_globals value_ctx value_sto port_in in
    put_ctx_sto value_ctx value_sto

  (* capture current
   * 1. evaluation context
   * 2. packet_in
   * and push to queue
   *)
  let schedule_packet (entrypoint : Packet.entrypoint) : unit pipe_ctx =
    let* value_ctx, value_sto = get_ctx_sto in
    let packet_in = get_packet_in value_sto in
    let packet : Packet.t = { value_ctx; packet_in; entrypoint } in
    let* arch_state = get_arch_state in
    let queue =
      match entrypoint with
      | Ingress -> Scheduler.push_front packet arch_state.queue
      | Egress -> Scheduler.push_back packet arch_state.queue
    in
    arch_state |> ArchState.with_queue queue |> put_arch_state

  let prepare_resubmit_ctx (index : int) : unit pipe_ctx =
    let* value_ctx, value_sto = get_ctx_sto in
    let value_ctx =
      Spec.Rel.v1model_setup_preserved_meta_fields value_ctx value_sto
        (Packet.ResubmitInfo.to_v index)
    in
    (* Set standard_metadata.instance_type as 6 *)
    let value_ctx =
      let value_instance_type =
        Interface.Pack.pack_p4_fixedBit (Bigint.of_int 32) (Bigint.of_int 6)
      in
      Spec.Rel.lvalue_write_dot_global value_ctx value_sto "standard_metadata"
        "instance_type" value_instance_type
    in
    put_ctx value_ctx

  let prepare_clone_ctx (clone_type : Packet.CloneInfo.clone_type) (port : int)
      (index : int) : unit pipe_ctx =
    let* value_ctx, value_sto = get_ctx_sto in
    let value_index =
      pack_p4_fixedBit (Bigint.of_int 8) (Bigint.of_int index)
    in
    let value_ctx =
      Spec.Rel.v1model_setup_preserved_meta_fields value_ctx value_sto
        value_index
    in
    let value_ctx =
      let instance_type = match clone_type with I2E -> 1 | E2E -> 2 in
      let value_instance_type =
        Interface.Pack.pack_p4_fixedBit (Bigint.of_int 32)
          (Bigint.of_int instance_type)
      in
      Spec.Rel.lvalue_write_dot_global value_ctx value_sto "standard_metadata"
        "instance_type" value_instance_type
    in
    let value_ctx =
      let value_egress_spec =
        Interface.Pack.pack_p4_fixedBit (Bigint.of_int 9) (Bigint.of_int port)
      in
      Spec.Rel.lvalue_write_dot_global value_ctx value_sto "standard_metadata"
        "egress_spec" value_egress_spec
    in
    put_ctx value_ctx

  let drive_p : unit pipe_ctx =
    let* value_parser_result = with_pipe_ctx Spec.Rel.v1model_parser in
    let* value_ctx, value_sto = get_ctx_sto in
    let value_ctx =
      match flatten_case_v_opt value_parser_result with
      | Some (_, [ [ "REJECT" ]; [] ], [ value_error ]) ->
          Spec.Rel.lvalue_write_dot_global value_ctx value_sto
            "standard_metadata" "parser_error" value_error
      | Some _ -> value_ctx
      | None -> assert false
    in
    put_ctx_sto value_ctx value_sto

  let drive_vr : Value.t pipe_ctx = with_pipe_ctx Spec.Rel.v1model_verify

  let drive_pipe_pre : Value.t pipe_ctx =
    let* arch_state = get_arch_state in
    put_arch_state (ArchState.reset arch_state)
    >> reset_packet_in
    >> drive_p
    >> drive_vr

  let schedule_clone (arch_state : ArchState.t) : bool pipe_ctx =
    let open ArchState in
    match arch_state.clone_opt with
    | Some (clone_type, session, field_index) -> (
        match MirrorTable.find_opt session arch_state.mirror_tbl with
        | Some port ->
            let* value_ctx_original = get_ctx in
            prepare_clone_ctx clone_type port field_index
            >> drive_pipe_pre
            >> schedule_packet Egress
            >> put_ctx value_ctx_original
            >> return true
        | None -> return false)
    | _ -> return false

  let schedule_resubmit (arch_state : ArchState.t) : bool pipe_ctx =
    let open ArchState in
    match arch_state.resubmit_opt with
    | None -> return false
    | Some field_index ->
        let* value_ctx_original = get_ctx in
        prepare_resubmit_ctx field_index
        >> drive_pipe_pre
        >> schedule_packet Ingress
        >> put_ctx value_ctx_original
        >> return true

  (* Ingress block + Handle clone, resubmit, drop *)
  let drive_ig : Value.t pipe_ctx =
    let* result = with_pipe_ctx Spec.Rel.v1model_ingress in
    let* arch_state = get_arch_state in
    let* _cloned = schedule_clone arch_state in
    let* resubmitted = schedule_resubmit arch_state in
    if resubmitted then return result
    else
      let* drop = get_drop in
      if drop then
        return result
      else
        schedule_packet Egress
        >> return result

  (* Egress block + Handle clone *)
  let drive_eg : Value.t pipe_ctx =
    let* result = with_pipe_ctx Spec.Rel.v1model_egress in
    let* arch_state = get_arch_state in
    let* _cloned = schedule_clone arch_state in
    let* drop = get_drop in
    guard (not drop)
    >> return result

  let drive_ck : Value.t pipe_ctx = with_pipe_ctx Spec.Rel.v1model_check
  let drive_dep : Value.t pipe_ctx = with_pipe_ctx Spec.Rel.v1model_deparse

  let drive_pipe_post : Value.t pipe_ctx =
    let* result = drive_ck >> reset_packet_out >> drive_dep in
    let* value_ctx, value_sto = get_ctx_sto in
    let value_egress_spec =
      Spec.Rel.lvalue_read_dot_global value_ctx value_sto "standard_metadata"
        "egress_spec"
    in
    let _, int_egress_spec =
      unpack_p4_fixedBit value_egress_spec
    in
    (* Get egress port *)
    let port = Bigint.to_int_exn int_egress_spec in
    (* Get input packet *)
    let packet_in = get_packet_in value_sto in
    (* Get output packet *)
    let packet_out = get_packet_out value_sto in
    let packet =
      Format.asprintf "%a" Core.Object.Packet.pp (packet_in, packet_out)
    in
    (* Return port and packet *)
    let tx = (port, packet) in
    produce_tx tx
    >> return result

  let drive_packet (packet : Packet.t) : unit pipe_ctx =
    match packet.entrypoint with
    | Ingress -> with_packet packet >> drive_ig >> return ()
    | Egress -> with_packet packet >> drive_eg >> drive_pipe_post >> return ()

  let rec run_scheduler () : unit pipe_ctx =
    let* arch_state = get_arch_state in
    match Scheduler.pop_front_opt arch_state.queue with
    | None -> empty
    | Some (packet, queue) ->
        ArchState.(arch_state |> reset |> with_queue queue)
        |> put_arch_state >> drive_packet packet >> run_scheduler ()

  let drive_pipe (value_ctx : Value.t) (value_sto : Value.t) (rx : IO.rx) :
      Value.t * Value.t * IO.tx list =
    let pipe_ctx = (value_ctx, value_sto, []) in
    let pipe : unit pipe_ctx =
      (* Setup port and packet *)
      setup_rx rx
      >> drive_pipe_pre
      >> schedule_packet Ingress
      >> run_scheduler ()
    in
    let _, (value_ctx, value_sto, txs) = OptionState.run pipe pipe_ctx in
    (value_ctx, value_sto, List.rev txs)

  (* Initializer *)

  let init (mode_ : Sim.mode) : unit =
    init_mode mode_;
    init_call_rel ();
    init_call_func ()
end
