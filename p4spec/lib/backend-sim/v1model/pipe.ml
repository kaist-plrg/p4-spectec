open Lang
open Interface.Wrap
open Interface.Unwrap
open Interface.Unpack
open Interface.Flatten
module ArchState = Arch_state
module OptionState = Util.Option_state
module Value = Runtime.Sim.Value
module IO = Runtime.Sim.Io
module Sim = Runtime.Sim.Simulator
open Error

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

  type 'a pipe_ctx = (Value.t * Value.t, 'a) OptionState.t

  (* Extern objects *)

  let empty_arch_state = ArchState.empty |> ArchState.to_value

  type extern =
    | PacketIn of Core.Object.PacketIn.t
    | PacketOut of Core.Object.PacketOut.t
    | Counter of Object.Counter.t
    | Register of Object.Register.t
  [@@deriving yojson]

  let get_arch_state : ArchState.t pipe_ctx =
    let open OptionState in
    let* _, value_sto = get in
    value_sto |> Spec.Func.find_store_archState |> unwrap_extern_v
    |> ArchState.of_yojson |> Result.get_ok |> return

  let get_object_state (value_sto : Value.t) (value_objectId : Value.t) : extern =
    Spec.Func.find_store_objectState value_sto value_objectId
    |> unwrap_extern_v |> extern_of_yojson |> Result.get_ok

  let get_packet_in (value_sto : Value.t) : Core.Object.PacketIn.t =
    let value_objectId = wrap_list_v "id" [ wrap_text_v "packet_in" ] in
    match get_object_state value_sto value_objectId with
    | PacketIn packet_in -> packet_in
    | _ -> error_no_region "packet_in extern not found"

  let get_packet_out (value_sto : Value.t) : Core.Object.PacketOut.t =
    let value_objectId = wrap_list_v "id" [ wrap_text_v "packet_out" ] in
    match get_object_state value_sto value_objectId with
    | PacketOut packet_out -> packet_out
    | _ -> error_no_region "packet_out extern not found"

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
        counter |> extern_to_yojson |> wrap_extern_v "objectState"
    | "register" ->
        let register = Object.Register.init value_type_args value_args in
        let register = Register register in
        register |> extern_to_yojson |> wrap_extern_v "objectState"
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
    let extern = get_object_state value_sto value_objectId in
    let name_method = unwrap_text_v value_name_method in
    let names_param =
      value_names_param |> unwrap_list_v |> List.map unwrap_text_v
    in
    let extern, value_ctx, value_sto, value_callResult =
      match (extern, name_method, names_param) with
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
    let value_extern =
      extern |> extern_to_yojson |> wrap_extern_v "objectState"
    in
    let value_sto =
      Spec.Func.update_store_objectState value_sto value_objectId value_extern
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

  let with_pipe_ctx (f : Value.t -> Value.t -> Value.t * Value.t * Value.t) :
      Value.t pipe_ctx =
    let open OptionState in
    let* value_ctx, value_sto = get in
    let value_ctx, value_sto, value_callResult = f value_ctx value_sto in
    let* _ = put (value_ctx, value_sto) in
    return value_callResult

  let put_ctx (ctx : Value.t) : unit pipe_ctx =
    let open OptionState in
    modify (fun (_, sto) -> (ctx, sto))

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
    let open OptionState in
    let port_in, packet_in = rx in
    (* Setup packet_in extern *)
    let packet_in = PacketIn (Core.Object.PacketIn.init packet_in) in
    let packet_in_state = extern_to_yojson packet_in in
    let value_packet_in_state = wrap_extern_v "objectState" packet_in_state in
    let* value_ctx, value_sto = get in
    let value_ctx, value_sto =
      Spec.Rel.v1model_init_packet_in value_ctx value_sto value_packet_in_state
    in
    (* Setup packet_out extern *)
    let packet_out = PacketOut (Core.Object.PacketOut.init ()) in
    let packet_out_state = extern_to_yojson packet_out in
    let value_packet_out_state = wrap_extern_v "objectState" packet_out_state in
    let value_ctx, value_sto =
      Spec.Rel.v1model_init_packet_out value_ctx value_sto
        value_packet_out_state
    in
    (* Setup global variables *)
    let value_ctx = Spec.Rel.v1model_init_globals value_ctx value_sto port_in in
    put (value_ctx, value_sto)

  let drive_p : unit pipe_ctx =
    let open OptionState in
    let* value_parser_result = with_pipe_ctx Spec.Rel.v1model_parser in
    let* value_ctx, value_sto = get in
    let value_ctx =
      match flatten_case_v_opt value_parser_result with
      | Some (_, [ [ "REJECT" ]; [] ], [ value_error ]) ->
          Spec.Rel.lvalue_write_dot_global value_ctx value_sto
            "standard_metadata" "parser_error" value_error
      | Some _ -> value_ctx
      | None -> assert false
    in
    put (value_ctx, value_sto)

  let drive_vr : Value.t pipe_ctx = with_pipe_ctx Spec.Rel.v1model_verify
  let drive_ig : Value.t pipe_ctx = with_pipe_ctx Spec.Rel.v1model_ingress
  let drive_eg : Value.t pipe_ctx = with_pipe_ctx Spec.Rel.v1model_egress
  let drive_ck : Value.t pipe_ctx = with_pipe_ctx Spec.Rel.v1model_check
  let drive_dep : Value.t pipe_ctx = with_pipe_ctx Spec.Rel.v1model_deparse

  let resulting_port_packet : IO.tx pipe_ctx =
    let open OptionState in
    let* value_ctx, value_sto = get in
    let value_egress_spec =
      Spec.Rel.lvalue_read_dot_global value_ctx value_sto "standard_metadata"
        "egress_spec"
    in
    let width_egress_spec, int_egress_spec =
      unpack_p4_fixedBit value_egress_spec
    in
    let drop =
      Bigint.(width_egress_spec = of_int 9 && int_egress_spec = of_int 511)
    in
    let* () = guard (not drop) in
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
    return tx

  let drive_pipe_pre_inner : bool pipe_ctx =
    let open OptionState in
    (* Parser block *)
    let* () = drive_p in
    (* Verify block *)
    let* _value_verify_result = drive_vr in
    (* Ingress block *)
    let* _value_verify_result = drive_ig in
    (* Check if packet should be dropped *)
    let* value_ctx, value_sto = get in
    let drop =
      let value_egress_spec =
        Spec.Rel.lvalue_read_dot_global value_ctx value_sto "standard_metadata"
          "egress_spec"
      in
      let width_egress_spec, int_egress_spec =
        unpack_p4_fixedBit value_egress_spec
      in
      Bigint.(width_egress_spec = of_int 9 && int_egress_spec = of_int 511)
    in
    return drop

  let rec drive_pipe_pre () : unit pipe_ctx =
    let open OptionState in
    let* drop = drive_pipe_pre_inner in
    let* arch_state = get_arch_state in
    match arch_state.resubmit_opt with
    | None -> guard (not drop)
    | Some value_index ->
        let* value_ctx, value_sto = get in
        let value_ctx =
          Spec.Rel.v1model_setup_preserved_meta_fields value_ctx value_sto
            value_index
        in
        (* Set standard_metadata.instance_type as 6 *)
        let value_ctx =
          let value_instance_type =
            Interface.Pack.pack_p4_fixedBit (Bigint.of_int 32) (Bigint.of_int 6)
          in
          Spec.Rel.lvalue_write_dot_global value_ctx value_sto
            "standard_metadata" "instance_type" value_instance_type
        in
        (* Reset submit info to none *)
        let value_arch_state =
          ArchState.(arch_state |> with_resubmit_opt None |> to_value)
        in
        let value_sto =
          Spec.Func.update_store_archState value_sto value_arch_state
        in
        (* Reset packet_in extern in store *)
        let value_sto =
          let packet_in = get_packet_in value_sto in
          let packet_in = Core.Object.PacketIn.reset packet_in in
          let packet_in = PacketIn packet_in in
          let value_objectId = wrap_list_v "id" [ wrap_text_v "packet_in" ] in
          let value_packet_in =
            packet_in |> extern_to_yojson |> wrap_extern_v "externState"
          in
          Spec.Func.update_store_objectState value_sto value_objectId
            value_packet_in
        in
        let* _ = put (value_ctx, value_sto) in
        drive_pipe_pre ()

  let drive_pipe_post : IO.tx pipe_ctx =
    let open OptionState in
    (* Egress block *)
    let* _value_verify_result = drive_eg in
    (* Check block *)
    let* _value_check_result = drive_ck in
    (* Deparser block *)
    let* _value_deparse_result = drive_dep in
    (* Get resulting port and packet *)
    resulting_port_packet

  let drive_pipe (value_ctx : Value.t) (value_sto : Value.t) (rx : IO.rx) :
      Value.t * Value.t * IO.tx option =
    let pipe_ctx = (value_ctx, value_sto) in
    let pipe : IO.tx pipe_ctx = 
      let open OptionState in
      (* Setup port and packet *)
      let* () = setup_rx rx in
      let* () = drive_pipe_pre () in
      drive_pipe_post
    in
    let result, (value_ctx, value_sto) = OptionState.run pipe pipe_ctx in
    (value_ctx, value_sto, result)

  (* Initializer *)

  let init (mode_ : Sim.mode) : unit =
    init_mode mode_;
    init_call_rel ();
    init_call_func ()
end
