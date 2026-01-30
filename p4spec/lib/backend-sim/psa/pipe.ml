open Lang
open Interface.Wrap
open Interface.Unwrap
open Interface.Unpack
open Interface.Flatten
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

  (* Extern objects *)

  type extern =
    | PacketIn of Core.Object.PacketIn.t
    | PacketOut of Core.Object.PacketOut.t
    | Counter of Object.Counter.t
  [@@deriving yojson]

  let get_extern (value_sto : Value.t) (value_objectId : Value.t) : extern =
    Spec.Func.find_store_externState value_sto value_objectId
    |> unwrap_extern_v |> extern_of_yojson |> Result.get_ok

  let get_ingress_packet_in (value_sto : Value.t) : Core.Object.PacketIn.t =
    let value_objectId = wrap_list_v "id" [ wrap_text_v "ingress_packet_in" ] in
    match get_extern value_sto value_objectId with
    | PacketIn packet_in -> packet_in
    | _ -> error_no_region "ingress_packet_in extern not found"

  let get_ingress_packet_out (value_sto : Value.t) : Core.Object.PacketOut.t =
    let value_objectId =
      wrap_list_v "id" [ wrap_text_v "ingress_packet_out" ]
    in
    match get_extern value_sto value_objectId with
    | PacketOut packet_out -> packet_out
    | _ -> error_no_region "ingress_packet_out extern not found"

  let get_egress_packet_in (value_sto : Value.t) : Core.Object.PacketIn.t =
    let value_objectId = wrap_list_v "id" [ wrap_text_v "egress_packet_in" ] in
    match get_extern value_sto value_objectId with
    | PacketIn packet_in -> packet_in
    | _ -> error_no_region "egress_packet_in extern not found"

  let get_egress_packet_out (value_sto : Value.t) : Core.Object.PacketOut.t =
    let value_objectId = wrap_list_v "id" [ wrap_text_v "egress_packet_out" ] in
    match get_extern value_sto value_objectId with
    | PacketOut packet_out -> packet_out
    | _ -> error_no_region "egress_packet_out extern not found"

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
        counter |> extern_to_yojson |> wrap_extern_v "externState"
    | _ -> wrap_extern_v "externState" `Null

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
    let ( _value_ctx_caller,
          value_ctx,
          value_sto,
          value_name_func,
          value_names_param ) =
      match values_input with
      | [
       value_ctx_caller;
       value_ctx;
       value_sto;
       value_name_func;
       value_names_param;
      ] ->
          ( value_ctx_caller,
            value_ctx,
            value_sto,
            value_name_func,
            value_names_param )
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
    let extern = get_extern value_sto value_objectId in
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
          let counter, value_ctx, value_sto, value_callResult =
            Object.Counter.count value_ctx value_sto counter
          in
          let counter = Counter counter in
          (counter, value_ctx, value_sto, value_callResult)
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
      extern |> extern_to_yojson |> wrap_extern_v "externState"
    in
    let value_sto =
      Spec.Func.update_store_externState value_sto value_objectId value_extern
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
    | Pass [ value_ctx; value_sto ] -> (value_ctx, value_sto)
    | Pass _ -> error_no_region "unexpected return from PSA_init"
    | Fail (`Syntax (at, msg)) | Fail (`Runtime (at, msg)) -> error at msg

  (* Pipeline driver *)

  let setup_ingress_pipe (value_ctx : Value.t) (value_sto : Value.t)
      (rx : IO.rx) : Value.t * Value.t =
    let port_in, packet_in = rx in
    (* Setup packet_in extern *)
    let packet_in = PacketIn (Core.Object.PacketIn.init packet_in) in
    let packet_in_state = extern_to_yojson packet_in in
    let value_packet_in_state = wrap_extern_v "externState" packet_in_state in
    let value_ctx, value_sto =
      Spec.Rel.psa_ingress_init_packet_in value_ctx value_sto
        value_packet_in_state
    in
    (* Setup packet_out extern *)
    let packet_out = PacketOut (Core.Object.PacketOut.init ()) in
    let packet_out_state = extern_to_yojson packet_out in
    let value_packet_out_state = wrap_extern_v "externState" packet_out_state in
    let value_ctx, value_sto =
      Spec.Rel.psa_ingress_init_packet_out value_ctx value_sto
        value_packet_out_state
    in
    (* Setup global variables *)
    let value_ctx =
      Spec.Rel.psa_ingress_init_globals value_ctx value_sto port_in
    in
    (value_ctx, value_sto)

  let drive_ip (value_ctx : Value.t) (value_sto : Value.t) : Value.t * Value.t =
    let value_ctx, value_sto, value_parser_result =
      Spec.Rel.psa_ingress_parser value_ctx value_sto
    in
    let value_ctx =
      match flatten_case_v_opt value_parser_result with
      | Some (_, [ [ "REJECT" ]; [] ], [ value_error ]) ->
          Spec.Rel.lvalue_write_dot_global value_ctx value_sto
            "ingress_input_metadata" "parser_error" value_error
      | Some _ -> value_ctx
      | None -> assert false
    in
    (value_ctx, value_sto)

  let drive_ig (value_ctx : Value.t) (value_sto : Value.t) :
      Value.t * Value.t * Value.t =
    Spec.Rel.psa_ingress value_ctx value_sto

  let drive_id (value_ctx : Value.t) (value_sto : Value.t) :
      Value.t * Value.t * Value.t =
    Spec.Rel.psa_ingress_deparser value_ctx value_sto

  let ingress_resulting_port_packet (value_ctx : Value.t) (value_sto : Value.t)
      : IO.tx option =
    let value_drop =
      Spec.Rel.lvalue_read_dot_global value_ctx value_sto
        "ingress_output_metadata" "drop"
    in
    let drop = unpack_p4_bool value_drop in
    if drop then None
    else
      (* Get egress port *)
      let value_egress_port =
        Spec.Rel.lvalue_read_dot_global value_ctx value_sto
          "ingress_output_metadata" "egress_port"
      in
      let _, int_egress_port = unpack_p4_fixedBit value_egress_port in
      let port = Bigint.to_int_exn int_egress_port in
      (* Get input packet *)
      let packet_in = get_ingress_packet_in value_sto in
      (* Get output packet *)
      let packet_out = get_ingress_packet_out value_sto in
      let packet =
        Format.asprintf "%a" Core.Object.Packet.pp (packet_in, packet_out)
      in
      (* Return port and packet *)
      let tx = (port, packet) in
      Some tx

  let drive_ingress_pipe (value_ctx : Value.t) (value_sto : Value.t) :
      Value.t * Value.t * IO.tx option =
    (* Ingress parser block *)
    let value_ctx, value_sto = drive_ip value_ctx value_sto in
    (* Ingress block *)
    let value_ctx, value_sto, _value_result = drive_ig value_ctx value_sto in
    (* Ingress deparser block *)
    let value_ctx, value_sto, _value_result = drive_id value_ctx value_sto in
    (* Get resulting port and packet *)
    let result_opt = ingress_resulting_port_packet value_ctx value_sto in
    (value_ctx, value_sto, result_opt)

  let setup_egress_pipe (value_ctx : Value.t) (value_sto : Value.t) (rx : IO.rx)
      : Value.t * Value.t =
    let port_in, packet_in = rx in
    (* Setup packet_in extern *)
    let packet_in = PacketIn (Core.Object.PacketIn.init packet_in) in
    let packet_in_state = extern_to_yojson packet_in in
    let value_packet_in_state = wrap_extern_v "externState" packet_in_state in
    let value_ctx, value_sto =
      Spec.Rel.psa_egress_init_packet_in value_ctx value_sto
        value_packet_in_state
    in
    (* Setup packet_out extern *)
    let packet_out = PacketOut (Core.Object.PacketOut.init ()) in
    let packet_out_state = extern_to_yojson packet_out in
    let value_packet_out_state = wrap_extern_v "externState" packet_out_state in
    let value_ctx, value_sto =
      Spec.Rel.psa_egress_init_packet_out value_ctx value_sto
        value_packet_out_state
    in
    (* Setup global variables *)
    let value_ctx =
      Spec.Rel.psa_egress_init_globals value_ctx value_sto port_in
    in
    (value_ctx, value_sto)

  let drive_ep (value_ctx : Value.t) (value_sto : Value.t) : Value.t * Value.t =
    let value_ctx, value_sto, value_parser_result =
      Spec.Rel.psa_egress_parser value_ctx value_sto
    in
    let value_ctx =
      match flatten_case_v_opt value_parser_result with
      | Some (_, [ [ "REJECT" ]; [] ], [ value_error ]) ->
          Spec.Rel.lvalue_write_dot_global value_ctx value_sto
            "egress_input_metadata" "parser_error" value_error
      | Some _ -> value_ctx
      | None -> assert false
    in
    (value_ctx, value_sto)

  let drive_eg (value_ctx : Value.t) (value_sto : Value.t) :
      Value.t * Value.t * Value.t =
    Spec.Rel.psa_egress value_ctx value_sto

  let drive_ed (value_ctx : Value.t) (value_sto : Value.t) :
      Value.t * Value.t * Value.t =
    Spec.Rel.psa_egress_deparser value_ctx value_sto

  let egress_resulting_port_packet (value_ctx : Value.t) (value_sto : Value.t) :
      IO.tx option =
    let value_drop =
      Spec.Rel.lvalue_read_dot_global value_ctx value_sto
        "egress_output_metadata" "drop"
    in
    let drop = unpack_p4_bool value_drop in
    if drop then None
    else
      (* Get egress port *)
      let value_egress_port =
        Spec.Rel.lvalue_read_dot_global value_ctx value_sto
          "ingress_output_metadata" "egress_port"
      in
      let _, int_egress_port = unpack_p4_fixedBit value_egress_port in
      let port = Bigint.to_int_exn int_egress_port in
      (* Get input packet *)
      let packet_in = get_egress_packet_in value_sto in
      (* Get output packet *)
      let packet_out = get_egress_packet_out value_sto in
      let packet =
        Format.asprintf "%a" Core.Object.Packet.pp (packet_in, packet_out)
      in
      (* Return port and packet *)
      let tx = (port, packet) in
      Some tx

  let drive_egress_pipe (value_ctx : Value.t) (value_sto : Value.t) :
      Value.t * Value.t * IO.tx option =
    (* Egress parser block *)
    let value_ctx, value_sto = drive_ep value_ctx value_sto in
    (* Egress block *)
    let value_ctx, value_sto, _value_result = drive_eg value_ctx value_sto in
    (* Egress deparser block *)
    let value_ctx, value_sto, _value_result = drive_ed value_ctx value_sto in
    (* Get resulting port and packet *)
    let result_opt = egress_resulting_port_packet value_ctx value_sto in
    (value_ctx, value_sto, result_opt)

  let drive_pipe (value_ctx : Value.t) (value_sto : Value.t)
      (ingress_rx : IO.rx) : Value.t * Value.t * IO.tx option =
    (* Setup port and packet *)
    let value_ctx, value_sto =
      setup_ingress_pipe value_ctx value_sto ingress_rx
    in
    let value_ctx, value_sto, ingress_tx_opt =
      drive_ingress_pipe value_ctx value_sto
    in
    match ingress_tx_opt with
    | None -> (value_ctx, value_sto, None)
    | Some ingress_tx ->
        let value_ctx, value_sto =
          setup_egress_pipe value_ctx value_sto ingress_tx
        in
        drive_egress_pipe value_ctx value_sto

  (* Initializer *)

  let init (mode_ : Sim.mode) : unit =
    init_mode mode_;
    init_call_rel ();
    init_call_func ()
end
