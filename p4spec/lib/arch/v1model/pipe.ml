open Interface.Wrap
open Interface.Unwrap
open Interface.Unpack
open Interface.Flatten
module Value = Runtime_dynamic.Value
module IO = Runtime_simulator.Io
module Sim = Runtime_simulator.Simulator
open Error

module Make (Interp_IL : Sim.INTERP_IL) (Interp_SL : Sim.INTERP_SL) : Sim.ARCH =
struct
  (* Specification *)

  let spec : Sim.spec ref = ref Sim.Empty
  let init_spec (spec_ : Sim.spec) : unit = spec := spec_

  (* Call entry points *)

  let call_rel (relname : string) (values_input : Value.t list) : Value.t list =
    let result =
      match !spec with
      | IL spec_il -> Interp_IL.eval_rel spec_il relname values_input
      | SL spec_sl -> Interp_SL.eval_rel spec_sl relname values_input
      | Empty -> assert false
    in
    match result with
    | Pass (values_output, _) -> values_output
    | Fail (at, msg, _) -> error at msg

  let init_call_rel () = Spec.Rel.register call_rel

  let call_func (funcname : string) (typs_input : Sl.Ast.typ list)
      (values_input : Value.t list) : Value.t =
    let result =
      match !spec with
      | IL spec_il ->
          Interp_IL.eval_func spec_il funcname typs_input values_input
      | SL spec_sl ->
          Interp_SL.eval_func spec_sl funcname typs_input values_input
      | Empty -> assert false
    in
    match result with
    | Pass (value_output, _) -> value_output
    | Fail (at, msg, _) -> error at msg

  let init_call_func () = Spec.Func.register call_func

  (* Extern objects *)

  type extern =
    | PacketIn of Core.Object.PacketIn.t
    | PacketOut of Core.Object.PacketOut.t
    | Counter of Object.Counter.t
  [@@deriving yojson]

  let get_extern (value_sto : Value.t) (value_oid : Value.t) : extern =
    Spec.Func.find_store_externState value_sto value_oid
    |> unwrap_extern_v |> extern_of_yojson |> Result.get_ok

  let get_packet_in (value_sto : Value.t) : Core.Object.PacketIn.t =
    let value_oid = wrap_list_v "id" [ wrap_text_v "packet_in" ] in
    match get_extern value_sto value_oid with
    | PacketIn packet_in -> packet_in
    | _ -> error_no_region "packet_in extern not found"

  let get_packet_out (value_sto : Value.t) : Core.Object.PacketOut.t =
    let value_oid = wrap_list_v "id" [ wrap_text_v "packet_out" ] in
    match get_extern value_sto value_oid with
    | PacketOut packet_out -> packet_out
    | _ -> error_no_region "packet_out extern not found"

  (* Extern functions *)

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
        counter |> extern_to_yojson |> wrap_extern_v "externState"
    | _ -> wrap_extern_v "externState" `Null

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
      | "hash", [ "result"; "algo"; "base"; "data"; "max" ] ->
          Func.hash value_ctx value_sto
      | _ ->
          error_no_region
            ("unsupported extern function call: " ^ name_func ^ "("
            ^ String.concat ", " names_param
            ^ ")")
    in
    [ value_ctx; value_sto; value_callResult ]

  let eval_extern_method_call (values_input : Value.t list) : Value.t list =
    let value_ctx, value_sto, value_oid, value_name_method, value_names_param =
      match values_input with
      | [
       value_ctx; value_sto; value_oid; value_name_method; value_names_param;
      ] ->
          (value_ctx, value_sto, value_oid, value_name_method, value_names_param)
      | _ ->
          error_no_region "unexpected number of arguments to extern method call"
    in
    let extern = get_extern value_sto value_oid in
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
      | _ ->
          let oid =
            value_oid |> unwrap_list_v |> List.map unwrap_text_v
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
      Spec.Func.update_store_externState value_sto value_oid value_extern
    in
    [ value_ctx; value_sto; value_callResult ]

  (* Pipeline initializer *)

  let init_pipe (spec_ : Sim.spec) (includes_p4 : string list)
      (filename_p4 : string) : Value.t * Value.t =
    init_spec spec_;
    init_call_rel ();
    init_call_func ();
    let result =
      match !spec with
      | IL spec_il ->
          Interp_IL.eval_program spec_il "V1Model_init" includes_p4 filename_p4
      | SL spec_sl ->
          Interp_SL.eval_program ~derive:false spec_sl "V1Model_init"
            includes_p4 filename_p4
      | Empty -> assert false
    in
    match result with
    | Pass ([ value_ctx; value_sto ], _, _, _) -> (value_ctx, value_sto)
    | _ -> error_no_region "unexpected return from V1Model_init"

  (* Pipeline driver *)

  let setup_rx (value_ctx : Value.t) (value_sto : Value.t) (rx : IO.rx) :
      Value.t * Value.t =
    let port_in, packet_in = rx in
    (* Setup packet_in extern *)
    let packet_in = PacketIn (Core.Object.PacketIn.init packet_in) in
    let packet_in_state = extern_to_yojson packet_in in
    let value_packet_in_state = wrap_extern_v "externState" packet_in_state in
    let value_ctx, value_sto =
      Spec.Rel.v1model_init_packet_in value_ctx value_sto value_packet_in_state
    in
    (* Setup packet_out extern *)
    let packet_out = PacketOut (Core.Object.PacketOut.init ()) in
    let packet_out_state = extern_to_yojson packet_out in
    let value_packet_out_state = wrap_extern_v "externState" packet_out_state in
    let value_ctx, value_sto =
      Spec.Rel.v1model_init_packet_out value_ctx value_sto
        value_packet_out_state
    in
    (* Setup global variables *)
    let value_ctx = Spec.Rel.v1model_init_globals value_ctx value_sto port_in in
    (value_ctx, value_sto)

  let drive_p (value_ctx : Value.t) (value_sto : Value.t) : Value.t * Value.t =
    let value_ctx, value_sto, value_parser_result =
      Spec.Rel.v1model_parser value_ctx value_sto
    in
    let value_ctx =
      match flatten_case_v_opt value_parser_result with
      | Some (_, [ [ "REJECT" ]; [] ], [ value_error ]) ->
          Spec.Rel.lvalue_write_dot_global value_ctx value_sto
            "standard_metadata" "parser_error" value_error
      | Some _ -> value_ctx
      | None -> assert false
    in
    (value_ctx, value_sto)

  let drive_vr (value_ctx : Value.t) (value_sto : Value.t) :
      Value.t * Value.t * Value.t =
    Spec.Rel.v1model_verify value_ctx value_sto

  let drive_ig (value_ctx : Value.t) (value_sto : Value.t) :
      Value.t * Value.t * Value.t =
    Spec.Rel.v1model_ingress value_ctx value_sto

  let drive_eg (value_ctx : Value.t) (value_sto : Value.t) :
      Value.t * Value.t * Value.t =
    Spec.Rel.v1model_egress value_ctx value_sto

  let drive_ck (value_ctx : Value.t) (value_sto : Value.t) :
      Value.t * Value.t * Value.t =
    Spec.Rel.v1model_check value_ctx value_sto

  let drive_dep (value_ctx : Value.t) (value_sto : Value.t) :
      Value.t * Value.t * Value.t =
    Spec.Rel.v1model_deparse value_ctx value_sto

  let resulting_port_packet (value_ctx : Value.t) (value_sto : Value.t) :
      IO.tx option =
    (* Get egress port *)
    let port =
      Spec.Rel.lvalue_read_dot_global value_ctx value_sto "standard_metadata"
        "egress_spec"
      |> unpack_p4_fixedBit |> snd |> Bigint.to_int_exn
    in
    (* Get output packet *)
    let header =
      get_packet_out value_sto |> Format.asprintf "%a" Core.Object.PacketOut.pp
    in
    let payload =
      get_packet_in value_sto
      |> Format.asprintf "%a" Core.Object.PacketIn.pp_payload
    in
    let packet = header ^ payload in
    (* Return port and packet *)
    let tx = (port, packet) in
    Some tx

  let drive_pipe_pre (value_ctx : Value.t) (value_sto : Value.t) (rx : IO.rx) :
      Value.t * Value.t * bool =
    (* Setup port and packet *)
    let value_ctx, value_sto = setup_rx value_ctx value_sto rx in
    (* Parser block *)
    let value_ctx, value_sto = drive_p value_ctx value_sto in
    (* Verify block *)
    let value_ctx, value_sto, _value_verify_result =
      drive_vr value_ctx value_sto
    in
    (* Ingress block *)
    let value_ctx, value_sto, _value_verify_result =
      drive_ig value_ctx value_sto
    in
    (* Check if packet should be dropped *)
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
    (value_ctx, value_sto, drop)

  let drive_pipe_post (value_ctx : Value.t) (value_sto : Value.t) :
      Value.t * Value.t * IO.tx option =
    (* Egress block *)
    let value_ctx, value_sto, _value_verify_result =
      drive_eg value_ctx value_sto
    in
    (* Check block *)
    let value_ctx, value_sto, _value_check_result =
      drive_ck value_ctx value_sto
    in
    (* Deparser block *)
    let value_ctx, value_sto, _value_deparse_result =
      drive_dep value_ctx value_sto
    in
    (* Get resulting port and packet *)
    let result_opt = resulting_port_packet value_ctx value_sto in
    (value_ctx, value_sto, result_opt)

  let drive_pipe (value_ctx : Value.t) (value_sto : Value.t) (rx : IO.rx) :
      Value.t * Value.t * IO.tx option =
    let value_ctx, value_sto, drop = drive_pipe_pre value_ctx value_sto rx in
    if drop then (value_ctx, value_sto, None)
    else drive_pipe_post value_ctx value_sto
end
