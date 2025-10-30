open Interface.Wrap
open Interface.Unwrap
module Value = Runtime_dynamic.Value
module IO = Runtime_simulator.Io
module Sim = Runtime_simulator.Simulator
open Error

module Make (Interp_IL : Sim.INTERP_IL) (Interp_SL : Sim.INTERP_SL) : Sim.ARCH =
struct
  (* Specification *)

  let spec : Sim.spec ref = ref Sim.Empty
  let init_spec (spec_ : Sim.spec) : unit = spec := spec_

  (* Extern objects *)

  module Externs = Map.Make (String)

  type extern = PacketIn of Core.PacketIn.t | PacketOut of Core.PacketOut.t

  let externs = ref Externs.empty

  let get_pkt_in () =
    match Externs.find "packet_in" !externs with
    | PacketIn pkt_in -> pkt_in
    | _ -> assert false

  let get_pkt_out () =
    match Externs.find "packet_out" !externs with
    | PacketOut pkt_out -> pkt_out
    | _ -> assert false

  (* Transactions *)

  type checkpoints = extern Externs.t list

  let checkpoints : checkpoints ref = ref []
  let checkpoint () : unit = checkpoints := !externs :: !checkpoints

  let commit () : unit =
    match !checkpoints with
    | _ :: checkpoints_t -> checkpoints := checkpoints_t
    | [] -> assert false

  let restore () : unit =
    match !checkpoints with
    | checkpoint_h :: _ -> externs := checkpoint_h
    | [] -> assert false

  let rollback () : unit =
    match !checkpoints with
    | checkpoint_h :: checkpoints_t ->
        externs := checkpoint_h;
        checkpoints := checkpoints_t
    | [] -> assert false

  (* Call entry points *)

  let call_rel (relname : string) (expect : int) (values_input : Value.t list) :
      Value.t list =
    let result =
      match !spec with
      | IL spec_il -> Interp_IL.eval_rel spec_il relname values_input
      | SL spec_sl -> Interp_SL.eval_rel spec_sl relname values_input
      | Empty -> assert false
    in
    let values_output =
      match result with
      | Pass (values_output, _) -> values_output
      | Fail (at, msg, _) -> error at msg
    in
    if List.length values_output <> expect then
      failwith
        (Printf.sprintf "Expected %d return values from %s, got %d" expect
           relname
           (List.length values_output));
    values_output

  let call_rel_one (relname : string) (values_input : Value.t list) : Value.t =
    match call_rel relname 1 values_input with
    | [ value ] -> value
    | _ -> assert false

  let call_rel_two (relname : string) (values_input : Value.t list) :
      Value.t * Value.t =
    match call_rel relname 2 values_input with
    | [ value_a; value_b ] -> (value_a, value_b)
    | _ -> assert false

  let call_rel_three (relname : string) (values_input : Value.t list) :
      Value.t * Value.t * Value.t =
    match call_rel relname 3 values_input with
    | [ value_a; value_b; value_c ] -> (value_a, value_b, value_c)
    | _ -> assert false

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

  (* Extern calls *)

  let eval_extern_func_call (_values_input : Value.t list) : Value.t list =
    failwith "TODO"

  let eval_extern_method_call (values_input : Value.t list) : Value.t list =
    let value_ctx, value_sto, value_oid, value_name, value_names_param =
      match values_input with
      | [ value_ctx; value_sto; value_oid; value_name; value_names_param ] ->
          (value_ctx, value_sto, value_oid, value_name, value_names_param)
      | _ -> failwith "Unexpected number of arguments to extern method call"
    in
    let oid =
      value_oid |> unwrap_list_v |> List.map unwrap_text_v |> String.concat "."
    in
    let extern = Externs.find oid !externs in
    let name = unwrap_text_v value_name in
    let names_param =
      value_names_param |> unwrap_list_v |> List.map unwrap_text_v
    in
    match (extern, name, names_param) with
    | PacketIn packet_in, "extract", [ "hdr" ] ->
        let packet_in, value_ctx, value_sto, value_callResult =
          Core.PacketIn.extract call_rel_one call_func value_ctx value_sto
            packet_in
        in
        externs := Externs.add oid (PacketIn packet_in) !externs;
        [ value_ctx; value_sto; value_callResult ]
    | PacketOut packet_out, "emit", [ "hdr" ] ->
        let packet_out, value_ctx, value_sto, value_callResult =
          Core.PacketOut.emit call_func value_ctx value_sto packet_out
        in
        externs := Externs.add oid (PacketOut packet_out) !externs;
        [ value_ctx; value_sto; value_callResult ]
    | _ ->
        failwith
          ("Unsupported extern method call: " ^ oid ^ "." ^ name ^ "("
          ^ String.concat ", " names_param
          ^ ")")

  (* Pipeline initializer *)

  let init_pipe (spec_ : Sim.spec) (includes_p4 : string list)
      (filename_p4 : string) : Value.t * Value.t =
    init_spec spec_;
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
    | _ -> failwith "Unexpected return from V1Model_init"

  (* Pipeline driver *)

  let setup_rx (value_ctx : Value.t) (value_sto : Value.t) (rx : IO.rx) :
      Value.t * Value.t =
    let port_in, packet_in = rx in
    (* Setup packet_in and packet_out externs *)
    let value_ctx, value_sto =
      call_rel_two "V1Model_init_packet" [ value_ctx; value_sto ]
    in
    let packet_in = PacketIn (Core.PacketIn.init packet_in) in
    externs := Externs.add "packet_in" packet_in !externs;
    let packet_out = PacketOut (Core.PacketOut.init ()) in
    externs := Externs.add "packet_out" packet_out !externs;
    (* Setup ingress port *)
    let value_cursor = [ Term "GLOBAL" ] #@ "cursor" in
    let value_ref =
      let value_ref_base =
        let value_ref_name = wrap_text_v "standard_metadata" in
        [ Term "`"; NT value_ref_name ] #@ "prefixedNameIR"
      in
      let value_field_name = wrap_text_v "ingress_port" in
      [ NT value_ref_base; Term "."; NT value_field_name ] #@ "storageReference"
    in
    let value_field =
      let value_width = wrap_num_v_nat 9 in
      let value_bits = wrap_num_v_int port_in in
      [ NT value_width; Term "W"; NT value_bits ] #@ "numberLiteral"
    in
    let value_ctx =
      call_rel_one "Lvalue_write"
        [ value_cursor; value_ctx; value_sto; value_ref; value_field ]
    in
    (value_ctx, value_sto)

  let drive_p (value_ctx : Value.t) (value_sto : Value.t) :
      Value.t * Value.t * Value.t =
    call_rel_three "V1Model_parser" [ value_ctx; value_sto ]

  let drive_vr (value_ctx : Value.t) (value_sto : Value.t) :
      Value.t * Value.t * Value.t =
    call_rel_three "V1Model_verify" [ value_ctx; value_sto ]

  let drive_ig (value_ctx : Value.t) (value_sto : Value.t) :
      Value.t * Value.t * Value.t =
    call_rel_three "V1Model_ingress" [ value_ctx; value_sto ]

  let drive_eg (value_ctx : Value.t) (value_sto : Value.t) :
      Value.t * Value.t * Value.t =
    call_rel_three "V1Model_egress" [ value_ctx; value_sto ]

  let drive_ck (value_ctx : Value.t) (value_sto : Value.t) :
      Value.t * Value.t * Value.t =
    call_rel_three "V1Model_check" [ value_ctx; value_sto ]

  let drive_dep (value_ctx : Value.t) (value_sto : Value.t) :
      Value.t * Value.t * Value.t =
    call_rel_three "V1Model_deparse" [ value_ctx; value_sto ]

  let resulting_port_packet (value_ctx : Value.t) (_value_sto : Value.t) :
      IO.tx option =
    (* Get egress port *)
    let value_cursor = [ Term "GLOBAL" ] #@ "cursor" in
    let value_prefixedNameIR =
      let value_nameIR = wrap_text_v "standard_metadata" in
      [ Term "`"; NT value_nameIR ] #@ "prefixedNameIR"
    in
    let _value_standard_metadata =
      call_func "find_value_eval" []
        [ value_cursor; value_ctx; value_prefixedNameIR ]
    in
    (* Get output packet *)
    let header = get_pkt_out () |> Format.asprintf "%a" Core.PacketOut.pp in
    let payload =
      get_pkt_in () |> Format.asprintf "%a" Core.PacketIn.pp_payload
    in
    let packet = header ^ payload in
    Some (0, packet)

  let drive_pipe (value_ctx : Value.t) (value_sto : Value.t) (rx : IO.rx) :
      Value.t * Value.t * IO.tx option =
    (* Setup port and packet *)
    let value_ctx, value_sto = setup_rx value_ctx value_sto rx in
    (* Parser block *)
    let value_ctx, value_sto, _value_parser_result =
      drive_p value_ctx value_sto
    in
    (* Verify block *)
    let value_ctx, value_sto, _value_verify_result =
      drive_vr value_ctx value_sto
    in
    (* Ingress block *)
    let value_ctx, value_sto, _value_verify_result =
      drive_ig value_ctx value_sto
    in
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
end
