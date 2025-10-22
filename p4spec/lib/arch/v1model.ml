open Interface.Wrap
open Interface.Unwrap
module IO = Runtime_simulator.Io
module Sim = Runtime_simulator.Simulator

module Make (Interp : Sim.INTERP) : Sim.ARCH = struct
  (* Specification *)

  let spec : Sl.Ast.spec ref = ref []
  let init_spec (spec_ : Sl.Ast.spec) : unit = spec := spec_

  (* Extern objects *)

  module Externs = Map.Make (String)

  type extern = PacketIn of Core.PacketIn.t | PacketOut of Core.PacketOut.t

  let externs = ref Externs.empty

  (* Call entry points *)

  let call_rel (relname : string) (expect : int)
      (values_input : Sl.Ast.value list) : Sl.Ast.value list =
    let values_output = Interp.eval_rel_call !spec relname values_input in
    if List.length values_output <> expect then
      failwith
        (Printf.sprintf "Expected %d return values from %s, got %d" expect
           relname
           (List.length values_output));
    values_output

  let call_rel_one (relname : string) (values_input : Sl.Ast.value list) :
      Sl.Ast.value =
    match call_rel relname 1 values_input with
    | [ value ] -> value
    | _ -> assert false

  let call_rel_two (relname : string) (values_input : Sl.Ast.value list) :
      Sl.Ast.value * Sl.Ast.value =
    match call_rel relname 2 values_input with
    | [ value_a; value_b ] -> (value_a, value_b)
    | _ -> assert false

  let call_rel_three (relname : string) (values_input : Sl.Ast.value list) :
      Sl.Ast.value * Sl.Ast.value * Sl.Ast.value =
    match call_rel relname 3 values_input with
    | [ value_a; value_b; value_c ] -> (value_a, value_b, value_c)
    | _ -> assert false

  let call_func (funcname : string) (typs_input : Sl.Ast.typ list)
      (values_input : Sl.Ast.value list) : Sl.Ast.value =
    Interp.eval_func_call !spec funcname typs_input values_input

  (* Extern initialization *)

  (* Initialization *)

  let init (spec_ : Sl.Ast.spec) (includes_p4 : string list)
      (filename_p4 : string) : Sl.Ast.value * Sl.Ast.value =
    init_spec spec_;
    match
      Interp.eval_rel_call_program !spec "V1Model_init" includes_p4 filename_p4
    with
    | [ value_ctx; value_sto ] -> (value_ctx, value_sto)
    | _ -> failwith "Unexpected return from V1Model_init"

  (* Extern calls *)

  let eval_extern_func_call (_values_input : Sl.Ast.value list) :
      Sl.Ast.value list =
    failwith "TODO"

  let eval_extern_method_call (values_input : Sl.Ast.value list) :
      Sl.Ast.value list =
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
    | _ ->
        failwith
          ("Unsupported extern method call: " ^ oid ^ "." ^ name ^ "("
          ^ String.concat ", " names_param
          ^ ")")

  (* Pipeline driver *)

  let setup_port_packet (value_ctx : Sl.Ast.value) (value_sto : Sl.Ast.value)
      (port_in : IO.port) (packet_in : IO.packet) : Sl.Ast.value * Sl.Ast.value
      =
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

  let drive_parser_block (value_ctx : Sl.Ast.value) (value_sto : Sl.Ast.value) :
      Sl.Ast.value * Sl.Ast.value * Sl.Ast.value =
    call_rel_three "V1Model_parser" [ value_ctx; value_sto ]

  let drive_pipe (value_ctx : Sl.Ast.value) (value_sto : Sl.Ast.value)
      (port_in : IO.port) (packet_in : IO.packet) :
      Sl.Ast.value * Sl.Ast.value * IO.result option =
    (* Setup port and packet *)
    let value_ctx, value_sto =
      setup_port_packet value_ctx value_sto port_in packet_in
    in
    (* Parser block *)
    let value_ctx, value_sto, _value_parser_result =
      drive_parser_block value_ctx value_sto
    in
    (value_ctx, value_sto, None)
end
