open Error
open Interface.Wrap
open Util.Source

(* Extern objects *)

module Externs = Map.Make (String)

type extern = PacketIn of Core.PacketIn.t | PacketOut of Core.PacketOut.t

let externs = ref Externs.empty

(* Initialization *)

let init (spec : Sl.Ast.spec) (includes_p4 : string list) (filename_p4 : string)
    : Sl.Ast.value * Sl.Ast.value =
  match
    Interp_sl.Run.run_program spec "V1Model_init" includes_p4 filename_p4 []
  with
  | Pass ([ value_ctx; value_sto ], _, _, _) ->
      print_endline "Initial Context:";
      Il.Print.string_of_value value_ctx |> print_endline;
      print_endline "Initial Store:";
      Il.Print.string_of_value value_sto |> print_endline;
      (value_ctx, value_sto)
  | Pass (_, at, _, _) ->
      error_no_region "V1Model_init should return a context and a store"
  | Fail (at, msg, _) | IllFormed (at, msg, _) -> error at msg

(* Pipeline *)

type port = int
type packet = string
type result = port * packet

let setup_port_packet (spec : Sl.Ast.spec) (value_ctx : Sl.Ast.value)
    (value_sto : Sl.Ast.value) (port_in : port) (packet_in : packet) :
    Sl.Ast.value * Sl.Ast.value =
  (* Setup packet_in and packet_out externs *)
  let value_ctx, value_sto =
    match
      Interp_sl.Run.run spec "V1Model_init_packet" "FOO.p4"
        [ value_ctx; value_sto ]
    with
    | Pass ([ value_ctx; value_sto ], _, _, _) -> (value_ctx, value_sto)
    | Pass (_, at, _, _) ->
        error_no_region
          "V1Model_init_packet should return a context and a store"
    | Fail (at, msg, _) | IllFormed (at, msg, _) -> error at msg
  in
  let packet_in = PacketIn (Core.PacketIn.init packet_in) in
  externs := Externs.add "packet_in" packet_in !externs;
  let packet_out = PacketOut (Core.PacketOut.init ()) in
  externs := Externs.add "packet_out" packet_out !externs;
  (* Setup ingress port *)
  let value_cursor = [ Term "GLOBAL" ] #@ "cursor" in
  let value_ref =
    let value_ref_base =
      let value_ref_name =
        Il.Ast.TextV "standard_metadata" |> with_typ Il.Ast.TextT
      in
      [ Term "`"; NT value_ref_name ] #@ "prefixedNameIR"
    in
    let value_field_name =
      Il.Ast.TextV "ingress_port" |> with_typ Il.Ast.TextT
    in
    [ NT value_ref_base; Term "."; NT value_field_name ] #@ "storageReference"
  in
  let value_field =
    let value_width =
      Il.Ast.NumV (`Nat (Bigint.of_int 9)) |> with_typ (Il.Ast.NumT `NatT)
    in
    let value_bits =
      Il.Ast.NumV (`Int (Bigint.of_int port_in)) |> with_typ (Il.Ast.NumT `IntT)
    in
    [ NT value_width; Term "W"; NT value_bits ] #@ "numberLiteral"
  in
  let value_ctx =
    match
      Interp_sl.Run.run spec "Lvalue_write" "FOO.p4"
        [ value_cursor; value_ctx; value_sto; value_ref; value_field ]
    with
    | Pass ([ value_ctx ], _, _, _) -> value_ctx
    | Pass (_, at, _, _) ->
        error_no_region "Lvalue_write should return a context"
    | Fail (at, msg, _) | IllFormed (at, msg, _) -> error at msg
  in
  (value_ctx, value_sto)

let drive_parser_block (spec : Sl.Ast.spec) (value_ctx : Sl.Ast.value)
    (value_sto : Sl.Ast.value) : Sl.Ast.value * Sl.Ast.value * Sl.Ast.value =
  match
    Interp_sl.Run.run spec "V1Model_parser" "FOO.p4" [ value_ctx; value_sto ]
  with
  | Pass ([ value_ctx; value_sto; value_result ], _, _, _) ->
      (value_ctx, value_sto, value_result)
  | Pass (_, at, _, _) ->
      error_no_region
        "V1Model_parser should return a context, store, and result"
  | Fail (at, msg, _) | IllFormed (at, msg, _) -> error at msg

let drive_pipe (spec : Sl.Ast.spec) (value_ctx : Sl.Ast.value)
    (value_sto : Sl.Ast.value) (port_in : port) (packet_in : packet) :
    Sl.Ast.value * Sl.Ast.value * result option =
  (* Setup port and packet *)
  let value_ctx, value_sto =
    setup_port_packet spec value_ctx value_sto port_in packet_in
  in
  (* Parser block *)
  let value_ctx, value_sto, _value_parser_result =
    drive_parser_block spec value_ctx value_sto
  in
  (value_ctx, value_sto, None)

(* STF interpreter *)

let compare_packet packet_out packet_expect : bool =
  let to_list s = List.init (String.length s) (String.get s) in
  let packet_out = to_list packet_out in
  let packet_expect = to_list packet_expect in
  List.length packet_out = List.length packet_expect
  && List.fold_left2
       (fun same o e -> same && (e = '*' || o = e))
       true packet_out packet_expect

let compare_result (port_out, packet_out) (port_expect, packet_expect) : bool =
  let pass =
    port_out = port_expect && compare_packet packet_out packet_expect
  in
  if pass then
    Format.printf "[PASS] Expected: %d %s / Got: %d %s\n" port_expect
      packet_expect port_out packet_out
  else
    Format.printf "[FAIL] Expected: %d %s / Got: %d %s\n" port_expect
      packet_expect port_out packet_out;
  pass

let run_stf_stmt (spec : Sl.Ast.spec) (value_ctx : Il.Ast.value)
    (value_sto : Il.Ast.value) (pass : bool) (queue_packet : result list)
    (queue_expect : result list) (stmt_stf : Stf.Ast.stmt) :
    Il.Ast.value * Il.Ast.value * bool * result list * result list =
  match stmt_stf with
  (* Packet I/O *)
  | Stf.Ast.Packet (port_in, packet_in) -> (
      let port_in = int_of_string port_in in
      let packet_in = String.uppercase_ascii packet_in in
      let value_ctx, value_sto, result_out =
        drive_pipe spec value_ctx value_sto port_in packet_in
      in
      match result_out with
      | None -> (value_ctx, value_sto, pass, queue_packet, queue_expect)
      | Some (port_out, packet_out) -> (
          match queue_expect with
          | [] ->
              let queue_packet = queue_packet @ [ (port_out, packet_out) ] in
              (value_ctx, value_sto, pass, queue_packet, queue_expect)
          | (port_expect, packet_expect) :: queue_expect ->
              let pass =
                compare_result (port_out, packet_out)
                  (port_expect, packet_expect)
                && pass
              in
              (value_ctx, value_sto, pass, queue_packet, queue_expect)))
  | Stf.Ast.Expect (port_expect, Some packet_expect) -> (
      let port_expect = int_of_string port_expect in
      let packet_expect = String.uppercase_ascii packet_expect in
      match queue_packet with
      | [] ->
          ( value_ctx,
            value_sto,
            pass,
            queue_packet,
            queue_expect @ [ (port_expect, packet_expect) ] )
      | (port_out, packet_out) :: queue_packet ->
          let pass =
            compare_result (port_out, packet_out) (port_expect, packet_expect)
            && pass
          in
          (value_ctx, value_sto, pass, queue_packet, queue_expect))
  (* Async *)
  | Stf.Ast.Wait -> (value_ctx, value_sto, pass, queue_packet, queue_expect)
  | _ ->
      Format.asprintf "not yet supported: %a" Stf.Print.print_stmt stmt_stf
      |> error_no_region

let run_stf_stmts (spec : Sl.Ast.spec) (value_ctx : Il.Ast.value)
    (value_sto : Il.Ast.value) (stmts_stf : Stf.Ast.stmt list) : bool =
  let _, _, pass, queue_packet, queue_expect =
    List.fold_left
      (fun (value_ctx, value_sto, pass, queue_packet, queue_expect) stmt_stf ->
        run_stf_stmt spec value_ctx value_sto pass queue_packet queue_expect
          stmt_stf)
      (value_ctx, value_sto, true, [], [])
      stmts_stf
  in
  let pass = pass && queue_packet = [] && queue_expect = [] in
  if queue_packet <> [] then (
    Format.printf "[FAIL] Remaining packets to be matched:\n";
    List.iteri
      (fun idx (port, packet) -> Format.printf "(%d) %d %s\n" idx port packet)
      queue_packet);
  if queue_expect <> [] then (
    Format.printf "[FAIL] Expected packets to be output:\n";
    List.iteri
      (fun idx (port, packet) -> Format.printf "(%d) %d %s\n" idx port packet)
      queue_expect);
  pass

let run (spec : Sl.Ast.spec) (includes_p4 : string list) (filename_p4 : string)
    (filename_stf : string) =
  let value_ctx, value_sto = init spec includes_p4 filename_p4 in
  let stf_stmts = Stf.Parse.parse_file filename_stf in
  let _ = run_stf_stmts spec value_ctx value_sto stf_stmts in
  ()
