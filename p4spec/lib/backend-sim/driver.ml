open Lang
open Runtime.Sim.Io
open Runtime.Sim.Simulator
open Error
open Interface.Wrap
open Util.Source

(* Functor to create a DRIVER from ARCH and INTERP implementations *)

module Make
    (MakeArch : functor (Interp_IL : INTERP_IL) (Interp_SL : INTERP_SL) -> ARCH)
    (MakeInterp_IL : functor (Arch : ARCH) -> INTERP_IL)
    (MakeInterp_SL : functor (Arch : ARCH) -> INTERP_SL) : DRIVER = struct
  module rec Arch : ARCH = MakeArch (Interp_IL) (Interp_SL)
  and Interp_IL : INTERP_IL = MakeInterp_IL (Arch)
  and Interp_SL : INTERP_SL = MakeInterp_SL (Arch)

  (* Initialization *)

  let spec : spec ref = ref Empty

  let init ?(cache = true) ?(det = false) (spec_ : spec) : unit =
    match spec_ with
    | IL spec_il ->
        spec := IL spec_il;
        Arch.init IL_mode;
        Interp_IL.init ~cache ~det spec_il
    | SL spec_sl ->
        spec := SL spec_sl;
        Arch.init SL_mode;
        Interp_SL.init ~cache ~det spec_sl
    | Empty -> assert false

  (* Logger *)

  let verbose = ref true
  let log (msg : string) : unit = if !verbose then print_endline msg

  (* Relation runner *)

  let run_program (relname : string) (includes_p4 : string list)
      (filename_p4 : string) : program_result =
    match !spec with
    | IL _ -> Interp_IL.eval_program relname includes_p4 filename_p4
    | SL _ -> Interp_SL.eval_program relname includes_p4 filename_p4
    | Empty -> assert false

  let run_program_internal (relname : string) (value_program : Il.value) :
      rel_result =
    match !spec with
    | IL _ -> Interp_IL.eval_rel relname [ value_program ]
    | SL _ -> Interp_SL.eval_rel relname [ value_program ]
    | Empty -> assert false

  (* STF test runner *)

  (* Find the first expect element that has the same output port,
     then compare packet output.
     Return matched element and the rest of the list, preserving order. *)

  let extract_matching_expect (tx : IO.tx) (expect_queue : IO.expect list) :
      (IO.expect * IO.expect list) option =
    let tx_port, _ = tx in
    let rec extract_matching_expect expects = function
      | [] -> None
      | expect_h :: expect_t ->
          let (expect_port, expect_packet), exact = expect_h in
          if expect_port = tx_port then
            if compare_tx ~exact tx (expect_port, expect_packet) then
              Some (expect_h, List.rev_append expects expect_t)
            else
              error_stf
                (Format.asprintf "expected %s but got %s"
                   (string_of_tx (expect_port, expect_packet))
                   (string_of_tx tx))
          else extract_matching_expect (expect_h :: expects) expect_t
    in
    extract_matching_expect [] expect_queue

  let on_tx_output (txs : IO.tx list) (tx_output_queue : IO.tx list)
      (expect_queue : IO.expect list) : IO.tx list * IO.expect list =
    match txs with
    (* Packet was dropped *)
    | [] -> (tx_output_queue, expect_queue)
    (* Packet was transmitted *)
    | tx_h :: tx_t -> (
        match extract_matching_expect tx_h expect_queue with
        | None ->
            (* No expected packet (yet) *)
            let tx_output_queue = tx_output_queue @ txs in
            (tx_output_queue, expect_queue)
        | Some (expect, expect_queue) ->
            let tx, _ = expect in
            Format.asprintf "[PASS] Transmitted %s" (string_of_tx tx) |> log;
            (tx_output_queue @ tx_t, expect_queue))

  let extract_matching_output (expect : IO.expect)
      (tx_output_queue : IO.tx list) : (IO.tx * IO.tx list) option =
    let (expect_port, expect_packet), exact = expect in
    let rec extract_matching_output txs = function
      | [] -> None
      | tx_h :: tx_t ->
          let tx_port, _ = tx_h in
          if expect_port = tx_port then
            if compare_tx ~exact tx_h (expect_port, expect_packet) then
              Some (tx_h, List.rev_append txs tx_t)
            else
              error_stf
                (Format.asprintf "expected %s but got %s"
                   (string_of_tx (expect_port, expect_packet))
                   (string_of_tx tx_h))
          else extract_matching_output (tx_h :: txs) tx_t
    in
    extract_matching_output [] tx_output_queue

  let on_tx_expect (expect : IO.expect) (tx_output_queue : IO.tx list)
      (expect_queue : IO.expect list) : IO.tx list * expect list =
    match extract_matching_output expect tx_output_queue with
    | None ->
        (* No output packet (yet) *)
        let expect_queue = expect_queue @ [ expect ] in
        (tx_output_queue, expect_queue)
    | Some (tx_output, tx_output_queue) ->
        Format.asprintf "[PASS] Transmitted %s" (string_of_tx tx_output) |> log;
        (tx_output_queue, expect_queue)

  let run_stf_stmt (value_ctx : Il.value) (value_arch : Il.value)
      (tx_output_queue : IO.tx list) (expect_queue : IO.expect list)
      (stmt_stf : Stf.Ast.stmt) :
      Il.value * Il.value * IO.tx list * IO.expect list =
    (* Apply architecture-specific STF transformation *)
    let stmt_stf = Arch.transform_stf_stmt stmt_stf in
    match stmt_stf with
    (* Packet I/O *)
    | Stf.Ast.Packet (port_in, packet_in) ->
        let port_in = int_of_string port_in in
        let packet_in = String.uppercase_ascii packet_in in
        let rx = (port_in, packet_in) in
        let value_ctx, value_arch, tx_outputs =
          Arch.drive_pipe value_ctx value_arch rx
        in
        let tx_output_queue, expect_queue =
          on_tx_output tx_outputs tx_output_queue expect_queue
        in
        (value_ctx, value_arch, tx_output_queue, expect_queue)
    | Stf.Ast.Expect (port_expect, packet_expect_opt, exact) ->
        let port_expect = int_of_string port_expect in
        let packet_expect = Option.value packet_expect_opt ~default:"" in
        let packet_expect = String.uppercase_ascii packet_expect in
        let expect = ((port_expect, packet_expect), exact) in
        let tx_output_queue, expect_queue =
          on_tx_expect expect tx_output_queue expect_queue
        in
        (value_ctx, value_arch, tx_output_queue, expect_queue)
    (* Match-action table updates *)
    | Stf.Ast.Add
        ( table_name,
          table_entry_priority_opt,
          table_entry_keys,
          table_entry_action,
          _ ) ->
        (* Encode name *)
        let value_tableName = table_name |> String.escaped |> wrap_text_v in
        (* Encode priority *)
        let value_tableEntryPriorityInterface =
          table_entry_priority_opt
          |> Option.map (fun table_entry_priority ->
                 table_entry_priority |> Bigint.of_int |> wrap_num_v_int)
          |> wrap_opt_v_typed (Il.NumT `IntT)
        in
        (* Encode keys *)
        let value_tableKeysetInterface =
          table_entry_keys
          |> List.map (fun (table_entry_key : Stf.Ast.mtch) ->
                 let table_key_name, table_key_value = table_entry_key in
                 let table_key_name =
                   Stf.Print.convert_dollar_to_brackets table_key_name
                 in
                 let value_table_key_name = wrap_text_v table_key_name in
                 let value_table_key_value =
                   match table_key_value with
                   | Num number ->
                       let value_number =
                         if String.starts_with ~prefix:"0x" number then
                           let number_base_len = String.length number - 2 in
                           let number_base =
                             String.sub number 2 number_base_len
                           in
                           wrap_case_v
                             [ Term "`HEX"; NT (wrap_text_v number_base) ]
                         else if String.starts_with ~prefix:"0b" number then
                           let number_base_len = String.length number - 2 in
                           let number_base =
                             String.sub number 2 number_base_len
                           in
                           wrap_case_v
                             [ Term "`BIN"; NT (wrap_text_v number_base) ]
                         else
                           wrap_case_v [ Term "`DEC"; NT (wrap_text_v number) ]
                       in
                       value_number
                       |> with_typ (wrap_var_t "tableKeyValueInterface")
                   | Slash (prefix, mask) ->
                       let value_prefix = wrap_text_v prefix in
                       let mask = Bigint.of_int (int_of_string mask) in
                       let value_mask = wrap_num_v_nat mask in
                       wrap_case_v
                         [ NT value_prefix; Term "`SLASH"; NT value_mask ]
                       |> with_typ (wrap_var_t "tableKeyValueInterface")
                 in
                 wrap_tuple_v "tableKeyInterface"
                   [ value_table_key_name; value_table_key_value ])
          |> wrap_list_v "tableKeyInterface"
        in
        (* Encode action *)
        let value_tableActionInterface =
          let table_action_name, table_action_args = table_entry_action in
          let value_table_action_name = wrap_text_v table_action_name in
          let value_tableActionArgumentInterfaces =
            table_action_args
            |> List.map (fun (name, number) ->
                   let value_name = wrap_text_v name in
                   let value_number =
                     number |> int_of_string |> Bigint.of_int |> wrap_num_v_int
                   in
                   wrap_tuple_v "tableActionArgumentInterface"
                     [ value_name; value_number ])
            |> wrap_list_v "tableActionArgumentInterface"
          in
          wrap_tuple_v "tableActionInterface"
            [ value_table_action_name; value_tableActionArgumentInterfaces ]
        in
        let value_arch =
          Arch.table_add_entry value_ctx value_arch value_tableName
            value_tableEntryPriorityInterface value_tableKeysetInterface
            value_tableActionInterface
        in
        (value_ctx, value_arch, tx_output_queue, expect_queue)
    | Stf.Ast.SetDefault (table_name, table_entry_action) ->
        (* Encode name *)
        let value_tableName = wrap_text_v table_name in
        (* Encode action *)
        let value_tableActionInterface =
          let table_action_name, table_action_args = table_entry_action in
          let value_table_action_name = wrap_text_v table_action_name in
          let value_tableActionArgumentInterfaces =
            table_action_args
            |> List.map (fun (name, number) ->
                   let value_name = wrap_text_v name in
                   let value_number =
                     number |> int_of_string |> Bigint.of_int |> wrap_num_v_int
                   in
                   wrap_tuple_v "tableActionArgumentInterface"
                     [ value_name; value_number ])
            |> wrap_list_v "tableActionArgumentInterface"
          in
          wrap_tuple_v "tableActionInterface"
            [ value_table_action_name; value_tableActionArgumentInterfaces ]
        in
        let value_arch =
          Arch.table_add_default_action value_ctx value_arch value_tableName
            value_tableActionInterface
        in
        (value_ctx, value_arch, tx_output_queue, expect_queue)
    (* Mirror session updates *)
    | Stf.Ast.MirroringAdd (session, port) ->
        let session = int_of_string session in
        let port = int_of_string port in
        let value_arch = Arch.add_mirror_session value_arch session port in
        (value_ctx, value_arch, tx_output_queue, expect_queue)
    | Stf.Ast.MirroringAddMc (session, id) ->
        let session = int_of_string session in
        let id = int_of_string id in
        let value_arch = Arch.add_mirror_session_mc value_arch session id in
        (value_ctx, value_arch, tx_output_queue, expect_queue)
    | Stf.Ast.MirroringGet _session ->
        (value_ctx, value_arch, tx_output_queue, expect_queue)
    (* Multicast group updates *)
    | Stf.Ast.McGroupCreate mgid ->
        let mgid = int_of_string mgid in
        let value_arch = Arch.mc_mgrp_create value_arch mgid in
        (value_ctx, value_arch, tx_output_queue, expect_queue)
    | Stf.Ast.McNodeCreate (rid, ports) ->
        let rid = int_of_string rid in
        let ports = List.map int_of_string ports in
        let value_arch = Arch.mc_node_create value_arch rid ports in
        (value_ctx, value_arch, tx_output_queue, expect_queue)
    | Stf.Ast.McNodeAssociate (mgid, handle) ->
        let mgid = int_of_string mgid in
        let handle = int_of_string handle in
        let value_arch = Arch.mc_node_associate value_arch mgid handle in
        (value_ctx, value_arch, tx_output_queue, expect_queue)
    (* Register updates *)
    | Stf.Ast.RegisterRead (reg_name, index) ->
        let index = int_of_string index in
        let value_arch = Arch.register_read value_arch reg_name index in
        (value_ctx, value_arch, tx_output_queue, expect_queue)
    | Stf.Ast.RegisterWrite (reg_name, index, value) ->
        let index = int_of_string index in
        let value = int_of_string value in
        let value_arch = Arch.register_write value_arch reg_name index value in
        (value_ctx, value_arch, tx_output_queue, expect_queue)
    | Stf.Ast.RegisterReset reg_name ->
        let value_arch = Arch.register_reset value_arch reg_name in
        (value_ctx, value_arch, tx_output_queue, expect_queue)
    (* Async *)
    | Stf.Ast.Wait -> (value_ctx, value_arch, tx_output_queue, expect_queue)
    | _ ->
        error_stf
          (Format.asprintf "not yet supported: %a" Stf.Print.print_stmt stmt_stf)

  let run_stf_stmts (value_ctx : Il.value) (value_arch : Il.value)
      (stmts_stf : Stf.Ast.stmt list) : unit =
    let _, _, tx_output_queue, expect_queue =
      List.fold_left
        (fun (value_ctx, value_arch, tx_output_queue, expect_queue) stmt_stf ->
          run_stf_stmt value_ctx value_arch tx_output_queue expect_queue
            stmt_stf)
        (value_ctx, value_arch, [], [])
        stmts_stf
    in
    match (tx_output_queue, expect_queue) with
    | [], [] -> ()
    | tx_output_queue, expect_queue ->
        let msg_output =
          if tx_output_queue <> [] then
            Format.asprintf "[FAIL] Remaining packets to be matched:\n%s"
              (tx_output_queue |> List.map string_of_tx |> String.concat "\n")
          else ""
        in
        let msg_expect =
          if expect_queue <> [] then
            Format.asprintf "[FAIL] Expected packets to be output:\n%s"
              (expect_queue
              |> List.map (fun (tx, _) -> string_of_tx tx)
              |> String.concat "\n")
          else ""
        in
        error_stf (msg_output ^ msg_expect)

  let run_stf_test (includes_p4 : string list) (filename_p4 : string)
      (filename_stf : string) : stf_result =
    try
      let value_ctx, value_arch = Arch.init_pipe includes_p4 filename_p4 in
      let stf_stmts = Stf.Parse.parse_file filename_stf in
      run_stf_stmts value_ctx value_arch stf_stmts;
      Pass
    with
    | Util.Error.ParseError (at, msg) -> Fail (`Syntax (at, msg))
    | Util.Error.InterpError (at, msg) | Util.Error.ArchError (at, msg) ->
        Fail (`Runtime (at, msg))
    | Util.Error.StfError msg -> Fail (`Runtime (no_region, msg))
end
