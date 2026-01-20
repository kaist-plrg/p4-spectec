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

  let init (spec_ : spec) : unit =
    match spec_ with
    | IL spec_il ->
        spec := IL spec_il;
        Arch.init IL_mode;
        Interp_IL.init spec_il
    | SL spec_sl ->
        spec := SL spec_sl;
        Arch.init SL_mode;
        Interp_SL.init spec_sl
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

  let on_tx_output (tx_opt : IO.tx option) (tx_output_queue : IO.tx list)
      (tx_expect_queue : IO.tx list) : IO.tx list * IO.tx list =
    match tx_opt with
    (* Packet was transmitted *)
    | Some tx -> (
        match tx_expect_queue with
        (* No expected packet (yet) *)
        | [] ->
            let tx_output_queue = tx_output_queue @ [ tx ] in
            (tx_output_queue, tx_expect_queue)
        (* There is an expected packet *)
        | tx_expect :: tx_expect_queue when compare_tx tx tx_expect ->
            Format.asprintf "[PASS] Transmitted %s" (string_of_tx tx_expect)
            |> log;
            (tx_output_queue, tx_expect_queue)
        | tx_expect :: _ ->
            error_stf
              (Format.asprintf "expected %s but got %s" (string_of_tx tx_expect)
                 (string_of_tx tx)))
    (* Packet was dropped *)
    | None -> (tx_output_queue, tx_expect_queue)

  let on_tx_expect (tx_expect : IO.tx) (tx_output_queue : IO.tx list)
      (tx_expect_queue : IO.tx list) : IO.tx list * IO.tx list =
    match tx_output_queue with
    (* No output packet (yet) *)
    | [] ->
        let tx_expect_queue = tx_expect_queue @ [ tx_expect ] in
        (tx_output_queue, tx_expect_queue)
    (* There is an output packet *)
    | tx_output :: tx_output_queue when compare_tx tx_output tx_expect ->
        Format.asprintf "[PASS] Transmitted %s" (string_of_tx tx_output) |> log;
        (tx_output_queue, tx_expect_queue)
    | tx_output :: _ ->
        error_stf
          (Format.asprintf "expected %s but got %s" (string_of_tx tx_expect)
             (string_of_tx tx_output))

  let run_stf_stmt (value_ctx : Il.value) (value_sto : Il.value)
      (tx_output_queue : IO.tx list) (tx_expect_queue : IO.tx list)
      (stmt_stf : Stf.Ast.stmt) : Il.value * Il.value * IO.tx list * IO.tx list
      =
    match stmt_stf with
    (* Packet I/O *)
    | Stf.Ast.Packet (port_in, packet_in, _exact) ->
        let port_in = int_of_string port_in in
        let packet_in = String.uppercase_ascii packet_in in
        let rx = (port_in, packet_in) in
        let value_ctx, value_sto, tx_output_opt =
          Arch.drive_pipe value_ctx value_sto rx
        in
        let tx_output_queue, tx_expect_queue =
          on_tx_output tx_output_opt tx_output_queue tx_expect_queue
        in
        (value_ctx, value_sto, tx_output_queue, tx_expect_queue)
    | Stf.Ast.Expect (port_expect, packet_expect_opt, _exact) ->
        let port_expect = int_of_string port_expect in
        let packet_expect = Option.value packet_expect_opt ~default:"" in
        let packet_expect = String.uppercase_ascii packet_expect in
        let tx_expect = (port_expect, packet_expect) in
        let tx_output_queue, tx_expect_queue =
          on_tx_expect tx_expect tx_output_queue tx_expect_queue
        in
        (value_ctx, value_sto, tx_output_queue, tx_expect_queue)
    (* Match-action table updates *)
    | Stf.Ast.Add
        ( table_name,
          table_entry_priority_opt,
          table_entry_keys,
          table_entry_action,
          _ ) ->
        (* Encode name *)
        let value_tableName = wrap_text_v table_name in
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
                       wrap_case_v [ Term "`NUM"; NT (wrap_text_v number) ]
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
        let value_sto =
          Arch.table_add_entry value_sto value_tableName
            value_tableEntryPriorityInterface value_tableKeysetInterface
            value_tableActionInterface
        in
        (value_ctx, value_sto, tx_output_queue, tx_expect_queue)
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
        let value_sto =
          Arch.table_add_default_action value_sto value_tableName
            value_tableActionInterface
        in
        (value_ctx, value_sto, tx_output_queue, tx_expect_queue)
    (* Async *)
    | Stf.Ast.Wait -> (value_ctx, value_sto, tx_output_queue, tx_expect_queue)
    | _ ->
        error_stf
          (Format.asprintf "not yet supported: %a" Stf.Print.print_stmt stmt_stf)

  let run_stf_stmts (value_ctx : Il.value) (value_sto : Il.value)
      (stmts_stf : Stf.Ast.stmt list) : unit =
    let _, _, tx_output_queue, tx_expect_queue =
      List.fold_left
        (fun (value_ctx, value_sto, tx_output_queue, tx_expect_queue) stmt_stf
           ->
          run_stf_stmt value_ctx value_sto tx_output_queue tx_expect_queue
            stmt_stf)
        (value_ctx, value_sto, [], [])
        stmts_stf
    in
    match (tx_output_queue, tx_expect_queue) with
    | [], [] -> ()
    | tx_output_queue, tx_expect_queue ->
        let msg_output =
          if tx_output_queue <> [] then
            Format.asprintf "[FAIL] Remaining packets to be matched:\n%s"
              (tx_output_queue |> List.map string_of_tx |> String.concat "\n")
          else ""
        in
        let msg_expect =
          if tx_expect_queue <> [] then
            Format.asprintf "[FAIL] Expected packets to be output:\n%s"
              (tx_expect_queue |> List.map string_of_tx |> String.concat "\n")
          else ""
        in
        error_stf (msg_output ^ msg_expect)

  let run_stf_test (includes_p4 : string list) (filename_p4 : string)
      (filename_stf : string) : stf_result =
    try
      let value_ctx, value_sto = Arch.init_pipe includes_p4 filename_p4 in
      let stf_stmts = Stf.Parse.parse_file filename_stf in
      run_stf_stmts value_ctx value_sto stf_stmts;
      Pass
    with
    | Util.Error.ParseError (at, msg) -> Fail (`Syntax (at, msg))
    | Util.Error.InterpError (at, msg) | Util.Error.ArchError (at, msg) ->
        Fail (`Runtime (at, msg))
    | Util.Error.StfError msg -> Fail (`Runtime (no_region, msg))
end
