open Lang
module ICov_multi = Coverage.Instr.Multi
module DCov_multi = Coverage.Dangling.Multi
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

  (* Logger *)

  let verbose = ref true
  let log (msg : string) : unit = if !verbose then print_endline msg

  (* Relation runner *)

  let run_program (spec : spec) (relname : string) (includes_p4 : string list)
      (filename_p4 : string) : program_result =
    Arch.init spec;
    match spec with
    | IL spec_il ->
        Interp_IL.eval_program spec_il relname includes_p4 filename_p4
    | SL spec_sl ->
        Interp_SL.eval_program spec_sl relname includes_p4 filename_p4
    | Empty -> assert false

  let run_program_internal (spec : spec) (relname : string)
      (value_program : Il.value) : rel_result =
    Arch.init spec;
    match spec with
    | IL spec_il -> Interp_IL.eval_rel spec_il relname [ value_program ]
    | SL spec_sl -> Interp_SL.eval_rel spec_sl relname [ value_program ]
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
                   | Num number -> wrap_text_v number
                   | Slash _ ->
                       error_stf "slash notation for table keys not supported"
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
          Arch.table_add_default value_sto value_tableName
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
        (fun (value_ctx, value_sto, tx_output_queue, tx_expect_queue) stmt_stf ->
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

  let run_stf_test (spec : spec) (includes_p4 : string list)
      (filename_p4 : string) (filename_stf : string) : stf_result =
    try
      let value_ctx, value_sto = Arch.init_pipe spec includes_p4 filename_p4 in
      let stf_stmts = Stf.Parse.parse_file filename_stf in
      run_stf_stmts value_ctx value_sto stf_stmts;
      Pass
    with
    | Util.Error.ParseError (at, msg) -> Fail (`Syntax (at, msg))
    | Util.Error.InterpError (at, msg) | Util.Error.ArchError (at, msg) ->
        Fail (`Runtime (at, msg))
    | Util.Error.StfError msg -> Fail (`Runtime (no_region, msg))

  (* Coverage runner *)

  let cover_instr_programs (spec : spec) (relname : string)
      (includes_p4 : string list) (filenames_p4 : string list) : ICov_multi.t =
    Arch.init spec;
    let cover_multi =
      match spec with
      | SL spec -> ICov_multi.init spec
      | _ -> error_no_region "instruction coverage is only supported for SL"
    in
    List.fold_left
      (fun cover_multi filename_p4 ->
        let (module IH : Inst.Handler.HANDLER), read_coverage_instr =
          Inst.Coverage_instr.make ()
        in
        Inst.Hook.register [ (module IH : Inst.Handler.HANDLER) ];
        Inst.Hook.init_spec spec;
        let _ = run_program spec relname includes_p4 filename_p4 in
        Inst.Hook.finish ();
        let cover_single = read_coverage_instr () in
        ICov_multi.extend cover_multi filename_p4 cover_single)
      cover_multi filenames_p4

  let cover_dangling_programs (spec : spec) (relname : string)
      (includes_p4 : string list) (filenames_p4 : string list) : DCov_multi.t =
    Arch.init spec;
    let cover_multi =
      match spec with
      | SL spec -> DCov_multi.init spec
      | _ -> error_no_region "dangling coverage is only supported for SL"
    in
    List.fold_left
      (fun cover_multi filename_p4 ->
        let (module DH : Inst.Handler.HANDLER), read_coverage_dangling =
          Inst.Coverage_dangling.make ()
        in
        Inst.Hook.register [ (module DH : Inst.Handler.HANDLER) ];
        Inst.Hook.init_spec spec;
        let program_result = run_program spec relname includes_p4 filename_p4 in
        Inst.Hook.finish ();
        let cover_single = read_coverage_dangling () in
        let wellformed, welltyped =
          match program_result with
          | Pass _ -> (true, true)
          | Fail (`Syntax _) -> (true, false)
          | Fail (`Runtime _) -> (false, false)
        in
        DCov_multi.extend cover_multi filename_p4 wellformed welltyped
          cover_single)
      cover_multi filenames_p4

  let cover_instr_stfs (spec : spec) (includes_p4 : string list)
      (filenames_p4 : string list) (filenames_stf : string list) : ICov_multi.t
      =
    verbose := false;
    Arch.init spec;
    let cover_multi =
      match spec with
      | SL spec -> ICov_multi.init spec
      | _ -> error_no_region "instruction coverage is only supported for SL"
    in
    List.combine filenames_p4 filenames_stf
    |> List.fold_left
         (fun cover_multi (filename_p4, filename_stf) ->
           let (module IH : Inst.Handler.HANDLER), read_coverage_instr =
             Inst.Coverage_instr.make ()
           in
           Inst.Hook.register [ (module IH : Inst.Handler.HANDLER) ];
           Inst.Hook.init_spec spec;
           let _ = run_stf_test spec includes_p4 filename_p4 filename_stf in
           Inst.Hook.finish ();
           let cover_single = read_coverage_instr () in
           ICov_multi.extend cover_multi filename_p4 cover_single)
         cover_multi

  let cover_dangling_stfs (spec : spec) (includes_p4 : string list)
      (filenames_p4 : string list) (filenames_stf : string list) : DCov_multi.t
      =
    verbose := false;
    Arch.init spec;
    let cover_multi =
      match spec with
      | SL spec -> DCov_multi.init spec
      | _ -> error_no_region "dangling coverage is only supported for SL"
    in
    List.combine filenames_p4 filenames_stf
    |> List.fold_left
         (fun cover_multi (filename_p4, filename_stf) ->
           let (module DH : Inst.Handler.HANDLER), read_coverage_instr =
             Inst.Coverage_dangling.make ()
           in
           Inst.Hook.register [ (module DH : Inst.Handler.HANDLER) ];
           Inst.Hook.init_spec spec;
           let program_result =
             run_stf_test spec includes_p4 filename_p4 filename_stf
           in
           Inst.Hook.finish ();
           let cover_single = read_coverage_instr () in
           let wellformed, welltyped =
             match program_result with
             | Pass -> (true, true)
             | Fail (`Syntax _) -> (true, false)
             | Fail (`Runtime _) -> (false, false)
           in
           DCov_multi.extend cover_multi filename_p4 wellformed welltyped
             cover_single)
         cover_multi
end
