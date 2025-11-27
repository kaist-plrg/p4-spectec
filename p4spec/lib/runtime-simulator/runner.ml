module MCov = Runtime_testgen.Cov.Multiple
open Io
open Simulator
open Error
open Util.Source

(* Functor to create a DRIVER from ARCH and INTERP implementations *)

module Make
    (MakeArch : functor (Interp_IL : INTERP_IL) (Interp_SL : INTERP_SL) -> ARCH)
    (MakeInterp_IL : functor (Arch : ARCH) -> INTERP_IL)
    (MakeInterp_SL : functor (Arch : ARCH) -> INTERP_SL) : DRIVER = struct
  module rec Arch : ARCH = MakeArch (Interp_IL) (Interp_SL)
  and Interp_IL : INTERP_IL = MakeInterp_IL (Arch)
  and Interp_SL : INTERP_SL = MakeInterp_SL (Arch)

  (* Relation runner *)

  let run_program ~(derive : bool) (spec : spec) (relname : string)
      (includes_p4 : string list) (filename_p4 : string) : program_result =
    match spec with
    | IL spec_il ->
        if derive then
          Format.eprintf
            "[WARNING] Derivation not supported for IL interpreter\n";
        Interp_IL.eval_program spec_il relname includes_p4 filename_p4
    | SL spec_sl ->
        Interp_SL.eval_program ~derive spec_sl relname includes_p4 filename_p4
    | Empty -> assert false

  let run_program_internal ~(derive : bool) (spec : Sl.Ast.spec)
      (relname : string) (value_program : Sl.Ast.value) : rel_result =
    derive |> ignore;
    Interp_SL.eval_rel spec relname [ value_program ]

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
        (tx_output_queue, tx_expect_queue)
    | tx_output :: _ ->
        error_stf
          (Format.asprintf "expected %s but got %s" (string_of_tx tx_expect)
             (string_of_tx tx_output))

  let run_stf_stmt (value_ctx : Sl.Ast.value) (value_sto : Sl.Ast.value)
      (tx_output_queue : IO.tx list) (tx_expect_queue : IO.tx list)
      (stmt_stf : Stf.Ast.stmt) :
      Sl.Ast.value * Sl.Ast.value * IO.tx list * IO.tx list =
    match stmt_stf with
    (* Packet I/O *)
    | Stf.Ast.Packet (port_in, packet_in) ->
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
    | Stf.Ast.Expect (port_expect, Some packet_expect) ->
        let port_expect = int_of_string port_expect in
        let packet_expect = String.uppercase_ascii packet_expect in
        let tx_expect = (port_expect, packet_expect) in
        let tx_output_queue, tx_expect_queue =
          on_tx_expect tx_expect tx_output_queue tx_expect_queue
        in
        (value_ctx, value_sto, tx_output_queue, tx_expect_queue)
    (* Async *)
    | Stf.Ast.Wait -> (value_ctx, value_sto, tx_output_queue, tx_expect_queue)
    | _ ->
        error_stf
          (Format.asprintf "not yet supported: %a" Stf.Print.print_stmt stmt_stf)

  let run_stf_stmts (value_ctx : Sl.Ast.value) (value_sto : Sl.Ast.value)
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
    | Util.Error.ParseError (at, msg) -> IllFormed (at, msg)
    | Util.Error.InterpError (at, msg) -> Fail (at, msg)
    | Util.Error.ArchError (at, msg) -> Fail (at, msg)
    | Util.Error.StfError msg -> Fail (no_region, msg)

  (* Coverage runner *)

  let cover_programs (spec : Sl.Ast.spec) (relname : string)
      (includes_p4 : string list) (filenames_p4 : string list) : MCov.Cover.t =
    Interp_SL.cover_programs spec relname includes_p4 filenames_p4
end
