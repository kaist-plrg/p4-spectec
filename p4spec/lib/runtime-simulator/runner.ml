open Domain.Lib
module MCov = Runtime_testgen.Cov.Multiple
open Io
open Simulator

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
    Interp_SL.eval_rel spec relname [ value_program ]

  (* STF test runner *)

  let run_stf_stmt (value_ctx : Sl.Ast.value) (value_sto : Sl.Ast.value)
      (pass : bool) (queue_packet : IO.tx list) (queue_expect : IO.tx list)
      (stmt_stf : Stf.Ast.stmt) :
      Sl.Ast.value * Sl.Ast.value * bool * IO.tx list * IO.tx list =
    match stmt_stf with
    (* Packet I/O *)
    | Stf.Ast.Packet (port_in, packet_in) -> (
        let port_in = int_of_string port_in in
        let packet_in = String.uppercase_ascii packet_in in
        let rx = (port_in, packet_in) in
        let value_ctx, value_sto, tx_opt =
          Arch.drive_pipe value_ctx value_sto rx
        in
        match tx_opt with
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
        |> failwith

  let run_stf_stmts (value_ctx : Sl.Ast.value) (value_sto : Sl.Ast.value)
      (stmts_stf : Stf.Ast.stmt list) : bool =
    let _, _, pass, queue_packet, queue_expect =
      List.fold_left
        (fun (value_ctx, value_sto, pass, queue_packet, queue_expect) stmt_stf ->
          run_stf_stmt value_ctx value_sto pass queue_packet queue_expect
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

  let run_stf_test (spec : spec) (includes_p4 : string list)
      (filename_p4 : string) (filename_stf : string) : unit =
    let value_ctx, value_sto = Arch.init_pipe spec includes_p4 filename_p4 in
    let stf_stmts = Stf.Parse.parse_file filename_stf in
    let _ = run_stf_stmts value_ctx value_sto stf_stmts in
    ()

  (* Coverage runner *)

  let cover_programs (spec : Sl.Ast.spec) (relname : string)
      (includes_p4 : string list) (filenames_p4 : string list) : MCov.Cover.t =
    Interp_SL.cover_programs spec relname includes_p4 filenames_p4
end
