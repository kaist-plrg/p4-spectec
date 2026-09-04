open Test_common
open Runtime.Sim.Signature
module Filesys = Util.Filesys

let run (module Simulator : SIM) neg relname includes_p4 path_p4 =
  let time_start = start () in
  try
    Simulator.Interp.clear ();
    (match Simulator.Interp.eval_program relname includes_p4 path_p4 with
    | Pass _ -> if neg then raise (TestRunNegErr time_start)
    | Fail (`Syntax (at, msg)) | Fail (`Runtime (at, msg)) ->
        raise (TestRunErr (msg, at, time_start)));
    time_start
  with
  | TestRunErr _ as err -> raise err
  | TestRunNegErr _ as err -> raise err
  | _ -> raise (TestUnknownErr time_start)

let run_test (module Simulator : SIM) neg stat relname includes_p4 path_p4 =
  try
    let time_start = run (module Simulator) neg relname includes_p4 path_p4 in
    let duration = stop time_start in
    let log = Format.asprintf "Run success: %s" path_p4 in
    log |> print_endline;
    Format.eprintf "%s\n" log;
    Format.eprintf ">>> took %.6f seconds\n" duration;
    { stat with durations = duration :: stat.durations }
  with
  | TestRunErr (msg, at, time_start) ->
      let duration = stop time_start in
      let open Util.Error in
      Format.asprintf "Error on run: %s" path_p4 |> print_endline;
      Format.eprintf "Error on run: %s\n%s\n" path_p4 (string_of_error at msg);
      Format.eprintf ">>> took %.6f seconds\n" duration;
      {
        stat with
        durations = duration :: stat.durations;
        fail_run = stat.fail_run + 1;
      }
  | TestRunNegErr time_start ->
      let duration = stop time_start in
      Format.asprintf "Error on run: %s (should fail)" path_p4 |> print_endline;
      Format.eprintf "Error on run: %s (should fail)\n" path_p4;
      Format.eprintf ">>> took %.6f seconds\n" duration;
      { stat with durations = duration :: stat.durations }
  | TestUnknownErr time_start ->
      let duration = stop time_start in
      Format.asprintf "Error on run: %s (unknown)" path_p4 |> print_endline;
      Format.eprintf "Error on run: %s (unknown)\n" path_p4;
      Format.eprintf ">>> took %.6f seconds\n" duration;
      {
        stat with
        durations = duration :: stat.durations;
        fail_run = stat.fail_run + 1;
      }

let run_nano_test_driver mode det neg path_spec relname includes_p4 testdirs_p4
    =
  let paths_p4 =
    testdirs_p4 |> List.concat_map (Filesys.collect_files ~suffix:".p4")
  in
  let total = List.length paths_p4 in
  let stat = empty_stat in
  Format.asprintf "Running interpreter test (%s) on %d files\n" relname total
  |> print_endline;
  let _spec_sim, (module Simulator) =
    Backend_sim.Build.build_nano ~det ~final:true mode [ path_spec ]
  in
  let stat =
    List.fold_left
      (fun stat path_p4 ->
        Format.asprintf "\n>>> Running interpreter test (%s) on %s" relname
          path_p4
        |> print_endline;
        run_test (module Simulator) neg stat relname includes_p4 path_p4)
      stat paths_p4
  in
  log_stat
    (Format.asprintf "\nRunning interpreter test (%s)" relname)
    stat total

let cover_run_nano_instr path_spec relname includes_p4 testdirs_p4 =
  let paths_p4 =
    testdirs_p4 |> List.concat_map (Filesys.collect_files ~suffix:".p4")
  in
  let spec_sim, (module Simulator) =
    Backend_sim.Build.build_nano ~det:false ~final:true SL_mode [ path_spec ]
  in
  let spec_sl =
    match spec_sim with SL spec_sl -> spec_sl | _ -> assert false
  in
  let cover_multi = Coverage.Instr.Multi.init spec_sl in
  let cover_multi =
    List.fold_left
      (fun cover_multi path_p4 ->
        let _, cover_single =
          run_with_instr (module Simulator) spec_sim relname includes_p4 path_p4
        in
        Coverage.Instr.Multi.extend cover_multi path_p4 cover_single)
      cover_multi paths_p4
  in
  Coverage.Instr.Log.log_spec ~path_cov_opt:None cover_multi spec_sl

let run_command =
  Core.Command.basic ~summary:"run interpreter test for nano-P4"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map path_spec =
       flag "-s" (required string) ~doc:"nano-p4 spec directory"
     and relname = flag "-rel" (required string) ~doc:"relation name"
     and includes_p4 = flag "-i" (listed string) ~doc:"p4 include paths"
     and testdirs_p4 = flag "-p4-dir" (listed string) ~doc:"p4 test directories"
     and neg = flag "-neg" no_arg ~doc:"neg testing (expect failure)"
     and det = flag "-det" no_arg ~doc:"deterministic mode"
     and mode =
       Command.Param.choose_one
         [
           flag "al" no_arg ~doc:"Run AL interpreter"
           |> map ~f:(fun b -> Core.Option.some_if b AL_mode);
           flag "sl" no_arg ~doc:"Run SL interpreter"
           |> map ~f:(fun b -> Core.Option.some_if b SL_mode);
         ]
         ~if_nothing_chosen:(Default_to SL_mode)
     in
     fun () ->
       run_nano_test_driver mode det neg path_spec relname includes_p4
         testdirs_p4)

let cover_run_command =
  Core.Command.basic ~summary:"measure instruction coverage of the nano-P4 spec"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map path_spec =
       flag "-s" (required string) ~doc:"nano-p4 spec directory"
     and relname = flag "-rel" (required string) ~doc:"relation name"
     and includes_p4 = flag "-i" (listed string) ~doc:"p4 include paths"
     and testdirs_p4 =
       flag "-p4-dir" (listed string) ~doc:"p4 test directories"
     in
     fun () -> cover_run_nano_instr path_spec relname includes_p4 testdirs_p4)

let command =
  Core.Command.group ~summary:"nano-p4spec-test-run"
    [ ("run", run_command); ("cover-run", cover_run_command) ]

let () = Command_unix.run ~version command
