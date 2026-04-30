open Test_common
open Util.Error
open Runtime.Sim.Signature
module Test = Util.Test
module Filesys = Util.Filesys

(* Interpreter test *)

let run (module Simulator : SIM) neg relname includes_p4 filename_p4 =
  let time_start = start () in
  try
    Simulator.Interp.clear ();
    (match Simulator.Interp.eval_program relname includes_p4 filename_p4 with
    | Pass _ -> if neg then raise (TestRunNegErr time_start)
    | Fail (`Syntax (at, msg)) | Fail (`Runtime (at, msg)) ->
        raise (TestRunErr (msg, at, time_start)));
    time_start
  with
  | TestRunErr _ as err -> raise err
  | TestRunNegErr _ as err -> raise err
  | _ -> raise (TestUnknownErr time_start)

let run_test (module Simulator : SIM) neg stat relname includes_p4 excludes_p4
    filename_p4 =
  if List.exists (String.equal filename_p4) excludes_p4 then (
    let log = Format.asprintf "Excluding file: %s" filename_p4 in
    log |> print_endline;
    {
      stat with
      durations = 0.0 :: stat.durations;
      exclude_run = stat.exclude_run + 1;
    })
  else
    try
      let time_start =
        run (module Simulator) neg relname includes_p4 filename_p4
      in
      let duration = stop time_start in
      let log = Format.asprintf "Run success: %s" filename_p4 in
      log |> print_endline;
      Format.eprintf "%s\n" log;
      Format.eprintf ">>> took %.6f seconds\n" duration;
      { stat with durations = duration :: stat.durations }
    with
    | TestRunErr (msg, at, time_start) ->
        let duration = stop time_start in
        Format.asprintf "Error on run: %s" filename_p4 |> print_endline;
        Format.eprintf "Error on run: %s\n%s\n" filename_p4
          (string_of_error at msg);
        Format.eprintf ">>> took %.6f seconds\n" duration;
        {
          stat with
          durations = duration :: stat.durations;
          fail_run = stat.fail_run + 1;
        }
    | TestRunNegErr time_start ->
        let duration = stop time_start in
        Format.asprintf "Error on run: %s (should fail)" filename_p4
        |> print_endline;
        Format.eprintf "Error on run: %s (should fail)\n" filename_p4;
        Format.eprintf ">>> took %.6f seconds\n" duration;
        { stat with durations = duration :: stat.durations }
    | TestUnknownErr time_start ->
        let duration = stop time_start in
        Format.asprintf "Error on run: %s (unknown)" filename_p4
        |> print_endline;
        Format.eprintf "Error on run: %s (unknown)\n" filename_p4;
        Format.eprintf ">>> took %.6f seconds\n" duration;
        {
          stat with
          durations = duration :: stat.durations;
          fail_run = stat.fail_run + 1;
        }

let run_test_driver mode det neg specdir relname includes_p4 excludes_p4
    testdirs_p4 =
  let excludes_p4 =
    excludes_p4 |> Test.collect_excludes
    |> List.map (fun exclude_p4 -> "../../../../../" ^ exclude_p4)
  in
  let filenames_p4 =
    testdirs_p4 |> List.concat_map (Filesys.collect_files ~suffix:".p4")
  in
  let total = List.length filenames_p4 in
  let stat = empty_stat in
  Format.asprintf "Running interpreter test (%s) on %d files\n" relname total
  |> print_endline;
  let _spec_sim, (module Simulator) = simulator ~det mode specdir in
  let stat =
    List.fold_left
      (fun stat filename_p4 ->
        Format.asprintf "\n>>> Running interpreter test (%s) on %s" relname
          filename_p4
        |> print_endline;
        run_test
          (module Simulator)
          neg stat relname includes_p4 excludes_p4 filename_p4)
      stat filenames_p4
  in
  log_stat
    (Format.asprintf "\nRunning interpreter test (%s)" relname)
    stat total

let run_command =
  Core.Command.basic ~summary:"run interpreter test"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map specdir = flag "-s" (required string) ~doc:"p4 spec directory"
     and relname = flag "-rel" (required string) ~doc:"relation name"
     and includes_p4 = flag "-i" (listed string) ~doc:"p4 include paths"
     and excludes_p4 = flag "-e" (listed string) ~doc:"p4 test exclude paths"
     and testdirs_p4 = flag "-p4-dir" (listed string) ~doc:"p4 test directories"
     and neg = flag "-neg" no_arg ~doc:"neg testsing (expect failure)"
     and det = flag "-det" no_arg ~doc:"deterministic mode"
     and mode =
       Command.Param.choose_one
         [
           flag "il" no_arg ~doc:"Run IL interpreter"
           |> map ~f:(fun b -> Core.Option.some_if b `IL);
           flag "sl" no_arg ~doc:"Run SL interpreter"
           |> map ~f:(fun b -> Core.Option.some_if b `SL);
         ]
         ~if_nothing_chosen:(Default_to `SL)
     in
     fun () ->
       run_test_driver mode det neg specdir relname includes_p4 excludes_p4
         testdirs_p4)

(* Coverage test *)

let cover_run mode specdir relname includes_p4 excludes_p4 testdirs_p4 =
  let excludes_p4 =
    excludes_p4 |> Test.collect_excludes
    |> List.map (fun exclude_p4 -> "../../../../../" ^ exclude_p4)
  in
  let filenames_p4 =
    testdirs_p4
    |> List.concat_map (Filesys.collect_files ~suffix:".p4")
    |> List.filter (fun filename_p4 -> not (List.mem filename_p4 excludes_p4))
  in
  match mode with
  | `Instr -> cover_run_instr `SL specdir relname includes_p4 filenames_p4
  | `Dangling -> cover_run_dangling `SL specdir relname includes_p4 filenames_p4

let cover_run_command =
  Core.Command.basic ~summary:"measure coverage of the spec"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map specdir = flag "-s" (required string) ~doc:"p4 spec directory"
     and relname = flag "-rel" (required string) ~doc:"relation name"
     and includes_p4 = flag "-i" (listed string) ~doc:"p4 include paths"
     and excludes_p4 = flag "-e" (listed string) ~doc:"p4 test exclude paths"
     and testdirs_p4 = flag "-p4-dir" (listed string) ~doc:"p4 test directories"
     and mode =
       Command.Param.choose_one
         [
           flag "instr" no_arg ~doc:"measure instruction coverage"
           |> map ~f:(fun b -> Core.Option.some_if b `Instr);
           flag "dangling" no_arg ~doc:"measure dangling coverage"
           |> map ~f:(fun b -> Core.Option.some_if b `Dangling);
         ]
         ~if_nothing_chosen:(Default_to `Instr)
     in
     fun () ->
       cover_run mode specdir relname includes_p4 excludes_p4 testdirs_p4)

let command =
  Core.Command.group ~summary:"p4spec-test-run"
    [ ("run", run_command); ("cover-run", cover_run_command) ]

let () = Command_unix.run ~version command
