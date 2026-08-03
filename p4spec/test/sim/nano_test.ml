open Test_common
open Runtime.Sim.Signature
module Filesys = Util.Filesys

let run_sim (module Simulator : SIM) includes_p4 path_p4 path_stf =
  let time_start = start () in
  try
    Simulator.clear ();
    (match Simulator.run_stf_test includes_p4 path_p4 path_stf with
    | Pass -> ()
    | Fail (`Syntax (at, msg)) | Fail (`Runtime (at, msg)) ->
        raise (TestRunErr (msg, at, time_start)));
    time_start
  with
  | TestRunErr _ as err -> raise err
  | _ -> raise (TestUnknownErr time_start)

let run_sim_test (module Simulator : SIM) stat includes_p4 path_p4 path_stf =
  try
    let time_start = run_sim (module Simulator) includes_p4 path_p4 path_stf in
    let duration = stop time_start in
    let log = Format.asprintf "Run success: %s" path_stf in
    log |> print_endline;
    Format.eprintf "%s\n" log;
    Format.eprintf ">>> took %.6f seconds\n" duration;
    { stat with durations = duration :: stat.durations }
  with
  | TestRunErr (msg, at, time_start) ->
      let duration = stop time_start in
      let open Util.Error in
      Format.asprintf "Error on run: %s" path_stf |> print_endline;
      Format.eprintf "Error on run: %s\n%s\n" path_stf (string_of_error at msg);
      Format.eprintf ">>> took %.6f seconds\n" duration;
      {
        stat with
        durations = duration :: stat.durations;
        fail_run = stat.fail_run + 1;
      }
  | TestUnknownErr time_start ->
      let duration = stop time_start in
      Format.asprintf "Error on run: %s (unknown)" path_stf |> print_endline;
      Format.eprintf "Error on run: %s (unknown)\n" path_stf;
      Format.eprintf ">>> took %.6f seconds\n" duration;
      {
        stat with
        durations = duration :: stat.durations;
        fail_run = stat.fail_run + 1;
      }

let run_nano_sim_driver mode det path_spec includes_p4 testdirs_p4 =
  let paths_p4 =
    testdirs_p4 |> List.concat_map (Filesys.collect_files ~suffix:".p4")
  in
  let path_pairs =
    paths_p4
    |> List.filter_map (fun path_p4 ->
           let path_stf =
             String.sub path_p4 0 (String.length path_p4 - 3) ^ ".stf"
           in
           if Sys.file_exists path_stf then Some (path_p4, path_stf) else None)
  in
  let total = List.length path_pairs in
  let stat = empty_stat in
  Format.asprintf "Running simulation test (nano) on %d files\n" total
  |> print_endline;
  let _spec_sim, (module Simulator) =
    Backend_sim.Build.build_nano ~det ~final:true mode [ path_spec ]
  in
  let stat =
    List.fold_left
      (fun stat (path_p4, path_stf) ->
        Format.asprintf "\n>>> Running simulation test (nano) on %s with packet input %s"
          path_p4 path_stf
        |> print_endline;
        run_sim_test (module Simulator) stat includes_p4 path_p4 path_stf)
      stat path_pairs
  in
  log_stat "\nRunning simulation test (nano)" stat total

let sim_command =
  Core.Command.basic ~summary:"run simulation test for nano-P4"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map path_spec =
       flag "-s" (required string) ~doc:"nano-p4 spec directory"
     and includes_p4 = flag "-i" (listed string) ~doc:"p4 include paths"
     and testdirs_p4 = flag "-p4-dir" (listed string) ~doc:"p4 test directories"
     and det = flag "-det" no_arg ~doc:"deterministic mode"
     and mode =
       Command.Param.choose_one
         [
           flag "il" no_arg ~doc:"Run IL interpreter"
           |> map ~f:(fun b -> Core.Option.some_if b IL_mode);
           flag "sl" no_arg ~doc:"Run SL interpreter"
           |> map ~f:(fun b -> Core.Option.some_if b SL_mode);
         ]
         ~if_nothing_chosen:(Default_to SL_mode)
     in
     fun () ->
       run_nano_sim_driver mode det path_spec includes_p4 testdirs_p4)

let command =
  Core.Command.group ~summary:"nano-p4spec-test-sim"
    [ ("sim", sim_command) ]

let () = Command_unix.run ~version command
