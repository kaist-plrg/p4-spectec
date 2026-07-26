open Test_common
open Util.Error
open Runtime.Sim.Signature
module Test = Util.Test
module Filesys = Util.Filesys

(* Simulator test *)

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

let incr_subdir label assoc =
  match List.assoc_opt label assoc with
  | Some n ->
      List.map
        (fun (k, v) -> if String.equal k label then (k, n + 1) else (k, v))
        assoc
  | None -> (label, 1) :: assoc

let find_exclude_subdir path_p4 path_stf excludes_by_subdir =
  List.find_map
    (fun (label, entries) ->
      if
        List.exists
          (fun e -> String.equal path_p4 e || String.equal path_stf e)
          entries
      then Some label
      else None)
    excludes_by_subdir

let run_sim_test (module Simulator : SIM) stat includes_p4 excludes
    excludes_by_subdir is_patched path_p4 path_stf =
  let stat =
    if is_patched then { stat with patch_run = stat.patch_run + 1 } else stat
  in
  if Test.should_exclude_pair path_p4 path_stf excludes then (
    let log = Format.asprintf "Excluding file: %s" path_stf in
    log |> print_endline;
    let exclude_by_subdir =
      match find_exclude_subdir path_p4 path_stf excludes_by_subdir with
      | Some label -> incr_subdir label stat.exclude_by_subdir
      | None -> stat.exclude_by_subdir
    in
    {
      stat with
      durations = 0.0 :: stat.durations;
      exclude_run = stat.exclude_run + 1;
      patch_exclude_run =
        (if is_patched then stat.patch_exclude_run + 1
         else stat.patch_exclude_run);
      exclude_by_subdir;
    })
  else
    try
      let time_start =
        run_sim (module Simulator) includes_p4 path_p4 path_stf
      in
      let duration = stop time_start in
      let log = Format.asprintf "Run success: %s" path_stf in
      log |> print_endline;
      Format.eprintf "%s\n" log;
      Format.eprintf ">>> took %.6f seconds\n" duration;
      { stat with durations = duration :: stat.durations }
    with
    | TestRunErr (msg, at, time_start) ->
        let duration = stop time_start in
        Format.asprintf "Error on run: %s" path_stf |> print_endline;
        Format.eprintf "Error on run: %s\n%s\n" path_stf
          (string_of_error at msg);
        Format.eprintf ">>> took %.6f seconds\n" duration;
        {
          stat with
          durations = duration :: stat.durations;
          fail_run = stat.fail_run + 1;
          patch_fail_run =
            (if is_patched then stat.patch_fail_run + 1 else stat.patch_fail_run);
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
          patch_fail_run =
            (if is_patched then stat.patch_fail_run + 1 else stat.patch_fail_run);
        }

let run_sim_test_driver mode det arch path_spec includes_p4 excludes_p4
    testdirs_p4 testdirs_stf patchdirs =
  let excludes_by_subdir =
    Test.collect_excludes_by_subdir excludes_p4
    |> List.map (fun (label, entries) ->
           (label, List.map (fun e -> "../../../" ^ e) entries))
  in
  let excludes_p4 =
    excludes_p4 |> Test.collect_excludes
    |> List.map (fun exclude_p4 -> "../../../" ^ exclude_p4)
  in
  let path_pairs =
    Test.collect_test_pairs arch testdirs_p4 testdirs_stf patchdirs
  in
  let total = List.length path_pairs in
  let stat = empty_stat in
  Format.asprintf "Running simulation test (%s) on %d files\n" arch total
  |> print_endline;
  let _spec_sim, (module Simulator) = build_sim ~det ~arch mode [ path_spec ] in
  let stat =
    List.fold_left
      (fun stat (path_p4, path_stf, is_patched) ->
        Format.asprintf
          "\n>>> Running simulation test (%s) on %s with packet input %s" arch
          path_p4 path_stf
        |> print_endline;
        run_sim_test
          (module Simulator : SIM)
          stat includes_p4 excludes_p4 excludes_by_subdir is_patched path_p4
          path_stf)
      stat path_pairs
  in
  log_stat (Format.asprintf "\nRunning simulation test (%s)" arch) stat total

let sim_command =
  Core.Command.basic ~summary:"run simulation test"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map path_spec = flag "-s" (required string) ~doc:"p4 spec directory"
     and includes_p4 = flag "-i" (listed string) ~doc:"p4 include paths"
     and excludes_p4 = flag "-e" (listed string) ~doc:"p4 test exclude paths"
     and testdirs_p4 = flag "-p4-dir" (listed string) ~doc:"p4 test directories"
     and testdirs_stf =
       flag "-stf-dir" (listed string) ~doc:"stf test directories"
     and patchdirs = flag "-p" (listed string) ~doc:"p4 patch directory"
     and arch = flag "-arch" (required string) ~doc:"architecture name"
     and det = flag "-det" no_arg ~doc:"deterministic mode"
     and mode =
       Command.Param.choose_one
         [
           flag "al" no_arg ~doc:"Run AL interpreter"
           |> map ~f:(fun b -> Core.Option.some_if b AL_mode);
           flag "sl" no_arg ~doc:"Run SL interpreter"
           |> map ~f:(fun b -> Core.Option.some_if b SL_mode);
           flag "pl" no_arg ~doc:"Run PL interpreter"
           |> map ~f:(fun b -> Core.Option.some_if b PL_mode);
         ]
         ~if_nothing_chosen:(Default_to SL_mode)
     in
     fun () ->
       run_sim_test_driver mode det arch path_spec includes_p4 excludes_p4
         testdirs_p4 testdirs_stf patchdirs)

(* Coverage test *)

let cover_sim mode arch path_spec includes_p4 excludesdir testdirs_p4
    testdirs_stf patchdirs =
  let excludes =
    excludesdir |> Test.collect_excludes
    |> List.map (fun exclude -> "../../../" ^ exclude)
  in
  let paths_p4, paths_stf =
    Test.collect_test_pairs arch testdirs_p4 testdirs_stf patchdirs
    |> List.filter_map (fun (path_p4, path_stf, _) ->
           if Test.should_exclude_pair path_p4 path_stf excludes then None
           else Some (path_p4, path_stf))
    |> List.split
  in
  match mode with
  | `Instr ->
      cover_sim_instr SL_mode [ path_spec ] includes_p4 paths_p4 paths_stf
  | `Dangling ->
      cover_sim_dangling SL_mode [ path_spec ] includes_p4 paths_p4 paths_stf

let cover_sim_command =
  Core.Command.basic
    ~summary:"measure instruction coverage of the P4 spec when simulated"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map path_spec = flag "-s" (required string) ~doc:"p4 spec directory"
     and includes_p4 = flag "-i" (listed string) ~doc:"p4 include paths"
     and excludes_p4 = flag "-e" (listed string) ~doc:"p4 test exclude paths"
     and testdirs_p4 = flag "-p4-dir" (listed string) ~doc:"p4 test directories"
     and testdirs_stf =
       flag "-stf-dir" (listed string) ~doc:"stf test directories"
     and patchdirs = flag "-p" (listed string) ~doc:"p4 patch directory"
     and arch = flag "-arch" (required string) ~doc:"architecture name"
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
       cover_sim mode arch path_spec includes_p4 excludes_p4 testdirs_p4
         testdirs_stf patchdirs)

let command =
  Core.Command.group ~summary:"p4spec-test-sim"
    [ ("sim", sim_command); ("cover-sim", cover_sim_command) ]

let () = Command_unix.run ~version command
