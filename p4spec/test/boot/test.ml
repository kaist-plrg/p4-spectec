open Test_common
open Util.Error
open Runtime.Sim.Signature
open Backend_boot.Config
module Test = Util.Test
module Filesys = Util.Filesys

(* Interpreter test *)

let boot (module Booter : RUNNER) neg tower =
  let time_start = start () in
  try
    Booter.clear ();
    let value_spectec = Backend_boot.Patch.apply_tower tower in
    let rel = tower.level_boot.layer.rel in
    (match Booter.Interp.eval_rel rel [ value_spectec ] with
    | Pass _ -> if neg then raise (TestRunNegErr time_start)
    | Fail (at, msg) -> raise (TestRunErr (msg, at, time_start)));
    time_start
  with
  | TestRunErr _ as err -> raise err
  | TestRunNegErr _ as err -> raise err
  | _ -> raise (TestUnknownErr time_start)

let boot_test (module Booter : RUNNER) neg stat tower excludes_p4 path_p4 =
  if List.exists (String.equal tower.target.path) excludes_p4 then (
    let log = Format.asprintf "Excluding file: %s" tower.target.path in
    log |> print_endline;
    {
      stat with
      durations = 0.0 :: stat.durations;
      exclude_run = stat.exclude_run + 1;
    })
  else
    try
      let time_start = boot (module Booter) neg tower in
      let duration = stop time_start in
      let log = Format.asprintf "Run success: %s" path_p4 in
      log |> print_endline;
      Format.eprintf "%s\n" log;
      Format.eprintf ">>> took %.6f seconds\n" duration;
      { stat with durations = duration :: stat.durations }
    with
    | TestRunErr (msg, at, time_start) ->
        let duration = stop time_start in
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
        Format.asprintf "Error on run: %s (should fail)" path_p4
        |> print_endline;
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

let boot_test_driver path_tower det neg includes_p4 excludes_p4 testdirs_p4 =
  let excludes_p4 =
    excludes_p4 |> Test.collect_excludes
    |> List.map (fun exclude_p4 -> "../../../../../" ^ exclude_p4)
  in
  let paths_p4 =
    testdirs_p4 |> List.concat_map (Filesys.collect_files ~suffix:".p4")
  in
  let total = List.length paths_p4 in
  let tower =
    let target = { includes = includes_p4; path = "" } in
    Backend_boot.Config.tower_of_file path_tower target
  in
  let rel = tower.level_boot.layer.rel in
  let rel_p4 = tower.level_target.layer.rel in
  Format.asprintf "Running boot test (%s/%s) on %d files\n" rel rel_p4 total
  |> print_endline;
  let _, _, _, (module Booter) = Backend_boot.Build.build_tower ~det tower in
  let stat =
    List.fold_left
      (fun stat path_p4 ->
        Format.asprintf "\n>>> Running boot test (%s/%s) on %s" rel rel_p4
          path_p4
        |> print_endline;
        let tower =
          { tower with target = { tower.target with path = path_p4 } }
        in
        boot_test (module Booter) neg stat tower excludes_p4 path_p4)
      empty_stat paths_p4
  in
  log_stat (Format.asprintf "\nRunning boot test (%s/%s)" rel rel_p4) stat total

let boot_command =
  Core.Command.basic ~summary:"run interpreter test (boot)"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map path_tower =
       flag "-tower" (required string) ~doc:"FILE tower config JSON file"
     and includes_p4 = flag "-i" (listed string) ~doc:"p4 include path"
     and excludes_p4 = flag "-e" (listed string) ~doc:"p4 test exclude paths"
     and testdirs_p4 = flag "-p" (listed string) ~doc:"p4 test directories"
     and neg = flag "-neg" no_arg ~doc:"neg testing (expect failure)"
     and det = flag "-det" no_arg ~doc:"deterministic mode" in
     fun () ->
       boot_test_driver path_tower det neg includes_p4 excludes_p4 testdirs_p4)

let command =
  Core.Command.group ~summary:"p4spec-test-boot" [ ("boot", boot_command) ]

let () = Command_unix.run ~version command
