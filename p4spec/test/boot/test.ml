open Test_common
open Util.Error
open Runtime.Sim.Signature
module Test = Util.Test
module Filesys = Util.Filesys

(* Interpreter test *)

let boot (module Booter : RUNNER) neg rel specdir_p4 rel_p4 includes_p4
    filename_p4 =
  let time_start = start () in
  try
    Booter.Interp.clear ();
    let filenames_spec_p4 =
      specdir_p4 |> Filesys.collect_files ~suffix:".watsup"
    in
    let value_spectec =
      Backend_boot.Patch.apply_square filenames_spec_p4 rel_p4 includes_p4
        filename_p4
    in
    (match Booter.Interp.eval_rel rel [ value_spectec ] with
    | Pass _ -> if neg then raise (TestRunNegErr time_start)
    | Fail (at, msg) -> raise (TestRunErr (msg, at, time_start)));
    time_start
  with
  | TestRunErr _ as err -> raise err
  | TestRunNegErr _ as err -> raise err
  | _ -> raise (TestUnknownErr time_start)

let boot_test (module Booter : RUNNER) neg stat rel specdir_p4 rel_p4
    includes_p4 excludes_p4 filename_p4 =
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
        boot (module Booter) neg rel specdir_p4 rel_p4 includes_p4 filename_p4
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

let boot_test_driver mode det neg specdir rel specdir_p4 rel_p4 includes_p4
    excludes_p4 testdirs_p4 =
  let excludes_p4 =
    excludes_p4 |> Test.collect_excludes
    |> List.map (fun exclude_p4 -> "../../../../../" ^ exclude_p4)
  in
  let filenames_p4 =
    testdirs_p4 |> List.concat_map (Filesys.collect_files ~suffix:".p4")
  in
  let total = List.length filenames_p4 in
  let stat = empty_stat in
  Format.asprintf "Running boot test (%s/%s) on %d files\n" rel rel_p4 total
  |> print_endline;
  let _, (module Booter) = booter ~det mode specdir specdir_p4 in
  let stat =
    List.fold_left
      (fun stat filename_p4 ->
        Format.asprintf "\n>>> Running boot test (%s/%s) on %s" rel rel_p4
          filename_p4
        |> print_endline;
        boot_test
          (module Booter)
          neg stat rel specdir_p4 rel_p4 includes_p4 excludes_p4 filename_p4)
      stat filenames_p4
  in
  log_stat (Format.asprintf "\nRunning boot test (%s/%s)" rel rel_p4) stat total

let boot_command =
  Core.Command.basic ~summary:"run interpreter test (boot)"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map specdir = flag "-s0" (required string) ~doc:"boot spec directory"
     and relname = flag "-r0" (required string) ~doc:"boot relation to run"
     and specdir_p4 = flag "-s1" (required string) ~doc:"p4 spec directory"
     and rel_p4 = flag "-r1" (required string) ~doc:"p4 spec relation to run"
     and includes_p4 = flag "-i1" (listed string) ~doc:"p4 include path"
     and excludes_p4 = flag "-e1" (listed string) ~doc:"p4 test exclude paths"
     and testdirs_p4 = flag "-p1" (listed string) ~doc:"p4 test directories"
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
       boot_test_driver mode det neg specdir relname specdir_p4 rel_p4
         includes_p4 excludes_p4 testdirs_p4)

let command =
  Core.Command.group ~summary:"p4spec-test-boot" [ ("boot", boot_command) ]

let () = Command_unix.run ~version command
