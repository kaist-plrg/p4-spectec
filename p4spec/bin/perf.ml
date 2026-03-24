open Pass
module Sim = Runtime.Sim.Simulator
module Test = Util.Test
module Filesys = Util.Filesys

(* Exceptions *)

exception TestRunErr
exception TestRunNegErr
exception TestUnknownErr

(* Statistics *)

type stat = {
  exclude_run : int;
  fail_run : int;
  patch_run : int;
  patch_exclude_run : int;
  patch_fail_run : int;
  exclude_by_subdir : (string * int) list;
}

let empty_stat =
  {
    exclude_run = 0;
    fail_run = 0;
    patch_run = 0;
    patch_exclude_run = 0;
    patch_fail_run = 0;
    exclude_by_subdir = [];
  }

let log_stat name stat total : unit =
  let excludes = stat.exclude_run in
  let fails = stat.fail_run in
  let passes = total - excludes - fails in
  let patches = stat.patch_run in
  let exclude_rate = float_of_int excludes /. float_of_int total *. 100.0 in
  let pass_rate = float_of_int passes /. float_of_int total *. 100.0 in
  let fail_rate = float_of_int fails /. float_of_int total *. 100.0 in
  let patch_rate = float_of_int patches /. float_of_int total *. 100.0 in
  let patch_excludes = stat.patch_exclude_run in
  let patch_fails = stat.patch_fail_run in
  let patch_passes = patches - patch_excludes - patch_fails in
  let patch_exclude_rate =
    if patches = 0 then 0.0
    else float_of_int patch_excludes /. float_of_int patches *. 100.0
  in
  let patch_pass_rate =
    if patches = 0 then 0.0
    else float_of_int patch_passes /. float_of_int patches *. 100.0
  in
  let patch_fail_rate =
    if patches = 0 then 0.0
    else float_of_int patch_fails /. float_of_int patches *. 100.0
  in
  Format.asprintf
    "%s: [EXCLUDE] %d/%d (%.2f%%) [PASS] %d/%d (%.2f%%) [FAIL] %d/%d (%.2f%%) \
     [PATCH] %d/%d (%.2f%%)"
    name excludes total exclude_rate passes total pass_rate fails total
    fail_rate patches total patch_rate
  |> print_endline;
  Format.asprintf
    "%s: [PATCH]: [EXCLUDE] %d/%d (%.2f%%) [PASS] %d/%d (%.2f%%) [FAIL] %d/%d \
     (%.2f%%)"
    name patch_excludes patches patch_exclude_rate patch_passes patches
    patch_pass_rate patch_fails patches patch_fail_rate
  |> print_endline;
  let exclude_by_subdir =
    stat.exclude_by_subdir
    |> List.sort (fun (a, _) (b, _) -> String.compare a b)
  in
  if exclude_by_subdir <> [] then (
    Format.asprintf "%s [EXCLUDE by subdir]:" name |> print_endline;
    List.iter
      (fun (label, count) ->
        Format.asprintf "  %s: %d" label count |> print_endline)
      exclude_by_subdir)

(* Operations *)

let frontend specdir =
  specdir
  |> Filesys.collect_files ~suffix:".watsup"
  |> List.concat_map Frontend.Parse.parse_file

let elab specdir = specdir |> frontend |> Elaborate.Elab.elab_spec
let structure specdir = specdir |> elab |> Structure.Struct.struct_spec

let driver ?(det = false) ?(arch : string option) mode specdir =
  let spec_sim =
    match mode with
    | `IL ->
        let spec_il = elab specdir in
        (Runtime.Sim.Simulator.IL spec_il : Runtime.Sim.Simulator.spec)
    | `SL ->
        let spec_sl = structure specdir in
        (Runtime.Sim.Simulator.SL spec_sl : Runtime.Sim.Simulator.spec)
  in
  let (module Driver) =
    match arch with
    | Some arch -> Backend_sim.Gen.gen arch
    | None -> Backend_sim.Gen.gen_placeholder ()
  in
  Driver.init ~det spec_sim;
  (spec_sim, (module Driver : Runtime.Sim.Simulator.DRIVER))

(* Interpreter test *)

let run (module Driver : Sim.DRIVER) neg relname includes_p4 filename_p4 =
  try
    match Driver.run_program relname includes_p4 filename_p4 with
    | Pass _ -> if neg then raise TestRunNegErr
    | Fail (`Syntax _) | Fail (`Runtime _) -> raise TestRunErr
  with
  | TestRunErr as err -> raise err
  | TestRunNegErr as err -> raise err
  | _ -> raise TestUnknownErr

let run_test (module Driver : Sim.DRIVER) neg stat relname includes_p4
    excludes_p4 filename_p4 =
  if List.exists (String.equal filename_p4) excludes_p4 then (
    let log = Format.asprintf "[EXCL] Excluding file: %s" filename_p4 in
    log |> print_endline;
    { stat with exclude_run = stat.exclude_run + 1 })
  else
    try
      run (module Driver) neg relname includes_p4 filename_p4;
      let log = Format.asprintf "[OK] %s" filename_p4 in
      log |> print_endline;
      stat
    with
    | TestRunErr ->
        Format.asprintf "[FAIL] Error on run: %s" filename_p4 |> print_endline;
        { stat with fail_run = stat.fail_run + 1 }
    | TestRunNegErr ->
        Format.asprintf "[FAIL] Error on run: %s (should fail)" filename_p4
        |> print_endline;
        stat
    | TestUnknownErr ->
        Format.asprintf "[FAIL] Error on run: %s (unknown)" filename_p4
        |> print_endline;
        { stat with fail_run = stat.fail_run + 1 }

let run_test_driver mode det neg specdir relname includes_p4 excludes_p4
    testdirs_p4 =
  let excludes_p4 = excludes_p4 |> Test.collect_excludes in
  let filenames_p4 =
    testdirs_p4 |> List.concat_map (Filesys.collect_files ~suffix:".p4")
  in
  let total = List.length filenames_p4 in
  let stat = empty_stat in
  Format.asprintf "Running interpreter test (%s) on %d files\n" relname total
  |> print_endline;
  let _spec_sim, (module Driver) = driver ~det mode specdir in
  let stat =
    List.fold_left
      (fun stat filename_p4 ->
        run_test
          (module Driver)
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

(* Simulator test *)

let run_sim (module Driver : Sim.DRIVER) includes_p4 filename_p4 filename_stf =
  try
    match Driver.run_stf_test includes_p4 filename_p4 filename_stf with
    | Pass -> ()
    | Fail (`Syntax _) | Fail (`Runtime _) -> raise TestRunErr
  with
  | TestRunErr as err -> raise err
  | _ -> raise TestUnknownErr

let incr_subdir label assoc =
  match List.assoc_opt label assoc with
  | Some n ->
      List.map
        (fun (k, v) -> if String.equal k label then (k, n + 1) else (k, v))
        assoc
  | None -> (label, 1) :: assoc

let find_exclude_subdir filename_p4 filename_stf excludes_by_subdir =
  List.find_map
    (fun (label, entries) ->
      if
        List.exists
          (fun e -> String.equal filename_p4 e || String.equal filename_stf e)
          entries
      then Some label
      else None)
    excludes_by_subdir

let run_sim_test (module Driver : Sim.DRIVER) stat includes_p4 excludes
    excludes_by_subdir is_patched filename_p4 filename_stf =
  let stat =
    if is_patched then { stat with patch_run = stat.patch_run + 1 } else stat
  in
  if Test.should_exclude_pair filename_p4 filename_stf excludes then (
    let log = Format.asprintf "[EXCL] Excluding file: %s" filename_stf in
    log |> print_endline;
    let exclude_by_subdir =
      match find_exclude_subdir filename_p4 filename_stf excludes_by_subdir with
      | Some label -> incr_subdir label stat.exclude_by_subdir
      | None -> stat.exclude_by_subdir
    in
    {
      stat with
      exclude_run = stat.exclude_run + 1;
      patch_exclude_run =
        (if is_patched then stat.patch_exclude_run + 1
         else stat.patch_exclude_run);
      exclude_by_subdir;
    })
  else
    try
      run_sim (module Driver) includes_p4 filename_p4 filename_stf;
      let log = Format.asprintf "[OK] Run success: %s" filename_stf in
      log |> print_endline;
      stat
    with
    | TestRunErr ->
        Format.asprintf "[FAIL] Error on run: %s" filename_stf |> print_endline;
        {
          stat with
          fail_run = stat.fail_run + 1;
          patch_fail_run =
            (if is_patched then stat.patch_fail_run + 1 else stat.patch_fail_run);
        }
    | TestUnknownErr ->
        Format.asprintf "[FAIL] Error on run: %s (unknown)" filename_stf
        |> print_endline;
        {
          stat with
          fail_run = stat.fail_run + 1;
          patch_fail_run =
            (if is_patched then stat.patch_fail_run + 1 else stat.patch_fail_run);
        }

let run_sim_test_driver mode det arch specdir includes_p4 excludes_p4
    testdirs_p4 testdirs_stf patchdirs =
  let excludes_by_subdir = Test.collect_excludes_by_subdir excludes_p4 in
  let excludes_p4 = excludes_p4 |> Test.collect_excludes in
  let filename_pairs =
    Test.collect_test_pairs arch testdirs_p4 testdirs_stf patchdirs
  in
  let total = List.length filename_pairs in
  let stat = empty_stat in
  Format.asprintf "Running simulation test (%s) on %d files\n" arch total
  |> print_endline;
  let _spec_sim, (module Driver) = driver ~det ~arch mode specdir in
  let stat =
    List.fold_left
      (fun stat (filename_p4, filename_stf, is_patched) ->
        run_sim_test
          (module Driver : Sim.DRIVER)
          stat includes_p4 excludes_p4 excludes_by_subdir is_patched filename_p4
          filename_stf)
      stat filename_pairs
  in
  log_stat (Format.asprintf "\nRunning simulation test (%s)" arch) stat total

let sim_command =
  Core.Command.basic ~summary:"run simulation test"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map specdir = flag "-s" (required string) ~doc:"p4 spec directory"
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
           flag "il" no_arg ~doc:"Run IL interpreter"
           |> map ~f:(fun b -> Core.Option.some_if b `IL);
           flag "sl" no_arg ~doc:"Run SL interpreter"
           |> map ~f:(fun b -> Core.Option.some_if b `SL);
         ]
         ~if_nothing_chosen:(Default_to `SL)
     in
     fun () ->
       run_sim_test_driver mode det arch specdir includes_p4 excludes_p4
         testdirs_p4 testdirs_stf patchdirs)

(* Entry point *)

let command =
  Core.Command.group ~summary:"p4perf"
    [ ("run", run_command); ("sim", sim_command) ]

let () = Command_unix.run ~version:"0.1" command
