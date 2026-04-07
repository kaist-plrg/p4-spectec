open Test_common
open Util.Error
module Sim = Runtime.Sim.Simulator
module Test = Util.Test
module Filesys = Util.Filesys

(* Simulator test *)

let run_sim ~fmt (module Driver : Sim.DRIVER) includes_p4 filename_p4 filename_stf =
  let time_start = start () in
  try
    (match Driver.run_stf_test fmt includes_p4 filename_p4 filename_stf with
    | Pass -> ()
    | Fail (`Syntax (at, msg)) | Fail (`Runtime (at, msg)) ->
        raise (TestRunErr (msg, at, time_start)));
    time_start
  with
  | TestRunErr _ as err -> raise err
  | _ -> raise (TestUnknownErr time_start)

let run_sim_test ~fmt (module Driver : Sim.DRIVER) stat includes_p4 excludes
    filename_p4 filename_stf =
  if Test.should_exclude_pair filename_p4 filename_stf excludes then
    let log = Format.asprintf "Excluding file: %s\n" filename_stf in
    (* let log = Format.asprintf "Excluding file: %s" filename_stf in *)
    (* log |> print_endline; *)

    ( {
        stat with
        durations = 0.0 :: stat.durations;
        exclude_run = stat.exclude_run + 1;
      },
      log )
  else
    try
      let time_start =
        run_sim ~fmt (module Driver) includes_p4 filename_p4 filename_stf
      in
      let duration = stop time_start in
      let log = Format.asprintf "Run success: %s\n" filename_stf in
      (* let log = Format.asprintf "Run success: %s" filename_stf in *)
      (* log |> print_endline; *)
      (* Format.eprintf "%s\n" log; *)
      (* Format.eprintf ">>> took %.6f seconds\n" duration; *)
      ({ stat with durations = duration :: stat.durations }, log)
    with
    | TestRunErr (msg, at, time_start) ->
        let duration = stop time_start in
        let log = Format.asprintf "Error on run: %s\n%s\n" filename_stf (string_of_error at msg) in
        (* Format.asprintf "Error on run: %s" filename_stf |> print_endline; *)
        (* Format.eprintf "Error on run: %s\n%s\n" filename_stf *)
        (*   (string_of_error at msg); *)
        (* Format.eprintf ">>> took %.6f seconds\n" duration; *)
        ( {
            stat with
            durations = duration :: stat.durations;
            fail_run = stat.fail_run + 1;
          },
          log )
    | TestUnknownErr time_start ->
        let duration = stop time_start in
        let log = Format.asprintf "Error on run: %s (unknown)\n" filename_stf in
        (* Format.asprintf "Error on run: %s (unknown)" filename_stf *)
        (* |> print_endline; *)
        (* Format.eprintf "Error on run: %s (unknown)\n" filename_stf; *)
        (* Format.eprintf ">>> took %.6f seconds\n" duration; *)
        ( {
            stat with
            durations = duration :: stat.durations;
            fail_run = stat.fail_run + 1;
          },
          log )

let run_sim_test_driver pool mode det arch specdir includes_p4 excludes_p4
    testdirs_p4 testdirs_stf patchdir =
  let excludes_p4 =
    excludes_p4 |> Test.collect_excludes
    |> List.map (fun exclude_p4 -> "../../../../../" ^ exclude_p4)
  in
  let filename_pairs =
    Test.collect_test_pairs arch testdirs_p4 testdirs_stf patchdir
  in
  let pairs_array = Array.of_list filename_pairs in
  let total = List.length filename_pairs in
  let _stat = empty_stat in
  Format.asprintf "Running simulation test (%s) on %d files\n" arch total
  |> print_endline;
  let _spec_sim, (module Driver) = driver ~det ~arch mode specdir in

  let (stat, log) = 
    Domainslib.Task.parallel_for_reduce
      ~start:0
      ~finish:(total - 1)
      ~body:(fun i ->
        let (filename_p4, filename_stf) = pairs_array.(i) in
        let buf = Buffer.create 512 in
        let fmt = Format.formatter_of_buffer buf in
        let (stat, log) = run_sim_test ~fmt (module Driver : Sim.DRIVER)
          empty_stat includes_p4 excludes_p4 filename_p4 filename_stf
        in
        Format.pp_print_flush fmt ();
        (stat, (Format.asprintf
          ">>> Running simulation test (%s) on %s with packet input %s\n" arch
          filename_p4 filename_stf) ^ Buffer.contents buf ^ log)
      )
      pool
      merge_stat
      (empty_stat, "")
  in

  print_endline log;

  log_stat (Format.asprintf "Running simulation test (%s)" arch) stat total

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
     and patchdir = flag "-p" (required string) ~doc:"p4 patch directory"
     and arch = flag "-arch" (required string) ~doc:"architecture name"
     and det = flag "-det" no_arg ~doc:"deterministic mode"
     and cores = flag "-j" (optional_with_default 1 int) ~doc:"number of jobs (cores)"
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
       let num_domains = cores - 1 in
       let pool = Domainslib.Task.setup_pool ~num_domains () in
       Core.Exn.protect
         ~f:(fun () ->
           Domainslib.Task.run pool (fun () ->
               run_sim_test_driver pool mode det arch specdir includes_p4
                 excludes_p4 testdirs_p4 testdirs_stf patchdir))
         ~finally:(fun () -> Domainslib.Task.teardown_pool pool))

(* Coverage test *)

let cover_sim mode arch specdir includes_p4 excludesdir testdirs_p4 testdirs_stf
    patchdir =
  let excludes =
    excludesdir |> Test.collect_excludes
    |> List.map (fun exclude -> "../../../../../" ^ exclude)
  in
  let filenames_p4, filenames_stf =
    Test.collect_test_pairs arch testdirs_p4 testdirs_stf patchdir
    |> List.filter (fun (filename_p4, filename_stf) ->
           not (Test.should_exclude_pair filename_p4 filename_stf excludes))
    |> List.split
  in
  match mode with
  | `Instr -> cover_sim_instr `SL specdir includes_p4 filenames_p4 filenames_stf
  | `Dangling ->
      cover_sim_dangling `SL specdir includes_p4 filenames_p4 filenames_stf

let cover_sim_command =
  Core.Command.basic
    ~summary:"measure instruction coverage of the P4 spec when simulated"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map specdir = flag "-s" (required string) ~doc:"p4 spec directory"
     and includes_p4 = flag "-i" (listed string) ~doc:"p4 include paths"
     and excludes_p4 = flag "-e" (listed string) ~doc:"p4 test exclude paths"
     and testdirs_p4 = flag "-p4-dir" (listed string) ~doc:"p4 test directories"
     and testdirs_stf =
       flag "-stf-dir" (listed string) ~doc:"stf test directories"
     and patchdir = flag "-p" (required string) ~doc:"p4 patch directory"
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
       cover_sim mode arch specdir includes_p4 excludes_p4 testdirs_p4
         testdirs_stf patchdir)

let command =
  Core.Command.group ~summary:"p4spec-test-sim"
    [ ("sim", sim_command); ("cover-sim", cover_sim_command) ]

let () = Command_unix.run ~version command
