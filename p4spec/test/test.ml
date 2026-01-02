open Lang
open Pass
module Sim = Runtime.Sim.Simulator
module Strings = Util.Strings
module Filesys = Util.Filesys
open Util.Error
open Util.Source

let version = "0.1"

(* Statistics *)

type stat = { durations : float list; exclude_run : int; fail_run : int }

let empty_stat = { durations = []; exclude_run = 0; fail_run = 0 }

let log_stat name stat total : unit =
  let excludes = stat.exclude_run in
  let fails = stat.fail_run in
  let passes = total - excludes - fails in
  let exclude_rate = float_of_int excludes /. float_of_int total *. 100.0 in
  let pass_rate = float_of_int passes /. float_of_int total *. 100.0 in
  let fail_rate = float_of_int fails /. float_of_int total *. 100.0 in
  let durations = List.sort compare stat.durations in
  let duration_total = List.fold_left ( +. ) 0.0 durations in
  let duration_avg = duration_total /. float_of_int total in
  let duration_max = durations |> List.rev |> List.hd in
  let duration_min = durations |> List.hd in
  Format.asprintf
    "%s: [EXCLUDE] %d/%d (%.2f%%) [PASS] %d/%d (%.2f%%) [FAIL] %d/%d (%.2f%%)"
    name excludes total exclude_rate passes total pass_rate fails total
    fail_rate
  |> print_endline;
  Format.eprintf "%s: [TOTAL] %.6f [AVG] %.6f [MAX] %.6f [MIN] %.6f\n" name
    duration_total duration_avg duration_max duration_min

(* Exceptions *)

exception TestRunErr of string * region * float
exception TestRunNegErr of float
exception TestParseFileErr of string * region * float
exception TestParseStringErr of string * region * float
exception TestParseRoundtripErr of float
exception TestUnknownErr of float

(* Timer *)

let start () = Unix.gettimeofday ()
let stop start = Unix.gettimeofday () -. start

(* Spec elaboration test *)

let elab specdir =
  specdir
  |> Filesys.collect_files ~suffix:".watsup"
  |> List.concat_map Frontend.Parse.parse_file
  |> Elaborate.Elab.elab_spec

let elab_test specdir =
  let spec_il = elab specdir in
  Il.Print.string_of_spec spec_il |> print_endline

let elab_command =
  Core.Command.basic ~summary:"run elaboration test"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map specdir = flag "-s" (required string) ~doc:"p4 spec directory" in
     fun () ->
       try elab_test specdir
       with ParseError (at, msg) | ElabError (at, msg) ->
         Format.printf "Error on elaboration: %s\n" (string_of_error at msg))

(* Structuring test *)

let structure specdir = specdir |> elab |> Structure.Struct.struct_spec

let structure_test specdir =
  let spec_sl = structure specdir in
  Sl.Print.string_of_spec spec_sl |> print_endline

let structure_command =
  Core.Command.basic ~summary:"run structuring test"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map specdir = flag "-s" (required string) ~doc:"p4 spec directory" in
     fun () ->
       try structure_test specdir
       with ParseError (at, msg) | ElabError (at, msg) ->
         Format.printf "%s\n" (string_of_error at msg))

(* Prose test *)

let prosify specdir = specdir |> structure |> Prose.Prosify.prosify_spec

let prose_test specdir =
  let spec_pl = prosify specdir in
  Pl.Render.render_spec spec_pl |> print_endline

let prose_command =
  Core.Command.basic ~summary:"run prose test"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map specdir = flag "-s" (required string) ~doc:"p4 spec directory" in
     fun () ->
       try prose_test specdir
       with ParseError (at, msg) | ElabError (at, msg) ->
         Format.printf "%s\n" (string_of_error at msg))

(* Interpreter test *)

let run (module Runner : Sim.DRIVER) negative spec_sim relname includes_p4
    filename_p4 =
  let time_start = start () in
  try
    (match
       Runner.run_program ~derive:false spec_sim relname includes_p4 filename_p4
     with
    | Pass _ -> if negative then raise (TestRunNegErr time_start)
    | Fail (at, msg, _) -> raise (TestRunErr (msg, at, time_start))
    | IllFormed (at, msg, _) -> raise (TestRunErr (msg, at, time_start)));
    time_start
  with
  | TestRunErr _ as err -> raise err
  | TestRunNegErr _ as err -> raise err
  | _ -> raise (TestUnknownErr time_start)

let run_test negative stat spec_sim relname includes_p4 excludes_p4 filename_p4
    =
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
      let (module Runner) = Backend_sim.Gen.gen_placeholder () in
      let time_start =
        run (module Runner) negative spec_sim relname includes_p4 filename_p4
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
        let log =
          Format.asprintf "Error on run: %s\n%s" filename_p4
            (string_of_error at msg)
        in
        log |> print_endline;
        Format.eprintf "%s\n" log;
        Format.eprintf ">>> took %.6f seconds\n" duration;
        {
          stat with
          durations = duration :: stat.durations;
          fail_run = stat.fail_run + 1;
        }
    | TestRunNegErr time_start ->
        let duration = stop time_start in
        let log = Format.asprintf "Error on run: should fail" in
        log |> print_endline;
        Format.eprintf "%s\n" log;
        Format.eprintf ">>> took %.6f seconds\n" duration;
        { stat with durations = duration :: stat.durations }
    | TestUnknownErr time_start ->
        let duration = stop time_start in
        let log = Format.asprintf "Error on run: unknown" in
        log |> print_endline;
        Format.eprintf "%s\n" log;
        Format.eprintf ">>> took %.6f seconds\n" duration;
        {
          stat with
          durations = duration :: stat.durations;
          fail_run = stat.fail_run + 1;
        }

let run_test_driver mode negative specdir relname includes_p4 excludes_p4
    testdir_p4 =
  let spec_sim =
    match mode with
    | `IL ->
        let spec_il = elab specdir in
        Sim.IL spec_il
    | `SL ->
        let spec_sl = structure specdir in
        Sim.SL spec_sl
  in
  let excludes_p4 =
    excludes_p4 |> Filesys.collect_excludes
    |> List.map (fun exclude_p4 -> "../../../../" ^ exclude_p4)
  in
  let filenames_p4 = Filesys.collect_files ~suffix:".p4" testdir_p4 in
  let total = List.length filenames_p4 in
  let stat = empty_stat in
  Format.asprintf "Running interpreter test (%s) on %d files\n" relname total
  |> print_endline;
  let stat =
    List.fold_left
      (fun stat filename_p4 ->
        Format.asprintf "\n>>> Running interpreter test (%s) on %s" relname
          filename_p4
        |> print_endline;
        run_test negative stat spec_sim relname includes_p4 excludes_p4
          filename_p4)
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
     and testdir_p4 = flag "-d" (required string) ~doc:"p4 test directory"
     and negative = flag "-neg" no_arg ~doc:"use negative typing rules"
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
       run_test_driver mode negative specdir relname includes_p4 excludes_p4
         testdir_p4)

(* Simulator test *)

let run_sim (module Runner : Sim.DRIVER) spec_sim includes_p4 filename_p4
    filename_stf =
  let time_start = start () in
  try
    (match
       Runner.run_stf_test spec_sim includes_p4 filename_p4 filename_stf
     with
    | Pass -> ()
    | Fail (at, msg) -> raise (TestRunErr (msg, at, time_start))
    | IllFormed (at, msg) -> raise (TestRunErr (msg, at, time_start)));
    time_start
  with
  | TestRunErr _ as err -> raise err
  | _ -> raise (TestUnknownErr time_start)

let run_sim_test stat arch spec_sim includes_p4 excludes_p4 filename_p4
    filename_stf =
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
      let (module Runner) = Backend_sim.Gen.gen arch in
      let time_start =
        run_sim (module Runner) spec_sim includes_p4 filename_p4 filename_stf
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
        let log =
          Format.asprintf "Error on run: %s\n%s" filename_p4
            (string_of_error at msg)
        in
        log |> print_endline;
        Format.eprintf "%s\n" log;
        Format.eprintf ">>> took %.6f seconds\n" duration;
        {
          stat with
          durations = duration :: stat.durations;
          fail_run = stat.fail_run + 1;
        }
    | TestUnknownErr time_start ->
        let duration = stop time_start in
        let log = Format.asprintf "Error on run: unknown" in
        log |> print_endline;
        Format.eprintf "%s\n" log;
        Format.eprintf ">>> took %.6f seconds\n" duration;
        {
          stat with
          durations = duration :: stat.durations;
          fail_run = stat.fail_run + 1;
        }

let run_sim_test_driver mode arch specdir includes_p4 excludes_p4 testdir
    patchdir =
  let spec_sim =
    match mode with
    | `IL ->
        let spec_il = elab specdir in
        Sim.IL spec_il
    | `SL ->
        let spec_sl = structure specdir in
        Sim.SL spec_sl
  in
  let excludes_p4 =
    excludes_p4 |> Filesys.collect_excludes
    |> List.map (fun exclude_p4 -> "../../../../" ^ exclude_p4)
  in
  let filenames_p4 = Filesys.collect_files ~suffix:".p4" testdir in
  let filenames_p4 =
    List.filter
      (fun filename_p4 ->
        let contents = Filesys.read_file filename_p4 in
        match arch with
        | "v1model" ->
            Strings.contains_substring "#include <v1model.p4>" contents
            || Strings.contains_substring "#include \"v1model.p4\"" contents
        | _ -> false)
      filenames_p4
  in
  let filenames_p4_patch = Filesys.collect_files ~suffix:".p4" patchdir in
  let filenames_p4 =
    Filesys.patch ~suffix:".p4" filenames_p4 filenames_p4_patch
  in
  let filenames_stf = Filesys.collect_files ~suffix:".stf" testdir in
  let filenames_stf_patch = Filesys.collect_files ~suffix:".stf" patchdir in
  let filenames_stf =
    Filesys.patch ~suffix:".stf" filenames_stf filenames_stf_patch
  in
  let filenames_test =
    List.filter_map
      (fun filename_p4 ->
        let filename_base = Filesys.base ~suffix:".p4" filename_p4 in
        let filename_stf_opt =
          List.find_opt
            (fun filename_stf ->
              let filename_stf_base =
                Filesys.base ~suffix:".stf" filename_stf
              in
              String.equal filename_base filename_stf_base)
            filenames_stf
        in
        match filename_stf_opt with
        | Some filename_stf -> Some (filename_p4, filename_stf)
        | None -> None)
      filenames_p4
  in
  let total = List.length filenames_test in
  let stat = empty_stat in
  Format.asprintf "Running simulation test (%s) on %d files\n" arch total
  |> print_endline;
  let stat =
    List.fold_left
      (fun stat (filename_p4, filename_stf) ->
        Format.asprintf "\n>>> Running simulation test (%s) on %s" arch
          filename_p4
        |> print_endline;
        run_sim_test stat arch spec_sim includes_p4 excludes_p4 filename_p4
          filename_stf)
      stat filenames_test
  in
  log_stat (Format.asprintf "\nRunning simulation test (%s)" arch) stat total

let run_sim_command =
  Core.Command.basic ~summary:"run simulation test"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map specdir = flag "-s" (required string) ~doc:"p4 spec directory"
     and includes_p4 = flag "-i" (listed string) ~doc:"p4 include paths"
     and excludes_p4 = flag "-e" (listed string) ~doc:"p4 test exclude paths"
     and testdir = flag "-d" (required string) ~doc:"p4 and stf test directory"
     and patchdir = flag "-p" (required string) ~doc:"p4 patch directory"
     and arch = flag "-arch" (required string) ~doc:"architecture name"
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
       run_sim_test_driver mode arch specdir includes_p4 excludes_p4 testdir
         patchdir)

(* Dangling coverage test *)

let cover_dangling_test specdir relname includes_p4 excludes_p4 testdirs_p4 =
  let spec_sl = structure specdir in
  let excludes_p4 =
    excludes_p4 |> Filesys.collect_excludes
    |> List.map (fun exclude_p4 -> "../../../../" ^ exclude_p4)
  in
  let filenames_p4 =
    List.concat_map (Filesys.collect_files ~suffix:".p4") testdirs_p4
  in
  let filenames_p4 =
    List.filter
      (fun filename_p4 -> not (List.mem filename_p4 excludes_p4))
      filenames_p4
  in
  let (module Runner) = Backend_sim.Gen.gen_placeholder () in
  let cover = Runner.cover_programs spec_sl relname includes_p4 filenames_p4 in
  Runtime.Testgen_neg.Dangling.Multi.log ~filename_cov_opt:None cover

let cover_dangling_command =
  Core.Command.basic ~summary:"measure dangling coverage of the P4 type system"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map specdir = flag "-s" (required string) ~doc:"p4 spec directory"
     and relname = flag "-rel" (required string) ~doc:"relation name"
     and includes_p4 = flag "-i" (listed string) ~doc:"p4 include paths"
     and excludes_p4 = flag "-e" (listed string) ~doc:"p4 test exclude paths"
     and testdirs_p4 = flag "-d" (listed string) ~doc:"p4 test directory" in
     fun () ->
       cover_dangling_test specdir relname includes_p4 excludes_p4 testdirs_p4)

(* P4 Parser test *)

let parse_file time_start includes filename =
  try Interface.Parse.parse_file includes filename
  with ParseError (at, msg) -> raise (TestParseFileErr (msg, at, time_start))

let parse_string time_start filename program_dump =
  try Interface.Parse.parse_string filename program_dump
  with ParseError (at, msg) ->
    raise (TestParseStringErr (msg, at, time_start))

let parse_roundtrip time_start includes filename spec =
  let program = parse_file time_start includes filename in
  let program_dump =
    Format.asprintf "%a\n" (Interface.Unparse.pp_program_il spec) program
  in
  let program_roundtrip = parse_string time_start filename program_dump in
  if not (Il.Eq.eq_value ~dbg:true program program_roundtrip) then
    raise (TestParseRoundtripErr time_start)
  else time_start

let run_parser includes_p4 filename_p4 spec =
  let time_start = start () in
  try parse_roundtrip time_start includes_p4 filename_p4 spec with
  | TestParseFileErr _ as err -> raise err
  | TestParseStringErr _ as err -> raise err
  | TestParseRoundtripErr _ as err -> raise err
  | _ -> raise (TestUnknownErr time_start)

let run_parser_test stat includes_p4 excludes_p4 filename_p4 spec =
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
      let time_start = run_parser includes_p4 filename_p4 spec in
      let duration = stop time_start in
      let log = Format.asprintf "Parser roundtrip success: %s" filename_p4 in
      log |> print_endline;
      Format.eprintf "%s\n" log;
      Format.eprintf ">>> took %.6f seconds\n" duration;
      { stat with durations = duration :: stat.durations }
    with
    | TestParseFileErr (msg, at, time_start) ->
        let duration = stop time_start in
        let log =
          Format.asprintf "Error parsing file: %s\n%s" filename_p4
            (string_of_error at msg)
        in
        log |> print_endline;
        Format.eprintf "%s\n" log;
        Format.eprintf ">>> took %.6f seconds\n" duration;
        {
          stat with
          durations = duration :: stat.durations;
          fail_run = stat.fail_run + 1;
        }
    | TestParseStringErr (msg, at, time_start) ->
        let duration = stop time_start in
        let log =
          Format.asprintf "Error parsing string: %s\n%s" filename_p4
            (string_of_error at msg)
        in
        log |> print_endline;
        Format.eprintf "%s\n" log;
        Format.eprintf ">>> took %.6f seconds\n" duration;
        {
          stat with
          durations = duration :: stat.durations;
          fail_run = stat.fail_run + 1;
        }
    | TestParseRoundtripErr time_start ->
        let duration = stop time_start in
        let log =
          Format.asprintf "Error roundtripping parser: %s" filename_p4
        in
        log |> print_endline;
        Format.eprintf "%s\n" log;
        Format.eprintf ">>> took %.6f seconds\n" duration;
        {
          stat with
          durations = duration :: stat.durations;
          fail_run = stat.fail_run + 1;
        }
    | TestUnknownErr time_start ->
        let duration = stop time_start in
        let log = Format.asprintf "Unknown error on parser: %s" filename_p4 in
        log |> print_endline;
        Format.eprintf "%s\n" log;
        Format.eprintf ">>> took %.6f seconds\n" duration;
        {
          stat with
          durations = duration :: stat.durations;
          fail_run = stat.fail_run + 1;
        }

let run_parser_test_driver includes_p4 excludes_p4 testdir_p4 specdir =
  let excludes_p4 =
    excludes_p4 |> Filesys.collect_excludes
    |> List.map (fun exclude_p4 -> "../../../../" ^ exclude_p4)
  in
  let filenames_p4 = Filesys.collect_files ~suffix:".p4" testdir_p4 in
  let spec = elab specdir in
  let total = List.length filenames_p4 in
  let stat = empty_stat in
  Format.asprintf "Running parser tests on %d files\n" total |> print_endline;
  let stat =
    List.fold_left
      (fun stat filename_p4 ->
        Format.asprintf "\n>>> Running parser test on %s" filename_p4
        |> print_endline;
        run_parser_test stat includes_p4 excludes_p4 filename_p4 spec)
      stat filenames_p4
  in
  log_stat "\nRunning parser" stat total

let run_parser_command =
  Core.Command.basic ~summary:"run parser test on P4 files"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map includes_p4 = flag "-i" (listed string) ~doc:"p4 include paths"
     and excludes_p4 = flag "-e" (listed string) ~doc:"p4 test exclude paths"
     and testdir_p4 = flag "-d" (required string) ~doc:"p4 test directory"
     and specdir = flag "-s" (required string) ~doc:"p4 spec directory" in
     fun () -> run_parser_test_driver includes_p4 excludes_p4 testdir_p4 specdir)

let command =
  Core.Command.group ~summary:"p4spec-test"
    [
      ("elab", elab_command);
      ("struct", structure_command);
      ("prose", prose_command);
      ("run", run_command);
      ("sim", run_sim_command);
      ("cover-dangling", cover_dangling_command);
      ("parser", run_parser_command);
    ]

let () = Command_unix.run ~version command
