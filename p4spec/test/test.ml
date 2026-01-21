open Lang
open Pass
module Sim = Runtime.Sim.Simulator
module Strings = Util.Strings
module Filesys = Util.Filesys
module Test = Util.Test
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

let start () = Util.Time.now ()
let stop start = Util.Time.now () -. start

(* Operations *)

let frontend specdir =
  specdir
  |> Filesys.collect_files ~suffix:".watsup"
  |> List.concat_map Frontend.Parse.parse_file

let elab specdir = specdir |> frontend |> Elaborate.Elab.elab_spec
let structure specdir = specdir |> elab |> Structure.Struct.struct_spec
let prosify specdir = specdir |> structure |> Prose.Prosify.prosify_spec

let driver ?(arch : string option) mode specdir =
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
  Driver.init spec_sim;
  (spec_sim, (module Driver : Runtime.Sim.Simulator.DRIVER))

let run_with_instr (module Driver : Runtime.Sim.Simulator.DRIVER) spec_sim
    relname includes_p4 filename_p4 =
  let (module IH : Inst.Handler.HANDLER), read_coverage_instr =
    Inst.Coverage_instr.make ()
  in
  Inst.Hook.register [ (module IH : Inst.Handler.HANDLER) ];
  Inst.Hook.init_spec spec_sim;
  let result = Driver.run_program relname includes_p4 filename_p4 in
  Inst.Hook.finish ();
  let cover = read_coverage_instr () in
  (result, cover)

let run_with_dangling (module Driver : Runtime.Sim.Simulator.DRIVER) spec_sim
    relname includes_p4 filename_p4 =
  let (module DH : Inst.Handler.HANDLER), read_coverage_dangling =
    Inst.Coverage_dangling.make ()
  in
  Inst.Hook.register [ (module DH : Inst.Handler.HANDLER) ];
  Inst.Hook.init_spec spec_sim;
  let result = Driver.run_program relname includes_p4 filename_p4 in
  Inst.Hook.finish ();
  let cover = read_coverage_dangling () in
  (result, cover)

let sim_with_instr (module Driver : Runtime.Sim.Simulator.DRIVER) spec_sim
    includes_p4 filename_p4 filename_stf =
  let (module IH : Inst.Handler.HANDLER), read_coverage_instr =
    Inst.Coverage_instr.make ()
  in
  Inst.Hook.register [ (module IH : Inst.Handler.HANDLER) ];
  Inst.Hook.init_spec spec_sim;
  let result = Driver.run_stf_test includes_p4 filename_p4 filename_stf in
  Inst.Hook.finish ();
  let cover = read_coverage_instr () in
  (result, cover)

let sim_with_dangling (module Driver : Runtime.Sim.Simulator.DRIVER) spec_sim
    includes_p4 filename_p4 filename_stf =
  let (module DH : Inst.Handler.HANDLER), read_coverage_dangling =
    Inst.Coverage_dangling.make ()
  in
  Inst.Hook.register [ (module DH : Inst.Handler.HANDLER) ];
  Inst.Hook.init_spec spec_sim;
  let result = Driver.run_stf_test includes_p4 filename_p4 filename_stf in
  Inst.Hook.finish ();
  let cover = read_coverage_dangling () in
  (result, cover)

let cover_run_instr ?(arch : string option) mode filenames_spec relname
    includes_p4 filenames_p4 =
  let spec_sim, (module Driver) = driver ?arch mode filenames_spec in
  let spec_sl =
    match spec_sim with
    | Runtime.Sim.Simulator.SL spec_sl -> spec_sl
    | _ -> assert false
  in
  let cover_multi = Coverage.Instr.Multi.init spec_sl in
  let cover_multi =
    List.fold_left
      (fun cover_multi filename_p4 ->
        let _, cover_single =
          run_with_instr
            (module Driver)
            spec_sim relname includes_p4 filename_p4
        in
        Coverage.Instr.Multi.extend cover_multi filename_p4 cover_single)
      cover_multi filenames_p4
  in
  Coverage.Instr.Log.log_spec ~filename_cov_opt:None cover_multi spec_sl

let cover_run_dangling ?(arch : string option) mode filenames_spec relname
    includes_p4 filenames_p4 =
  let spec_sim, (module Driver) = driver ?arch mode filenames_spec in
  let spec_sl =
    match spec_sim with
    | Runtime.Sim.Simulator.SL spec_sl -> spec_sl
    | _ -> assert false
  in
  let cover_multi = Coverage.Dangling.Multi.init spec_sl in
  let cover_multi =
    List.fold_left
      (fun cover_multi filename_p4 ->
        let program_result, cover_single =
          run_with_dangling
            (module Driver)
            spec_sim relname includes_p4 filename_p4
        in
        let wellformed, welltyped =
          match program_result with
          | Pass _ -> (true, true)
          | Fail (`Syntax _) -> (true, false)
          | Fail (`Runtime _) -> (false, false)
        in
        Coverage.Dangling.Multi.extend cover_multi filename_p4 wellformed
          welltyped cover_single)
      cover_multi filenames_p4
  in
  Coverage.Dangling.Multi.log ~filename_cov_opt:None cover_multi

let cover_sim_instr ?(arch : string option) mode filenames_spec includes_p4
    filenames_p4 filenames_stf =
  let spec_sim, (module Driver) = driver ?arch mode filenames_spec in
  let spec_sl =
    match spec_sim with
    | Runtime.Sim.Simulator.SL spec_sl -> spec_sl
    | _ -> assert false
  in
  let cover_multi = Coverage.Instr.Multi.init spec_sl in
  let cover_multi =
    List.fold_left2
      (fun cover_multi filename_p4 filename_stf ->
        let _, cover_single =
          sim_with_instr
            (module Driver)
            spec_sim includes_p4 filename_p4 filename_stf
        in
        Coverage.Instr.Multi.extend cover_multi filename_p4 cover_single)
      cover_multi filenames_p4 filenames_stf
  in
  Coverage.Instr.Log.log_spec ~filename_cov_opt:None cover_multi spec_sl

let cover_sim_dangling ?(arch : string option) mode filenames_spec includes_p4
    filenames_p4 filenames_stf =
  let spec_sim, (module Driver) = driver ?arch mode filenames_spec in
  let spec_sl =
    match spec_sim with
    | Runtime.Sim.Simulator.SL spec_sl -> spec_sl
    | _ -> assert false
  in
  let cover_multi = Coverage.Dangling.Multi.init spec_sl in
  let cover_multi =
    List.fold_left2
      (fun cover_multi filename_p4 filename_stf ->
        let program_result, cover_single =
          sim_with_dangling
            (module Driver)
            spec_sim includes_p4 filename_p4 filename_stf
        in
        let wellformed, welltyped =
          match program_result with
          | Pass -> (true, true)
          | Fail (`Syntax _) -> (true, false)
          | Fail (`Runtime _) -> (false, false)
        in
        Coverage.Dangling.Multi.extend cover_multi filename_p4 wellformed
          welltyped cover_single)
      cover_multi filenames_p4 filenames_stf
  in
  Coverage.Dangling.Multi.log ~filename_cov_opt:None cover_multi

(* Spec elaboration test *)

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

let run (module Driver : Sim.DRIVER) negative relname includes_p4 filename_p4 =
  let time_start = start () in
  try
    (match Driver.run_program relname includes_p4 filename_p4 with
    | Pass _ -> if negative then raise (TestRunNegErr time_start)
    | Fail (`Syntax (at, msg)) | Fail (`Runtime (at, msg)) ->
        raise (TestRunErr (msg, at, time_start)));
    time_start
  with
  | TestRunErr _ as err -> raise err
  | TestRunNegErr _ as err -> raise err
  | _ -> raise (TestUnknownErr time_start)

let run_test (module Driver : Sim.DRIVER) negative stat relname includes_p4
    excludes_p4 filename_p4 =
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
        run (module Driver) negative relname includes_p4 filename_p4
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
    testdirs_p4 =
  let excludes_p4 =
    excludes_p4 |> Test.collect_excludes
    |> List.map (fun exclude_p4 -> "../../../../" ^ exclude_p4)
  in
  let filenames_p4 =
    testdirs_p4 |> List.concat_map (Filesys.collect_files ~suffix:".p4")
  in
  let total = List.length filenames_p4 in
  let stat = empty_stat in
  Format.asprintf "Running interpreter test (%s) on %d files\n" relname total
  |> print_endline;
  let spec_sim, (module Driver) = driver mode specdir in
  Driver.init spec_sim;
  let stat =
    List.fold_left
      (fun stat filename_p4 ->
        Format.asprintf "\n>>> Running interpreter test (%s) on %s" relname
          filename_p4
        |> print_endline;
        run_test
          (module Driver)
          negative stat relname includes_p4 excludes_p4 filename_p4)
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
         testdirs_p4)

(* Simulator test *)

let run_sim (module Driver : Sim.DRIVER) includes_p4 filename_p4 filename_stf =
  let time_start = start () in
  try
    (match Driver.run_stf_test includes_p4 filename_p4 filename_stf with
    | Pass -> ()
    | Fail (`Syntax (at, msg)) | Fail (`Runtime (at, msg)) ->
        raise (TestRunErr (msg, at, time_start)));
    time_start
  with
  | TestRunErr _ as err -> raise err
  | _ -> raise (TestUnknownErr time_start)

let run_sim_test (module Driver : Sim.DRIVER) stat includes_p4 excludes
    filename_p4 filename_stf =
  if Test.should_exclude_pair filename_p4 filename_stf excludes then (
    let log = Format.asprintf "Excluding file: %s" filename_stf in
    log |> print_endline;
    {
      stat with
      durations = 0.0 :: stat.durations;
      exclude_run = stat.exclude_run + 1;
    })
  else
    try
      let time_start =
        run_sim (module Driver) includes_p4 filename_p4 filename_stf
      in
      let duration = stop time_start in
      let log = Format.asprintf "Run success: %s" filename_stf in
      log |> print_endline;
      Format.eprintf "%s\n" log;
      Format.eprintf ">>> took %.6f seconds\n" duration;
      { stat with durations = duration :: stat.durations }
    with
    | TestRunErr (msg, at, time_start) ->
        let duration = stop time_start in
        let log =
          Format.asprintf "Error on run: %s\n%s" filename_stf
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

let run_sim_test_driver mode arch specdir includes_p4 excludes_p4 testdirs_p4
    testdirs_stf patchdir =
  let excludes_p4 =
    excludes_p4 |> Test.collect_excludes
    |> List.map (fun exclude_p4 -> "../../../../" ^ exclude_p4)
  in
  let filename_pairs =
    Test.collect_test_pairs arch testdirs_p4 testdirs_stf patchdir
  in
  let total = List.length filename_pairs in
  let stat = empty_stat in
  Format.asprintf "Running simulation test (%s) on %d files\n" arch total
  |> print_endline;
  let spec_sim, (module Driver) = driver ~arch mode specdir in
  Driver.init spec_sim;
  let stat =
    List.fold_left
      (fun stat (filename_p4, filename_stf) ->
        Format.asprintf
          "\n>>> Running simulation test (%s) on %s with packet input %s" arch
          filename_p4 filename_stf
        |> print_endline;
        run_sim_test
          (module Driver : Sim.DRIVER)
          stat includes_p4 excludes_p4 filename_p4 filename_stf)
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
       run_sim_test_driver mode arch specdir includes_p4 excludes_p4 testdirs_p4
         testdirs_stf patchdir)

(* Dangling coverage test *)

let cover_run mode specdir relname includes_p4 excludes_p4 testdirs_p4 =
  let excludes_p4 =
    excludes_p4 |> Test.collect_excludes
    |> List.map (fun exclude_p4 -> "../../../../" ^ exclude_p4)
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

(* Instruction coverage test - on simulation *)

let cover_sim mode arch specdir includes_p4 excludesdir testdirs_p4 testdirs_stf
    patchdir =
  let excludes =
    excludesdir |> Test.collect_excludes
    |> List.map (fun exclude -> "../../../../" ^ exclude)
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

let parser_ includes_p4 filename_p4 spec =
  let time_start = start () in
  try parse_roundtrip time_start includes_p4 filename_p4 spec with
  | TestParseFileErr _ as err -> raise err
  | TestParseStringErr _ as err -> raise err
  | TestParseRoundtripErr _ as err -> raise err
  | _ -> raise (TestUnknownErr time_start)

let parser_test_ stat includes_p4 excludes_p4 filename_p4 spec =
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
      let time_start = parser_ includes_p4 filename_p4 spec in
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

let parser_test_driver includes_p4 excludes_p4 testdirs_p4 specdir =
  let excludes_p4 =
    excludes_p4 |> Test.collect_excludes
    |> List.map (fun exclude_p4 -> "../../../../" ^ exclude_p4)
  in
  let filenames_p4 =
    testdirs_p4 |> List.concat_map (Filesys.collect_files ~suffix:".p4")
  in
  let spec = elab specdir in
  let total = List.length filenames_p4 in
  let stat = empty_stat in
  Format.asprintf "Running parser tests on %d files\n" total |> print_endline;
  let stat =
    List.fold_left
      (fun stat filename_p4 ->
        Format.asprintf "\n>>> Running parser test on %s" filename_p4
        |> print_endline;
        parser_test_ stat includes_p4 excludes_p4 filename_p4 spec)
      stat filenames_p4
  in
  log_stat "\nRunning parser" stat total

let parser_command_ =
  Core.Command.basic ~summary:"run parser test on P4 files"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map includes_p4 = flag "-i" (listed string) ~doc:"p4 include paths"
     and excludes_p4 = flag "-e" (listed string) ~doc:"p4 test exclude paths"
     and testdirs_p4 = flag "-p4-dir" (listed string) ~doc:"p4 test directories"
     and specdir = flag "-s" (required string) ~doc:"p4 spec directory" in
     fun () -> parser_test_driver includes_p4 excludes_p4 testdirs_p4 specdir)

let command =
  Core.Command.group ~summary:"p4spec-test"
    [
      ("elab", elab_command);
      ("struct", structure_command);
      ("prose", prose_command);
      ("run", run_command);
      ("sim", sim_command);
      ("cover-run", cover_run_command);
      ("cover-sim", cover_sim_command);
      ("parser", parser_command_);
    ]

let () = Command_unix.run ~version command
