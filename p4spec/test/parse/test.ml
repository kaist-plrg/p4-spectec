open Lang
open Test_common
open Runtime.Sim.Signature
open Util.Error
module Test = Util.Test
module Filesys = Util.Filesys

(* P4 Parser test *)

let parse_file time_start (module Simulator : SIM) includes filename =
  match Simulator.parse_file includes [ filename ] with
  | Pass value -> value
  | Fail (`Syntax (at, msg)) -> raise (TestParseFileErr (msg, at, time_start))

let parse_string time_start (module Simulator : SIM) filename program_dump =
  match Simulator.parse_string filename program_dump with
  | Pass value -> value
  | Fail (`Syntax (at, msg)) -> raise (TestParseStringErr (msg, at, time_start))

let parse_roundtrip time_start (module Simulator : SIM) includes filename =
  let program = parse_file time_start (module Simulator) includes filename in
  let program_dump = Simulator.unparse_program program in
  let program_roundtrip =
    parse_string time_start (module Simulator) filename program_dump
  in
  if not (Il.Eq.eq_value ~dbg:true program program_roundtrip) then
    raise (TestParseRoundtripErr time_start)
  else time_start

let parser_ (module Simulator : SIM) includes_p4 filename_p4 =
  let time_start = start () in
  try parse_roundtrip time_start (module Simulator) includes_p4 filename_p4 with
  | TestParseFileErr _ as err -> raise err
  | TestParseStringErr _ as err -> raise err
  | TestParseRoundtripErr _ as err -> raise err
  | _ -> raise (TestUnknownErr time_start)

let parser_test_ stat (module Simulator : SIM) includes_p4 excludes_p4
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
      let time_start = parser_ (module Simulator) includes_p4 filename_p4 in
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
    |> List.map (fun exclude_p4 -> "../../../../../" ^ exclude_p4)
  in
  let filenames_p4 =
    testdirs_p4 |> List.concat_map (Filesys.collect_files ~suffix:".p4")
  in
  let _, (module Simulator) = simulator `IL specdir in
  let total = List.length filenames_p4 in
  let stat = empty_stat in
  Format.asprintf "Running parser tests on %d files\n" total |> print_endline;
  let stat =
    List.fold_left
      (fun stat filename_p4 ->
        Format.asprintf "\n>>> Running parser test on %s" filename_p4
        |> print_endline;
        parser_test_ stat (module Simulator) includes_p4 excludes_p4 filename_p4)
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
  Core.Command.group ~summary:"p4spec-test-parse"
    [ ("parser", parser_command_) ]

let () = Command_unix.run ~version command
