open Lang
open Test_common
open Util.Error
module Test = Util.Test
module Filesys = Util.Filesys

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
  if not (Il.Eq.eq_value program program_roundtrip) then
    let msg =
      Format.asprintf "@eq_value: %s does not equal %s\n"
        (Il.Print.string_of_value program)
        (Il.Print.string_of_value program_roundtrip)
    in
    raise (TestParseRoundtripErr (msg, time_start))
  else time_start

let parser_ includes_p4 filename_p4 spec =
  let time_start = start () in
  try parse_roundtrip time_start includes_p4 filename_p4 spec with
  | TestParseFileErr _ as err -> raise err
  | TestParseStringErr _ as err -> raise err
  | TestParseRoundtripErr _ as err -> raise err
  | _ -> raise (TestUnknownErr time_start)

let parser_test_ stat includes_p4 excludes_p4 filename_p4 spec =
  if List.exists (String.equal filename_p4) excludes_p4 then
    let log = Format.asprintf "Excluding file: %s" filename_p4 in
    ( {
        stat with
        durations = 0.0 :: stat.durations;
        exclude_run = stat.exclude_run + 1;
      },
      log )
  else
    try
      let time_start = parser_ includes_p4 filename_p4 spec in
      let duration = stop time_start in
      let log = Format.asprintf "Parser roundtrip success: %s" filename_p4 in
      ({ stat with durations = duration :: stat.durations }, log)
    with
    | TestParseFileErr (msg, at, time_start) ->
        let duration = stop time_start in
        let log =
          Format.asprintf "Error parsing file: %s\n%s" filename_p4
            (string_of_error at msg)
        in
        ( {
            stat with
            durations = duration :: stat.durations;
            fail_run = stat.fail_run + 1;
          },
          log )
    | TestParseStringErr (msg, at, time_start) ->
        let duration = stop time_start in
        let log =
          Format.asprintf "Error parsing string: %s\n%s" filename_p4
            (string_of_error at msg)
        in
        (* log |> print_endline; *)
        (* Format.eprintf "%s\n" log; *)
        (* Format.eprintf ">>> took %.6f seconds\n" duration; *)
        ( {
            stat with
            durations = duration :: stat.durations;
            fail_run = stat.fail_run + 1;
          },
          log )
    | TestParseRoundtripErr (msg, time_start) ->
        let duration = stop time_start in
        let log =
          Format.asprintf "Error roundtripping parser: %s\n%s" filename_p4 msg
        in
        ( {
            stat with
            durations = duration :: stat.durations;
            fail_run = stat.fail_run + 1;
          },
          log )
    | TestUnknownErr time_start ->
        let duration = stop time_start in
        let log = Format.asprintf "Unknown error on parser: %s" filename_p4 in
        ( {
            stat with
            durations = duration :: stat.durations;
            fail_run = stat.fail_run + 1;
          },
          log )

let parser_test_driver pool includes_p4 excludes_p4 testdirs_p4 specdir =
  let excludes_p4 =
    excludes_p4 |> Test.collect_excludes
    |> List.map (fun exclude_p4 -> "../../../../../" ^ exclude_p4)
  in
  let filenames_p4 =
    testdirs_p4 |> List.concat_map (Filesys.collect_files ~suffix:".p4")
  in
  let filenames_arr = Array.of_list filenames_p4 in
  let spec = elab specdir in
  let total = List.length filenames_p4 in
  let stat = empty_stat in
  Format.asprintf "Running parser tests on %d files" total |> print_endline;

  let stat, log =
    Domainslib.Task.parallel_for_reduce ~start:0 ~finish:(total - 1)
      ~body:(fun i ->
        let filename_p4 = filenames_arr.(i) in
        let (stat, log) = parser_test_ stat includes_p4 excludes_p4 filename_p4 spec in
        (stat, "\n>>> Running parser test on " ^ filename_p4 ^ "\n" ^ log))
      pool merge_stat (empty_stat, "")
  in
  print_endline log;
  log_stat "\nRunning parser" stat total

let parser_command_ =
  Core.Command.basic ~summary:"run parser test on P4 files"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map includes_p4 = flag "-i" (listed string) ~doc:"p4 include paths"
     and excludes_p4 = flag "-e" (listed string) ~doc:"p4 test exclude paths"
     and testdirs_p4 = flag "-p4-dir" (listed string) ~doc:"p4 test directories"
     and specdir = flag "-s" (required string) ~doc:"p4 spec directory" in
     fun () ->
       let num_domains = Stdlib.Domain.recommended_domain_count () - 1 in
       let pool = Domainslib.Task.setup_pool ~num_domains () in
       Core.Exn.protect
         ~f:(fun () ->
           Domainslib.Task.run pool (fun () ->
               parser_test_driver pool includes_p4 excludes_p4 testdirs_p4
                 specdir))
         ~finally:(fun () -> Domainslib.Task.teardown_pool pool))

let command =
  Core.Command.group ~summary:"p4spec-test-parse"
    [ ("parser", parser_command_) ]

let () = Command_unix.run ~version command
