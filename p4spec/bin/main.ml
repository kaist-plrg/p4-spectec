open Util.Error
open Util.Source

let version = "0.1"

(* File collector *)

let rec collect_files ~(suffix : string) dir =
  let files = Sys_unix.readdir dir in
  Array.sort String.compare files;
  Array.fold_left
    (fun files file ->
      let filename = dir ^ "/" ^ file in
      if Sys_unix.is_directory_exn filename && file <> "include" then
        files @ collect_files ~suffix filename
      else if String.ends_with ~suffix filename then files @ [ filename ]
      else files)
    [] files

(* Exclude collector *)

let collect_exclude filename_exclude =
  let ic = open_in filename_exclude in
  let rec parse_lines excludes =
    try
      let exclude = input_line ic in
      if String.starts_with ~prefix:"#" exclude then parse_lines excludes
      else parse_lines (exclude :: excludes)
    with End_of_file -> excludes
  in
  let excludes = parse_lines [] in
  close_in ic;
  excludes

let collect_excludes (paths_exclude : string list) =
  let filenames_exclude =
    List.concat_map (collect_files ~suffix:".exclude") paths_exclude
  in
  List.concat_map collect_exclude filenames_exclude

(* Commands *)

let elab_command =
  Core.Command.basic ~summary:"parse and elaborate a p4_16 spec"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map filenames = anon (sequence ("filename" %: string)) in
     fun () ->
       try
         let spec = List.concat_map Frontend.Parse.parse_file filenames in
         let spec_il = Elaborate.Elab.elab_spec spec in
         Format.printf "%s\n" (Il.Print.string_of_spec spec_il);
         ()
       with
       | ParseError (at, msg) -> Format.printf "%s\n" (string_of_error at msg)
       | ElabError (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

let struct_command =
  Core.Command.basic ~summary:"insert structured control flow to a p4_16 spec"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map filenames = anon (sequence ("filename" %: string)) in
     fun () ->
       try
         let spec = List.concat_map Frontend.Parse.parse_file filenames in
         let spec_il = Elaborate.Elab.elab_spec spec in
         let spec_sl = Structure.Struct.struct_spec spec_il in
         Format.printf "%s\n" (Sl.Print.string_of_spec spec_sl);
         ()
       with
       | ParseError (at, msg) -> Format.printf "%s\n" (string_of_error at msg)
       | ElabError (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

let run_il_command =
  Core.Command.basic
    ~summary:"run static semantics of a p4_16 spec based on backtracking IL"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map filenames_spec = anon (sequence ("filename" %: string))
     and includes_p4 = flag "-i" (listed string) ~doc:"p4 include paths"
     and filename_p4 = flag "-p" (required string) ~doc:"p4 file to typecheck"
     and debug = flag "-dbg" no_arg ~doc:"print debug traces" in
     fun () ->
       try
         let spec = List.concat_map Frontend.Parse.parse_file filenames_spec in
         let spec_il = Elaborate.Elab.elab_spec spec in
         match
           Interp_il.Typing.run_typing ~debug spec_il includes_p4 filename_p4
         with
         | WellTyped -> Format.printf "well-typed\n"
         | IllTyped (_, msg) -> Format.printf "ill-typed: %s\n" msg
         | IllFormed (_, msg) -> Format.printf "ill-formed: %s\n" msg
       with
       | ParseError (at, msg) -> Format.printf "%s\n" (string_of_error at msg)
       | ElabError (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

let run_sl_command =
  Core.Command.basic
    ~summary:"run static semantics of a p4_16 spec based on non-backtracking SL"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map filenames_spec = anon (sequence ("filename" %: string))
     and includes_p4 = flag "-i" (listed string) ~doc:"p4 include paths"
     and filename_p4 = flag "-p" (required string) ~doc:"p4 file to typecheck"
     and derive = flag "-derive" no_arg ~doc:"derive value dependency graph"
     and filenames_ignore =
       flag "-ignore" (listed string)
         ~doc:"relations or functions to ignore when reporting coverage"
     in
     fun () ->
       try
         let spec = List.concat_map Frontend.Parse.parse_file filenames_spec in
         let spec_il = Elaborate.Elab.elab_spec spec in
         let spec_sl = Structure.Struct.struct_spec spec_il in
         match
           Interp_sl.Typing.run_typing ~derive spec_sl includes_p4 filename_p4
             filenames_ignore
         with
         | WellTyped _ -> Format.printf "well-typed\n"
         | IllTyped (_, msg, _) -> Format.printf "ill-typed: %s\n" msg
         | IllFormed (_, msg, _) -> Format.printf "ill-formed: %s\n" msg
       with
       | ParseError (at, msg) -> Format.printf "%s\n" (string_of_error at msg)
       | ElabError (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

let cover_dangling_command =
  Core.Command.basic ~summary:"measure dangling coverage of the P4 type system"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map filenames_spec = anon (sequence ("filename" %: string))
     and includes_p4 = flag "-i" (listed string) ~doc:"p4 include paths"
     and excludes_p4 = flag "-e" (listed string) ~doc:"p4 test exclude paths"
     and dirnames_p4 =
       flag "-d" (listed string) ~doc:"p4 directories to typecheck"
     and filenames_ignore =
       flag "-ignore" (listed string)
         ~doc:"relations or functions to ignore when reporting coverage"
     and filename_cov =
       flag "-cov" (required string) ~doc:"output coverage file"
     in
     fun () ->
       try
         let spec = List.concat_map Frontend.Parse.parse_file filenames_spec in
         let spec_il = Elaborate.Elab.elab_spec spec in
         let spec_sl = Structure.Struct.struct_spec spec_il in
         let excludes_p4 = collect_excludes excludes_p4 in
         let filenames_p4 =
           List.concat_map (collect_files ~suffix:".p4") dirnames_p4
         in
         let filenames_p4 =
           List.filter
             (fun filename_p4 ->
               not (List.exists (String.equal filename_p4) excludes_p4))
             filenames_p4
         in
         let cover =
           Interp_sl.Typing.cover_typings spec_sl includes_p4 filenames_p4
             filenames_ignore
         in
         Runtime_testgen.Cov.Multiple.log ~filename_cov_opt:(Some filename_cov)
           cover
       with
       | ParseError (at, msg) -> Format.printf "%s\n" (string_of_error at msg)
       | ElabError (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

let run_testgen_command =
  Core.Command.basic
    ~summary:"generate negative type checker tests from a p4_16 spec"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map filenames_spec = anon (sequence ("filename" %: string))
     and fuel = flag "-fuel" (required int) ~doc:"fuel for test generation"
     and includes_p4 = flag "-i" (listed string) ~doc:"p4 include paths"
     and excludes_p4 = flag "-e" (listed string) ~doc:"p4 test exclude paths"
     and filenames_ignore =
       flag "-ignore" (listed string)
         ~doc:"relations or functions to ignore when reporting coverage"
     and dirname_gen =
       flag "-gen" (required string) ~doc:"directory for generated p4 programs"
     and name_campaign =
       flag "-name" (optional string)
         ~doc:"name of the test generation campaign"
     and silent = flag "-silent" no_arg ~doc:"do not print logs to stdout"
     and randseed =
       flag "-seed" (optional int) ~doc:"seed for random number generator"
     and dirname_cold_boot =
       flag "-cold" (optional string) ~doc:"seed p4 directory for cold boot"
     and filename_boot =
       flag "-warm" (optional string) ~doc:"coverage file for warm boot"
     and random = flag "-random" no_arg ~doc:"randomize AST selection"
     and hybrid =
       flag "-hybrid" no_arg
         ~doc:"randomize AST selection when no derivations exist"
     and strict =
       flag "-strict" no_arg
         ~doc:"cover a new phantom only if it was intended by a mutation"
     in
     fun () ->
       try
         let spec = List.concat_map Frontend.Parse.parse_file filenames_spec in
         let spec_il = Elaborate.Elab.elab_spec spec in
         let spec_sl = Structure.Struct.struct_spec spec_il in
         let logmode =
           if silent then Testgen.Modes.Silent else Testgen.Modes.Verbose
         in
         let bootmode =
           match (dirname_cold_boot, filename_boot) with
           | Some dirname_cold_boot, None ->
               Testgen.Modes.Cold (excludes_p4, dirname_cold_boot)
           | None, Some filename_boot -> Testgen.Modes.Warm filename_boot
           | Some _, Some _ ->
               Format.asprintf
                 "Error: should specify only one of -cold or -warm\n"
               |> failwith
           | None, None ->
               Format.asprintf "Error: should specify either -cold or -warm\n"
               |> failwith
         in
         let mutationmode =
           if random then Testgen.Modes.Random
           else if hybrid then Testgen.Modes.Hybrid
           else Testgen.Modes.Derive
         in
         let covermode =
           if strict then Testgen.Modes.Strict else Testgen.Modes.Relaxed
         in
         Testgen.Gen.fuzz_typing fuel spec_sl includes_p4 filenames_ignore
           dirname_gen name_campaign randseed logmode bootmode mutationmode
           covermode
       with
       | ParseError (at, msg) -> Format.printf "%s\n" (string_of_error at msg)
       | ElabError (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

let run_testgen_debug_command =
  Core.Command.basic
    ~summary:"debug close-AST deriver in negative type checker generator"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map filenames_spec = anon (sequence ("filename" %: string))
     and includes_p4 = flag "-i" (listed string) ~doc:"p4 include paths"
     and filename_p4 = flag "-p" (required string) ~doc:"p4 file to typecheck"
     and filenames_ignore =
       flag "-ignore" (listed string)
         ~doc:"relations or functions to ignore when reporting coverage"
     and dirname_debug =
       flag "-debug" (required string) ~doc:"directory for debug files"
     and pid = flag "-pid" (required int) ~doc:"phantom id to close-miss" in
     fun () ->
       try
         let spec = List.concat_map Frontend.Parse.parse_file filenames_spec in
         let spec_il = Elaborate.Elab.elab_spec spec in
         let spec_sl = Structure.Struct.struct_spec spec_il in
         Testgen.Derive.debug_phantom spec_sl includes_p4 filename_p4
           filenames_ignore dirname_debug pid
       with
       | ParseError (at, msg) -> Format.printf "%s\n" (string_of_error at msg)
       | ElabError (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

let interesting_command =
  Core.Command.basic ~summary:"interestingness test for reducing p4_16 programs"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map filenames_spec = anon (sequence ("filename" %: string))
     and includes_p4 = flag "-i" (listed string) ~doc:"p4 include paths"
     and check_well_typed =
       flag "-well" no_arg
         ~doc:"'interesting' if well-typed (default: ill-typed)"
     and check_close_miss =
       flag "-close" no_arg ~doc:"'interesting' if close-miss (default: hit)"
     and pid = flag "-pid" (required int) ~doc:"phantom id to test"
     and filename_p4 = flag "-p" (required string) ~doc:"p4 file to typecheck"
     and dbg = flag "-dbg" no_arg ~doc:"print single coverage"
     and filenames_ignore =
       flag "-ignore" (listed string)
         ~doc:"relations or functions to ignore when reporting coverage"
     in
     fun () ->
       try
         let spec = List.concat_map Frontend.Parse.parse_file filenames_spec in
         let spec_il = Elaborate.Elab.elab_spec spec in
         let spec_sl = Structure.Struct.struct_spec spec_il in
         let typing_result =
           Interp_sl.Typing.run_typing spec_sl includes_p4 filename_p4
             filenames_ignore
         in
         if dbg then
           match typing_result with
           | IllTyped (_, _, cover_single) | WellTyped (_, _, cover_single) ->
               Interp_sl.Interp.SCov.Cover.iter
                 (fun pid (branch : Interp_sl.Interp.SCov.Branch.t) ->
                   match branch.status with
                   | Hit -> Printf.printf "%d Hit\n" pid
                   | Miss [] -> Printf.printf "%d Miss\n" pid
                   | Miss _ -> Printf.printf "%d Close\n" pid)
                 cover_single
           | _ -> ()
         else ();
         match typing_result with
         | WellTyped (_, _, cover_single) ->
             if check_well_typed then (
               let branch = Interp_sl.Interp.SCov.Cover.find pid cover_single in
               match branch.status with
               | Hit ->
                   Printf.printf "WellTyped: Hit\n";
                   if check_close_miss then exit 3 else exit 0
               | Miss (_ :: _) ->
                   Printf.printf "WellTyped: Close\n";
                   if check_close_miss then exit 0 else exit 2
               | Miss [] ->
                   Printf.printf "WellTyped: Miss\n";
                   exit 1)
             else (
               Printf.printf "WellTyped\n";
               exit 11)
         | IllTyped (_, _, cover_single) -> (
             if check_well_typed then (
               Printf.printf "IllTyped\n";
               exit 10)
             else
               let branch = Interp_sl.Interp.SCov.Cover.find pid cover_single in
               match branch.status with
               | Hit ->
                   Printf.printf "IllTyped: Hit\n";
                   if check_close_miss then exit 3 else exit 0
               | Miss (_ :: _) ->
                   Printf.printf "IllTyped: Close\n";
                   if check_close_miss then exit 0 else exit 2
               | Miss [] ->
                   Printf.printf "IllTyped: Miss\n";
                   exit 1)
         | IllFormed _ ->
             Printf.printf "IllFormed";
             exit 12
       with
       | ParseError (at, msg) -> Format.printf "%s\n" (string_of_error at msg)
       | ElabError (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

let parse_command =
  Core.Command.basic ~summary:"parse a P4 program"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map includes_p4 = flag "-i" (listed string) ~doc:"p4 include paths"
     and filename_p4 = flag "-p" (required string) ~doc:"p4 file to typecheck"
     and roundtrip =
       flag "-r" no_arg ~doc:"perform a round-trip parse/unparse"
     in
     fun () ->
       try
         let parsed_il_file =
           Interface.Parse.parse_file includes_p4 filename_p4
         in
         let string_p4 =
           Format.asprintf "%a\n" Interface.Unparse.pp_program parsed_il_file
         in
         if roundtrip then
           let parsed_il_str =
             Interface.Parse.parse_string filename_p4 string_p4
           in
           Il.Eq.eq_value ~dbg:true parsed_il_file parsed_il_str
           |> (fun b ->
                if b then "Roundtrip successful" else "Roundtrip failed")
           |> print_endline
         else string_p4 |> print_endline
       with
       | Sys_error msg -> Format.printf "File error: %s\n" msg
       | ElabError (at, msg) ->
           Format.printf "Elaboration error: %s\n" (string_of_error at msg)
       | ParseError (at, msg) ->
           Format.printf "Parse error: %s\n" (string_of_error at msg)
       | Interface.Lexer.Error msg -> Format.printf "Lexer error: %s\n" msg
       | e -> Format.printf "Unknown error: %s\n" (Printexc.to_string e))

let json_ast_command =
  Core.Command.basic ~summary:"Emit/Parse JSON AST for Structured Language"
    ~readme:(fun () ->
      "./p4spectec json-ast -emit spec/*.watsup\n\
       ./p4spectec json-ast -parse <ast-file.json>")
    (let%map_open.Command mode =
       Command.Param.choose_one
         [
           flag "emit" no_arg ~doc:"Emit JSON AST from supplied spec files"
           |> map ~f:(fun b -> Core.Option.some_if b `Emit);
           flag "parse" no_arg
             ~doc:
               "Parse JSON AST from supplied JSON file and produce Structured \
                Language"
           |> map ~f:(fun b -> Core.Option.some_if b `Parse);
         ]
         ~if_nothing_chosen:(Default_to `Emit)
     and filenames = anon (non_empty_sequence_as_list ("filename" %: string)) in
     fun () ->
       match mode with
       | `Emit -> (
           try
             let spec = List.concat_map Frontend.Parse.parse_file filenames in
             let spec_il = Elaborate.Elab.elab_spec spec in
             let spec_sl = Structure.Struct.struct_spec spec_il in
             let sl_ast_json = Sl.Ast.spec_to_yojson spec_sl in
             Yojson.Safe.pretty_print Format.std_formatter sl_ast_json;
             ()
           with
           | ParseError (at, msg) ->
               Format.printf "%s\n" (string_of_error at msg)
           | ElabError (at, msg) ->
               Format.printf "%s\n" (string_of_error at msg))
       | `Parse -> (
           (* only take the first argument *)
           let filename = List.hd filenames in
           let parsed =
             Yojson.Safe.from_file filename |> Sl.Ast.spec_of_yojson
           in
           match parsed with
           | Ok spec -> Format.printf "%s\n" (Sl.Print.string_of_spec spec)
           | Error err ->
               Format.printf "Error while parsing %s: %s" filename err))

let p4_program_value_json_command =
  Core.Command.basic
    ~summary:"convert a P4 program to a value and output as JSON"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map includes_p4 = flag "-i" (listed string) ~doc:"p4 include paths"
     and filename_p4 = flag "-p" (required string) ~doc:"p4 file to convert" in
     fun () ->
       try
         let value_program =
           Interp_sl.Typing.convert_program_to_value includes_p4 filename_p4
         in
         let json = Sl.Ast.value_to_yojson value_program in
         Yojson.Safe.to_string json |> print_string
       with ParseError (at, msg) ->
         Format.printf "ill-formed: %s\n" (string_of_error at msg))

let command =
  Core.Command.group
    ~summary:"p4spec: a language design framework for the p4_16 language"
    [
      ("elab", elab_command);
      ("struct", struct_command);
      ("run-il", run_il_command);
      ("run-sl", run_sl_command);
      ("cover-dangling", cover_dangling_command);
      ("testgen", run_testgen_command);
      ("testgen-dbg", run_testgen_debug_command);
      ("interesting", interesting_command);
      ("parse", parse_command);
      ("json-ast", json_ast_command);
      ("p4-program-value-json", p4_program_value_json_command);
    ]

let () = Command_unix.run ~version command
