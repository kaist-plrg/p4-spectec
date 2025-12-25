open Lang
open Util.Error
open Util.Source

let version = "0.1"

exception CommandError of string

(* Commands *)

let elab_command =
  Core.Command.basic ~summary:"parse and elaborate a p4_16 spec"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map filenames_spec =
       anon (non_empty_sequence_as_list ("filename" %: string))
     in
     fun () ->
       try
         let spec = List.concat_map Frontend.Parse.parse_file filenames_spec in
         let spec_il = Elaborate.Elab.elab_spec spec in
         Format.printf "%s\n" (Il.Print.string_of_spec spec_il);
         ()
       with
       | CommandError msg -> Format.printf "%s\n" msg
       | ParseError (at, msg) -> Format.printf "%s\n" (string_of_error at msg)
       | ElabError (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

let struct_command =
  Core.Command.basic ~summary:"insert structured control flow to a p4_16 spec"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map filenames_spec =
       anon (non_empty_sequence_as_list ("filename" %: string))
     in
     fun () ->
       try
         let spec = List.concat_map Frontend.Parse.parse_file filenames_spec in
         let spec_il = Elaborate.Elab.elab_spec spec in
         let spec_sl = Structure.Struct.struct_spec spec_il in
         Format.printf "%s\n" (Sl.Print.string_of_spec spec_sl);
         ()
       with
       | ParseError (at, msg) -> Format.printf "%s\n" (string_of_error at msg)
       | ElabError (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

let run_command =
  Core.Command.basic ~summary:"run semantics of a p4_16 spec"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map filenames_spec =
       anon (non_empty_sequence_as_list ("filename" %: string))
     and relname = flag "-rel" (required string) ~doc:"relation to run"
     and includes_p4 = flag "-i" (listed string) ~doc:"p4 include paths"
     and filename_p4 = flag "-p" (required string) ~doc:"p4 file of interest"
     and _debug = flag "-dbg" no_arg ~doc:"print debug traces"
     and _profile = flag "-profile" no_arg ~doc:"profiling"
     and mode =
       Command.Param.choose_one
         [
           flag "il" no_arg ~doc:"run IL interpreter"
           |> map ~f:(fun b -> Core.Option.some_if b `IL);
           flag "sl" no_arg ~doc:"run SL interpreter"
           |> map ~f:(fun b -> Core.Option.some_if b `SL);
         ]
         ~if_nothing_chosen:(Default_to `SL)
     in
     fun () ->
       try
         let spec = List.concat_map Frontend.Parse.parse_file filenames_spec in
         let spec_il = Elaborate.Elab.elab_spec spec in
         let spec_sim =
           match mode with
           | `IL -> Runtime.Sim.Simulator.IL spec_il
           | `SL ->
               let spec_sl = Structure.Struct.struct_spec spec_il in
               Runtime.Sim.Simulator.SL spec_sl
         in
         let (module Runner) = Arch.Gen.gen_placeholder () in
         match
           Runner.run_program ~derive:false spec_sim relname includes_p4
             filename_p4
         with
         | Pass _ -> Format.printf "passed\n"
         | Fail (_, msg, _) -> Format.printf "failed: %s\n" msg
         | IllFormed (_, msg, _) -> Format.printf "ill-formed: %s\n" msg
       with
       | CommandError msg -> Format.printf "%s\n" msg
       | ParseError (at, msg) -> Format.printf "%s\n" (string_of_error at msg)
       | ElabError (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

let sim_command =
  Core.Command.basic
    ~summary:"simulate a target architecture with a p4_16 program and spec"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map filenames_spec =
       anon (non_empty_sequence_as_list ("filename" %: string))
     and includes_p4 = flag "-i" (listed string) ~doc:"p4 include paths"
     and filename_p4 = flag "-p" (required string) ~doc:"p4 file of interest"
     and filename_stf = flag "-stf" (required string) ~doc:"stf test file"
     and arch = flag "-arch" (required string) ~doc:"target architecture"
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
       try
         let spec = List.concat_map Frontend.Parse.parse_file filenames_spec in
         let spec_il = Elaborate.Elab.elab_spec spec in
         let spec_sim =
           match mode with
           | `IL -> Runtime.Sim.Simulator.IL spec_il
           | `SL ->
               let spec_sl = Structure.Struct.struct_spec spec_il in
               Runtime.Sim.Simulator.SL spec_sl
         in
         let (module Runner) = Arch.Gen.gen arch in
         match
           Runner.run_stf_test spec_sim includes_p4 filename_p4 filename_stf
         with
         | Pass -> Format.printf "passed\n"
         | Fail (_, msg) -> Format.printf "failed: %s\n" msg
         | IllFormed (_, msg) -> Format.printf "ill-formed: %s\n" msg
       with
       | CommandError msg -> Format.printf "%s\n" msg
       | ParseError (at, msg) -> Format.printf "%s\n" (string_of_error at msg)
       | ElabError (at, msg) -> Format.printf "%s\n" (string_of_error at msg)
       | ArchError (at, msg) -> Format.printf "%s\n" (string_of_error at msg)
       | StfError msg -> Format.printf "%s\n" (string_of_error no_region msg))

let cover_dangling_command =
  Core.Command.basic ~summary:"measure dangling coverage of the P4 type system"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map filenames_spec =
       anon (non_empty_sequence_as_list ("filename" %: string))
     and relname = flag "-rel" (required string) ~doc:"relation to run"
     and includes_p4 = flag "-i" (listed string) ~doc:"p4 include paths"
     and excludes_p4 = flag "-e" (listed string) ~doc:"p4 test exclude paths"
     and dirnames_p4 =
       flag "-d" (listed string) ~doc:"p4 directories of interest"
     and filename_cov =
       flag "-cov" (required string) ~doc:"output coverage file"
     in
     fun () ->
       try
         let spec = List.concat_map Frontend.Parse.parse_file filenames_spec in
         let spec_il = Elaborate.Elab.elab_spec spec in
         let spec_sl = Structure.Struct.struct_spec spec_il in
         let excludes_p4 = Util.Filesys.collect_excludes excludes_p4 in
         let filenames_p4 =
           List.concat_map
             (Util.Filesys.collect_files ~suffix:".p4")
             dirnames_p4
         in
         let filenames_p4 =
           List.filter
             (fun filename_p4 ->
               not (List.exists (String.equal filename_p4) excludes_p4))
             filenames_p4
         in
         let (module Runner) = Arch.Gen.gen_placeholder () in
         let cover =
           Runner.cover_programs spec_sl relname includes_p4 filenames_p4
         in
         Runtime.Test.Cov.Multiple.log ~filename_cov_opt:(Some filename_cov)
           cover
       with
       | CommandError msg -> Format.printf "%s\n" msg
       | ParseError (at, msg) -> Format.printf "%s\n" (string_of_error at msg)
       | ElabError (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

let run_testgen_command =
  Core.Command.basic
    ~summary:"generate negative type checker tests from a p4_16 spec"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map filenames_spec =
       anon (non_empty_sequence_as_list ("filename" %: string))
     and relname = flag "-rel" (required string) ~doc:"relation to run"
     and fuel = flag "-fuel" (required int) ~doc:"fuel for test generation"
     and includes_p4 = flag "-i" (listed string) ~doc:"p4 include paths"
     and excludes_p4 = flag "-e" (listed string) ~doc:"p4 test exclude paths"
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
         Testgen.Gen.fuzzer fuel spec_il spec_sl relname includes_p4 dirname_gen
           name_campaign randseed logmode bootmode mutationmode covermode
       with
       | CommandError msg -> Format.printf "%s\n" msg
       | ParseError (at, msg) -> Format.printf "%s\n" (string_of_error at msg)
       | ElabError (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

let run_testgen_debug_command =
  Core.Command.basic
    ~summary:"debug close-AST deriver in negative type checker generator"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map filenames_spec =
       anon (non_empty_sequence_as_list ("filename" %: string))
     and relname = flag "-rel" (required string) ~doc:"relation to run"
     and includes_p4 = flag "-i" (listed string) ~doc:"p4 include paths"
     and filename_p4 = flag "-p" (required string) ~doc:"p4 file to typecheck"
     and dirname_debug =
       flag "-debug" (required string) ~doc:"directory for debug files"
     and pid = flag "-pid" (required int) ~doc:"phantom id to close-miss" in
     fun () ->
       try
         let spec = List.concat_map Frontend.Parse.parse_file filenames_spec in
         let spec_il = Elaborate.Elab.elab_spec spec in
         let spec_sl = Structure.Struct.struct_spec spec_il in
         Testgen.Derive.debug_phantom spec_sl relname includes_p4 filename_p4
           dirname_debug pid
       with
       | CommandError msg -> Format.printf "%s\n" msg
       | ParseError (at, msg) -> Format.printf "%s\n" (string_of_error at msg)
       | ElabError (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

let interesting_command =
  Core.Command.basic ~summary:"interestingness test for reducing p4_16 programs"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map filenames_spec =
       anon (non_empty_sequence_as_list ("filename" %: string))
     and relname = flag "-rel" (required string) ~doc:"relation to run"
     and includes_p4 = flag "-i" (listed string) ~doc:"p4 include paths"
     and check_well_typed =
       flag "-well" no_arg
         ~doc:"'interesting' if well-typed (default: ill-typed)"
     and check_close_miss =
       flag "-close" no_arg ~doc:"'interesting' if close-miss (default: hit)"
     and pid = flag "-pid" (required int) ~doc:"phantom id to test"
     and filename_p4 =
       flag "-p" (required string) ~doc:"p4 file to typecheck"
     in
     fun () ->
       try
         let spec = List.concat_map Frontend.Parse.parse_file filenames_spec in
         let spec_il = Elaborate.Elab.elab_spec spec in
         let spec_sl = Structure.Struct.struct_spec spec_il in
         let (module Runner) = Arch.Gen.gen_placeholder () in
         let spec_sim = Runtime.Sim.Simulator.SL spec_sl in
         let result =
           Runner.run_program ~derive:false spec_sim relname includes_p4
             filename_p4
         in
         match result with
         | Pass (_, _, _, cover_single) ->
             if check_well_typed then (
               let branch =
                 Runtime.Test.Cov.Single.Cover.find pid cover_single
               in
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
         | Fail (_, _, cover_single) -> (
             if check_well_typed then (
               Printf.printf "IllTyped\n";
               exit 10)
             else
               let branch =
                 Runtime.Test.Cov.Single.Cover.find pid cover_single
               in
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
       | CommandError msg -> Format.printf "%s\n" msg
       | ParseError (at, msg) -> Format.printf "%s\n" (string_of_error at msg)
       | ElabError (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

let splice_command =
  Core.Command.basic ~summary:"splice a skeleton p4_16 specification document"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map filenames_spec =
       anon (non_empty_sequence_as_list ("filename" %: string))
     and filenames_input =
       flag "-splice" (listed string) ~doc:"skeleton documents"
     and filenames_output = flag "-out" (listed string) ~doc:"output files"
     and inplace = flag "-inplace" no_arg ~doc:"splice in place" in
     fun () ->
       try
         if
           (not inplace)
           && List.length filenames_input <> List.length filenames_output
         then raise (CommandError "number of input and output files must match");
         let spec = List.concat_map Frontend.Parse.parse_file filenames_spec in
         let spec_il = Elaborate.Elab.elab_spec spec in
         let spec_sl = Structure.Struct.struct_spec spec_il in
         let filenames =
           if inplace then List.combine filenames_input filenames_input
           else List.combine filenames_input filenames_output
         in
         Splice.Driver.splice_files spec spec_sl filenames
       with
       | CommandError msg -> Format.printf "%s\n" msg
       | ParseError (at, msg) -> Format.printf "%s\n" (string_of_error at msg)
       | ElabError (at, msg) -> Format.printf "%s\n" (string_of_error at msg)
       | SpliceError (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

let parse_command =
  Core.Command.basic ~summary:"parse a P4 program"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map filenames_spec =
       anon (non_empty_sequence_as_list ("filename" %: string))
     and includes_p4 = flag "-i" (listed string) ~doc:"p4 include paths"
     and filename_p4 = flag "-p" (required string) ~doc:"p4 file to typecheck"
     and roundtrip =
       flag "-r" no_arg ~doc:"perform a round-trip parse/unparse"
     in
     fun () ->
       try
         let spec = List.concat_map Frontend.Parse.parse_file filenames_spec in
         let spec_il = Elaborate.Elab.elab_spec spec in
         let parsed_p4_file =
           Interface.Parse.parse_file includes_p4 filename_p4
         in
         let unparsed_p4_string =
           Format.asprintf "%a\n"
             (Interface.Unparse.pp_program_il spec_il)
             parsed_p4_file
         in
         if roundtrip then
           let parsed_p4_string =
             Interface.Parse.parse_string filename_p4 unparsed_p4_string
           in
           Il.Eq.eq_value ~dbg:true parsed_p4_file parsed_p4_string
           |> (fun b ->
                if b then "Roundtrip successful" else "Roundtrip failed")
           |> print_endline
         else unparsed_p4_string |> print_endline
       with
       | Sys_error msg -> Format.printf "File error: %s\n" msg
       | ElabError (at, msg) ->
           Format.printf "Elaboration error: %s\n" (string_of_error at msg)
       | ParseError (at, msg) ->
           Format.printf "Parse error: %s\n" (string_of_error at msg)
       | Interface.Lexer.Error msg -> Format.printf "Lexer error: %s\n" msg
       | e -> Format.printf "Unknown error: %s\n" (Printexc.to_string e))

let json_command =
  Core.Command.basic ~summary:"Emit/Parse JSON AST for Structured Language"
    ~readme:(fun () ->
      "./p4spectec json -emit spec/*.watsup\n\
       ./p4spectec json -parse <ast-file.json>")
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
             let sl_ast_json = Sl.spec_to_yojson spec_sl in
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
           let parsed = Yojson.Safe.from_file filename |> Sl.spec_of_yojson in
           match parsed with
           | Ok spec -> Format.printf "%s\n" (Sl.Print.string_of_spec spec)
           | Error err ->
               Format.printf "Error while parsing %s: %s" filename err))

let prose_command =
  Core.Command.basic ~summary:"generate asciidoc prose from a p4_16 spec"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map filenames_spec =
       anon (non_empty_sequence_as_list ("filename" %: string))
     in
     fun () ->
       try
         let spec = List.concat_map Frontend.Parse.parse_file filenames_spec in
         let spec_il = Elaborate.Elab.elab_spec spec in
         let spec_sl = Structure.Struct.struct_spec spec_il in
         let spec_pl = Prose.Prosify.prosify_spec spec_sl in
         Format.printf "%s\n" (Prose.Pl.Print.render_spec spec_pl);
         ()
       with
       | ParseError (at, msg) -> Format.printf "%s\n" (string_of_error at msg)
       | ElabError (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

let command =
  Core.Command.group
    ~summary:"p4spec: a language design framework for the p4_16 language"
    [
      ("elab", elab_command);
      ("struct", struct_command);
      ("prose", prose_command);
      ("run", run_command);
      ("sim", sim_command);
      ("cover-dangling", cover_dangling_command);
      ("testgen", run_testgen_command);
      ("testgen-dbg", run_testgen_debug_command);
      ("interesting", interesting_command);
      ("splice", splice_command);
      ("parse", parse_command);
      ("json", json_command);
    ]

let () = Command_unix.run ~version command
