open Lang
open Runtime.Sim.Signature
open Util.Error

exception CommandError of string

let elab_command =
  Core.Command.basic ~summary:"parse and elaborate a nano-P4 spec"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map paths_spec =
       anon (non_empty_sequence_as_list ("path" %: string))
     in
     fun () ->
       try
         let spec_il = Pass.elab paths_spec in
         Format.printf "%s\n" (Il.Print.string_of_spec spec_il);
         ()
       with
       | CommandError msg -> Format.printf "%s\n" msg
       | ParseError (at, msg) -> Format.printf "%s\n" (string_of_error at msg)
       | ElabError (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

let run_command =
  Core.Command.basic
    ~summary:"execute the nano-p4 spec against a nano-p4 program"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map paths_spec = anon (non_empty_sequence_as_list ("path" %: string))
     and relname = flag "-rel" (required string) ~doc:"relation to run"
     and includes_p4 = flag "-i" (listed string) ~doc:"Nano-P4 include paths"
     and path_p4 = flag "-p" (required string) ~doc:"Nano-P4 program"
     and no_cache = flag "-no-cache" no_arg ~doc:"disable caching"
     and det = flag "-det" no_arg ~doc:"deterministic mode"
     and guard =
       flag "-guard" no_arg ~doc:"enable guard for builtins and externs"
     and profile = flag "-profile" no_arg ~doc:"profiling"
     and trace =
       Command.Param.choose_one
         [
           flag "-trace" no_arg ~doc:"emit execution trace"
           |> map ~f:(fun b -> Core.Option.some_if b (Some Inst.Trace.Simple));
           flag "-trace-full" no_arg ~doc:"emit full execution trace"
           |> map ~f:(fun b -> Core.Option.some_if b (Some Inst.Trace.Full));
         ]
         ~if_nothing_chosen:(Default_to None)
     and mode =
       Command.Param.choose_one
         [
           flag "il" no_arg ~doc:"run AL interpreter"
           |> map ~f:(fun b -> Core.Option.some_if b AL_mode);
           flag "sl" no_arg ~doc:"run SL interpreter"
           |> map ~f:(fun b -> Core.Option.some_if b SL_mode);
         ]
         ~if_nothing_chosen:(Default_to SL_mode)
     in
     fun () ->
       try
         let cache = not no_cache in
         let spec_sim, (module Simulator) =
           Backend_sim.Build.build_nano ~cache ~det ~guard ~final:true SL_mode
             paths_spec
         in
         let handlers =
           if profile then
             let (module PH : Inst.Handler.HANDLER) = Inst.Profile.make () in
             [ (module PH : Inst.Handler.HANDLER) ]
           else []
         in
         let handlers =
           match trace with
           | Some level ->
               let (module TH : Inst.Handler.HANDLER) =
                 Inst.Trace.make ~level ()
               in
               handlers @ [ (module TH : Inst.Handler.HANDLER) ]
           | None -> handlers
         in
         Inst.Hook.register handlers;
         Inst.Hook.init_spec spec_sim;
         let result =
           Simulator.Interp.eval_program relname includes_p4 path_p4
         in
         Inst.Hook.finish ();
         match result with
         | Pass _ -> Format.printf "passed\n"
         | Fail (`Syntax (_, msg)) -> Format.printf "syntax error: %s\n" msg
         | Fail (`Runtime (_, msg)) -> Format.printf "runtime error: %s\n" msg
       with
       | CommandError msg -> Format.printf "%s\n" msg
       | ParseError (at, msg) | ElabError (at, msg) | StructError (at, msg) ->
           Format.printf "%s\n" (string_of_error at msg)
       | InterpError (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

let parse_command =
  Core.Command.basic ~summary:"parse a nano-P4 program"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map path_p4 = flag "-p" (required string) ~doc:"Nano-P4 program"
     and tree = flag "-t" no_arg ~doc:"print as tree"
     and includes_p4 = flag "-i" (listed string) ~doc:"Nano-P4 include paths" in
     fun () ->
       try
         let value_program = Nano.Parse.parse_file includes_p4 path_p4 in
         if tree then Nano.Print.print_tree value_program
         else Format.printf "%s\n" (Lang.Il.Print.string_of_value value_program)
       with
       | Sys_error msg -> Format.printf "File error: %s\n" msg
       | ParseError (at, msg) ->
           Format.printf "Parse error: %s\n" (string_of_error at msg)
       | e -> Format.printf "Unknown error: %s\n" (Printexc.to_string e))

let sim_command =
  Core.Command.basic
    ~summary:"simulate nano switch with a nano-P4 program and P4 spec"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map paths_spec = anon (non_empty_sequence_as_list ("path" %: string))
     and includes_p4 = flag "-i" (listed string) ~doc:"Nano-P4 include paths"
     and path_p4 = flag "-p" (required string) ~doc:"Nano-P4 program"
     and path_stf = flag "-stf" (required string) ~doc:"stf test file"
     and no_cache = flag "-no-cache" no_arg ~doc:"disable caching"
     and det = flag "-det" no_arg ~doc:"deterministic mode"
     and guard =
       flag "-guard" no_arg ~doc:"enable guard for builtins and externs"
     and profile = flag "-profile" no_arg ~doc:"profiling"
     and trace =
       Command.Param.choose_one
         [
           flag "-trace" no_arg ~doc:"emit execution trace"
           |> map ~f:(fun b -> Core.Option.some_if b (Some Inst.Trace.Simple));
           flag "-trace-full" no_arg ~doc:"emit full execution trace"
           |> map ~f:(fun b -> Core.Option.some_if b (Some Inst.Trace.Full));
         ]
         ~if_nothing_chosen:(Default_to None)
     in
     fun () ->
       try
         let cache = not no_cache in
         let spec_sim, (module Simulator) =
           Backend_sim.Build.build_nano ~cache ~det ~guard ~final:true SL_mode
             paths_spec
         in
         let handlers =
           if profile then
             let (module PH : Inst.Handler.HANDLER) = Inst.Profile.make () in
             [ (module PH : Inst.Handler.HANDLER) ]
           else []
         in
         let handlers =
           match trace with
           | Some level ->
               let (module TH : Inst.Handler.HANDLER) =
                 Inst.Trace.make ~level ()
               in
               handlers @ [ (module TH : Inst.Handler.HANDLER) ]
           | None -> handlers
         in
         Inst.Hook.register handlers;
         Inst.Hook.init_spec spec_sim;
         let result = Simulator.run_stf_test includes_p4 path_p4 path_stf in
         Inst.Hook.finish ();
         match result with
         | Pass -> Format.printf "passed\n"
         | Fail (`Syntax (_, msg)) -> Format.printf "syntax error: %s\n" msg
         | Fail (`Runtime (_, msg)) -> Format.printf "runtime error: %s\n" msg
       with
       | CommandError msg -> Format.printf "%s\n" msg
       | ParseError (at, msg) | ElabError (at, msg) | StructError (at, msg) ->
           Format.printf "%s\n" (string_of_error at msg)
       | InterpError (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

let test_check_command =
  Core.Command.basic
    ~summary:"batch typecheck nano-P4 programs against the spec"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map paths_spec = anon (non_empty_sequence_as_list ("path" %: string))
     and includes_p4 = flag "-i" (listed string) ~doc:"Nano-P4 include paths"
     and testdirs_p4 =
       flag "-p4-dir" (listed string) ~doc:"directories of .p4 files"
     and neg = flag "-neg" no_arg ~doc:"negative testing (expect failure)"
     and det = flag "-det" no_arg ~doc:"deterministic mode" in
     fun () ->
       let module Filesys = Util.Filesys in
       let paths_p4 =
         testdirs_p4 |> List.concat_map (Filesys.collect_files ~suffix:".p4")
       in
       let total = List.length paths_p4 in
       Format.printf "Running %d typecheck tests\n%!" total;
       let _spec_sim, (module Simulator : SIM) =
         Backend_sim.Build.build_nano ~det ~final:true SL_mode paths_spec
       in
       let fails =
         List.fold_left
           (fun fails path_p4 ->
             let result =
               try
                 Simulator.Interp.eval_program "Program_ok" includes_p4 path_p4
               with _ ->
                 Fail (`Runtime (Util.Source.no_region, "unexpected exception"))
             in
             let passed =
               match result with Pass _ -> not neg | Fail _ -> neg
             in
             (if passed then Format.printf "PASS %s\n%!" path_p4
              else
                match result with
                | Pass _ ->
                    Format.printf "FAIL %s (expected failure)\n%!" path_p4
                | Fail (`Syntax (_, msg)) ->
                    Format.printf "FAIL %s (syntax: %s)\n%!" path_p4 msg
                | Fail (`Runtime (_, msg)) ->
                    Format.printf "FAIL %s (runtime: %s)\n%!" path_p4 msg);
             if passed then fails else fails + 1)
           0 paths_p4
       in
       Format.printf "\n[PASS] %d/%d  [FAIL] %d/%d\n" (total - fails) total
         fails total)

let test_eval_command =
  Core.Command.basic
    ~summary:
      "batch-evaluate nano-P4 programs against the spec using .stf packet tests"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map paths_spec = anon (non_empty_sequence_as_list ("path" %: string))
     and includes_p4 = flag "-i" (listed string) ~doc:"Nano-P4 include paths"
     and testdirs_p4 =
       flag "-p4-dir" (listed string) ~doc:"directories of .p4/.stf test pairs"
     and det = flag "-det" no_arg ~doc:"deterministic mode" in
     fun () ->
       let module Filesys = Util.Filesys in
       let paths_p4 =
         testdirs_p4 |> List.concat_map (Filesys.collect_files ~suffix:".p4")
       in
       let pairs =
         paths_p4
         |> List.filter_map (fun path_p4 ->
                let path_stf =
                  String.sub path_p4 0 (String.length path_p4 - 3) ^ ".stf"
                in
                if Sys.file_exists path_stf then Some (path_p4, path_stf)
                else None)
       in
       let total = List.length pairs in
       Format.printf "Running %d evaluation tests\n%!" total;
       let _spec_sim, (module Simulator : SIM) =
         Backend_sim.Build.build_nano ~det ~final:true SL_mode paths_spec
       in
       let fails =
         List.fold_left
           (fun fails (path_p4, path_stf) ->
             let result =
               try Simulator.run_stf_test includes_p4 path_p4 path_stf
               with _ ->
                 Fail (`Runtime (Util.Source.no_region, "unexpected exception"))
             in
             (match result with
             | Pass -> Format.printf "PASS %s\n%!" path_stf
             | Fail (`Syntax (_, msg)) ->
                 Format.printf "FAIL %s (syntax: %s)\n%!" path_stf msg
             | Fail (`Runtime (_, msg)) ->
                 Format.printf "FAIL %s (runtime: %s)\n%!" path_stf msg);
             match result with Pass -> fails | Fail _ -> fails + 1)
           0 pairs
       in
       Format.printf "\n[PASS] %d/%d  [FAIL] %d/%d\n" (total - fails) total
         fails total)

let command =
  Core.Command.group
    ~summary:"nano-p4spectec: a language design framework for nano-P4"
    [
      ("elab", elab_command);
      ("run", run_command);
      ("parse", parse_command);
      ("sim", sim_command);
      ("test-check", test_check_command);
      ("test-eval", test_eval_command);
    ]

let () = Command_unix.run command
