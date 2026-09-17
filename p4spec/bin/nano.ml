open Lang
open Runtime.Sim.Signature
open Cli_error
module Error = P4spectec.Error

let ( let* ) = Result.bind

(* Operations *)

let render_failure (diagnostic : Diagnostic.t) : unit =
  diagnostic |> Diagnostic.Report.singleton |> render_diagnostics

let install_handlers ~(profile : bool) ~(trace : Inst.Trace.level option)
    (spec_sim : spec) : unit =
  let handlers =
    if profile then
      let (module PH : Inst.Handler.HANDLER) = Inst.Profile.make () in
      [ (module PH : Inst.Handler.HANDLER) ]
    else []
  in
  let handlers =
    match trace with
    | Some level ->
        let (module TH : Inst.Handler.HANDLER) = Inst.Trace.make ~level () in
        handlers @ [ (module TH : Inst.Handler.HANDLER) ]
    | None -> handlers
  in
  Inst.Hook.register handlers;
  Inst.Hook.init_spec spec_sim

(* Commands *)

let elab_command =
  Core.Command.basic ~summary:"parse and elaborate a nano-P4 spec"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map paths_spec =
       anon (non_empty_sequence_as_list ("path" %: string))
     in
     fun () ->
       run_with_diagnostics
         ~action:(fun () -> P4spectec.elab paths_spec)
         ~on_success:(fun spec_il ->
           Format.printf "%s\n" (Il.Print.string_of_spec spec_il);
           Ok ()))

let algo_command =
  Core.Command.basic ~summary:"check algorithmic property of a nano-P4 spec"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map paths_spec =
       anon (non_empty_sequence_as_list ("path" %: string))
     in
     fun () ->
       run_with_diagnostics
         ~action:(fun () -> P4spectec.algo paths_spec)
         ~on_success:(fun spec_al ->
           Format.printf "%s\n" (Al.Print.string_of_spec spec_al);
           Ok ()))

let check_command =
  Core.Command.basic ~summary:"typecheck a nano-P4 program against the spec"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map paths_spec = anon (non_empty_sequence_as_list ("path" %: string))
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
     and _mode =
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
       let cache = not no_cache in
       run_with_diagnostics
         ~action:(fun () ->
           let* spec_sim = P4spectec.spec_of_mode SL_mode paths_spec in
           let* simulator =
             P4spectec.build_nano_sim ~cache ~det ~guard spec_sim
           in
           Ok (spec_sim, simulator))
         ~on_success:(fun (spec_sim, simulator) ->
           let (module Simulator : SIM) = simulator in
           install_handlers ~profile ~trace spec_sim;
           let result =
             Simulator.Interp.eval_program "Program_ok" includes_p4 path_p4
           in
           Inst.Hook.finish ();
           match result with
           | Pass _ ->
               Format.printf "passed\n";
               Ok ()
           | Fail (`Syntax diagnostic) -> Error diagnostic
           | Fail (`Runtime failure) -> Error (diagnostic_of_failure failure)))

let parse_command =
  Core.Command.basic ~summary:"parse a nano-P4 program"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map path_p4 = flag "-p" (required string) ~doc:"Nano-P4 program"
     and tree = flag "-t" no_arg ~doc:"print as tree"
     and includes_p4 = flag "-i" (listed string) ~doc:"Nano-P4 include paths" in
     fun () ->
       match Interface.NanoP4.parse_program includes_p4 [ path_p4 ] with
       | Pass value_program ->
           if tree then Nano.Print.print_tree value_program
           else Format.printf "%s\n" (Il.Print.string_of_value value_program)
       | Fail diagnostic ->
           render_failure diagnostic;
           exit 1)

let eval_command =
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
     and _mode =
       Command.Param.choose_one
         [
           flag "al" no_arg ~doc:"run AL interpreter"
           |> map ~f:(fun b -> Core.Option.some_if b AL_mode);
           flag "sl" no_arg ~doc:"run SL interpreter"
           |> map ~f:(fun b -> Core.Option.some_if b SL_mode);
         ]
         ~if_nothing_chosen:(Default_to SL_mode)
     in
     fun () ->
       let cache = not no_cache in
       run_with_diagnostics
         ~action:(fun () ->
           let* spec_sim = P4spectec.spec_of_mode SL_mode paths_spec in
           let* simulator =
             P4spectec.build_nano_sim ~cache ~det ~guard spec_sim
           in
           Ok (spec_sim, simulator))
         ~on_success:(fun (spec_sim, simulator) ->
           let (module Simulator : SIM) = simulator in
           install_handlers ~profile ~trace spec_sim;
           let result = Simulator.run_stf_test includes_p4 path_p4 path_stf in
           Inst.Hook.finish ();
           match result with
           | Pass () ->
               Format.printf "passed\n";
               Ok ()
           | Fail (`Syntax diagnostic) -> Error diagnostic
           | Fail (`Runtime failure) -> Error (diagnostic_of_failure failure)))

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
       run_with_diagnostics
         ~action:(fun () ->
           let* spec_sim = P4spectec.spec_of_mode SL_mode paths_spec in
           let* simulator = P4spectec.build_nano_sim ~det spec_sim in
           Ok simulator)
         ~on_success:(fun simulator ->
           let (module Simulator : SIM) = simulator in
           let module Filesys = Util.Filesys in
           let paths_p4 =
             testdirs_p4
             |> List.concat_map (Filesys.collect_files ~suffix:".p4")
           in
           let total = List.length paths_p4 in
           Format.printf "Running %d typecheck tests\n%!" total;
           let fails =
             List.fold_left
               (fun fails path_p4 ->
                 let result =
                   Simulator.Interp.eval_program "Program_ok" includes_p4
                     path_p4
                 in
                 let passed =
                   match result with Pass _ -> not neg | Fail _ -> neg
                 in
                 (if passed then Format.printf "PASS %s\n%!" path_p4
                  else
                    match result with
                    | Pass _ ->
                        Format.printf "FAIL %s (expected failure)\n%!" path_p4
                    | Fail failure ->
                        Format.printf "FAIL %s\n%!" path_p4;
                        let diagnostic =
                          match failure with
                          | `Syntax diagnostic -> diagnostic
                          | `Runtime failure -> diagnostic_of_failure failure
                        in
                        render_failure diagnostic);
                 if passed then fails else fails + 1)
               0 paths_p4
           in
           Format.printf "\n[PASS] %d/%d  [FAIL] %d/%d\n" (total - fails) total
             fails total;
           if fails = 0 then Ok ()
           else
             Error
               (Diagnostic.error ~source:"nano" Util.Source.no_region
                  (Format.asprintf "%d of %d tests failed" fails total))))

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
       run_with_diagnostics
         ~action:(fun () ->
           let* spec_sim = P4spectec.spec_of_mode SL_mode paths_spec in
           let* simulator = P4spectec.build_nano_sim ~det spec_sim in
           Ok simulator)
         ~on_success:(fun simulator ->
           let (module Simulator : SIM) = simulator in
           let module Filesys = Util.Filesys in
           let paths_p4 =
             testdirs_p4
             |> List.concat_map (Filesys.collect_files ~suffix:".p4")
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
           let fails =
             List.fold_left
               (fun fails (path_p4, path_stf) ->
                 let result =
                   Simulator.run_stf_test includes_p4 path_p4 path_stf
                 in
                 (match result with
                 | Pass () -> Format.printf "PASS %s\n%!" path_stf
                 | Fail failure ->
                     Format.printf "FAIL %s\n%!" path_stf;
                     let diagnostic =
                       match failure with
                       | `Syntax diagnostic -> diagnostic
                       | `Runtime failure -> diagnostic_of_failure failure
                     in
                     render_failure diagnostic);
                 match result with Pass () -> fails | Fail _ -> fails + 1)
               0 pairs
           in
           Format.printf "\n[PASS] %d/%d  [FAIL] %d/%d\n" (total - fails) total
             fails total;
           if fails = 0 then Ok ()
           else
             Error
               (Diagnostic.error ~source:"nano" Util.Source.no_region
                  (Format.asprintf "%d of %d tests failed" fails total))))

let splice_command =
  Core.Command.basic ~summary:"splice a skeleton nano-P4 specification document"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map paths_spec = anon (non_empty_sequence_as_list ("path" %: string))
     and paths_input = flag "-splice" (listed string) ~doc:"skeleton documents"
     and paths_output = flag "-out" (listed string) ~doc:"output files"
     and inplace = flag "-inplace" no_arg ~doc:"splice in place" in
     fun () ->
       run_with_diagnostics
         ~action:(fun () ->
           let* path_pairs =
             if
               (not inplace)
               && List.length paths_input <> List.length paths_output
             then
               Error
                 (Error.error_splice_file_count_mismatch
                    (List.length paths_input) (List.length paths_output))
             else if inplace then Ok (List.combine paths_input paths_input)
             else Ok (List.combine paths_input paths_output)
           in
           P4spectec.splice paths_spec path_pairs)
         ~on_success:(fun () -> Ok ()))

let command =
  Core.Command.group
    ~summary:"nano-p4spectec: a language design framework for nano-P4"
    [
      ("elab", elab_command);
      ("algo", algo_command);
      ("check", check_command);
      ("parse", parse_command);
      ("eval", eval_command);
      ("test-check", test_check_command);
      ("test-eval", test_eval_command);
      ("splice", splice_command);
    ]

let () = Command_unix.run command
