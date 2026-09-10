open Lang
open Runtime.Sim.Signature
open Cli_error
module Error = P4spectec.Error

let version = "0.1"
let ( let* ) = Result.bind

(* Operations *)

let run_with_instr (module Simulator : SIM) spec_sim relname includes_p4 path_p4
    =
  let (module IH : Inst.Handler.HANDLER), read_coverage_instr =
    Inst.Coverage_instr.make ()
  in
  Inst.Hook.register [ (module IH : Inst.Handler.HANDLER) ];
  Inst.Hook.init_spec spec_sim;
  let result = Simulator.Interp.eval_program relname includes_p4 path_p4 in
  Inst.Hook.finish ();
  let cover = read_coverage_instr () in
  (result, cover)

let run_with_dangling (module Simulator : SIM) spec_sim relname includes_p4
    path_p4 =
  let (module DH : Inst.Handler.HANDLER), read_coverage_dangling =
    Inst.Coverage_dangling.make ()
  in
  Inst.Hook.register [ (module DH : Inst.Handler.HANDLER) ];
  Inst.Hook.init_spec spec_sim;
  let result = Simulator.Interp.eval_program relname includes_p4 path_p4 in
  Inst.Hook.finish ();
  let cover = read_coverage_dangling () in
  (result, cover)

let sim_with_instr (module Simulator : SIM) spec_sim includes_p4 path_p4
    path_stf =
  let (module IH : Inst.Handler.HANDLER), read_coverage_instr =
    Inst.Coverage_instr.make ()
  in
  Inst.Hook.register [ (module IH : Inst.Handler.HANDLER) ];
  Inst.Hook.init_spec spec_sim;
  let result = Simulator.run_stf_test includes_p4 path_p4 path_stf in
  Inst.Hook.finish ();
  let cover = read_coverage_instr () in
  (result, cover)

let sim_with_dangling (module Simulator : SIM) spec_sim includes_p4 path_p4
    path_stf =
  let (module DH : Inst.Handler.HANDLER), read_coverage_dangling =
    Inst.Coverage_dangling.make ()
  in
  Inst.Hook.register [ (module DH : Inst.Handler.HANDLER) ];
  Inst.Hook.init_spec spec_sim;
  let result = Simulator.run_stf_test includes_p4 path_p4 path_stf in
  Inst.Hook.finish ();
  let cover = read_coverage_dangling () in
  (result, cover)

let cover_run_instr ?(arch : string option) paths_spec relname includes_p4
    paths_p4 path_cov =
  let* spec_sl = P4spectec.structure ~final:true paths_spec in
  let spec_sim = SL spec_sl in
  let* simulator = P4spectec.build_sim ?arch spec_sim in
  let (module Simulator : SIM) = simulator in
  let cover_multi = Coverage.Instr.Multi.init spec_sl in
  let cover_multi =
    List.fold_left
      (fun cover_multi path_p4 ->
        let _, cover_single =
          run_with_instr (module Simulator) spec_sim relname includes_p4 path_p4
        in
        Coverage.Instr.Multi.extend cover_multi path_p4 cover_single)
      cover_multi paths_p4
  in
  Coverage.Instr.Log.log_spec ~path_cov_opt:(Some path_cov) cover_multi spec_sl;
  Ok ()

let cover_run_dangling ?(arch : string option) paths_spec relname includes_p4
    paths_p4 path_cov =
  let* spec_sl = P4spectec.structure ~final:true paths_spec in
  let spec_sim = SL spec_sl in
  let* simulator = P4spectec.build_sim ?arch spec_sim in
  let (module Simulator : SIM) = simulator in
  let cover_multi = Coverage.Dangling.Multi.init spec_sl in
  let cover_multi =
    List.fold_left
      (fun cover_multi path_p4 ->
        let program_result, cover_single =
          run_with_dangling
            (module Simulator)
            spec_sim relname includes_p4 path_p4
        in
        let wellformed, welltyped =
          match program_result with
          | Pass _ -> (true, true)
          | Fail (`Syntax _) -> (false, false)
          | Fail (`Runtime _) -> (true, false)
        in
        Coverage.Dangling.Multi.extend cover_multi path_p4 wellformed welltyped
          cover_single)
      cover_multi paths_p4
  in
  Coverage.Dangling.Multi.log ~path_cov_opt:(Some path_cov) cover_multi;
  Ok ()

let cover_sim_instr ?(arch : string option) paths_spec includes_p4 paths_p4
    paths_stf path_cov =
  let* spec_sl = P4spectec.structure ~final:true paths_spec in
  let spec_sim = SL spec_sl in
  let* simulator = P4spectec.build_sim ?arch spec_sim in
  let (module Simulator : SIM) = simulator in
  let cover_multi = Coverage.Instr.Multi.init spec_sl in
  let cover_multi =
    List.fold_left2
      (fun cover_multi path_p4 path_stf ->
        let _, cover_single =
          sim_with_instr
            (module Simulator)
            spec_sim includes_p4 path_p4 path_stf
        in
        Coverage.Instr.Multi.extend cover_multi path_p4 cover_single)
      cover_multi paths_p4 paths_stf
  in
  Coverage.Instr.Log.log_spec ~path_cov_opt:(Some path_cov) cover_multi spec_sl;
  Ok ()

let cover_sim_dangling ?(arch : string option) paths_spec includes_p4 paths_p4
    paths_stf path_cov =
  let* spec_sl = P4spectec.structure ~final:true paths_spec in
  let spec_sim = SL spec_sl in
  let* simulator = P4spectec.build_sim ?arch spec_sim in
  let (module Simulator : SIM) = simulator in
  let cover_multi = Coverage.Dangling.Multi.init spec_sl in
  let cover_multi =
    List.fold_left2
      (fun cover_multi path_p4 path_stf ->
        let program_result, cover_single =
          sim_with_dangling
            (module Simulator)
            spec_sim includes_p4 path_p4 path_stf
        in
        let wellformed, welltyped =
          match program_result with
          | Pass () -> (true, true)
          | Fail (`Syntax _) -> (true, false)
          | Fail (`Runtime _) -> (false, false)
        in
        Coverage.Dangling.Multi.extend cover_multi path_p4 wellformed welltyped
          cover_single)
      cover_multi paths_p4 paths_stf
  in
  Coverage.Dangling.Multi.log ~path_cov_opt:(Some path_cov) cover_multi;
  Ok ()

(* Commands *)

let elab_command =
  Core.Command.basic ~summary:"parse and elaborate a P4 spec"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map paths_spec =
       anon (non_empty_sequence_as_list ("path" %: string))
     in
     fun () ->
       run_with_diagnostics
         ~action:(fun () -> P4spectec.elab paths_spec)
         ~on_success:(fun spec_il ->
           Format.printf "%s\n" (Il.Print.string_of_spec spec_il)))

let algo_command =
  Core.Command.basic ~summary:"check algorithmic property of a P4 spec"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map paths_spec =
       anon (non_empty_sequence_as_list ("path" %: string))
     in
     fun () ->
       run_with_diagnostics
         ~action:(fun () -> P4spectec.algo paths_spec)
         ~on_success:(fun spec_al ->
           Format.printf "%s\n" (Al.Print.string_of_spec spec_al)))

let struct_command =
  Core.Command.basic ~summary:"insert structured control flow to a P4 spec"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map paths_spec =
       anon (non_empty_sequence_as_list ("path" %: string))
     in
     fun () ->
       run_with_diagnostics
         ~action:(fun () -> P4spectec.structure ~final:true paths_spec)
         ~on_success:(fun spec_sl ->
           Format.printf "%s\n" (Sl.Print.string_of_spec spec_sl)))

let prose_command =
  Core.Command.basic ~summary:"generate AsciiDoc prose from a P4 spec"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map paths_spec =
       anon (non_empty_sequence_as_list ("path" %: string))
     in
     fun () ->
       run_with_diagnostics
         ~action:(fun () -> P4spectec.annotate paths_spec)
         ~on_success:(fun spec_pl ->
           Format.printf "%s\n" (Pl.Render.render_spec spec_pl)))

let run_command =
  Core.Command.basic ~summary:"execute the P4 spec against a P4 program"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map paths_spec = anon (non_empty_sequence_as_list ("path" %: string))
     and relname = flag "-rel" (required string) ~doc:"relation to run"
     and includes_p4 = flag "-i" (listed string) ~doc:"P4 include paths"
     and path_p4 = flag "-p" (required string) ~doc:"P4 program"
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
           flag "al" no_arg ~doc:"run AL interpreter"
           |> map ~f:(fun b -> Core.Option.some_if b AL_mode);
           flag "sl" no_arg ~doc:"run SL interpreter"
           |> map ~f:(fun b -> Core.Option.some_if b SL_mode);
           flag "pl" no_arg ~doc:"run PL interpreter"
           |> map ~f:(fun b -> Core.Option.some_if b PL_mode);
         ]
         ~if_nothing_chosen:(Default_to SL_mode)
     in
     fun () ->
       let cache = not no_cache in
       run_with_diagnostics
         ~action:(fun () ->
           let* spec_sim = P4spectec.spec_of_mode mode paths_spec in
           let* simulator = P4spectec.build_sim ~cache ~det ~guard spec_sim in
           Ok (spec_sim, simulator))
         ~on_success:(fun (spec_sim, simulator) ->
           let (module Simulator : SIM) = simulator in
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
           | Fail (`Syntax diagnostic) ->
               diagnostic |> Diagnostic.Report.singleton |> render_diagnostics
           | Fail (`Runtime failure) ->
               failure |> diagnostic_of_failure |> Diagnostic.Report.singleton
               |> render_diagnostics))

let sim_command =
  Core.Command.basic
    ~summary:"simulate a target architecture with a P4 program and P4 spec"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map paths_spec = anon (non_empty_sequence_as_list ("path" %: string))
     and includes_p4 = flag "-i" (listed string) ~doc:"P4 include paths"
     and path_p4 = flag "-p" (required string) ~doc:"P4 program"
     and path_stf = flag "-stf" (required string) ~doc:"stf test file"
     and arch = flag "-arch" (required string) ~doc:"target architecture"
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
           flag "al" no_arg ~doc:"run AL interpreter"
           |> map ~f:(fun b -> Core.Option.some_if b AL_mode);
           flag "sl" no_arg ~doc:"run SL interpreter"
           |> map ~f:(fun b -> Core.Option.some_if b SL_mode);
           flag "pl" no_arg ~doc:"run PL interpreter"
           |> map ~f:(fun b -> Core.Option.some_if b PL_mode);
         ]
         ~if_nothing_chosen:(Default_to SL_mode)
     in
     fun () ->
       let cache = not no_cache in
       run_with_diagnostics
         ~action:(fun () ->
           let* spec_sim = P4spectec.spec_of_mode mode paths_spec in
           let* simulator =
             P4spectec.build_sim ~cache ~det ~guard ~arch spec_sim
           in
           Ok (spec_sim, simulator))
         ~on_success:(fun (spec_sim, simulator) ->
           let (module Simulator : SIM) = simulator in
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
           | Pass () -> Format.printf "passed\n"
           | Fail (`Syntax diagnostic) ->
               diagnostic |> Diagnostic.Report.singleton |> render_diagnostics
           | Fail (`Runtime failure) ->
               failure |> diagnostic_of_failure |> Diagnostic.Report.singleton
               |> render_diagnostics))

let cover_run_command =
  Core.Command.basic ~summary:"measure coverage of the spec"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map paths_spec = anon (non_empty_sequence_as_list ("path" %: string))
     and relname = flag "-rel" (required string) ~doc:"relation to run"
     and includes_p4 = flag "-i" (listed string) ~doc:"P4 include paths"
     and excludes_p4 = flag "-e" (listed string) ~doc:"P4 test exclude paths"
     and testdirs_p4 = flag "-p4-dir" (listed string) ~doc:"P4 test directories"
     and path_cov = flag "-cov" (required string) ~doc:"output coverage file"
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
       let excludes_p4 = Util.Test.collect_excludes excludes_p4 in
       let paths_p4 =
         testdirs_p4
         |> List.concat_map (Util.Filesys.collect_files ~suffix:".p4")
         |> List.filter (fun path_p4 ->
                not (List.exists (String.equal path_p4) excludes_p4))
       in
       run_with_diagnostics
         ~action:(fun () ->
           match mode with
           | `Instr ->
               cover_run_instr paths_spec relname includes_p4 paths_p4 path_cov
           | `Dangling ->
               cover_run_dangling paths_spec relname includes_p4 paths_p4
                 path_cov)
         ~on_success:ignore)

let cover_sim_command =
  Core.Command.basic
    ~summary:"measure coverage of the spec when simulated on STF"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map paths_spec = anon (non_empty_sequence_as_list ("path" %: string))
     and includes_p4 = flag "-i" (listed string) ~doc:"P4 include paths"
     and excludes_p4 = flag "-e" (listed string) ~doc:"P4 test exclude paths"
     and testdirs_p4 = flag "-p4-dir" (listed string) ~doc:"P4 test directories"
     and testdirs_stf =
       flag "-stf-dir" (listed string) ~doc:"STF test directories"
     and patchdir =
       flag "-patch-dir" (listed string) ~doc:"directory for P4/STF patches"
     and path_cov = flag "-cov" (required string) ~doc:"output coverage file"
     and arch = flag "-arch" (required string) ~doc:"target architecture"
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
       let excludes_p4 = Util.Test.collect_excludes excludes_p4 in
       let paths_p4, paths_stf =
         Util.Test.collect_test_pairs arch testdirs_p4 testdirs_stf patchdir
         |> List.map (fun (path_p4, path_stf, _) -> (path_p4, path_stf))
         |> List.filter (fun (path_p4, _) ->
                not (List.exists (String.equal path_p4) excludes_p4))
         |> List.split
       in
       run_with_diagnostics
         ~action:(fun () ->
           match mode with
           | `Instr ->
               cover_sim_instr ~arch paths_spec includes_p4 paths_p4 paths_stf
                 path_cov
           | `Dangling ->
               cover_sim_dangling ~arch paths_spec includes_p4 paths_p4
                 paths_stf path_cov)
         ~on_success:ignore)

let run_testgen_command =
  Core.Command.basic
    ~summary:"generate negative type checker tests from a p4_16 spec"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map paths_spec = anon (non_empty_sequence_as_list ("path" %: string))
     and relname = flag "-rel" (required string) ~doc:"relation to run"
     and fuel = flag "-fuel" (required int) ~doc:"fuel for test generation"
     and includes_p4 = flag "-i" (listed string) ~doc:"P4 include paths"
     and excludes_p4 = flag "-e" (listed string) ~doc:"P4 test exclude paths"
     and gendir =
       flag "-gen-dir" (required string)
         ~doc:"directory for generated p4 programs"
     and name_campaign =
       flag "-name" (optional string)
         ~doc:"name of the test generation campaign"
     and silent = flag "-silent" no_arg ~doc:"do not print logs to stdout"
     and randseed =
       flag "-seed" (optional int) ~doc:"seed for random number generator"
     and bootdir =
       flag "-boot-dir" (optional string) ~doc:"seed p4 directory for boot"
     and path_boot =
       flag "-boot-file" (optional string) ~doc:"coverage file for boot"
     and random = flag "-random" no_arg ~doc:"randomize AST selection"
     and hybrid =
       flag "-hybrid" no_arg
         ~doc:"randomize AST selection when no derivations exist"
     and strict =
       flag "-strict" no_arg
         ~doc:"cover a new dangling only if it was intended by a mutation"
     in
     fun () ->
       run_with_diagnostics
         ~action:(fun () ->
           let* spec_sl = P4spectec.structure ~final:true paths_spec in
           let logmode =
             if silent then Backend_testgen_neg.Modes.Silent
             else Backend_testgen_neg.Modes.Verbose
           in
           let* bootmode =
             match (bootdir, path_boot) with
             | Some bootdir, None ->
                 Ok (Backend_testgen_neg.Modes.Cold (excludes_p4, bootdir))
             | None, Some path_boot ->
                 Ok (Backend_testgen_neg.Modes.Warm path_boot)
             | Some _, Some _ -> Error Error.error_boot_source_conflict
             | None, None -> Error Error.error_boot_source_required
           in
           let mutationmode =
             if random then Backend_testgen_neg.Modes.Random
             else if hybrid then Backend_testgen_neg.Modes.Hybrid
             else Backend_testgen_neg.Modes.Derive
           in
           let covermode =
             if strict then Backend_testgen_neg.Modes.Strict
             else Backend_testgen_neg.Modes.Relaxed
           in
           P4spectec.fuzzer fuel spec_sl relname includes_p4 gendir
             name_campaign randseed logmode bootmode mutationmode covermode)
         ~on_success:ignore)

let run_testgen_debug_command =
  Core.Command.basic
    ~summary:"debug close-AST deriver in negative type checker generator"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map paths_spec = anon (non_empty_sequence_as_list ("path" %: string))
     and relname = flag "-rel" (required string) ~doc:"spec relation to run"
     and includes_p4 = flag "-i" (listed string) ~doc:"P4 include paths"
     and path_p4 = flag "-p" (required string) ~doc:"P4 program"
     and debugdir =
       flag "-debug" (required string) ~doc:"directory for debug files"
     and iid = flag "-iid" (required int) ~doc:"dangling id to close-miss" in
     fun () ->
       run_with_diagnostics
         ~action:(fun () ->
           let* spec_sl = P4spectec.structure ~final:true paths_spec in
           P4spectec.debug_dangling spec_sl relname includes_p4 path_p4 debugdir
             iid)
         ~on_success:ignore)

let interesting_command =
  Core.Command.basic ~summary:"interestingness test for reducing P4 programs"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map paths_spec = anon (non_empty_sequence_as_list ("path" %: string))
     and relname = flag "-rel" (required string) ~doc:"relation to run"
     and includes_p4 = flag "-i" (listed string) ~doc:"P4 include paths"
     and check_well_typed =
       flag "-well" no_arg
         ~doc:"'interesting' if well-typed (default: ill-typed)"
     and check_close_miss =
       flag "-close" no_arg ~doc:"'interesting' if close-miss (default: hit)"
     and iid = flag "-iid" (required int) ~doc:"dangling id to test"
     and path_p4 = flag "-p" (required string) ~doc:"P4 program" in
     fun () ->
       run_with_diagnostics
         ~action:(fun () ->
           let* spec_sim = P4spectec.spec_of_mode SL_mode paths_spec in
           let* simulator = P4spectec.build_sim spec_sim in
           Ok (spec_sim, simulator))
         ~on_success:(fun (spec_sim, simulator) ->
           let (module Simulator : SIM) = simulator in
           let result, cover =
             run_with_dangling
               (module Simulator)
               spec_sim relname includes_p4 path_p4
           in
           match result with
           | Pass _ ->
               if check_well_typed then (
                 let branch = Coverage.Dangling.Single.Cover.find iid cover in
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
           | Fail (`Syntax _) ->
               Printf.printf "IllFormed";
               exit 12
           | Fail (`Runtime _) -> (
               if check_well_typed then (
                 Printf.printf "IllTyped\n";
                 exit 10)
               else
                 let branch = Coverage.Dangling.Single.Cover.find iid cover in
                 match branch.status with
                 | Hit ->
                     Printf.printf "IllTyped: Hit\n";
                     if check_close_miss then exit 3 else exit 0
                 | Miss (_ :: _) ->
                     Printf.printf "IllTyped: Close\n";
                     if check_close_miss then exit 0 else exit 2
                 | Miss [] ->
                     Printf.printf "IllTyped: Miss\n";
                     exit 1)))

let splice_command =
  Core.Command.basic ~summary:"splice a skeleton p4_16 specification document"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map paths_spec = anon (non_empty_sequence_as_list ("path" %: string))
     and paths_input = flag "-splice" (listed string) ~doc:"skeleton documents"
     and paths_output = flag "-out" (listed string) ~doc:"output files"
     and inplace = flag "-inplace" no_arg ~doc:"splice in place" in
     fun () ->
       run_with_diagnostics
         ~action:(fun () ->
           let* spec = P4spectec.parse paths_spec in
           let* spec_pl = P4spectec.annotate paths_spec in
           let* paths =
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
           Ok (spec, spec_pl, paths))
         ~on_success:(fun (spec, spec_pl, paths) ->
           try Backend_splice.Driver.splice_files spec spec_pl paths
           with Backend_splice.Error.SpliceError (at, msg) ->
             Format.eprintf "%s\n" (Util.Error.string_of_error at msg);
             Format.printf "%s\n" (Util.Error.string_of_error at msg)))

let parse_command =
  Core.Command.basic ~summary:"parse a P4 program"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map paths_spec = anon (non_empty_sequence_as_list ("path" %: string))
     and includes_p4 = flag "-i" (listed string) ~doc:"P4 include paths"
     and path_p4 = flag "-p" (required string) ~doc:"P4 program"
     and roundtrip =
       flag "-r" no_arg ~doc:"perform a round-trip parse/unparse"
     in
     fun () ->
       run_with_diagnostics
         ~action:(fun () ->
           let* spec_sim = P4spectec.spec_of_mode AL_mode paths_spec in
           P4spectec.build_sim spec_sim)
         ~on_success:(fun simulator ->
           let (module Simulator : SIM) = simulator in
           try
             match
               Simulator.Interface.parse_program includes_p4 [ path_p4 ]
             with
             | Fail diagnostic ->
                 diagnostic |> Diagnostic.Report.singleton |> render_diagnostics
             | Pass value_program ->
                 let str_program =
                   Simulator.Interface.unparse_program value_program
                 in
                 if roundtrip then
                   match
                     Simulator.Interface.parse_string path_p4 str_program
                   with
                   | Fail diagnostic ->
                       diagnostic |> Diagnostic.Report.singleton
                       |> render_diagnostics
                   | Pass value_program_roundtrip ->
                       Il.Eq.eq_value ~dbg:true value_program
                         value_program_roundtrip
                       |> (fun b ->
                            if b then "Roundtrip successful"
                            else "Roundtrip failed")
                       |> print_endline
                 else str_program |> print_endline
           with
           | Sys_error msg -> Format.printf "File error: %s\n" msg
           | e -> Format.printf "Unknown error: %s\n" (Printexc.to_string e)))

let command =
  Core.Command.group
    ~summary:"p4spectec: a language design framework for the p4_16 language"
    [
      (* Transformations *)
      ("elab", elab_command);
      ("algo", algo_command);
      ("struct", struct_command);
      ("prose", prose_command);
      (* Execution *)
      ("run", run_command);
      ("sim", sim_command);
      (* Coverage *)
      ("cover-run", cover_run_command);
      ("cover-sim", cover_sim_command);
      (* Negative type checker test generation and coverage *)
      ("testgen", run_testgen_command);
      ("testgen-dbg", run_testgen_debug_command);
      ("interesting", interesting_command);
      (* Splicing *)
      ("splice", splice_command);
      (* Interfacing with P4 *)
      ("parse", parse_command);
    ]

let () = Command_unix.run ~version command
