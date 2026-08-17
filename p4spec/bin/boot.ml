open Lang
open Runtime.Dynamic_Runner.Signature
module Error = P4spectec.Error
open Backend_boot.Config

let ( let* ) = Result.bind
let version = "0.1"

(* Tune GC for the allocation-heavy meta-circular interpreter *)

let () =
  Gc.set
    {
      (Gc.get ()) with
      Gc.minor_heap_size = 16 * 1024 * 1024;
      Gc.space_overhead = 2000;
    }

(* Commands *)

let elab_command =
  Core.Command.basic ~summary:"parse and elaborate a spec"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map paths_spec =
       anon (non_empty_sequence_as_list ("path" %: string))
     in
     fun () ->
       match P4spectec.elab paths_spec with
       | Ok spec_il -> Format.printf "%s\n" (Il.Print.string_of_spec spec_il)
       | Error e -> Format.printf "%s\n" (Error.to_string e))

let algo_command =
  Core.Command.basic ~summary:"check algorithmic property of a spec"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map paths_spec =
       anon (non_empty_sequence_as_list ("path" %: string))
     in
     fun () ->
       match P4spectec.algo paths_spec with
       | Ok spec_al -> Format.printf "%s\n" (Al.Print.string_of_spec spec_al)
       | Error e -> Format.printf "%s\n" (Error.to_string e))

let struct_command =
  Core.Command.basic ~summary:"insert structured control flow to a spec"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map paths_spec =
       anon (non_empty_sequence_as_list ("path" %: string))
     in
     fun () ->
       match P4spectec.structure ~final:true paths_spec with
       | Ok spec_sl -> Format.printf "%s\n" (Sl.Print.string_of_spec spec_sl)
       | Error e -> Format.printf "%s\n" (Error.to_string e))

let prose_command =
  Core.Command.basic ~summary:"generate AsciiDoc prose from a spec"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map paths_spec =
       anon (non_empty_sequence_as_list ("path" %: string))
     in
     fun () ->
       match P4spectec.annotate paths_spec with
       | Ok spec_pl ->
           Format.printf "%s\n" (Backend_adoc.Pl.render_spec spec_pl)
       | Error e -> Format.printf "%s\n" (Error.to_string e))

let run_command =
  Core.Command.basic ~summary:"execute the spec"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map paths_spec = anon (non_empty_sequence_as_list ("path" %: string))
     and relname = flag "-rel" (required string) ~doc:"relation to run"
     and path_spectec = flag "-tec" (required string) ~doc:"SpecTec program"
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
     and interface =
       Command.Param.choose_one
         [
           flag "ali" no_arg ~doc:"AL interface"
           |> map ~f:(fun b -> Core.Option.some_if b AL_interface);
           flag "sli" no_arg ~doc:"SL interface"
           |> map ~f:(fun b -> Core.Option.some_if b SL_interface);
         ]
         ~if_nothing_chosen:(Default_to SL_interface)
     in
     fun () ->
       let cache = not no_cache in
       match
         let* spec = P4spectec.spec_of_mode mode paths_spec in
         let* runner = P4spectec.build_null ~cache ~det ~guard interface spec in
         Ok (spec, runner)
       with
       | Error e -> Format.printf "%s\n" (Error.to_string e)
       | Ok (spec, (module Runner : RUNNER)) -> (
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
           Inst.Hook.init_spec spec;
           let result = Runner.Interp.eval_program relname [] path_spectec in
           Inst.Hook.finish ();
           match result with
           | Pass _ -> Format.printf "passed\n"
           | Fail (`Syntax (_, msg)) -> Format.printf "syntax error: %s\n" msg
           | Fail (`Runtime (_, msg)) -> Format.printf "runtime error: %s\n" msg
           ))

let boot_n_command =
  Core.Command.basic ~summary:"run meta-circular interpreter"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map path_tower =
       flag "-tower" (required string) ~doc:"FILE tower config JSON file"
     and path_target = flag "-p" (required string) ~doc:"FILE P4 program"
     and includes_target = flag "-i" (listed string) ~doc:"DIR include path"
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
       let target = { includes = includes_target; path = path_target } in
       match
         let* tower = P4spectec.tower_of_file path_tower target in
         let* spec_boot, booter =
           P4spectec.build_tower ~cache:(not no_cache) ~det ~guard tower
         in
         Ok (tower, spec_boot, booter)
       with
       | Error e -> Format.printf "%s\n" (Error.to_string e)
       | Ok (tower, spec, (module Booter : RUNNER)) -> (
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
           Inst.Hook.init_spec spec;
           let rel_boot = tower.level_boot.layer.rel in
           let value = Backend_boot.Patch.apply_tower tower in
           let result = Booter.Interp.eval_rel rel_boot [ value ] in
           Inst.Hook.finish ();
           match result with
           | Pass _ -> Format.printf "passed\n"
           | Fail (_, msg) -> Format.printf "runtime error: %s\n" msg))

let parse_command =
  Core.Command.basic ~summary:"parse a SpecTec program"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map paths_spec = anon (non_empty_sequence_as_list ("path" %: string))
     and path_spectec = flag "-tec" (required string) ~doc:"SpecTec program"
     and roundtrip = flag "-r" no_arg ~doc:"perform a round-trip parse/unparse"
     and interface =
       Command.Param.choose_one
         [
           flag "al" no_arg ~doc:"AL interface"
           |> map ~f:(fun b -> Core.Option.some_if b AL_interface);
           flag "sl" no_arg ~doc:"SL interface"
           |> map ~f:(fun b -> Core.Option.some_if b SL_interface);
         ]
         ~if_nothing_chosen:(Default_to SL_interface)
     in
     fun () ->
       match
         let* spec = P4spectec.spec_of_mode SL_mode paths_spec in
         P4spectec.build_null interface spec
       with
       | Error e -> Format.printf "%s\n" (Error.to_string e)
       | Ok (module Runner) -> (
           try
             match Runner.Interface.parse_program [] [ path_spectec ] with
             | Fail (`Syntax (at, msg)) ->
                 Format.printf "Parse error: %s\n"
                   (Util.Error.string_of_error at msg)
             | Pass value_program ->
                 let str_program =
                   Runner.Interface.unparse_program value_program
                 in
                 if roundtrip then
                   match
                     Runner.Interface.parse_string path_spectec str_program
                   with
                   | Fail (`Syntax (at, msg)) ->
                       Format.printf "Parse error: %s\n"
                         (Util.Error.string_of_error at msg)
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

(* Command-line interface *)

let command_core =
  Core.Command.group
    ~summary:
      "spectec-boot: a language design framework for the p4_16 language, with \
       meta-circular interpretation"
    [
      (* Transformations *)
      ("elab", elab_command);
      ("algo", algo_command);
      ("struct", struct_command);
      ("prose", prose_command);
      (* Execution *)
      ("run", run_command);
      ("boot-n", boot_n_command);
      (* Interfacing with IL specification *)
      ("parse", parse_command);
    ]

let () = Command_unix.run ~version command_core
