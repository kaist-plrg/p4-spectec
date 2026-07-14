open Runtime.Dynamic_Runner.Signature
open Backend_boot.Config
open Util.Error

let version = "0.1"

exception CommandError of string

(* Commands *)

let run_command =
  Core.Command.basic
    ~summary:"execute a compiled spec-meta interpreter (no tower)"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map relname =
       flag "-rel" (optional string) ~doc:"relation to run (with -tec)"
     and path_spectec =
       flag "-tec" (optional string) ~doc:"SpecTec program (with -rel)"
     and funcname =
       flag "-func" (optional string) ~doc:"spec function to call directly"
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
     and interface =
       Command.Param.choose_one
         [
           flag "il" no_arg ~doc:"compiled IL interface (spec-meta/il)"
           |> map ~f:(fun b -> Core.Option.some_if b IL_interface);
           flag "sl" no_arg ~doc:"compiled SL interface (spec-meta/sl)"
           |> map ~f:(fun b -> Core.Option.some_if b SL_interface);
         ]
         ~if_nothing_chosen:(Default_to SL_interface)
     in
     fun () ->
       try
         let cache = not no_cache in
         let spec, (module Runner) =
           Backend_boot.Build.build_null ~cache ~det ~guard ML_mode interface []
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
         Inst.Hook.init_spec spec;
         let output =
           match (funcname, relname, path_spectec) with
           | Some fname, None, None -> (
               match Runner.Interp.eval_func fname [] [] with
               | Pass value ->
                   Format.sprintf "result: %s" (Runtime.Value.to_string value)
               | Fail (_, msg) -> Format.sprintf "runtime error: %s" msg)
           | None, Some rname, Some path -> (
               match Runner.Interp.eval_program rname [] path with
               | Pass _ -> "passed"
               | Fail (`Syntax (_, msg)) ->
                   Format.sprintf "syntax error: %s" msg
               | Fail (`Runtime (_, msg)) ->
                   Format.sprintf "runtime error: %s" msg)
           | _ ->
               raise
                 (CommandError
                    "provide either -func NAME or both -rel NAME -tec PATH")
         in
         Inst.Hook.finish ();
         Format.printf "%s\n" output
       with
       | CommandError msg -> Format.printf "%s\n" msg
       | ParseError (at, msg) -> Format.printf "%s\n" (string_of_error at msg)
       | ElabError (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

let boot_n_command =
  Core.Command.basic ~summary:"run a fully-compiled meta-circular tower"
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
       try
         let target = { includes = includes_target; path = path_target } in
         let tower =
           try Backend_boot.Config.tower_of_file path_tower target
           with Failure msg -> raise (CommandError msg)
         in
         let spec, _, _, (module Booter) =
           Backend_boot.Build.build_tower ~cache:(not no_cache) ~det ~guard
             tower
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
         Inst.Hook.init_spec spec;
         let rel_boot = tower.level_boot.layer.rel in
         let value = Backend_boot.Patch.apply_tower tower in
         let value = Booter.Interp.unmarshal_program value in
         let result = Booter.Interp.eval_rel rel_boot [ value ] in
         Inst.Hook.finish ();
         match result with
         | Pass _ -> Format.printf "passed\n"
         | Fail (_, msg) -> Format.printf "runtime error: %s\n" msg
       with
       | CommandError msg -> Format.eprintf "error: %s\n" msg
       | ParseError (at, msg) -> Format.printf "%s\n" (string_of_error at msg)
       | ElabError (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

(* Command-line interface *)

let command_core =
  Core.Command.group
    ~summary:
      "spectec-boot-comp: a fully-compiled meta-circular interpreter, running \
       spec-meta/il and spec-meta/sl as native OCaml"
    [ ("run", run_command); ("boot-n", boot_n_command) ]

let () = Command_unix.run ~version command_core
