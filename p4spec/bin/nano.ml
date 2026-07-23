open Lang
open Runtime.Sim.Signature
open Util.Error

exception CommandError of string

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
           Backend_sim.Build.build_nano ~cache ~det ~guard ~final:true mode
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

let command =
  Core.Command.group
    ~summary:"nano-p4spectec: a language design framework for nano-P4"
    [ ("run", run_command); ("parse", parse_command) ]

let () = Command_unix.run command
