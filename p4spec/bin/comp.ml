open Runtime.Dynamic_Runner.Signature
open Backend_boot.Config
open Util.Error
open Backend_boot.Error

exception CommandError of string

let run_command =
  Core.Command.basic
    ~summary:"execute the spec using the compiled ML interpreter"
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
     and interface =
       Command.Param.choose_one
         [
           flag "ili" no_arg ~doc:"IL interface"
           |> map ~f:(fun b -> Core.Option.some_if b IL_interface);
           flag "sli" no_arg ~doc:"SL interface"
           |> map ~f:(fun b -> Core.Option.some_if b SL_interface);
         ]
         ~if_nothing_chosen:(Default_to SL_interface)
     in
     fun () ->
       try
         let cache = not no_cache in
         let (module Interface_SpecTec) =
           match interface with
           | P4_interface -> error_no_region "P4 interface not supported"
           | IL_interface ->
               (module Interface.SpecTec_IL
               : Backend_boot.Spectec.INTERFACE_SPECTEC)
           | SL_interface ->
               (module Interface.SpecTec_SL
               : Backend_boot.Spectec.INTERFACE_SPECTEC)
         in
         let (module Runner) =
           (module Runner.Make.Make_rec
                     (Interface_SpecTec)
                     (Backend_boot.Spectec.Make_null (Interface_SpecTec))
                     (Interp_il.Interp.Make)
                     (Interp_sl.Interp.Make)
                     (Backend_ocaml.Spec_compiled.Make) : RUNNER)
         in
         Runner.init ~cache ~det ~guard (ML : spec);
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
         Format.printf "%s\n" output
       with
       | CommandError msg -> Format.printf "%s\n" msg
       | ParseError (at, msg) -> Format.printf "%s\n" (string_of_error at msg)
       | ElabError (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

let command_core =
  Core.Command.group ~summary:"p4spectec-comp: ML-compiled spec runner"
    [ ("run", run_command) ]

let () = Command_unix.run command_core
