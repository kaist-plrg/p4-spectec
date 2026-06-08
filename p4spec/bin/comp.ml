open Runtime.Dynamic_Runner.Signature
open Util.Error

let run_command =
  Core.Command.basic
    ~summary:"execute the spec using the compiled ML interpreter"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map relname =
       flag "-rel" (required string) ~doc:"relation to run (with -p4)"
     and path_p4 = flag "-p4" (required string) ~doc:"P4 program"
     and includes_p4 = flag "-i" (listed string) ~doc:"P4 include paths" in
     fun () ->
       try
         let _spec_sim, (module Simulator) =
           Backend_sim.Build.build ~cache:false ~det:false ~guard:false
             ~final:true ML_mode []
         in
         let result =
           Simulator.Interp.eval_program relname includes_p4 path_p4
         in
         match result with
         | Pass _ -> Format.printf "passed\n"
         | Fail (`Syntax (_, msg)) -> Format.printf "syntax error: %s\n" msg
         | Fail (`Runtime (_, msg)) -> Format.printf "runtime error: %s\n" msg
       with
       | ParseError (at, msg) -> Format.printf "%s\n" (string_of_error at msg)
       | ElabError (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

let command_core =
  Core.Command.group ~summary:"p4spectec-comp: ML-compiled spec runner"
    [ ("run", run_command) ]

let () = Command_unix.run command_core
