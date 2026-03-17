open Pass
open Util.Error
open Util.Source

let version = "0.1"

let exit_with_message ?(errorcode = 1) msg =
  Format.printf "%s\n" msg;
  exit errorcode

(* Operations *)

let frontend filenames_spec =
  filenames_spec |> List.concat_map Frontend.Parse.parse_file

let elab filenames_spec = filenames_spec |> frontend |> Elaborate.Elab.elab_spec

let structure filenames_spec =
  filenames_spec |> elab |> Structure.Struct.struct_spec

let runner ?(cache = true) ?(det = false) ?(arch : string option) mode
    filenames_spec =
  let spec_sim =
    match mode with
    | `IL ->
        let spec_il = elab filenames_spec in
        (Runtime.Sim.Simulator.IL spec_il : Runtime.Sim.Simulator.spec)
    | `SL ->
        let spec_sl = structure filenames_spec in
        (Runtime.Sim.Simulator.SL spec_sl : Runtime.Sim.Simulator.spec)
  in
  let (module Driver) =
    match arch with
    | Some arch -> Backend_sim.Gen.gen arch
    | None -> Backend_sim.Gen.gen_placeholder ()
  in
  Driver.init ~cache ~det spec_sim;
  (spec_sim, (module Driver : Runtime.Sim.Simulator.DRIVER))

let run_command =
  Core.Command.basic ~summary:"execute the P4 spec against a P4 program"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map filenames_spec =
       anon (non_empty_sequence_as_list ("filename" %: string))
     and relname = flag "-rel" (required string) ~doc:"relation to run"
     and includes_p4 = flag "-i" (listed string) ~doc:"P4 include paths"
     and filename_p4 = flag "-p" (required string) ~doc:"P4 program"
     and no_cache = flag "-no-cache" no_arg ~doc:"disable caching"
     and det = flag "-det" no_arg ~doc:"deterministic mode"
     and profile = flag "-profile" no_arg ~doc:"profiling"
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
         let cache = not no_cache in
         let spec_sim, (module Driver) =
           runner ~cache ~det mode filenames_spec
         in
         let handlers =
           if profile then
             let (module PH : Inst.Handler.HANDLER) = Inst.Profile.make () in
             [ (module PH : Inst.Handler.HANDLER) ]
           else []
         in
         Inst.Hook.register handlers;
         Inst.Hook.init_spec spec_sim;
         let result = Driver.run_program relname includes_p4 filename_p4 in
         Inst.Hook.finish ();
         match result with
         | Pass _ -> "passed\n" |> exit_with_message ~errorcode:42
         | Fail (`Syntax (_, msg)) ->
             "sytax error: " ^ msg |> exit_with_message ~errorcode:6
         | Fail (`Runtime (_, msg)) ->
             "runtime error: " ^ msg |> exit_with_message ~errorcode:6
       with
       | ParseError (at, msg) ->
           string_of_error at msg |> exit_with_message ~errorcode:1
       | ElabError (at, msg) ->
           string_of_error at msg |> exit_with_message ~errorcode:1)

let sim_command =
  Core.Command.basic
    ~summary:"simulate a target architecture with a P4 program and P4 spec"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map filenames_spec =
       anon (non_empty_sequence_as_list ("filename" %: string))
     and includes_p4 = flag "-i" (listed string) ~doc:"P4 include paths"
     and filename_p4 = flag "-p" (required string) ~doc:"P4 program"
     and filename_stf = flag "-stf" (required string) ~doc:"stf test file"
     and arch = flag "-arch" (required string) ~doc:"target architecture"
     and no_cache = flag "-no-cache" no_arg ~doc:"disable caching"
     and det = flag "-det" no_arg ~doc:"deterministic mode"
     and profile = flag "-profile" no_arg ~doc:"profiling"
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
         let cache = not no_cache in
         let spec_sim, (module Driver) =
           runner ~cache ~det ~arch mode filenames_spec
         in
         let handlers =
           if profile then
             let (module PH : Inst.Handler.HANDLER) = Inst.Profile.make () in
             [ (module PH : Inst.Handler.HANDLER) ]
           else []
         in
         Inst.Hook.register handlers;
         Inst.Hook.init_spec spec_sim;
         let result =
           Driver.run_stf_test includes_p4 filename_p4 filename_stf
         in
         Inst.Hook.finish ();
         match result with
         | Pass -> exit_with_message "passed" ~errorcode:42
         | Fail (`Syntax (_, msg)) ->
             "sytax error: " ^ msg |> exit_with_message ~errorcode:6
         | Fail (`Runtime (_, msg)) ->
             "runtime error: " ^ msg |> exit_with_message ~errorcode:6
       with
       | ParseError (at, msg) | ElabError (at, msg) | ArchError (at, msg) ->
           string_of_error at msg |> exit_with_message ~errorcode:1
       | StfError msg ->
           string_of_error no_region msg |> exit_with_message ~errorcode:1)

let command =
  Core.Command.group
    ~summary:"p4spectec: a language design framework for the p4_16 language"
    [ (* Execution *) ("run", run_command); ("sim", sim_command) ]

let () = Command_unix.run ~version command
