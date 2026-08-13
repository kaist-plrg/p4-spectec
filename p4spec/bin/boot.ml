open Lang
open Runtime.Dynamic_Runner.Signature
open Backend_boot.Config
open Util.Error

let version = "0.1"

(* Tune GC for the allocation-heavy meta-circular interpreter *)

let () =
  Gc.set
    {
      (Gc.get ()) with
      Gc.minor_heap_size = 16 * 1024 * 1024;
      Gc.space_overhead = 2000;
    }

exception CommandError of string

(* Commands *)

let elab_command =
  Core.Command.basic ~summary:"parse and elaborate a spec"
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
       | ParseError (at, msg) -> Format.printf "%s\n" (string_of_error at msg)
       | ElabError (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

let algo_command =
  Core.Command.basic ~summary:"check algorithmic property of a spec"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map paths_spec =
       anon (non_empty_sequence_as_list ("path" %: string))
     in
     fun () ->
       try
         let spec_al = Pass.algo paths_spec in
         Format.printf "%s\n" (Al.Print.string_of_spec spec_al);
         ()
       with
       | CommandError msg -> Format.printf "%s\n" msg
       | ParseError (at, msg) -> Format.printf "%s\n" (string_of_error at msg)
       | ElabError (at, msg) -> Format.printf "%s\n" (string_of_error at msg)
       | AlgoError (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

let struct_command =
  Core.Command.basic ~summary:"insert structured control flow to a spec"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map paths_spec =
       anon (non_empty_sequence_as_list ("path" %: string))
     in
     fun () ->
       try
         let spec_sl = Pass.structure ~final:true paths_spec in
         Format.printf "%s\n" (Sl.Print.string_of_spec spec_sl);
         ()
       with
       | ParseError (at, msg) -> Format.printf "%s\n" (string_of_error at msg)
       | ElabError (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

let prose_command =
  Core.Command.basic ~summary:"generate AsciiDoc prose from a spec"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map paths_spec =
       anon (non_empty_sequence_as_list ("path" %: string))
     in
     fun () ->
       try
         let spec_pl = Pass.annotate paths_spec in
         Format.printf "%s\n" (Pl.Render.render_spec spec_pl);
         ()
       with
       | ParseError (at, msg) | ElabError (at, msg) | ProseError (at, msg) ->
         Format.printf "%s\n" (string_of_error at msg))

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
       try
         let cache = not no_cache in
         let spec, (module Runner) =
           Backend_boot.Build.build_null ~cache ~det ~guard mode interface
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
         Inst.Hook.init_spec spec;
         let result = Runner.Interp.eval_program relname [] path_spectec in
         Inst.Hook.finish ();
         match result with
         | Pass _ -> Format.printf "passed\n"
         | Fail (`Syntax (_, msg)) -> Format.printf "syntax error: %s\n" msg
         | Fail (`Runtime (_, msg)) -> Format.printf "runtime error: %s\n" msg
       with
       | CommandError msg -> Format.printf "%s\n" msg
       | ParseError (at, msg) -> Format.printf "%s\n" (string_of_error at msg)
       | ElabError (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

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
         let result = Booter.Interp.eval_rel rel_boot [ value ] in
         Inst.Hook.finish ();
         match result with
         | Pass _ -> Format.printf "passed\n"
         | Fail (_, msg) -> Format.printf "runtime error: %s\n" msg
       with
       | CommandError msg -> Format.eprintf "error: %s\n" msg
       | ParseError (at, msg) -> Format.printf "%s\n" (string_of_error at msg)
       | ElabError (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

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
       try
         let _, (module Runner) =
           Backend_boot.Build.build_null SL_mode interface paths_spec
         in
         let value_program =
           match Runner.Interface.parse_program [] [ path_spectec ] with
           | Pass value_program -> value_program
           | Fail (`Syntax (at, msg)) -> raise (ParseError (at, msg))
         in
         let str_program = Runner.Interface.unparse_program value_program in
         if roundtrip then
           let value_program_roundtrip =
             match Runner.Interface.parse_string path_spectec str_program with
             | Pass value_program_roundtrip -> value_program_roundtrip
             | Fail (`Syntax (at, msg)) -> raise (ParseError (at, msg))
           in
           Il.Eq.eq_value ~dbg:true value_program value_program_roundtrip
           |> (fun b ->
                if b then "Roundtrip successful" else "Roundtrip failed")
           |> print_endline
         else str_program |> print_endline
       with
       | Sys_error msg -> Format.printf "File error: %s\n" msg
       | ElabError (at, msg) ->
           Format.printf "Elaboration error: %s\n" (string_of_error at msg)
       | ParseError (at, msg) ->
           Format.printf "Parse error: %s\n" (string_of_error at msg)
       | e -> Format.printf "Unknown error: %s\n" (Printexc.to_string e))

let kast_command =
  Core.Command.basic ~summary:"boot a SpecTec program and print it as KAST JSON"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map path_spectec = anon ("path" %: string)
     and path_out =
       flag "-o" (optional string) ~doc:"FILE write to FILE instead of stdout"
     in
     fun () ->
       try
         let spec_al = Pass.algo [ path_spectec ] in
         let kast = Interface.SpecTec_AL.kast_of_spec_al spec_al in
         match path_out with
         | Some path_out ->
             let oc = Out_channel.open_text path_out in
             Fun.protect
               ~finally:(fun () -> Out_channel.close oc)
               (fun () -> Out_channel.output_string oc (kast ^ "\n"))
         | None -> print_endline kast
       with
       | Sys_error msg ->
           Format.eprintf "File error: %s\n" msg;
           exit 1
       | Interface.SpecTec_AL.Kast_error msg ->
           Format.eprintf "KAST error: %s\n" msg;
           exit 1
       | ParseError (at, msg) ->
           Format.eprintf "Parse error: %s\n" (string_of_error at msg);
           exit 1
       | ElabError (at, msg) ->
           Format.eprintf "Elaboration error: %s\n" (string_of_error at msg);
           exit 1
       | AlgoError (at, msg) ->
           Format.eprintf "Algo error: %s\n" (string_of_error at msg);
           exit 1)

let kast_p4_command =
  Core.Command.basic
    ~summary:"parse a P4 program and print it as a KAST JSON term of sort Val"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map path_p4 = flag "-p" (required string) ~doc:"FILE P4 program"
     and includes_p4 = flag "-i" (listed string) ~doc:"DIR P4 include paths"
     and path_out =
       flag "-o" (optional string) ~doc:"FILE write to FILE instead of stdout"
     in
     fun () ->
       try
         (* The P4 program is parsed by the ordinary P4 front end, whose menhir
            grammar builds a `Value.t` directly — there is no separate P4 AST —
            so this is the very value `Program_ok` is applied to.

            It is emitted as K's structural `Val`, not through the script
            emitter: a `p4program` inhabits sorts the meta-language constructor
            table knows nothing about.  The spec is not loaded here; the program
            is independent of it, and reaches K as a separate configuration
            variable. *)
         let value_program =
           match Interface.P4.parse_program includes_p4 [ path_p4 ] with
           | Pass value_program -> value_program
           | Fail (`Syntax (at, msg)) -> raise (ParseError (at, msg))
         in
         let kast = Interface.SpecTec_AL.kast_of_value value_program in
         match path_out with
         | Some path_out ->
             let oc = Out_channel.open_text path_out in
             Fun.protect
               ~finally:(fun () -> Out_channel.close oc)
               (fun () -> Out_channel.output_string oc (kast ^ "\n"))
         | None -> print_endline kast
       with
       | Sys_error msg ->
           Format.eprintf "File error: %s\n" msg;
           exit 1
       | Interface.SpecTec_AL.Kast_error msg ->
           Format.eprintf "KAST error: %s\n" msg;
           exit 1
       | ParseError (at, msg) ->
           Format.eprintf "Parse error: %s\n" (string_of_error at msg);
           exit 1)

let builtin_command =
  Core.Command.basic
    ~summary:
      "evaluate a single builtin call given as JSON, and print the result"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map path_in =
       flag "-i" (required string) ~doc:"FILE read the request from FILE"
     and path_out =
       flag "-o" (optional string) ~doc:"FILE write to FILE instead of stdout"
     and paths_spec =
       flag "-spec" (listed string)
         ~doc:"PATH spec the P4 builtins resolve against (for $print_)"
     in
     fun () ->
       try
         let json_request = Yojson.Safe.from_file path_in in
         let name, targs, args =
           match Interface.SpecTec_AL.request_of_json json_request with
           | Builtin (name, targs, args) -> (name, targs, args)
           | ExternFunc _ | ExternRel _ ->
               raise
                 (CommandError
                    "use the `extern` subcommand for extern func/rel calls")
         in
         (* `$print_<X>` unparses a value back to P4 source, so unlike every
            other builtin it needs the spec: the unparser is driven by the
            spec's hints.  It is registered only on the P4 interface
            (`Interface.P4.Builtin_P4_Ext`), so a request naming it takes that
            registry instead of the SpecTec one.

            The spec is loaded only for such a request, never merely because
            `-spec` was passed.  Loading it costs ~0.85 s against ~13 ms for the
            whole rest of a call, and a run makes hundreds of calls of which
            only a few are `$print_`. *)
         let call_builtin =
           match (name, paths_spec) with
           | "print_", [] ->
               raise
                 (CommandError
                    "$print_ needs the spec: pass -spec PATH (it unparses a \
                     value back to P4 source)")
           | "print_", paths_spec ->
               let spec_sl = Pass.structure ~final:true paths_spec in
               Interface.P4.init (SL spec_sl);
               Interface.P4.call_builtin
           | _ -> Interface.SpecTec_AL.call_builtin
         in
         let value =
           call_builtin (fun _ -> ()) Util.Source.(name $ no_region) targs args
         in
         let response =
           Interface.SpecTec_AL.json_of_response value |> Yojson.Safe.to_string
         in
         match path_out with
         | Some path_out ->
             let oc = Out_channel.open_text path_out in
             Fun.protect
               ~finally:(fun () -> Out_channel.close oc)
               (fun () -> Out_channel.output_string oc (response ^ "\n"))
         | None -> print_endline response
       with
       (* Errors go to stderr and exit non-zero, and stdout stays empty.  K
          reads stdout as the response and has no other channel to inspect,
          so its only way to tell "OCaml said no" from "the wire broke" is
          the exit code from `#systemResult`. *)
       | CommandError msg ->
           Format.eprintf "error: %s\n" msg;
           exit 1
       | Sys_error msg ->
           Format.eprintf "File error: %s\n" msg;
           exit 1
       | Interface.SpecTec_AL.Extern_error msg ->
           Format.eprintf "Extern error: %s\n" msg;
           exit 1
       | Yojson.Json_error msg ->
           Format.eprintf "JSON error: %s\n" msg;
           exit 1
       | Util.Error.BuiltinError (at, msg) | Util.Error.InterpError (at, msg) ->
           Format.eprintf "Builtin error: %s\n" (string_of_error at msg);
           exit 1
       | e ->
           Format.eprintf "Unknown error: %s\n" (Printexc.to_string e);
           exit 1)

let extern_command =
  Core.Command.basic
    ~summary:
      "evaluate a single extern func/rel call given as JSON, against a lower \
       spec, and print the result"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map paths_lower =
       flag "-lower"
         (one_or_more_as_list string)
         ~doc:"PATH the lower spec the extern names resolve against"
     and path_in =
       flag "-i" (required string) ~doc:"FILE read the request from FILE"
     and path_out =
       flag "-o" (optional string) ~doc:"FILE write to FILE instead of stdout"
     and no_cache = flag "-no-cache" no_arg ~doc:"disable caching"
     and mode =
       Command.Param.choose_one
         [
           flag "al" no_arg ~doc:"run AL interpreter"
           |> map ~f:(fun b -> Core.Option.some_if b AL_mode);
           flag "sl" no_arg ~doc:"run SL interpreter"
           |> map ~f:(fun b -> Core.Option.some_if b SL_mode);
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
       try
         (* Unlike a builtin — a static registry lookup needing no state — an
            extern call needs a *loaded lower spec*: `Make_parametric` routes
            `Call_extern_func`/`Call_extern_rel` into a lower runner's
            `eval_func`/`eval_rel` (`backend-boot/spectec.ml:173-215`), and
            `build_null` produces exactly such a runner.  This is why the
            subcommand is separate from `builtin`, and why it is slow: the
            lower spec is parsed and elaborated on every call.  See CROSS.md. *)
         let _spec, (module Runner) =
           Backend_boot.Build.build_null ~cache:(not no_cache) mode interface
             paths_lower
         in
         (* The request is a file, never argv, for the same reason as
            `builtin_command`: mixop operator atoms render with quotes. *)
         let json_request = Yojson.Safe.from_file path_in in
         let response =
           match Interface.SpecTec_AL.request_of_json json_request with
           | ExternFunc (name, targs, args) -> (
               match Runner.Interp.eval_func name targs args with
               | Pass value -> Interface.SpecTec_AL.json_of_response value
               | Fail (at, msg) ->
                   (* A spec-level failure, not a wire error: the message has
                      nowhere to go on the wire, so it goes to stderr while
                      stdout carries a clean `{"fail":null}` and the exit
                      status stays 0. *)
                   Format.eprintf "extern func %s failed: %s\n" name
                     (string_of_error at msg);
                   Interface.SpecTec_AL.json_of_response_fail ())
           | ExternRel (name, args) -> (
               match Runner.Interp.eval_rel name args with
               | Pass values ->
                   Interface.SpecTec_AL.json_of_response_multi values
               | Fail (at, msg) ->
                   Format.eprintf "extern rel %s failed: %s\n" name
                     (string_of_error at msg);
                   Interface.SpecTec_AL.json_of_response_fail ())
           | Builtin _ ->
               raise
                 (CommandError "use the `builtin` subcommand for builtin calls")
         in
         let response = Yojson.Safe.to_string response in
         match path_out with
         | Some path_out ->
             let oc = Out_channel.open_text path_out in
             Fun.protect
               ~finally:(fun () -> Out_channel.close oc)
               (fun () -> Out_channel.output_string oc (response ^ "\n"))
         | None -> print_endline response
       with
       (* As in `builtin_command`: errors go to stderr and exit non-zero with
          stdout empty, so K's `#systemResult` exit code distinguishes "the
          wire broke" from a `{"fail":null}` the spec can recover from. *)
       | CommandError msg ->
           Format.eprintf "error: %s\n" msg;
           exit 1
       | Sys_error msg ->
           Format.eprintf "File error: %s\n" msg;
           exit 1
       | Interface.SpecTec_AL.Extern_error msg ->
           Format.eprintf "Extern error: %s\n" msg;
           exit 1
       | Yojson.Json_error msg ->
           Format.eprintf "JSON error: %s\n" msg;
           exit 1
       | ParseError (at, msg) ->
           Format.eprintf "Parse error: %s\n" (string_of_error at msg);
           exit 1
       | ElabError (at, msg) ->
           Format.eprintf "Elaboration error: %s\n" (string_of_error at msg);
           exit 1
       | e ->
           Format.eprintf "Unknown error: %s\n" (Printexc.to_string e);
           exit 1)

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
      ("kast", kast_command);
      ("kast-p4", kast_p4_command);
      ("builtin", builtin_command);
      ("extern", extern_command);
    ]

let () = Command_unix.run ~version command_core
