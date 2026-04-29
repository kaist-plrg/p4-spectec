open Lang
open Pass
open Runtime.Dynamic_Runner.Signature
open Util.Error

let version = "0.1"

(* Operations *)

let expand_spec filenames =
  List.concat_map
    (fun filename ->
      if Sys_unix.is_directory_exn filename then
        Util.Filesys.collect_files ~suffix:".watsup" filename
      else [ filename ])
    filenames

let frontend filenames_spec =
  filenames_spec |> expand_spec |> List.concat_map Frontend.Parse.parse_file

let elab filenames_spec = filenames_spec |> frontend |> Elaborate.Elab.elab_spec

let structure filenames_spec =
  filenames_spec |> elab |> Structure.Struct.struct_spec

let prosify filenames_spec =
  filenames_spec |> structure |> Prose.Prosify.prosify_spec

let booter ?(cache = true) ?(det = false) ?(guard = false) mode filenames_spec =
  let spec_sim =
    match mode with
    | `IL ->
        let spec_il = elab filenames_spec in
        (IL spec_il : spec)
    | `SL ->
        let spec_sl = structure filenames_spec in
        (SL spec_sl : spec)
  in
  let (module Booter) = Backend_boot.Gen.gen_zero_spectec () in
  Booter.init ~cache ~det ~guard spec_sim;
  (spec_sim, (module Booter : RUNNER))

let booter_square ?(cache = true) ?(det = false) ?(guard = false) mode
    filenames_spec filenames_spec_p4 =
  let spec =
    match mode with
    | `IL ->
        let spec_il = elab filenames_spec in
        (IL spec_il : spec)
    | `SL ->
        let spec_sl = structure filenames_spec in
        (SL spec_sl : spec)
  in
  let spec_p4 =
    match mode with
    | `IL ->
        let spec_il = elab filenames_spec_p4 in
        (IL spec_il : spec)
    | `SL ->
        let spec_sl = structure filenames_spec_p4 in
        (SL spec_sl : spec)
  in
  let (module Runner_P4), (module Booter) = Backend_boot.Gen.gen_square_p4 () in
  Runner_P4.init ~cache ~det ~guard spec_p4;
  Booter.init ~cache ~det ~guard spec;
  (spec, (module Booter : RUNNER))

let booter_cube_p4 ?(cache = true) ?(det = false) ?(guard = false) mode
    filenames_spec filenames_spec_p4 =
  let spec =
    match mode with
    | `IL ->
        let spec_il = elab filenames_spec in
        (IL spec_il : spec)
    | `SL ->
        let spec_sl = structure filenames_spec in
        (SL spec_sl : spec)
  in
  let spec_p4 =
    match mode with
    | `IL ->
        let spec_il = elab filenames_spec_p4 in
        (IL spec_il : spec)
    | `SL ->
        let spec_sl = structure filenames_spec_p4 in
        (SL spec_sl : spec)
  in
  let (module Runner_P4), (module Runner_SpecTec_mid), (module Booter) =
    Backend_boot.Gen.gen_cube_p4 ()
  in
  Runner_P4.init ~cache ~det ~guard spec_p4;
  Runner_SpecTec_mid.init ~cache ~det ~guard spec;
  Booter.init ~cache ~det ~guard spec;
  (spec, (module Booter : RUNNER))

let booter_cube_spectec ?(cache = true) ?(det = false) ?(guard = false) mode
    filenames_spec filenames_spec_pgm =
  let spec =
    match mode with
    | `IL ->
        let spec_il = elab filenames_spec in
        (IL spec_il : spec)
    | `SL ->
        let spec_sl = structure filenames_spec in
        (SL spec_sl : spec)
  in
  let spec_pgm =
    match mode with
    | `IL ->
        let spec_il = elab filenames_spec_pgm in
        (IL spec_il : spec)
    | `SL ->
        let spec_sl = structure filenames_spec_pgm in
        (SL spec_sl : spec)
  in
  let (module Runner_SpecTec_pgm), (module Runner_SpecTec_mid), (module Booter)
      =
    Backend_boot.Gen.gen_cube_spectec ()
  in
  Runner_SpecTec_pgm.init ~cache ~det ~guard spec_pgm;
  Runner_SpecTec_mid.init ~cache ~det ~guard spec_pgm;
  Booter.init ~cache ~det ~guard spec;
  (spec, (module Booter : RUNNER))

(* Commands *)

let elab_command =
  Core.Command.basic ~summary:"parse and elaborate a P4 spec"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map filenames_spec =
       anon (non_empty_sequence_as_list ("filename" %: string))
     in
     fun () ->
       try
         let spec_il = elab filenames_spec in
         Format.printf "%s\n" (Il.Print.string_of_spec spec_il);
         ()
       with
       | ParseError (at, msg) -> Format.printf "%s\n" (string_of_error at msg)
       | ElabError (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

let struct_command =
  Core.Command.basic ~summary:"insert structured control flow to a P4 spec"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map filenames_spec =
       anon (non_empty_sequence_as_list ("filename" %: string))
     in
     fun () ->
       try
         let spec_sl = structure filenames_spec in
         Format.printf "%s\n" (Sl.Print.string_of_spec spec_sl);
         ()
       with
       | ParseError (at, msg) -> Format.printf "%s\n" (string_of_error at msg)
       | ElabError (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

let prose_command =
  Core.Command.basic ~summary:"generate AsciiDoc prose from a P4 spec"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map filenames_spec =
       anon (non_empty_sequence_as_list ("filename" %: string))
     in
     fun () ->
       try
         let spec_pl = prosify filenames_spec in
         Format.printf "%s\n" (Pl.Render.render_spec spec_pl);
         ()
       with
       | ParseError (at, msg) | ElabError (at, msg) | ProseError (at, msg) ->
         Format.printf "%s\n" (string_of_error at msg))

let run_command =
  Core.Command.basic
    ~summary:"execute a P4-SpecTec spec against a P4-SpecTec program"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map filenames_spec =
       anon (non_empty_sequence_as_list ("filename" %: string))
     and relname = flag "-rel" (required string) ~doc:"relation to run"
     and filename_spectec = flag "-tec" (required string) ~doc:"SpecTec program"
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
         let spec_sim, (module Booter) =
           booter ~cache ~det ~guard mode filenames_spec
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
         let result = Booter.run_program relname [] filename_spectec in
         Inst.Hook.finish ();
         match result with
         | Pass _ -> Format.printf "passed\n"
         | Fail (`Syntax (_, msg)) -> Format.printf "syntax error: %s\n" msg
         | Fail (`Runtime (_, msg)) -> Format.printf "runtime error: %s\n" msg
       with
       | ParseError (at, msg) -> Format.printf "%s\n" (string_of_error at msg)
       | ElabError (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

let boot_square_command =
  Core.Command.basic ~summary:"execute a P4 spec against a P4-SpecTec spec"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map dirname_spec =
       flag "-s0" (required string) ~doc:"directory for boot spec files"
     and rel = flag "-r0" (required string) ~doc:"boot spec relation to run"
     and dirname_spec_p4 =
       flag "-s1" (required string) ~doc:"directory for p4 spec files"
     and rel_p4 = flag "-r1" (required string) ~doc:"p4 spec relation to run"
     and includes_p4 = flag "-i1" (listed string) ~doc:"p4 include paths"
     and filename_p4 = flag "-p1" (required string) ~doc:"p4 program"
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
         let filenames_spec = expand_spec [ dirname_spec ] in
         let filenames_spec_p4 = expand_spec [ dirname_spec_p4 ] in
         let spec, (module Booter) =
           booter_square ~cache ~det ~guard mode filenames_spec
             filenames_spec_p4
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
         let value_p4_spec =
           Backend_boot.Patch.apply_square filenames_spec_p4 rel_p4 includes_p4
             filename_p4
         in
         let result = Booter.run_program_internal rel value_p4_spec in
         Inst.Hook.finish ();
         match result with
         | Pass _ -> Format.printf "passed\n"
         | Fail (_, msg) -> Format.printf "runtime error: %s\n" msg
       with
       | ParseError (at, msg) -> Format.printf "%s\n" (string_of_error at msg)
       | ElabError (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

let boot_cube_p4_command =
  Core.Command.basic
    ~summary:
      "execute a P4 spec against a P4-SpecTec spec against a P4-SpecTec spec"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map dirname_spec =
       flag "-s0" (required string) ~doc:"directory for boot spec files"
     and rel = flag "-r0" (required string) ~doc:"boot spec relation to run"
     and dirname_spec_p4 =
       flag "-s1" (required string) ~doc:"directory for p4 spec files"
     and rel_p4 = flag "-r1" (required string) ~doc:"p4 spec relation to run"
     and includes_p4 = flag "-i1" (listed string) ~doc:"p4 include paths"
     and filename_p4 = flag "-p1" (required string) ~doc:"p4 program"
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
         let filenames_spec = expand_spec [ dirname_spec ] in
         let filenames_spec_p4 = expand_spec [ dirname_spec_p4 ] in
         let spec, (module Booter) =
           booter_cube_p4 ~cache ~det ~guard mode filenames_spec
             filenames_spec_p4
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
         let value_spec =
           Backend_boot.Patch.apply_cube filenames_spec rel filenames_spec_p4
             rel_p4 includes_p4 filename_p4
         in
         let result = Booter.run_program_internal rel value_spec in
         Inst.Hook.finish ();
         match result with
         | Pass _ -> Format.printf "passed\n"
         | Fail (_, msg) -> Format.printf "runtime error: %s\n" msg
       with
       | ParseError (at, msg) -> Format.printf "%s\n" (string_of_error at msg)
       | ElabError (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

let boot_cube_spectec_command =
  Core.Command.basic
    ~summary:
      "execute a P4-SpecTec program against a P4-SpecTec spec against a \
       P4-SpecTec spec"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map dirname_spec =
       flag "-s0" (required string) ~doc:"directory for boot spec files"
     and rel = flag "-r0" (required string) ~doc:"boot spec relation to run"
     and dirname_spec_pgm =
       flag "-s1" (required string) ~doc:"directory for spectec spec files"
     and rel_pgm =
       flag "-r1" (required string) ~doc:"SpecTec program relation to run"
     and filename_pgm = flag "-p1" (required string) ~doc:"SpecTec program"
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
         let filenames_spec = expand_spec [ dirname_spec ] in
         let filenames_spec_pgm = expand_spec [ dirname_spec_pgm ] in
         let spec, (module Booter) =
           booter_cube_spectec ~cache ~det ~guard mode filenames_spec
             filenames_spec_pgm
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
         let value_spec =
           Backend_boot.Patch.apply_cube_small filenames_spec rel
             filenames_spec_pgm rel_pgm filename_pgm
         in
         let result = Booter.run_program_internal rel value_spec in
         Inst.Hook.finish ();
         match result with
         | Pass _ -> Format.printf "passed\n"
         | Fail (_, msg) -> Format.printf "runtime error: %s\n" msg
       with
       | ParseError (at, msg) -> Format.printf "%s\n" (string_of_error at msg)
       | ElabError (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

let parse_command =
  Core.Command.basic ~summary:"parse a SpecTec program"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map filenames_spec =
       anon (non_empty_sequence_as_list ("filename" %: string))
     and filename_spectec = flag "-tec" (required string) ~doc:"SpecTec program"
     and roundtrip =
       flag "-r" no_arg ~doc:"perform a round-trip parse/unparse"
     in
     fun () ->
       try
         let _, (module Booter) = booter `IL filenames_spec in
         let filenames_spectec = expand_spec [ filename_spectec ] in
         let value_program =
           match Booter.parse_file [] filenames_spectec with
           | Pass value_program -> value_program
           | Fail (`Syntax (at, msg)) -> raise (ParseError (at, msg))
         in
         let str_program = Booter.unparse_program value_program in
         if roundtrip then
           let value_program_roundtrip =
             match Booter.parse_string filename_spectec str_program with
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

let command =
  Core.Command.group
    ~summary:
      "p4spectec-boot: a language design framework for the p4_16 language, \
       bootstrapped"
    [
      (* Transformations *)
      ("elab", elab_command);
      ("struct", struct_command);
      ("prose", prose_command);
      (* Execution *)
      ("run", run_command);
      ("boot-2-p4", boot_square_command);
      ("boot-3-p4", boot_cube_p4_command);
      ("boot-3-spectec", boot_cube_spectec_command);
      (* Interfacing with IL specification *)
      ("parse", parse_command);
    ]

let () = Command_unix.run ~version command
