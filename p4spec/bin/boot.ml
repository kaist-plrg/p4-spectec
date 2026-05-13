open Lang
open Pass
open Runtime.Dynamic_Runner.Signature
open Util.Error

let version = "0.1"

exception CommandError of string

(* Modes *)

let interp_mode_arg =
  Command.Arg_type.create (function
    | "il" -> IL_mode
    | "sl" -> SL_mode
    | _ -> failwith "invalid mode, expected il or sl")

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

let structure ~(final : bool) filenames_spec =
  filenames_spec |> elab |> Structure.Struct.struct_spec ~final

let prosify filenames_spec =
  filenames_spec |> structure ~final:false |> Prose.Prosify.prosify_spec

let booter ?(cache = true) ?(det = false) ?(guard = false) ~(final : bool) mode
    specmode filenames_spec =
  let spec =
    match mode with
    | IL_mode ->
        let spec_il = elab filenames_spec in
        (IL spec_il : spec)
    | SL_mode ->
        let spec_sl = structure ~final filenames_spec in
        (SL spec_sl : spec)
    | Empty_mode -> assert false
  in
  let (module Booter) =
    match specmode with
    | IL_mode -> Backend_boot.Gen.gen_zero_il ()
    | SL_mode -> Backend_boot.Gen.gen_zero_sl ()
    | Empty_mode -> assert false
  in
  Booter.init ~cache ~det ~guard spec;
  (spec, (module Booter : RUNNER))

let booter_n_p4 ?(cache = true) ?(det = false) ?(guard = false) ~(final : bool)
    ~(depth : int) mode specmode filenames_spec filenames_spec_p4 =
  let spec =
    match mode with
    | IL_mode ->
        let spec_il = elab filenames_spec in
        (IL spec_il : spec)
    | SL_mode ->
        let spec_sl = structure ~final filenames_spec in
        (SL spec_sl : spec)
    | Empty_mode -> assert false
  in
  let spec_p4 =
    match mode with
    | IL_mode ->
        let spec_il = elab filenames_spec_p4 in
        (IL spec_il : spec)
    | SL_mode ->
        let spec_sl = structure ~final filenames_spec_p4 in
        (SL spec_sl : spec)
    | Empty_mode -> assert false
  in
  let (module Runner_P4), runners_intermediate, (module Booter) =
    match specmode with
    | IL_mode -> Backend_boot.Gen.gen_n_p4_il ~depth
    | SL_mode -> Backend_boot.Gen.gen_n_p4_sl ~depth
    | Empty_mode -> assert false
  in
  Runner_P4.init ~cache ~det ~guard spec_p4;
  List.iter
    (fun (module Runner_SpecTec_mid : RUNNER) ->
      Runner_SpecTec_mid.init ~cache ~det ~guard spec)
    runners_intermediate;
  Booter.init ~cache ~det ~guard spec;
  (spec, (module Booter : RUNNER))

let booter_n_spectec ?(cache = true) ?(det = false) ?(guard = false)
    ~(final : bool) ~(depth : int) mode specmode filenames_spec
    filenames_spec_pgm =
  let spec =
    match mode with
    | IL_mode ->
        let spec_il = elab filenames_spec in
        (IL spec_il : spec)
    | SL_mode ->
        let spec_sl = structure ~final filenames_spec in
        (SL spec_sl : spec)
    | Empty_mode -> assert false
  in
  let spec_pgm =
    match mode with
    | IL_mode ->
        let spec_il = elab filenames_spec_pgm in
        (IL spec_il : spec)
    | SL_mode ->
        let spec_sl = structure ~final filenames_spec_pgm in
        (SL spec_sl : spec)
    | Empty_mode -> assert false
  in
  let (module Runner_SpecTec_pgm), runners_intermediate, (module Booter) =
    match specmode with
    | IL_mode -> Backend_boot.Gen.gen_n_spectec_il ~depth
    | SL_mode -> Backend_boot.Gen.gen_n_spectec_sl ~depth
    | Empty_mode -> assert false
  in
  Runner_SpecTec_pgm.init ~cache ~det ~guard spec_pgm;
  List.iter
    (fun (module Runner_SpecTec_mid : RUNNER) ->
      Runner_SpecTec_mid.init ~cache ~det ~guard spec)
    runners_intermediate;
  Booter.init ~cache ~det ~guard spec;
  (spec, (module Booter : RUNNER))

(* Commands using the Command module *)

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
         let spec_sl = structure ~final:true filenames_spec in
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
       flag "mode"
         (optional_with_default SL_mode interp_mode_arg)
         ~doc:"on {il|sl} interpreter"
     and specmode =
       flag "specmode"
         (optional_with_default SL_mode interp_mode_arg)
         ~doc:"on {il|sl} meta-spec"
     in
     fun () ->
       try
         let cache = not no_cache in
         let spec, (module Booter) =
           booter ~cache ~det ~guard ~final:true mode specmode filenames_spec
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
         let result = Booter.Interp.eval_program relname [] filename_spectec in
         Inst.Hook.finish ();
         match result with
         | Pass _ -> Format.printf "passed\n"
         | Fail (`Syntax (_, msg)) -> Format.printf "syntax error: %s\n" msg
         | Fail (`Runtime (_, msg)) -> Format.printf "runtime error: %s\n" msg
       with
       | ParseError (at, msg) -> Format.printf "%s\n" (string_of_error at msg)
       | ElabError (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

let boot_n_p4_command =
  Core.Command.basic
    ~summary:
      "execute a P4 spec against a P4-SpecTec spec against N P4-SpecTec spec(s)"
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
     and depth =
       flag "-n" (required int) ~doc:"number of layers of P4-SpecTec specs"
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
       flag "mode"
         (optional_with_default SL_mode interp_mode_arg)
         ~doc:"on {il|sl} meta-spec"
     and specmode =
       flag "specmode"
         (optional_with_default SL_mode interp_mode_arg)
         ~doc:"on {il|sl} meta-spec"
     in
     fun () ->
       try
         let cache = not no_cache in
         let depth =
           if depth < 2 then raise (CommandError "depth must be at least 2")
           else depth - 2
         in
         let filenames_spec = expand_spec [ dirname_spec ] in
         let filenames_spec_p4 = expand_spec [ dirname_spec_p4 ] in
         let spec, (module Booter) =
           booter_n_p4 ~cache ~det ~guard ~final:true ~depth mode specmode
             filenames_spec filenames_spec_p4
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
           Backend_boot.Patch.apply_n_p4 ~depth ~mode:specmode filenames_spec
             rel filenames_spec_p4 rel_p4 includes_p4 filename_p4
         in
         let result = Booter.Interp.eval_rel rel [ value_spec ] in
         Inst.Hook.finish ();
         match result with
         | Pass _ -> Format.printf "passed\n"
         | Fail (_, msg) -> Format.printf "runtime error: %s\n" msg
       with
       | CommandError msg -> Format.printf "command error: %s\n" msg
       | ParseError (at, msg) -> Format.printf "%s\n" (string_of_error at msg)
       | ElabError (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

let boot_n_spectec_command =
  Core.Command.basic
    ~summary:
      "execute a P4-SpecTec program against a P4-SpecTec spec against N \
       P4-SpecTec spec(s)"
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
     and depth =
       flag "-n" (required int) ~doc:"number of layers of P4-SpecTec specs"
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
       flag "mode"
         (optional_with_default SL_mode interp_mode_arg)
         ~doc:"on {il|sl} interpreter"
     and specmode =
       flag "specmode"
         (optional_with_default SL_mode interp_mode_arg)
         ~doc:"on {il|sl} meta-spec"
     in
     fun () ->
       try
         let cache = not no_cache in
         let depth =
           if depth < 2 then raise (CommandError "depth must be at least 2")
           else depth - 2
         in
         let filenames_spec = expand_spec [ dirname_spec ] in
         let filenames_spec_pgm = expand_spec [ dirname_spec_pgm ] in
         let spec, (module Booter) =
           booter_n_spectec ~cache ~det ~guard ~final:true ~depth mode specmode
             filenames_spec filenames_spec_pgm
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
           Backend_boot.Patch.apply_n_spectec ~depth ~mode:specmode
             filenames_spec rel filenames_spec_pgm rel_pgm filename_pgm
         in
         let result = Booter.Interp.eval_rel rel [ value_spec ] in
         Inst.Hook.finish ();
         match result with
         | Pass _ -> Format.printf "passed\n"
         | Fail (_, msg) -> Format.printf "runtime error: %s\n" msg
       with
       | CommandError msg -> Format.printf "command error: %s\n" msg
       | ParseError (at, msg) -> Format.printf "%s\n" (string_of_error at msg)
       | ElabError (at, msg) -> Format.printf "%s\n" (string_of_error at msg))

let boot_n_command =
  Core.Command.basic
    ~summary:
      "execute N layers of bootstrapped specs against a program (flags: -n N, \
       -lK {il|sl}, -sK <dir>, -rK <rel>, -p <prog>, -i <inc>)"
    (Core.Command.Param.return (fun () -> ()))

let parse_command =
  Core.Command.basic ~summary:"parse a SpecTec program"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map filenames_spec =
       anon (non_empty_sequence_as_list ("filename" %: string))
     and filename_spectec = flag "-tec" (required string) ~doc:"SpecTec program"
     and roundtrip = flag "-r" no_arg ~doc:"perform a round-trip parse/unparse"
     and specmode =
       flag "specmode"
         (optional_with_default SL_mode interp_mode_arg)
         ~doc:"on {il|sl} meta-spec"
     in
     fun () ->
       try
         let _, (module Booter) =
           booter ~final:true SL_mode specmode filenames_spec
         in
         let filenames_spectec = expand_spec [ filename_spectec ] in
         let value_program =
           match Booter.Interface.parse_program [] filenames_spectec with
           | Pass value_program -> value_program
           | Fail (`Syntax (at, msg)) -> raise (ParseError (at, msg))
         in
         let str_program = Booter.Interface.unparse_program value_program in
         if roundtrip then
           let value_program_roundtrip =
             match
               Booter.Interface.parse_string filename_spectec str_program
             with
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

(* `boot-n` command does not use the Command module *)

let boot_n_main args =
  try
    (* Arguments *)
    let depth = ref None in
    let dirnames_spec = ref [] in
    let rels = ref [] in
    let langs = ref [] in
    let filename_p4 = ref None in
    let includes_p4 = ref [] in
    (* Argument parsing *)
    let rec parse = function
      | [] -> ()
      | "-n" :: arg :: args -> (
          match int_of_string_opt arg with
          | Some n ->
              depth := Some n;
              parse args
          | None ->
              raise
                (CommandError
                   (Format.asprintf "-n: expected integer, got %s" arg)))
      | "-p" :: arg :: args ->
          filename_p4 := Some arg;
          parse args
      | "-i" :: arg :: args ->
          includes_p4 := !includes_p4 @ [ arg ];
          parse args
      | flag :: arg :: args when String.length flag >= 3 && flag.[0] = '-' -> (
          let flag_prefix = String.sub flag 0 2 in
          let s_idx = String.sub flag 2 (String.length flag - 2) in
          match (flag_prefix, int_of_string_opt s_idx) with
          | "-s", Some idx ->
              dirnames_spec := (idx, arg) :: !dirnames_spec;
              parse args
          | "-s", None ->
              raise
                (CommandError
                   (Format.asprintf
                      "invalid flag: %s (expected -sN where N is an integer)"
                      flag))
          | "-r", Some idx ->
              rels := (idx, arg) :: !rels;
              parse args
          | "-r", None ->
              raise
                (CommandError
                   (Format.asprintf
                      "invalid flag: %s (expected -rN where N is an integer)"
                      flag))
          | "-l", Some idx -> (
              match arg with
              | "il" ->
                  langs := (idx, IL_mode) :: !langs;
                  parse args
              | "sl" ->
                  langs := (idx, SL_mode) :: !langs;
                  parse args
              | _ ->
                  raise
                    (CommandError
                       (Format.asprintf
                          "invalid language: %s (expected 'il' or 'sl')" arg)))
          | "-l", None ->
              raise
                (CommandError
                   (Format.asprintf
                      "invalid flag: %s (expected -lN where N is an integer)"
                      flag))
          | _ ->
              raise
                (CommandError (Format.asprintf "unrecognized flag: %s" flag)))
      | args ->
          raise
            (CommandError
               (Format.asprintf "unexpected argument: %s"
                  (String.concat " " args)))
    in
    parse args;
    (* Validate parsed result *)
    (* -n is required and must be >= 1 *)
    let depth =
      match !depth with
      | None -> raise (CommandError "-n is required")
      | Some depth when depth < 1 ->
          raise (CommandError (Format.asprintf "-n must be >= 1, got %d" depth))
      | Some depth -> depth
    in
    (* -p is required *)
    let filename_p4 =
      match !filename_p4 with
      | None -> raise (CommandError "-p is required")
      | Some filename -> filename
    in
    (* -s0 and -r0 are disallowed *)
    if List.mem_assoc 0 !dirnames_spec then
      raise (CommandError "-s0 is disallowed (only -l0 is allowed at index 0)");
    if List.mem_assoc 0 !rels then
      raise (CommandError "-r0 is disallowed (only -l0 is allowed at index 0)");
    (* For each index 1..N: -sN and -rN must be present;
       -lN is required for 1..N-1 only *)
    for idx = 1 to depth do
      if not (List.mem_assoc idx !dirnames_spec) then
        raise (CommandError (Format.asprintf "missing -s%d" idx));
      if not (List.mem_assoc idx !rels) then
        raise (CommandError (Format.asprintf "missing -r%d" idx));
      if idx < depth && not (List.mem_assoc idx !langs) then
        raise (CommandError (Format.asprintf "missing -l%d" idx))
    done;
    (* Indices must be in-range *)
    List.iter
      (fun (idx, _) ->
        if idx > depth then
          raise
            (CommandError
               (Format.asprintf "-s%d is out of range (n = %d)" idx depth)))
      !dirnames_spec;
    List.iter
      (fun (idx, _) ->
        if idx > depth then
          raise
            (CommandError
               (Format.asprintf "-r%d is out of range (n = %d)" idx depth)))
      !rels;
    List.iter
      (fun (idx, _) ->
        if idx > depth && idx <> 0 then
          raise
            (CommandError
               (Format.asprintf "-l%d is out of range (n = %d)" idx depth)))
      !langs;
    (* Booting *)
    let sort lst =
      lst
      |> List.sort (fun (idx_a, _) (idx_b, _) -> compare idx_a idx_b)
      |> List.map snd
    in
    let dirnames_spec_interm, dirname_spec_target =
      !dirnames_spec |> sort |> List.rev |> fun dirnames_spec ->
      (dirnames_spec |> List.tl |> List.rev, List.hd dirnames_spec)
    in
    let rels_interm, rel_target =
      !rels |> sort |> List.rev |> fun rels ->
      (rels |> List.tl |> List.rev, List.hd rels)
    in
    let lang_boot, langs_interm =
      !langs |> sort |> fun langs -> (List.hd langs, List.tl langs)
    in
    let lang_to_string = function
      | IL_mode -> "IL"
      | SL_mode -> "SL"
      | Empty_mode -> assert false
    in
    Format.asprintf "Booting with language %s" (lang_to_string lang_boot)
    |> print_endline;
    let interms =
      List.combine dirnames_spec_interm rels_interm
      |> List.combine langs_interm
      |> List.map (fun (lang, (dirname_spec, rel)) -> (dirname_spec, rel, lang))
    in
    List.iter
      (fun (dirname_spec, rel, lang) ->
        Format.asprintf
          "Booting intermediate with spec %s and relation %s in language %s"
          dirname_spec rel (lang_to_string lang)
        |> print_endline)
      interms;
    let _target = (dirname_spec_target, rel_target) in
    Format.asprintf "Booting target with spec %s and relation %s"
      dirname_spec_target rel_target
    |> print_endline;
    let includes_p4 = !includes_p4 in
    Format.asprintf "Running %s on includes %s" filename_p4
      (includes_p4 |> String.concat ", ")
    |> print_endline;
    ()
  with CommandError msg -> Format.eprintf "error: %s\n" msg

(* Command-line interface *)

let command_core =
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
      ("boot-p4", boot_n_p4_command);
      ("boot-spectec", boot_n_spectec_command);
      ("boot-n", boot_n_command);
      (* Interfacing with IL specification *)
      ("parse", parse_command);
    ]

let () =
  match Array.to_list Sys.argv with
  | _ :: "boot-n" :: args -> (
      match args with
      | ("-help" | "--help" | "help") :: _ ->
          Command_unix.run ~version command_core
      | _ ->
          boot_n_main args;
          exit 0)
  | _ -> Command_unix.run ~version command_core
