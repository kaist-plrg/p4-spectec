open Lang
open Runtime.Dynamic_Runner.Signature
open Backend_boot.Config
open Util.Error

let version = "0.1"

exception CommandError of string

(* Commands *)

let elab_command =
  Core.Command.basic ~summary:"parse and elaborate a P4 spec"
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

let struct_command =
  Core.Command.basic ~summary:"insert structured control flow to a P4 spec"
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
  Core.Command.basic ~summary:"generate AsciiDoc prose from a P4 spec"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map paths_spec =
       anon (non_empty_sequence_as_list ("path" %: string))
     in
     fun () ->
       try
         let spec_pl = Pass.prosify paths_spec in
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
           flag "il" no_arg ~doc:"run IL interpreter"
           |> map ~f:(fun b -> Core.Option.some_if b IL_mode);
           flag "sl" no_arg ~doc:"run SL interpreter"
           |> map ~f:(fun b -> Core.Option.some_if b SL_mode);
         ]
         ~if_nothing_chosen:(Default_to SL_mode)
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

let boot_n_main args =
  try
    (* Arguments *)
    (* The boot mode, either IL or SL, -m, defaults to SL *)
    let mode = ref SL_mode in
    (* Directory paths for specs, -sN where N is the layer index *)
    let dirnames_spec = ref [] in
    (* Relations to run in the specs, -rN where N is the layer index *)
    let rels = ref [] in
    (* Interfaces of layers, -iN where N is the layer index *)
    let interfaces = ref [] in
    (* The target program to run against, -p *)
    let path_p4 = ref None in
    (* The include paths for the target program, -i *)
    let includes_p4 = ref [] in
    (* Caching, determinism, and guard options *)
    let no_cache = ref false in
    let det = ref false in
    let guard = ref false in
    (* Profiling and tracing options *)
    let profile = ref false in
    let trace = ref None in
    (* Argument parsing *)
    let rec parse = function
      | [] -> ()
      | "-m" :: arg :: args -> (
          match arg with
          | "il" ->
              mode := IL_mode;
              parse args
          | "sl" ->
              mode := SL_mode;
              parse args
          | _ ->
              raise
                (CommandError
                   (Format.asprintf "-l: expected 'il' or 'sl', got %s" arg)))
      | flag :: arg :: args
        when String.length flag >= 3 && flag.[0] = '-' && flag.[1] = 's' -> (
          let s_idx = String.sub flag 2 (String.length flag - 2) in
          match int_of_string_opt s_idx with
          | Some idx ->
              dirnames_spec := (idx, arg) :: !dirnames_spec;
              parse args
          | None ->
              raise
                (CommandError
                   (Format.asprintf
                      "invalid flag: %s (expected -sN where N is an integer)"
                      flag)))
      | flag :: arg :: args
        when String.length flag >= 3 && flag.[0] = '-' && flag.[1] = 'r' -> (
          let s_idx = String.sub flag 2 (String.length flag - 2) in
          match int_of_string_opt s_idx with
          | Some idx ->
              rels := (idx, arg) :: !rels;
              parse args
          | None ->
              raise
                (CommandError
                   (Format.asprintf
                      "invalid flag: %s (expected -rN where N is an integer)"
                      flag)))
      | flag :: arg :: args
        when String.length flag >= 3 && flag.[0] = '-' && flag.[1] = 'i' -> (
          let s_idx = String.sub flag 2 (String.length flag - 2) in
          match int_of_string_opt s_idx with
          | Some idx -> (
              match arg with
              | "p4" ->
                  interfaces := (idx, P4_interface) :: !interfaces;
                  parse args
              | "il" ->
                  interfaces := (idx, IL_interface) :: !interfaces;
                  parse args
              | "sl" ->
                  interfaces := (idx, SL_interface) :: !interfaces;
                  parse args
              | _ ->
                  raise
                    (CommandError
                       (Format.asprintf
                          "invalid interface: %s (expected 'il' or 'sl')" arg)))
          | None ->
              raise
                (CommandError
                   (Format.asprintf
                      "invalid flag: %s (expected -iN where N is an integer)"
                      flag)))
      | "-p" :: arg :: args ->
          path_p4 := Some arg;
          parse args
      | "-i" :: arg :: args ->
          includes_p4 := !includes_p4 @ [ arg ];
          parse args
      | "-no-cache" :: args ->
          no_cache := true;
          parse args
      | "-det" :: args ->
          det := true;
          parse args
      | "-guard" :: args ->
          guard := true;
          parse args
      | "-profile" :: args ->
          profile := true;
          parse args
      | "-trace" :: args ->
          if !trace <> None then
            raise
              (CommandError
                 "-trace: multiple trace flags provided (only one of -trace or \
                  -trace-full allowed)");
          trace := Some Inst.Trace.Simple;
          parse args
      | "-trace-full" :: args ->
          if !trace <> None then
            raise
              (CommandError
                 "-trace-full: multiple trace flags provided (only one of \
                  -trace or -trace-full allowed)");
          trace := Some Inst.Trace.Full;
          parse args
      | args ->
          raise
            (CommandError
               (Format.asprintf "unexpected argument: %s"
                  (String.concat " " args)))
    in
    parse args;
    (* Validate parsed result *)
    (* -p is required *)
    let path_p4 =
      match !path_p4 with
      | None -> raise (CommandError "-p is required")
      | Some path -> path
    in
    (* Height is determined by the maximum index of -sN, -rN, and -iN *)
    let height =
      List.map fst !dirnames_spec
      @ List.map fst !rels @ List.map fst !interfaces
      |> List.fold_left max 0
    in
    (* Height must be at least 2 *)
    if height < 2 then
      raise
        (CommandError "at least two layers are required (index 2 or higher)");
    (* For each index 0..N: -sN, -rN, and -iN must be present *)
    for idx = 0 to height do
      if not (List.mem_assoc idx !dirnames_spec) then
        raise (CommandError (Format.asprintf "missing -s%d" idx));
      if not (List.mem_assoc idx !rels) then
        raise (CommandError (Format.asprintf "missing -r%d" idx));
      if not (List.mem_assoc idx !interfaces) then
        raise (CommandError (Format.asprintf "missing -i%d" idx))
    done;
    (* Indices must be in-range *)
    List.iter
      (fun (idx, _) ->
        if idx > height then
          raise
            (CommandError
               (Format.asprintf "-s%d is out of range (n = %d)" idx height)))
      !dirnames_spec;
    List.iter
      (fun (idx, _) ->
        if idx > height then
          raise
            (CommandError
               (Format.asprintf "-r%d is out of range (n = %d)" idx height)))
      !rels;
    List.iter
      (fun (idx, _) ->
        if idx > height then
          raise
            (CommandError
               (Format.asprintf "-i%d is out of range (n = %d)" idx height)))
      !interfaces;
    (* Booting *)
    let sort lst =
      lst
      |> List.sort (fun (idx_a, _) (idx_b, _) -> compare idx_a idx_b)
      |> List.map snd
    in
    (* Join levels *)
    let dirnames_spec = !dirnames_spec |> sort in
    let rels = !rels |> sort in
    let interfaces = !interfaces |> sort in
    let levels =
      let layers =
        List.map2
          (fun dirname_spec rel -> { specdir = dirname_spec; rel })
          dirnames_spec rels
      in
      List.map2 (fun layer interface -> { layer; interface }) layers interfaces
    in
    (* Split levels into boot, interm, and target *)
    let level_boot, levels_interm, level_target =
      let level_boot = List.hd levels in
      let levels_interm =
        levels |> List.tl |> List.rev |> List.tl |> List.rev
      in
      let level_target = levels |> List.rev |> List.hd in
      (level_boot, levels_interm, level_target)
    in
    (* Join target *)
    let target = { includes = !includes_p4; path = path_p4 } in
    (* Create a tower *)
    let tower =
      { mode = !mode; level_boot; levels_interm; level_target; target }
    in
    (* Build the tower *)
    let spec, _, _, (module Booter) =
      Backend_boot.Build.build ~cache:(not !no_cache) ~det:!det ~guard:!guard
        tower
    in
    (* Set up hooks for profiling and tracing *)
    let handlers =
      if !profile then
        let (module PH : Inst.Handler.HANDLER) = Inst.Profile.make () in
        [ (module PH : Inst.Handler.HANDLER) ]
      else []
    in
    let handlers =
      match !trace with
      | Some level ->
          let (module TH : Inst.Handler.HANDLER) = Inst.Trace.make ~level () in
          handlers @ [ (module TH : Inst.Handler.HANDLER) ]
      | None -> handlers
    in
    Inst.Hook.register handlers;
    Inst.Hook.init_spec spec;
    (* Evaluate the boot layer *)
    let rel_boot = level_boot.layer.rel in
    let value = Backend_boot.Patch.apply_tower tower in
    let result = Booter.Interp.eval_rel rel_boot [ value ] in
    (* Finish hooks and print result *)
    Inst.Hook.finish ();
    match result with
    | Pass _ -> Format.printf "passed\n"
    | Fail (_, msg) -> Format.printf "runtime error: %s\n" msg
  with
  | CommandError msg -> Format.eprintf "error: %s\n" msg
  | ParseError (at, msg) -> Format.printf "%s\n" (string_of_error at msg)
  | ElabError (at, msg) -> Format.printf "%s\n" (string_of_error at msg)

let boot_n_command =
  Core.Command.basic
    ~summary:
      "execute N layers of bootstrapped specs against a program (flags: -h N, \
       -iK {il|sl}, -sK <dir>, -rK <rel>, -p <prog>, -i <inc>)"
    (Core.Command.Param.return (fun () -> ()))

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
           flag "il" no_arg ~doc:"IL interface"
           |> map ~f:(fun b -> Core.Option.some_if b IL_interface);
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

(* Command-line interface *)

let command_core =
  Core.Command.group
    ~summary:
      "spectec-boot: a language design framework for the p4_16 language, with \
       meta-circular interpretation"
    [
      (* Transformations *)
      ("elab", elab_command);
      ("struct", struct_command);
      ("prose", prose_command);
      (* Execution *)
      ("run", run_command);
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
