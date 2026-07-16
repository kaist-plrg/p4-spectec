open Test_common
open Util.Error
open Runtime.Sim.Signature
module Test = Util.Test
module Filesys = Util.Filesys

(* Interpreter test *)

let run (module Simulator : SIM) neg relname includes_p4 path_p4 =
  let time_start = start () in
  try
    Simulator.Interp.clear ();
    (match Simulator.Interp.eval_program relname includes_p4 path_p4 with
    | Pass _ -> if neg then raise (TestRunNegErr time_start)
    | Fail (`Syntax (at, msg)) | Fail (`Runtime (at, msg)) ->
        raise (TestRunErr (msg, at, time_start)));
    time_start
  with
  | TestRunErr _ as err -> raise err
  | TestRunNegErr _ as err -> raise err
  | _ -> raise (TestUnknownErr time_start)

let run_test (module Simulator : SIM) neg stat relname includes_p4 excludes_p4
    path_p4 =
  if List.exists (String.equal path_p4) excludes_p4 then (
    let log = Format.asprintf "Excluding file: %s" path_p4 in
    log |> print_endline;
    {
      stat with
      durations = 0.0 :: stat.durations;
      exclude_run = stat.exclude_run + 1;
    })
  else
    try
      let time_start = run (module Simulator) neg relname includes_p4 path_p4 in
      let duration = stop time_start in
      let log = Format.asprintf "Run success: %s" path_p4 in
      log |> print_endline;
      Format.eprintf "%s\n" log;
      Format.eprintf ">>> took %.6f seconds\n" duration;
      { stat with durations = duration :: stat.durations }
    with
    | TestRunErr (msg, at, time_start) ->
        let duration = stop time_start in
        Format.asprintf "Error on run: %s" path_p4 |> print_endline;
        Format.eprintf "Error on run: %s\n%s\n" path_p4 (string_of_error at msg);
        Format.eprintf ">>> took %.6f seconds\n" duration;
        {
          stat with
          durations = duration :: stat.durations;
          fail_run = stat.fail_run + 1;
        }
    | TestRunNegErr time_start ->
        let duration = stop time_start in
        Format.asprintf "Error on run: %s (should fail)" path_p4
        |> print_endline;
        Format.eprintf "Error on run: %s (should fail)\n" path_p4;
        Format.eprintf ">>> took %.6f seconds\n" duration;
        { stat with durations = duration :: stat.durations }
    | TestUnknownErr time_start ->
        let duration = stop time_start in
        Format.asprintf "Error on run: %s (unknown)" path_p4 |> print_endline;
        Format.eprintf "Error on run: %s (unknown)\n" path_p4;
        Format.eprintf ">>> took %.6f seconds\n" duration;
        {
          stat with
          durations = duration :: stat.durations;
          fail_run = stat.fail_run + 1;
        }

let run_test_driver mode det neg path_spec relname includes_p4 excludes_p4
    testdirs_p4 =
  let excludes_p4 =
    excludes_p4 |> Test.collect_excludes
    |> List.map (fun exclude_p4 -> "../../../../../" ^ exclude_p4)
  in
  let paths_p4 =
    testdirs_p4 |> List.concat_map (Filesys.collect_files ~suffix:".p4")
  in
  let total = List.length paths_p4 in
  let stat = empty_stat in
  Format.asprintf "Running interpreter test (%s) on %d files\n" relname total
  |> print_endline;
  let _spec_sim, (module Simulator) =
    Backend_sim.Build.build ~det ~final:true mode [ path_spec ]
  in
  let stat =
    List.fold_left
      (fun stat path_p4 ->
        Format.asprintf "\n>>> Running interpreter test (%s) on %s" relname
          path_p4
        |> print_endline;
        run_test
          (module Simulator)
          neg stat relname includes_p4 excludes_p4 path_p4)
      stat paths_p4
  in
  log_stat
    (Format.asprintf "\nRunning interpreter test (%s)" relname)
    stat total

let run_command =
  Core.Command.basic ~summary:"run interpreter test"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map path_spec = flag "-s" (required string) ~doc:"p4 spec directory"
     and relname = flag "-rel" (required string) ~doc:"relation name"
     and includes_p4 = flag "-i" (listed string) ~doc:"p4 include paths"
     and excludes_p4 = flag "-e" (listed string) ~doc:"p4 test exclude paths"
     and testdirs_p4 = flag "-p4-dir" (listed string) ~doc:"p4 test directories"
     and neg = flag "-neg" no_arg ~doc:"neg testsing (expect failure)"
     and det = flag "-det" no_arg ~doc:"deterministic mode"
     and mode =
       Command.Param.choose_one
         [
           flag "il" no_arg ~doc:"Run IL interpreter"
           |> map ~f:(fun b -> Core.Option.some_if b IL_mode);
           flag "sl" no_arg ~doc:"Run SL interpreter"
           |> map ~f:(fun b -> Core.Option.some_if b SL_mode);
           flag "ml" no_arg ~doc:"Run ML (compiled) interpreter"
           |> map ~f:(fun b -> Core.Option.some_if b ML_mode);
         ]
         ~if_nothing_chosen:(Default_to SL_mode)
     in
     fun () ->
       run_test_driver mode det neg path_spec relname includes_p4 excludes_p4
         testdirs_p4)

(* Coverage test *)

let cover_run mode path_spec relname includes_p4 excludes_p4 testdirs_p4 =
  let excludes_p4 =
    excludes_p4 |> Test.collect_excludes
    |> List.map (fun exclude_p4 -> "../../../../../" ^ exclude_p4)
  in
  let paths_p4 =
    testdirs_p4
    |> List.concat_map (Filesys.collect_files ~suffix:".p4")
    |> List.filter (fun path_p4 -> not (List.mem path_p4 excludes_p4))
  in
  match mode with
  | `Instr -> cover_run_instr SL_mode [ path_spec ] relname includes_p4 paths_p4
  | `Dangling ->
      cover_run_dangling SL_mode [ path_spec ] relname includes_p4 paths_p4

let cover_run_command =
  Core.Command.basic ~summary:"measure coverage of the spec"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map path_spec = flag "-s" (required string) ~doc:"p4 spec directory"
     and relname = flag "-rel" (required string) ~doc:"relation name"
     and includes_p4 = flag "-i" (listed string) ~doc:"p4 include paths"
     and excludes_p4 = flag "-e" (listed string) ~doc:"p4 test exclude paths"
     and testdirs_p4 = flag "-p4-dir" (listed string) ~doc:"p4 test directories"
     and mode =
       Command.Param.choose_one
         [
           flag "instr" no_arg ~doc:"measure instruction coverage"
           |> map ~f:(fun b -> Core.Option.some_if b `Instr);
           flag "dangling" no_arg ~doc:"measure dangling coverage"
           |> map ~f:(fun b -> Core.Option.some_if b `Dangling);
         ]
         ~if_nothing_chosen:(Default_to `Instr)
     in
     fun () ->
       cover_run mode path_spec relname includes_p4 excludes_p4 testdirs_p4)

(* [has_dup_<X>] is generic, never instantiated at [text] anywhere — only
   succeeds if [eval_func] can build [text]'s witnesses at runtime. *)

let eval_func_has_dup_test () : unit =
  let module Extern_stub = struct
    module Cache = struct
      let cache_on () = ()
      let cache_off () = ()
    end

    let eval_extern_rel (_ : string) (_ : Runtime.Value.t list) : rel_result =
      failwith "eval_func_has_dup_test: unexpected extern relation call"

    let eval_extern_func (_ : string) (_ : Runtime.Type.Typ.t list)
        (_ : Runtime.Value.t list) : func_result =
      failwith "eval_func_has_dup_test: unexpected extern function call"

    let checkpoint () : int = 0
    let seff (_ : int) (_ : int) : bool = false
    let clear () : unit = ()
    let init_mode (_ : mode) : unit = ()
  end in
  let module Interp =
    Backend_ocaml.Interp_ml.Make (Interface.P4) (Extern_stub) ()
  in
  Interp.init ~cache:false ~det:false ~guard:false ();
  let typ_text = Runtime.Type.Typ.Make.text in
  let value_dup = Runtime.Value.Make.text "dup" in
  let value_args =
    Runtime.Value.Make.list
      (Runtime.Type.Typ.Make.list typ_text)
      [ value_dup; value_dup ]
  in
  (match Interp.eval_func "has_dup_" [ typ_text ] [ value_args ] with
  | Pass value ->
      if Runtime.Value.Get.bool value then
        print_endline
          "[PASS] eval_func \"has_dup_\" at text (no ground call site) \
           returned true"
      else
        failwith "eval_func_has_dup_test: expected true (duplicate), got false"
  | Fail (_, msg) ->
      failwith
        (Printf.sprintf "eval_func_has_dup_test: eval_func failed: %s" msg));
  (* [interface_name]/[interface_name_] registry-key agreement: a mismatch
     for a registered witness type surfaces as [No_marshaller_] below. *)
  let check_has_dup (label : string) (typ_x : Runtime.Type.Typ.t)
      (value_dup : Runtime.Value.t) : unit =
    let value_args =
      Runtime.Value.Make.list
        (Runtime.Type.Typ.Make.list typ_x)
        [ value_dup; value_dup ]
    in
    match Interp.eval_func "has_dup_" [ typ_x ] [ value_args ] with
    | Pass value ->
        if Runtime.Value.Get.bool value then
          print_endline
            (Printf.sprintf "[PASS] eval_func \"has_dup_\" at %s" label)
        else
          failwith
            (Printf.sprintf
               "eval_func_has_dup_test: %s: expected true, got false" label)
    | Fail (_, msg) ->
        failwith (Printf.sprintf "eval_func_has_dup_test: %s: %s" label msg)
  in
  (* [name]'s [`TYPE] case — a VarT with a plain id (no sanitizing needed). *)
  let typ_name =
    Util.Source.(Runtime.Type.Typ.Make.var ("name" $ no_region) [])
  in
  let value_name = Runtime.Value.Make.(("TYPE" <| []) <<| "name") in
  check_has_dup "name (VarT, plain id)" typ_name value_name;
  (* Witness itself is [text list] — exercises the [IterT (_, List)] arm. *)
  let typ_text_list = Runtime.Type.Typ.Make.list typ_text in
  let value_text_list =
    Runtime.Value.Make.list typ_text_list [ Runtime.Value.Make.text "a" ]
  in
  check_has_dup "text list (IterT List)" typ_text_list value_text_list;
  (* Witness itself is [text opt] — exercises the [IterT (_, Opt)] arm. *)
  let typ_text_opt = Runtime.Type.Typ.Make.opt typ_text in
  let value_text_opt =
    Runtime.Value.Make.opt typ_text_opt (Some (Runtime.Value.Make.text "a"))
  in
  check_has_dup "text opt (IterT Opt)" typ_text_opt value_text_opt;
  (* Wrong arity (missing the [X] witness type) must fail cleanly, not crash. *)
  match Interp.eval_func "has_dup_" [] [ value_args ] with
  | Pass _ ->
      failwith "eval_func_has_dup_test: expected arity failure, got Pass"
  | Fail (_, _) ->
      print_endline
        "[PASS] eval_func \"has_dup_\" with wrong arity failed cleanly"

(* [split.ml]'s embedded keyword list must match [Names.keywords], or a
   keyword-colliding type name sanitizes differently at runtime vs compile. *)

let extract_bracketed_list (s : string) (marker : string) : string list =
  let n = String.length s and m = String.length marker in
  let rec find i =
    if i + m > n then failwith "extract_bracketed_list: marker not found"
    else if String.sub s i m = marker then i
    else find (i + 1)
  in
  let marker_idx = find 0 in
  let open_idx = String.index_from s marker_idx '[' in
  let close_idx = String.index_from s open_idx ']' in
  String.sub s (open_idx + 1) (close_idx - open_idx - 1)
  |> String.split_on_char ';'
  |> List.filter_map (fun tok ->
         match String.trim tok with
         | "" -> None
         | tok -> Some (String.sub tok 1 (String.length tok - 2)))

let interface_name_keywords_test () : unit =
  let embedded =
    extract_bracketed_list Pass.Compile.Template.Split.interface_name_fn
      "interface_keywords_ ="
  in
  let expected = Pass.Compile.Gen.Names.keywords in
  if List.sort compare embedded <> List.sort compare expected then
    failwith
      "interface_name_keywords_test: split.ml's interface_keywords_ has \
       drifted from Names.keywords"
  else
    print_endline
      "[PASS] interface_name_fn's embedded keyword list matches \
       Names.keywords"

let eval_func_has_dup_and_keywords_test () : unit =
  eval_func_has_dup_test ();
  interface_name_keywords_test ()

let eval_func_command =
  Core.Command.basic ~summary:"direct eval_func test (Task 6)"
    (Core.Command.Param.return eval_func_has_dup_and_keywords_test)

let command =
  Core.Command.group ~summary:"p4spec-test-run"
    [
      ("run", run_command);
      ("cover-run", cover_run_command);
      ("eval-func", eval_func_command);
    ]

let () = Command_unix.run ~version command
