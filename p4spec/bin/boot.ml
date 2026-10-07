open Lang
open Runtime.Dynamic_Runner.Signature
module Error = P4spectec.Error
open Backend_boot.Config

let ( let* ) = Result.bind
let version = "0.1"

(* Tune GC for the allocation-heavy meta-circular interpreter *)

let () =
  Gc.set
    {
      (Gc.get ()) with
      Gc.minor_heap_size = 16 * 1024 * 1024;
      Gc.space_overhead = 2000;
    }

(* Commands *)

let elab_command =
  Core.Command.basic ~summary:"parse and elaborate a spec"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map paths_spec =
       anon (non_empty_sequence_as_list ("path" %: string))
     in
     fun () ->
       match P4spectec.elab paths_spec with
       | Ok spec_il -> Format.printf "%s\n" (Il.Print.string_of_spec spec_il)
       | Error e -> Format.printf "%s\n" (Error.to_string e))

let algo_command =
  Core.Command.basic ~summary:"check algorithmic property of a spec"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map paths_spec =
       anon (non_empty_sequence_as_list ("path" %: string))
     in
     fun () ->
       match P4spectec.algo paths_spec with
       | Ok spec_al -> Format.printf "%s\n" (Al.Print.string_of_spec spec_al)
       | Error e -> Format.printf "%s\n" (Error.to_string e))

let struct_command =
  Core.Command.basic ~summary:"insert structured control flow to a spec"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map paths_spec =
       anon (non_empty_sequence_as_list ("path" %: string))
     in
     fun () ->
       match P4spectec.structure ~final:true paths_spec with
       | Ok spec_sl -> Format.printf "%s\n" (Sl.Print.string_of_spec spec_sl)
       | Error e -> Format.printf "%s\n" (Error.to_string e))

let prose_command =
  Core.Command.basic ~summary:"annotate a spec"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map paths_spec =
       anon (non_empty_sequence_as_list ("path" %: string))
     in
     fun () ->
       match P4spectec.annotate paths_spec with
       | Ok spec_pl -> Format.printf "%s\n" (Pl.Print.string_of_spec spec_pl)
       | Error e -> Format.printf "%s\n" (Error.to_string e))

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
       let cache = not no_cache in
       match
         let* spec = P4spectec.spec_of_mode mode paths_spec in
         let* runner = P4spectec.build_null ~cache ~det ~guard interface spec in
         Ok (spec, runner)
       with
       | Error e -> Format.printf "%s\n" (Error.to_string e)
       | Ok (spec, (module Runner : RUNNER)) -> (
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
           ))

(* The configurations of a disjunction, as the search binary of a kompiled
   definition writes them *)
let rec kore_alternatives (p : Kore.Ast.pattern) : Kore.Ast.pattern list =
  match p with
  | Kore.Ast.App ("\\or", _, ps) -> List.concat_map kore_alternatives ps
  | Kore.Ast.App ("\\bottom", _, _) -> []
  | Kore.Ast.App ("\\equals", _, [ _; p ]) -> [ p ]
  | p -> [ p ]

(* Checking a run step by step against K (krun -check-steps DIR). Each next
   configuration must be one of those the search binary of the kompiled
   definition DIR finds one step on, and the interpreter must take no step
   from the last configuration of a run that ends. A nondeterministic program
   is checked along the run the spec takes, without exploring the others. *)
module Step_check = struct
  type t = {
    dir : string;
    info : Kore.Load.info;
    current : string;  (* the configuration given to K *)
    next : string;  (* what K writes *)
    mutable last : Value.t option;
    mutable steps : int;
    mutable branching : int;  (* steps where K allows more than one *)
    mutable failure : string option;
  }

  let make dir info =
    {
      dir;
      info;
      current = Filename.temp_file "kstep" ".kore";
      next = Filename.temp_file "knext" ".kore";
      last = None;
      steps = 0;
      branching = 0;
      failure = None;
    }

  (* Run DIR/binary on a configuration; the exit code *)
  let run_k c binary term depth extra =
    Out_channel.with_open_bin c.current (fun oc ->
        output_string oc
          (Kore.Ast.string_of_pattern (Kore.Unparse.pattern_of_term c.info term)));
    if Sys.file_exists c.next then Sys.remove c.next;
    Sys.command
      (Filename.quote_command ~stdin:"/dev/null" ~stderr:"/dev/null"
         (Filename.concat c.dir binary)
         ([ c.current; string_of_int depth; c.next ] @ extra))

  (* A new configuration of the run: one step after the last one *)
  let step c term =
    (match (c.failure, c.last) with
    | None, Some last ->
        if run_k c "search" last 1 [] <> 0 then
          c.failure <-
            Some (Printf.sprintf "search failed after %d steps" c.steps)
        else
          let nexts =
            kore_alternatives (Kore.Parse.pattern_of_file c.next)
            |> List.map (fun p ->
                   Kore.Unparse.normalize c.info p |> Kore.Ast.string_of_pattern)
          in
          if List.mem (Kore.Unparse.string_of_term c.info term) nexts then (
            c.steps <- c.steps + 1;
            if List.length (List.sort_uniq String.compare nexts) > 1 then
              c.branching <- c.branching + 1)
          else
            c.failure <-
              Some
                (Printf.sprintf "step %d is none of the %d steps K allows"
                   (c.steps + 1) (List.length nexts))
    | _ -> ());
    c.last <- Some term

  (* After the run: K takes no step from the last configuration *)
  let finish c ~ended =
    (match (c.failure, c.last, ended) with
    | None, Some last, true ->
        if run_k c "interpreter" last 1 [ "--statistics" ] <> 0 then
          c.failure <- Some "the interpreter failed on the last configuration"
        else if
          not
            (String.equal "0"
               (In_channel.with_open_bin c.next In_channel.input_line
               |> Option.value ~default:""))
        then
          c.failure <-
            Some
              (Printf.sprintf "K takes a step from the configuration after %d steps"
                 c.steps)
    | _ -> ());
    List.iter (fun f -> if Sys.file_exists f then Sys.remove f) [ c.current; c.next ];
    match c.failure with
    | None ->
        Format.printf
          "steps check: each of %d steps is one K allows (K allows more than one \
           at %d)%s\n"
          c.steps c.branching
          (if ended then ", and K takes no step from the last" else "")
    | Some msg -> Format.printf "steps check FAILED: %s\n" msg

  (* A handler that sees the configuration of each step: the last argument
     of each call of $run (spec-k/6-entry.watsup) *)
  let handler c : (module Inst.Handler.HANDLER) =
    (module struct
      include Inst.Handler.Default

      let on_func_enter (fid : Domain.Lib.FId.t) (values : Value.t list) =
        if String.equal (Domain.Lib.FId.to_string fid) "run" then
          match List.rev values with term :: _ -> step c term | [] -> ()
    end)
end

let krun_command =
  Core.Command.basic
    ~summary:"run a kompiled K definition (definition.kore) with the K spec"
    (let open Core.Command.Let_syntax in
     let open Core.Command.Param in
     let%map paths_spec = anon (non_empty_sequence_as_list ("path" %: string))
     and path_def =
       flag "-def" (required string) ~doc:"FILE definition.kore from kompile"
     and path_init =
       flag "-init" (required string)
         ~doc:"FILE initial term, as krun --save-temps writes it (tmp.in.*)"
     and depth = flag "-depth" (optional int) ~doc:"N stop after N steps"
     and path_expect =
       flag "-expect" (optional string)
         ~doc:"FILE expected final configuration (krun --output kore)"
     and path_expect_any =
       flag "-expect-any" (optional string)
         ~doc:
           "FILE final configurations krun --search found (a disjunction); \
            the result must be one of them"
     and path_out =
       flag "-o" (optional string) ~doc:"FILE write the final configuration"
     and det = flag "-det" no_arg ~doc:"deterministic mode"
     and no_cache = flag "-no-cache" no_arg ~doc:"disable caching"
     and profile = flag "-profile" no_arg ~doc:"profiling"
     and path_cover =
       flag "-cover" (optional string)
         ~doc:"FILE write the spec with the instructions the run executes marked"
     and path_check =
       flag "-check-steps" (optional string)
         ~doc:
           "DIR check each step against the kompiled definition DIR (kompile \
            --enable-search): the next configuration must be one DIR/search \
            finds one step on"
     in
     fun () ->
       (* The GC settings above favour speed over memory. K runs keep large
          configurations alive, so use OCaml's default space overhead (120),
          unless SPECTEC_SPACE_OVERHEAD says otherwise: at 1,500 steps of
          KOOL factorial it halves the heap (949 MB to 424 MB) at no cost. *)
       let space_overhead =
         match Sys.getenv_opt "SPECTEC_SPACE_OVERHEAD" with
         | Some s -> ( try int_of_string s with Failure _ -> 120)
         | None -> 120
       in
       Gc.set { (Gc.get ()) with Gc.space_overhead };
       let time f =
         let t = Unix.gettimeofday () in
         let x = f () in
         (x, Unix.gettimeofday () -. t)
       in
       match
         let* spec = P4spectec.spec_of_mode SL_mode paths_spec in
         let* runner =
           Kore.K.runner ~cache:(not no_cache) ~det spec
           |> Result.map_error (fun e -> Error.RunError e)
         in
         Ok (spec, runner)
       with
       | Error e -> Format.printf "%s\n" (Error.to_string e)
       | Ok (spec, (module Runner : RUNNER)) -> (
           try
             let (loaded, value_init), t_load =
               time (fun () ->
                   let loaded =
                     Kore.Load.load_definition
                       (Kore.Parse.definition_of_file path_def)
                   in
                   let value_init =
                     Kore.Load.load_pattern loaded.info
                       (Kore.Parse.pattern_of_file path_init)
                   in
                   (loaded, value_init))
             in
             Format.printf "loaded %d rules, %d equations (%.2fs)\n"
               loaded.rules loaded.equations t_load;
             let value_depth =
               Value.Make.opt
                 (Runtime.Type.Typ.Make.opt Runtime.Type.Typ.Make.nat)
                 (Option.map (fun n -> Value.Make.nat (Bigint.of_int n)) depth)
             in
             let handlers =
               if profile then
                 let (module PH : Inst.Handler.HANDLER) = Inst.Profile.make () in
                 [ (module PH : Inst.Handler.HANDLER) ]
               else []
             in
             (* instruction coverage, as p4spectec's cover-run *)
             let handlers, read_cover =
               match path_cover with
               | Some _ ->
                   let (module CH : Inst.Handler.HANDLER), read =
                     Inst.Coverage_instr.make ()
                   in
                   (handlers @ [ (module CH : Inst.Handler.HANDLER) ], Some read)
               | None -> (handlers, None)
             in
             let check = Option.map (fun dir -> Step_check.make dir loaded.info) path_check in
             let handlers =
               match check with
               | Some c -> handlers @ [ Step_check.handler c ]
               | None -> handlers
             in
             Inst.Hook.register handlers;
             Inst.Hook.init_spec spec;
             let result, t_run =
               time (fun () ->
                   Runner.Interp.eval_func "krun" []
                     [ loaded.definition; value_init; value_depth ])
             in
             Inst.Hook.finish ();
             (* the spec with each instruction marked executed (+) or not (-) *)
             (match (path_cover, read_cover, spec) with
             | Some path, Some read, SL spec_sl ->
                 let cover =
                   Coverage.Instr.Multi.extend
                     (Coverage.Instr.Multi.init spec_sl)
                     path_init (read ())
                 in
                 Coverage.Instr.Log.log_spec ~path_cov_opt:(Some path) cover
                   spec_sl
             | _ -> ());
             let report status steps term =
               Format.printf "%s after %s steps (%.2fs)\n" status
                 (Value.to_string steps) t_run;
               let output = Kore.Unparse.string_of_term loaded.info term in
               (match path_out with
               | Some path ->
                   Out_channel.with_open_bin path (fun oc ->
                       output_string oc output;
                       output_char oc '\n')
               | None -> Format.printf "%s\n" output);
               match path_expect with
               | Some path ->
                   let expected =
                     Kore.Parse.pattern_of_file path
                     |> Kore.Unparse.normalize loaded.info
                     |> Kore.Ast.string_of_pattern
                   in
                   if String.equal expected output then
                     Format.printf "matches %s\n" path
                   else (
                     Format.printf "DIFFERS from %s\n" path;
                     (* keep the normalized expectation next to the output for diffing *)
                     Option.iter
                       (fun out ->
                         Out_channel.with_open_bin (out ^ ".expected") (fun oc ->
                             output_string oc expected;
                             output_char oc '\n'))
                       path_out)
               | None -> ()
             in
             let report status steps term =
               report status steps term;
               match path_expect_any with
               | Some path ->
                   let output =
                     Kore.Unparse.string_of_term loaded.info term
                   in
                   let solutions =
                     kore_alternatives (Kore.Parse.pattern_of_file path)
                     |> List.map (fun p ->
                            Kore.Unparse.normalize loaded.info p
                            |> Kore.Ast.string_of_pattern)
                   in
                   if List.mem output solutions then
                     Format.printf "matches one of %d final states in %s\n"
                       (List.length solutions) path
                   else
                     Format.printf "DIFFERS from all %d final states in %s\n"
                       (List.length solutions) path
               | None -> ()
             in
             match result with
             | Fail (_, msg) -> Format.printf "runtime error: %s\n" msg
             | Pass value -> (
                 match Value.Get.(value |>>? "FINAL nat term") with
                 | Some [ steps; term ] ->
                     report "final" steps term;
                     (* the run ended, unless it stopped at the step limit *)
                     let ended =
                       match (depth, int_of_string_opt (Value.to_string steps)) with
                       | Some d, Some n -> n < d
                       | _ -> true
                     in
                     Option.iter (Step_check.finish ~ended) check
                 | _ -> (
                     match Value.Get.(value |>>? "ERROR nat term") with
                     | Some [ steps; term ] ->
                         report "ERROR" steps term;
                         Option.iter (Step_check.finish ~ended:false) check
                     | _ ->
                         Format.printf "initial term failed to evaluate: %s\n"
                           (Value.to_string value)))
           with
           | Kore.Parse.Error msg -> Format.printf "KORE parse error: %s\n" msg
           | Kore.Load.Error msg -> Format.printf "KORE load error: %s\n" msg))

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
       let target = { includes = includes_target; path = path_target } in
       match
         let* tower = P4spectec.tower_of_file path_tower target in
         let* spec_boot, booter =
           P4spectec.build_tower ~cache:(not no_cache) ~det ~guard tower
         in
         Ok (tower, spec_boot, booter)
       with
       | Error e -> Format.printf "%s\n" (Error.to_string e)
       | Ok (tower, spec, (module Booter : RUNNER)) -> (
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
           | Fail (_, msg) -> Format.printf "runtime error: %s\n" msg))

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
       match
         let* spec = P4spectec.spec_of_mode SL_mode paths_spec in
         P4spectec.build_null interface spec
       with
       | Error e -> Format.printf "%s\n" (Error.to_string e)
       | Ok (module Runner) -> (
           try
             match Runner.Interface.parse_program [] [ path_spectec ] with
             | Fail (`Syntax (at, msg)) ->
                 Format.printf "Parse error: %s\n"
                   (Util.Error.string_of_error at msg)
             | Pass value_program ->
                 let str_program =
                   Runner.Interface.unparse_program value_program
                 in
                 if roundtrip then
                   match
                     Runner.Interface.parse_string path_spectec str_program
                   with
                   | Fail (`Syntax (at, msg)) ->
                       Format.printf "Parse error: %s\n"
                         (Util.Error.string_of_error at msg)
                   | Pass value_program_roundtrip ->
                       Il.Eq.eq_value ~dbg:true value_program
                         value_program_roundtrip
                       |> (fun b ->
                            if b then "Roundtrip successful"
                            else "Roundtrip failed")
                       |> print_endline
                 else str_program |> print_endline
           with
           | Sys_error msg -> Format.printf "File error: %s\n" msg
           | e -> Format.printf "Unknown error: %s\n" (Printexc.to_string e)))

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
      ("krun", krun_command);
      (* Interfacing with IL specification *)
      ("parse", parse_command);
    ]

let () = Command_unix.run ~version command_core
