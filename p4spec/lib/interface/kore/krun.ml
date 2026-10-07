(* spectec-boot krun: run a kompiled K definition (definition.kore) with the K
   spec from the initial term krun makes, as the LLVM backend's interpreter
   does, and report the final configuration in KORE *)

module Run = Runtime.Dynamic_Runner.Signature
module Value = Runtime.Value

let time f =
  let t = Unix.gettimeofday () in
  let x = f () in
  (x, Unix.gettimeofday () -. t)

let run ((module Runner : Run.RUNNER) : (module Run.RUNNER)) (spec : Run.spec)
    ~path_def ~path_init ~depth ~path_expect ~path_out ~profile ~path_cover
    ~path_check =
  (* The GC settings of spectec-boot favour speed over memory. K runs keep
     large configurations alive, so use OCaml's default space overhead (120),
     unless SPECTEC_SPACE_OVERHEAD says otherwise: at 1,500 steps of KOOL
     factorial it halves the heap (949 MB to 424 MB) at no cost. *)
  let space_overhead =
    match Sys.getenv_opt "SPECTEC_SPACE_OVERHEAD" with
    | Some s -> ( try int_of_string s with Failure _ -> 120)
    | None -> 120
  in
  Gc.set { (Gc.get ()) with Gc.space_overhead };
  try
    let (loaded, value_init), t_load =
      time (fun () ->
          let loaded =
            Load.load_definition (Parse.definition_of_file path_def)
          in
          ( loaded,
            Load.load_pattern loaded.info (Parse.pattern_of_file path_init) ))
    in
    Format.printf "loaded %d rules, %d equations (%.2fs)\n" loaded.rules
      loaded.equations t_load;
    let value_depth =
      Value.Make.opt
        (Runtime.Type.Typ.Make.opt Runtime.Type.Typ.Make.nat)
        (Option.map (fun n -> Value.Make.nat (Bigint.of_int n)) depth)
    in
    let handlers = if profile then [ Inst.Profile.make () ] else [] in
    (* instruction coverage, as p4spectec's cover-run *)
    let handlers, read_cover =
      match path_cover with
      | Some _ ->
          let handler, read = Inst.Coverage_instr.make () in
          (handlers @ [ handler ], Some read)
      | None -> (handlers, None)
    in
    let check =
      Option.map (fun dir -> Step_check.make dir loaded.info) path_check
    in
    let handlers =
      handlers @ Option.to_list (Option.map Step_check.handler check)
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
    | Some path, Some read, Run.SL spec_sl ->
        let cover =
          Coverage.Instr.Multi.extend
            (Coverage.Instr.Multi.init spec_sl)
            path_init (read ())
        in
        Coverage.Instr.Log.log_spec ~path_cov_opt:(Some path) cover spec_sl
    | _ -> ());
    let report status steps term =
      Format.printf "%s after %s steps (%.2fs)\n" status (Value.to_string steps)
        t_run;
      let output = Unparse.string_of_term loaded.info term in
      (match path_out with
      | Some path ->
          Out_channel.with_open_bin path (fun oc ->
              output_string oc output;
              output_char oc '\n')
      | None -> Format.printf "%s\n" output);
      Option.iter
        (fun path ->
          let expected =
            Parse.pattern_of_file path
            |> Unparse.normalize loaded.info
            |> Ast.string_of_pattern
          in
          Format.printf "%s %s\n"
            (if String.equal expected output then "matches" else "DIFFERS from")
            path)
        path_expect
    in
    match result with
    | Run.Fail (_, msg) -> Format.printf "runtime error: %s\n" msg
    | Run.Pass value -> (
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
  | Parse.Error msg -> Format.printf "KORE parse error: %s\n" msg
  | Load.Error msg -> Format.printf "KORE load error: %s\n" msg
