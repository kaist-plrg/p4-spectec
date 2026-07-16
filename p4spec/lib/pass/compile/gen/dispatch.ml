open Lang
open Sl
open Util.Source

let compile_eval_func (ctx : Ctx.t) (spec : Sl.spec) : Ml.funcdef =
  (* Signatures for generic defs (tparams <> []) — the generic arm needs
     its own signature table. *)
  let poly_func_sigs : (string, Sl.param list * Sl.typ) Hashtbl.t =
    Hashtbl.create 16
  in
  List.iter
    (fun def ->
      match def.it with
      | FuncDecD (id, tparams, params, typ_ret, _, _, _)
      | BuiltinDecD (id, tparams, params, typ_ret, _)
      | ExternDecD (id, tparams, params, typ_ret, _)
        when tparams <> [] ->
          Hashtbl.replace poly_func_sigs id.it (params, typ_ret)
      | _ -> ())
    spec;
  (* Helpers *)
  let run_fail msg =
    Ml.AppE
      (Ml.LitE "Run.Fail", [ Ml.TupleE [ Ml.LitE "no_region"; Ml.StrE msg ] ])
  in
  let build_dispatch_body params typ_ret ocaml_name =
    let has_defp =
      List.exists
        (fun p -> match p.it with DefP _ -> true | _ -> false)
        params
    in
    if has_defp then None
    else
      let exp_typs =
        List.filter_map
          (fun p -> match p.it with ExpP (typ, _) -> Some typ | _ -> None)
          params
      in
      let n = List.length exp_typs in
      let unmarshal_lets =
        List.mapi
          (fun i typ ->
            ( "a" ^ string_of_int i,
              Ml.AppE
                ( Ml.VarE ("unmarshal_" ^ Interface.interface_name typ),
                  [
                    Ml.AppE
                      ( Ml.LitE "List.nth",
                        [ Ml.VarE "args__"; Ml.LitE (string_of_int i) ] );
                  ] ) ))
          exp_typs
      in
      let exprs_arg_ml =
        List.init n (fun i -> Ml.VarE ("a" ^ string_of_int i))
      in
      let expr_call_ml = Ml.AppE (Ml.LitE ocaml_name, exprs_arg_ml) in
      let marshal_ret = Interface.interface_name typ_ret in
      let expr_run_pass_ml =
        Ml.AppE
          ( Ml.LitE "Run.Pass",
            [ Ml.AppE (Ml.VarE ("marshal_" ^ marshal_ret), [ expr_call_ml ]) ]
          )
      in
      let expr_body_ml =
        List.fold_right
          (fun (var_name, rhs) acc -> Ml.LetE (Ml.VarP var_name, rhs, acc))
          unmarshal_lets expr_run_pass_ml
      in
      Some expr_body_ml
  in
  (* Arms for mono functions *)
  let arms_mono_ml =
    List.filter_map
      (fun def ->
        let make_arm id params typ_ret =
          let name = id.it in
          let ocaml_name = Names.func id in
          let expr_wrong_targs_ml =
            run_fail (Printf.sprintf "eval_func: wrong type args for %s" name)
          in
          let expr_body_ml =
            match build_dispatch_body params typ_ret ocaml_name with
            | None ->
                run_fail
                  (Printf.sprintf
                     "eval_func: higher-order parameter not supported: %s" name)
            | Some expr_body_ml ->
                Ml.IfE
                  ( Ml.BinopE ("=", Ml.VarE "typs__", Ml.ListE []),
                    expr_body_ml,
                    Some expr_wrong_targs_ml )
          in
          Some (Ml.LitP ("\"" ^ name ^ "\""), expr_body_ml)
        in
        match def.it with
        | FuncDecD (id, [], params, typ_ret, _, _, _) ->
            make_arm id params typ_ret
        | BuiltinDecD (id, [], params, typ_ret, _) -> make_arm id params typ_ret
        | ExternDecD (id, [], params, typ_ret, _) -> make_arm id params typ_ret
        | TableDecD (id, params, typ_ret, _, _) -> make_arm id params typ_ret
        | _ -> None)
      spec
  in
  (* Arms for generic defs: witnesses come from [interface_lookup_],
     so no ground call site is needed. *)
  let build_generic_arm (name : string) (tparams : Il.tparam list)
      (params : Sl.param list) (typ_ret : Sl.typ) : Ml.arm option =
    let has_defp =
      List.exists
        (fun p -> match p.it with DefP _ -> true | _ -> false)
        params
    in
    if has_defp then None
    else
      let tvars = List.map (fun (tp : Il.tparam) -> tp.it) tparams in
      let tparams_ml = List.map Names.tvar tparams in
      let ocaml_name = "f__" ^ Names.sanitize name in
      (* Bind [marshal__x]/[unmarshal__x] per tparam; the annotation is needed
         or OCaml can't prove the later application principal ([-w 20]). *)
      let witness_lets =
        List.concat
          (List.mapi
             (fun i tv ->
               let id_entry = "entry__" ^ string_of_int i in
               let expr_lookup_ml =
                 Ml.AppE
                   ( Ml.LitE "interface_lookup_",
                     [
                       Ml.AppE
                         ( Ml.LitE "List.nth",
                           [ Ml.VarE "typs__"; Ml.LitE (string_of_int i) ] );
                     ] )
               in
               let typ_marshal_ml =
                 Ml.NameT (Printf.sprintf "('%s -> Value.t)" tv)
               in
               let typ_unmarshal_ml =
                 Ml.NameT (Printf.sprintf "(Value.t -> '%s)" tv)
               in
               [
                 (id_entry, expr_lookup_ml);
                 ( Interface.witness_marshal_name tv,
                   Ml.AnnotE
                     ( Ml.AppE
                         ( Ml.LitE "Obj.obj",
                           [ Ml.AppE (Ml.LitE "fst", [ Ml.VarE id_entry ]) ] ),
                       typ_marshal_ml ) );
                 ( Interface.witness_unmarshal_name tv,
                   Ml.AnnotE
                     ( Ml.AppE
                         ( Ml.LitE "Obj.obj",
                           [ Ml.AppE (Ml.LitE "snd", [ Ml.VarE id_entry ]) ] ),
                       typ_unmarshal_ml ) );
               ])
             tparams_ml)
      in
      let exprs_witness_ml =
        List.concat_map
          (fun tv ->
            [
              Ml.VarE (Interface.witness_marshal_name tv);
              Ml.VarE (Interface.witness_unmarshal_name tv);
            ])
          tparams_ml
      in
      let exp_typs =
        List.filter_map
          (fun p -> match p.it with ExpP (typ, _) -> Some typ | _ -> None)
          params
      in
      let n_tparams = List.length tparams in
      try
        let vars_arg_ml, exprs_marshal_arg_ml =
          List.mapi
            (fun i typ ->
              ( "a" ^ string_of_int i,
                Func.apply_witness (string_of_int i)
                  (Interface.resolve_unmarshal ctx tvars typ)
                  (Ml.AppE
                     ( Ml.LitE "List.nth",
                       [ Ml.VarE "args__"; Ml.LitE (string_of_int i) ] )) ))
            exp_typs
          |> List.split
        in
        let exprs_arg_ml = List.map (fun v -> Ml.VarE v) vars_arg_ml in
        let expr_call_ml =
          Ml.AppE (Ml.LitE ocaml_name, exprs_witness_ml @ exprs_arg_ml)
        in
        let expr_marshal_ret_ml =
          Func.apply_witness "ret__"
            (Interface.resolve_marshal ctx tvars typ_ret)
            expr_call_ml
        in
        let expr_run_pass_ml =
          Ml.AppE (Ml.LitE "Run.Pass", [ expr_marshal_ret_ml ])
        in
        let expr_ok_ml =
          List.fold_right
            (fun (var, rhs) acc -> Ml.LetE (Ml.VarP var, rhs, acc))
            (witness_lets @ List.combine vars_arg_ml exprs_marshal_arg_ml)
            expr_run_pass_ml
        in
        (* Guard arity before any [List.nth typs__ i] below can crash. *)
        let expr_fail_arity_ml =
          Ml.AppE
            ( Ml.LitE "Run.Fail",
              [
                Ml.TupleE
                  [
                    Ml.LitE "no_region";
                    Ml.AppE
                      ( Ml.LitE "Printf.sprintf",
                        [
                          Ml.StrE
                            (Printf.sprintf
                               "eval_func: %s expects %d type argument(s), got \
                                %%d"
                               name n_tparams);
                          Ml.AppE (Ml.LitE "List.length", [ Ml.VarE "typs__" ]);
                        ] );
                  ];
              ] )
        in
        let expr_body_ml =
          Ml.IfE
            ( Ml.BinopE
                ( "=",
                  Ml.AppE (Ml.LitE "List.length", [ Ml.VarE "typs__" ]),
                  Ml.LitE (string_of_int n_tparams) ),
              expr_ok_ml,
              Some expr_fail_arity_ml )
        in
        Some (Ml.LitP ("\"" ^ name ^ "\""), expr_body_ml)
      with Failure msg ->
        (* Same tuple/set/map boundary shapes Task 5's bridges skip (e.g. a
           tuple-of-lists return) — skip this arm too. *)
        Util.Error.warn_compile no_region
          (Format.asprintf "generic function %s: %s — skipping eval_func arm"
             name msg);
        None
  in
  let arms_generic_ml =
    let poly_names_tparams =
      Ctx.fold_funcs
        (fun id func acc ->
          let tparams, _, _ = Runtime.Dynamic_Sl.Func.get_signature func in
          if tparams = [] then acc else (id.it, tparams) :: acc)
        ctx []
    in
    List.filter_map
      (fun (name, tparams) ->
        match Hashtbl.find_opt poly_func_sigs name with
        | None -> None
        | Some (params, typ_ret) ->
            build_generic_arm name tparams params typ_ret)
      poly_names_tparams
  in
  (* Fallback wild arm *)
  let arm_wild_ml =
    ( Ml.WildP,
      Ml.AppE
        ( Ml.LitE "Run.Fail",
          [
            Ml.TupleE
              [
                Ml.LitE "no_region";
                Ml.AppE
                  ( Ml.LitE "Printf.sprintf",
                    [
                      Ml.StrE "eval_func: unknown function: %s";
                      Ml.VarE "name__";
                    ] );
              ];
          ] ) )
  in
  let arms_ml = arms_mono_ml @ arms_generic_ml @ [ arm_wild_ml ] in
  let expr_match_ml = Ml.MatchE (Ml.VarE "name__", arms_ml) in
  (* Unmatch handler *)
  let expr_fail_unmatch_ml =
    Ml.AppE
      ( Ml.LitE "Run.Fail",
        [
          Ml.TupleE
            [
              Ml.LitE "no_region";
              Ml.AppE
                ( Ml.LitE "Printf.sprintf",
                  [
                    Ml.StrE "eval_func: unmatch in %s: %s";
                    Ml.VarE "name__";
                    Ml.VarE "msg_";
                  ] );
            ];
        ] )
  in
  (* [interface_lookup_] raises this when a generic arm's witness type has
     no ground call site anywhere in the spec. *)
  let expr_fail_no_marshaller_ml =
    Ml.AppE
      ( Ml.LitE "Run.Fail",
        [
          Ml.TupleE
            [
              Ml.LitE "no_region";
              Ml.AppE
                ( Ml.LitE "Printf.sprintf",
                  [
                    Ml.StrE "eval_func: no marshaller registered for type %s";
                    Ml.VarE "tyname__";
                  ] );
            ];
        ] )
  in
  let expr_try_ml =
    Ml.TryE
      ( expr_match_ml,
        [
          ( Ml.VariantP (`Mono ("Unmatch", [ Ml.VarP "msg_" ])),
            expr_fail_unmatch_ml );
          ( Ml.VariantP (`Mono ("No_marshaller_", [ Ml.VarP "tyname__" ])),
            expr_fail_no_marshaller_ml );
        ] )
  in
  let params_ml =
    [
      ("name__", Some (Ml.NameT "string"));
      ("typs__", Some (Ml.AppT ("list", [ Ml.NameT "Typ.t" ])));
      ("args__", Some (Ml.AppT ("list", [ Ml.NameT "Value.t" ])));
    ]
  in
  ("eval_func", [], params_ml, Some (Ml.NameT "Run.func_result"), expr_try_ml)

let compile_eval_rel (ctx : Ctx.t) (spec : Sl.spec) : Ml.funcdef =
  ignore ctx;
  (* Build rel_sigs: id.it -> (typs_input, typs_output) derived from nottyp *)
  let rel_sigs : (string, Sl.typ list * Sl.typ list) Hashtbl.t =
    Hashtbl.create 32
  in
  List.iter
    (fun def ->
      match def.it with
      | RelD (id, (nottyp, inputs), _, _, _, _)
      | ExternRelD (id, (nottyp, inputs), _, _) ->
          let typs_rel = Domain.Mixfix.args nottyp.it in
          let typs_input, typs_output = Hints.Input.split inputs typs_rel in
          Hashtbl.replace rel_sigs id.it (typs_input, typs_output)
      | _ -> ())
    spec;
  (* Arms for each relation *)
  let arms_ml =
    List.filter_map
      (fun def ->
        let make_arm (id : Sl.id) =
          match Hashtbl.find_opt rel_sigs id.it with
          | None -> None
          | Some (typs_input, typs_output) ->
              let n_in = List.length typs_input in
              let id_rel_ml = Names.rel id in
              let unmarshal_lets =
                List.mapi
                  (fun i typ ->
                    ( "a" ^ string_of_int i,
                      Ml.AppE
                        ( Ml.VarE ("unmarshal_" ^ Interface.interface_name typ),
                          [
                            Ml.AppE
                              ( Ml.LitE "List.nth",
                                [ Ml.VarE "args__"; Ml.LitE (string_of_int i) ]
                              );
                          ] ) ))
                  typs_input
              in
              let exprs_arg_ml =
                List.init n_in (fun i -> Ml.VarE ("a" ^ string_of_int i))
              in
              let expr_call_ml = Ml.AppE (Ml.VarE id_rel_ml, exprs_arg_ml) in
              let expr_body_ml =
                match typs_output with
                | [] ->
                    let expr_seq_ml =
                      Ml.LetE
                        ( Ml.WildP,
                          expr_call_ml,
                          Ml.AppE (Ml.LitE "Run.Pass", [ Ml.ListE [] ]) )
                    in
                    List.fold_right
                      (fun (var, rhs) acc -> Ml.LetE (Ml.VarP var, rhs, acc))
                      unmarshal_lets expr_seq_ml
                | _ ->
                    let n_out = List.length typs_output in
                    let ids_out_ml =
                      List.init n_out (fun i -> "out" ^ string_of_int i)
                    in
                    let pat_out_ml =
                      match ids_out_ml with
                      | [ id_out_ml ] -> Ml.VarP id_out_ml
                      | _ ->
                          Ml.TupleP (List.map (fun s -> Ml.VarP s) ids_out_ml)
                    in
                    let exprs_marshal_out_ml =
                      List.mapi
                        (fun i typ ->
                          Ml.AppE
                            ( Ml.VarE ("marshal_" ^ Interface.interface_name typ),
                              [ Ml.VarE ("out" ^ string_of_int i) ] ))
                        typs_output
                    in
                    let expr_run_pass_ml =
                      Ml.AppE
                        (Ml.LitE "Run.Pass", [ Ml.ListE exprs_marshal_out_ml ])
                    in
                    let expr_bind_ml =
                      Ml.LetE (pat_out_ml, expr_call_ml, expr_run_pass_ml)
                    in
                    List.fold_right
                      (fun (var, rhs) acc -> Ml.LetE (Ml.VarP var, rhs, acc))
                      unmarshal_lets expr_bind_ml
              in
              Some (Ml.LitP ("\"" ^ id.it ^ "\""), expr_body_ml)
        in
        match def.it with
        | RelD (id, _, _, _, _, _) -> make_arm id
        | ExternRelD (id, _, _, _) -> make_arm id
        | _ -> None)
      spec
  in
  (* Fallback arm *)
  let arm_wild_ml =
    ( Ml.WildP,
      Ml.AppE
        ( Ml.LitE "Run.Fail",
          [
            Ml.TupleE
              [
                Ml.LitE "no_region";
                Ml.AppE
                  ( Ml.LitE "Printf.sprintf",
                    [
                      Ml.StrE "eval_rel: unknown relation: %s"; Ml.VarE "name__";
                    ] );
              ];
          ] ) )
  in
  let expr_match_ml = Ml.MatchE (Ml.VarE "name__", arms_ml @ [ arm_wild_ml ]) in
  (* Unmatch handler *)
  let expr_fail_unmatch_ml =
    Ml.AppE
      ( Ml.LitE "Run.Fail",
        [
          Ml.TupleE
            [
              Ml.LitE "no_region";
              Ml.AppE
                ( Ml.LitE "Printf.sprintf",
                  [
                    Ml.StrE "eval_rel: unmatch in %s: %s";
                    Ml.VarE "name__";
                    Ml.VarE "msg_";
                  ] );
            ];
        ] )
  in
  let expr_try_ml =
    Ml.TryE
      ( expr_match_ml,
        [
          ( Ml.VariantP (`Mono ("Unmatch", [ Ml.VarP "msg_" ])),
            expr_fail_unmatch_ml );
        ] )
  in
  let params_ml =
    [
      ("name__", Some (Ml.NameT "string"));
      ("args__", Some (Ml.AppT ("list", [ Ml.NameT "Value.t" ])));
    ]
  in
  ("eval_rel", [], params_ml, Some (Ml.NameT "Run.rel_result"), expr_try_ml)
