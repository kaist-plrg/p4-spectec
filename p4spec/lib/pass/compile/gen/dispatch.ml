open Lang
open Sl
open Util.Source

let compile_eval_func (ctx : Ctx.t) (spec : Sl.spec)
    (dispatch_table : Mono.dispatch_table) : Ml.funcdef =
  ignore ctx;
  (* Build func_sigs from monomorphized spec *)
  let func_sigs : (string, Sl.param list * Sl.typ) Hashtbl.t =
    Hashtbl.create 32
  in
  List.iter
    (fun def ->
      match def.it with
      | FuncDecD (id, [], params, typ_ret, _, _, _)
      | BuiltinDecD (id, [], params, typ_ret, _)
      | ExternDecD (id, [], params, typ_ret, _) ->
          Hashtbl.replace func_sigs id.it (params, typ_ret)
      | TableDecD (id, params, typ_ret, _, _) ->
          Hashtbl.replace func_sigs id.it (params, typ_ret)
      | _ -> ())
    spec;
  (* Collect poly-instance original names *)
  let poly_names : (string, unit) Hashtbl.t = Hashtbl.create 16 in
  Hashtbl.iter
    (fun orig_name _ -> Hashtbl.replace poly_names orig_name ())
    dispatch_table;
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
  (* Arms for poly dispatch (dispatch_table entries) *)
  let arms_poly_ml =
    Hashtbl.fold
      (fun orig_name instances acc ->
        let fail_no_inst =
          run_fail
            (Printf.sprintf "eval_func: no matching instance for %s" orig_name)
        in
        let expr_body_ml =
          List.fold_right
            (fun (inst : Mono.poly_instance) expr_else_ml ->
              let exprs_targ_ml =
                List.map Interface.typ_make_expr inst.concrete_targs
              in
              let expr_guard_ml =
                Ml.AppE
                  ( Ml.LitE "Il.Eq.eq_typs",
                    [ Ml.VarE "typs__"; Ml.ListE exprs_targ_ml ] )
              in
              let mangled = inst.mangled_name in
              let ocaml_name = "f__" ^ Names.sanitize mangled in
              let expr_then_ml =
                match Hashtbl.find_opt func_sigs mangled with
                | None ->
                    run_fail (Printf.sprintf "eval_func: no sig for %s" mangled)
                | Some (params, typ_ret) -> (
                    match build_dispatch_body params typ_ret ocaml_name with
                    | None ->
                        run_fail
                          (Printf.sprintf
                             "eval_func: higher-order parameter not supported: \
                              %s"
                             orig_name)
                    | Some expr_body_ml -> expr_body_ml)
              in
              Ml.IfE (expr_guard_ml, expr_then_ml, Some expr_else_ml))
            instances fail_no_inst
        in
        (Ml.LitP ("\"" ^ orig_name ^ "\""), expr_body_ml) :: acc)
      dispatch_table []
  in
  (* Arms for mono functions (not in dispatch_table) *)
  let arms_mono_ml =
    List.filter_map
      (fun def ->
        let make_arm id params typ_ret =
          let name = id.it in
          if Hashtbl.mem poly_names name then None
          else
            let ocaml_name = Names.func id in
            let expr_wrong_targs_ml =
              run_fail (Printf.sprintf "eval_func: wrong type args for %s" name)
            in
            let expr_body_ml =
              match build_dispatch_body params typ_ret ocaml_name with
              | None ->
                  run_fail
                    (Printf.sprintf
                       "eval_func: higher-order parameter not supported: %s"
                       name)
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
  let arms_ml = arms_poly_ml @ arms_mono_ml @ [ arm_wild_ml ] in
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
      ("typs__", Some (Ml.AppT ("list", [ Ml.NameT "Typ.t" ])));
      ("args__", Some (Ml.AppT ("list", [ Ml.NameT "Value.t" ])));
    ]
  in
  ("eval_func", params_ml, Some (Ml.NameT "Run.func_result"), expr_try_ml)

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
  ("eval_rel", params_ml, Some (Ml.NameT "Run.rel_result"), expr_try_ml)
