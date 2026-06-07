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
      let arg_vars = List.init n (fun i -> Ml.VarE ("a" ^ string_of_int i)) in
      let call_expr = Ml.AppE (Ml.LitE ocaml_name, arg_vars) in
      let marshal_ret = Interface.interface_name typ_ret in
      let run_pass =
        Ml.AppE
          ( Ml.LitE "Run.Pass",
            [ Ml.AppE (Ml.VarE ("marshal_" ^ marshal_ret), [ call_expr ]) ] )
      in
      let body =
        List.fold_right
          (fun (var_name, rhs) acc -> Ml.LetE (Ml.VarP var_name, rhs, acc))
          unmarshal_lets run_pass
      in
      Some body
  in
  (* Arms for poly dispatch (dispatch_table entries) *)
  let poly_arms =
    Hashtbl.fold
      (fun orig_name instances acc ->
        let fail_no_inst =
          run_fail
            (Printf.sprintf "eval_func: no matching instance for %s" orig_name)
        in
        let arm_body =
          List.fold_right
            (fun (inst : Mono.poly_instance) else_expr ->
              let targs_exprs =
                List.map Interface.typ_make_expr inst.concrete_targs
              in
              let guard =
                Ml.AppE
                  ( Ml.LitE "Il.Eq.eq_typs",
                    [ Ml.VarE "typs__"; Ml.ListE targs_exprs ] )
              in
              let mangled = inst.mangled_name in
              let ocaml_name = "f__" ^ Names.sanitize mangled in
              let then_expr =
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
                    | Some body -> body)
              in
              Ml.IfE (guard, then_expr, Some else_expr))
            instances fail_no_inst
        in
        (Ml.LitP ("\"" ^ orig_name ^ "\""), arm_body) :: acc)
      dispatch_table []
  in
  (* Arms for mono functions (not in dispatch_table) *)
  let mono_arms =
    List.filter_map
      (fun def ->
        let make_arm id params typ_ret =
          let name = id.it in
          if Hashtbl.mem poly_names name then None
          else
            let ocaml_name = Names.func id in
            let wrong_targs =
              run_fail (Printf.sprintf "eval_func: wrong type args for %s" name)
            in
            let arm_body =
              match build_dispatch_body params typ_ret ocaml_name with
              | None ->
                  run_fail
                    (Printf.sprintf
                       "eval_func: higher-order parameter not supported: %s"
                       name)
              | Some body ->
                  Ml.IfE
                    ( Ml.BinopE ("=", Ml.VarE "typs__", Ml.ListE []),
                      body,
                      Some wrong_targs )
            in
            Some (Ml.LitP ("\"" ^ name ^ "\""), arm_body)
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
  let wild_arm =
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
  let all_arms = poly_arms @ mono_arms @ [ wild_arm ] in
  let match_expr = Ml.MatchE (Ml.VarE "name__", all_arms) in
  (* Unmatch handler *)
  let fail_unmatch =
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
  let try_expr =
    Ml.TryE
      ( match_expr,
        [ (Ml.VariantP (`Mono ("Unmatch", [ Ml.VarP "msg_" ])), fail_unmatch) ]
      )
  in
  let params_ml =
    [
      ("name__", Some (Ml.NameT "string"));
      ("typs__", Some (Ml.AppT ("list", [ Ml.NameT "Typ.t" ])));
      ("args__", Some (Ml.AppT ("list", [ Ml.NameT "Value.t" ])));
    ]
  in
  ("eval_func", params_ml, Some (Ml.NameT "Run.func_result"), try_expr)
