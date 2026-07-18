open Domain
open Lang
open Sl
open Util.Source

(* Failures *)

let run_fail_msg (msg : string) : Ml.expr =
  Ml.AppE
    (Ml.LitE "Run.Fail", [ Ml.TupleE [ Ml.LitE "no_region"; Ml.StrE msg ] ])

let run_fail_fmt (s_fmt : string) (exprs : Ml.expr list) : Ml.expr =
  Ml.AppE
    ( Ml.LitE "Run.Fail",
      [
        Ml.TupleE
          [
            Ml.LitE "no_region";
            Ml.AppE (Ml.LitE "Printf.sprintf", Ml.StrE s_fmt :: exprs);
          ];
      ] )

(* Dispatcher *)

let compile_dispatcher (name : string)
    (params_ml : (string * Ml.typ option) list) (typ_ret_ml : Ml.typ)
    (msg_unknown : string) (arms_ml : Ml.arm list) (arms_extra_ml : Ml.arm list)
    : Ml.funcdef =
  let expr_match_ml =
    let arm_wild_ml =
      (Ml.WildP, run_fail_fmt msg_unknown [ Ml.VarE "name__" ])
    in
    Ml.MatchE (Ml.VarE "name__", arms_ml @ [ arm_wild_ml ])
  in
  let arm_unmatch_ml =
    ( Ml.VariantP (`Mono ("Unmatch", [ Ml.VarP "msg_" ])),
      run_fail_fmt
        (Printf.sprintf "%s: unmatch in %%s: %%s" name)
        [ Ml.VarE "name__"; Ml.VarE "msg_" ] )
  in
  let expr_try_ml = Ml.TryE (expr_match_ml, arm_unmatch_ml :: arms_extra_ml) in
  (name, [], params_ml, Some typ_ret_ml, expr_try_ml)

(* Runtime function dispatcher: [eval_func name typs args]

   [let eval_func name__ typs__ args__ =
      try match name__ with
        | "f" ->
            let marshal__x, unmarshal__x = find_converter_dynamic (List.nth typs__ i) in ..
            let a0 = unmarshal__.. (List.nth args__ 0) in ..
            Run.Pass (marshal__.. (f__f marshal__x unmarshal__x .. a0 ..))
        | .. -> Run.Fail (no_region, "eval_func: unknown function: %s" name__)
      with Unmatch msg_ -> Run.Fail (no_region, ..)
         | NoConverter tyname__ -> Run.Fail (no_region, ..)] *)

let compile_converter_binding (tparams_ml : string list) :
    (string * Ml.expr) list =
  tparams_ml
  |> List.mapi (fun i tparam_ml ->
         (* [find_converter_dynamic (List.nth typs__ i)] *)
         let id_entry_ml = "entry__" ^ string_of_int i in
         let expr_lookup_ml =
           Ml.AppE
             ( Ml.LitE "find_converter_dynamic",
               [
                 Ml.AppE
                   ( Ml.LitE "List.nth",
                     [ Ml.VarE "typs__"; Ml.LitE (string_of_int i) ] );
               ] )
         in
         let binding_converter_ml = (id_entry_ml, expr_lookup_ml) in
         (* [let marshal__x, unmarshal__x = ..] *)
         let id_marshal_ml = Interface.Converter.name_marshal tparam_ml in
         let expr_marshal_ml =
           let typ_marshal_ml =
             Ml.FuncT (Ml.VarT tparam_ml, Ml.NameT "Value.t")
           in
           Ml.AnnotE
             ( Ml.AppE
                 ( Ml.LitE "Obj.obj",
                   [ Ml.AppE (Ml.LitE "fst", [ Ml.VarE id_entry_ml ]) ] ),
               typ_marshal_ml )
         in
         let binding_marshal_ml = (id_marshal_ml, expr_marshal_ml) in
         let id_unmarshal_ml = Interface.Converter.name_unmarshal tparam_ml in
         let expr_unmarshal_ml =
           let typ_unmarshal_ml =
             Ml.FuncT (Ml.NameT "Value.t", Ml.VarT tparam_ml)
           in
           Ml.AnnotE
             ( Ml.AppE
                 ( Ml.LitE "Obj.obj",
                   [ Ml.AppE (Ml.LitE "snd", [ Ml.VarE id_entry_ml ]) ] ),
               typ_unmarshal_ml )
         in
         let binding_unmarshal_ml = (id_unmarshal_ml, expr_unmarshal_ml) in
         [ binding_converter_ml; binding_marshal_ml; binding_unmarshal_ml ])
  |> List.concat

let compile_func_arm_body (ctx : Ctx.t) (name : string) (tparams : string list)
    (tparams_ml : string list) (id_func_ml : string) (typs_param : Sl.typ list)
    (typ_ret : Sl.typ) : Ml.expr =
  (* [let marshal__x, unmarshal__x = find_converter_dynamic (..) in ..] *)
  let bindings_converter_ml = compile_converter_binding tparams_ml in
  (* [marshal__x unmarshal__x ..], the leading args of [f__f ..] *)
  let exprs_converter_ml =
    List.concat_map
      (fun tparam ->
        [
          Ml.VarE (Interface.Converter.name_marshal tparam);
          Ml.VarE (Interface.Converter.name_unmarshal tparam);
        ])
      tparams_ml
  in
  (* [let a0 = unmarshal__.. (List.nth args__ 0) in ..] *)
  let vars_arg_ml, exprs_unmarshal_arg_ml =
    typs_param
    |> List.mapi (fun i typ ->
           let converter = Interface.Converter.resolve ctx tparams typ in
           ( "a" ^ string_of_int i,
             Interface.Converter.apply_converter (string_of_int i)
               converter.unmarshal
               (Ml.AppE
                  ( Ml.LitE "List.nth",
                    [ Ml.VarE "args__"; Ml.LitE (string_of_int i) ] )) ))
    |> List.split
  in
  let exprs_arg_ml =
    List.map (fun var_arg_ml -> Ml.VarE var_arg_ml) vars_arg_ml
  in
  let bindings_arg_ml = List.combine vars_arg_ml exprs_unmarshal_arg_ml in
  (* [f__f marshal__x unmarshal__x .. a0 ..] *)
  let expr_call_ml =
    Ml.AppE (Ml.LitE id_func_ml, exprs_converter_ml @ exprs_arg_ml)
  in
  (* [Run.Pass (marshal__.. (..))] *)
  let converter_ret = Interface.Converter.resolve ctx tparams typ_ret in
  let expr_marshal_ret_ml =
    Interface.Converter.apply_converter "ret__" converter_ret.marshal
      expr_call_ml
  in
  let expr_pass_ml = Ml.AppE (Ml.LitE "Run.Pass", [ expr_marshal_ret_ml ]) in
  (* [let .. in ..] chain wrapping the call above *)
  let expr_ok_ml =
    List.fold_right
      (fun (var, expr_ml) expr_cont_ml ->
        Ml.LetE (Ml.VarP var, expr_ml, expr_cont_ml))
      (bindings_converter_ml @ bindings_arg_ml)
      expr_pass_ml
  in
  (* Arity guard around the [| "f" -> ..] arm *)
  let n_tparams = List.length tparams in
  let expr_fail_arity_ml =
    run_fail_fmt
      (Printf.sprintf "eval_func: %s expects %d type argument(s), got %%d" name
         n_tparams)
      [ Ml.AppE (Ml.LitE "List.length", [ Ml.VarE "typs__" ]) ]
  in
  Ml.IfE
    ( Ml.BinopE
        ( "=",
          Ml.AppE (Ml.LitE "List.length", [ Ml.VarE "typs__" ]),
          Ml.LitE (string_of_int n_tparams) ),
      expr_ok_ml,
      Some expr_fail_arity_ml )

let compile_func_arm (ctx : Ctx.t) (name : string) (tparams : Il.tparam list)
    (params : Sl.param list) (typ_ret : Sl.typ) : Ml.arm =
  let is_high_order_func =
    List.exists
      (fun param -> match param.it with DefP _ -> true | _ -> false)
      params
  in
  (* Pattern matching on function name ["f" -> ..] *)
  let pat_ml = Ml.LitP ("\"" ^ name ^ "\"") in
  let expr_func_ml =
    if is_high_order_func then
      run_fail_msg
        (Printf.sprintf "eval_func: higher-order parameter not supported: %s"
           name)
    else
      let tparams_ml = List.map Names.tvar tparams in
      let tparams = List.map it tparams in
      let id_func_ml = "f__" ^ Names.sanitize name in
      let typs_param =
        List.filter_map
          (fun param ->
            match param.it with ExpP (typ, _) -> Some typ | _ -> None)
          params
      in
      compile_func_arm_body ctx name tparams tparams_ml id_func_ml typs_param
        typ_ret
  in
  (pat_ml, expr_func_ml)

let compile_func_arms (ctx : Ctx.t) (spec : Sl.spec) : Ml.arm list =
  List.filter_map
    (fun def ->
      match def.it with
      | BuiltinDecD (id, tparams, params, typ_ret, _)
      | ExternDecD (id, tparams, params, typ_ret, _) ->
          let arm_ml = compile_func_arm ctx id.it tparams params typ_ret in
          Some arm_ml
      | TableDecD (id, params, typ_ret, _, _) ->
          let arm_ml = compile_func_arm ctx id.it [] params typ_ret in
          Some arm_ml
      | FuncDecD (id, tparams, params, typ_ret, _, _, _) ->
          let arm_ml = compile_func_arm ctx id.it tparams params typ_ret in
          Some arm_ml
      | _ -> None)
    spec

let compile_eval_func (ctx : Ctx.t) (spec : Sl.spec) : Ml.funcdef =
  let params_ml =
    [
      ("name__", Some (Ml.NameT "string"));
      ("typs__", Some (Ml.AppT ("list", [ Ml.NameT "Typ.t" ])));
      ("args__", Some (Ml.AppT ("list", [ Ml.NameT "Value.t" ])));
    ]
  in
  let arms_ml = compile_func_arms ctx spec in
  let arm_no_interface_ml =
    ( Ml.VariantP (`Mono ("NoConverter", [ Ml.VarP "tyname__" ])),
      run_fail_fmt "eval_func: no marshaller registered for type %s"
        [ Ml.VarE "tyname__" ] )
  in
  compile_dispatcher "eval_func" params_ml (Ml.NameT "Run.func_result")
    "eval_func: unknown function: %s" arms_ml [ arm_no_interface_ml ]

(* Runtime relation dispatcher: [eval_rel name args]

   [let eval_rel name__ args__ =
      try match name__ with
        | "r" ->
            let a0 = unmarshal__.. (List.nth args__ 0) in ..
            let out0, .. = r__r a0 .. in
            Run.Pass [marshal__.. (out0); ..]
        | .. -> Run.Fail (no_region, "eval_rel: unknown relation: %s" name__)
      with Unmatch msg_ -> Run.Fail (no_region, ..)] *)

let compile_rel_arm_body (id_rel_ml : string) (typs_input : Sl.typ list)
    (typs_output : Sl.typ list) : Ml.expr =
  (* [let a0 = unmarshal__.. (List.nth args__ 0) in ..] *)
  let vars_arg_ml, exprs_unmarshal_arg_ml =
    typs_input
    |> List.mapi (fun i typ ->
           let converter = Interface.Converter.resolve_ground typ in
           ( "a" ^ string_of_int i,
             Ml.AppE
               ( converter.unmarshal,
                 [
                   Ml.AppE
                     ( Ml.LitE "List.nth",
                       [ Ml.VarE "args__"; Ml.LitE (string_of_int i) ] );
                 ] ) ))
    |> List.split
  in
  let exprs_arg_ml =
    List.map (fun var_arg_ml -> Ml.VarE var_arg_ml) vars_arg_ml
  in
  let bindings_arg_ml = List.combine vars_arg_ml exprs_unmarshal_arg_ml in
  (* [r__r a0 ..] *)
  let expr_call_ml = Ml.AppE (Ml.VarE id_rel_ml, exprs_arg_ml) in
  match typs_output with
  | [] ->
      (* [Run.Pass []], sequenced after the call *)
      let expr_seq_ml =
        Ml.LetE
          (Ml.WildP, expr_call_ml, Ml.AppE (Ml.LitE "Run.Pass", [ Ml.ListE [] ]))
      in
      List.fold_right
        (fun (var, expr_ml) expr_cont_ml ->
          Ml.LetE (Ml.VarP var, expr_ml, expr_cont_ml))
        bindings_arg_ml expr_seq_ml
  | _ ->
      let n_out = List.length typs_output in
      let ids_out_ml = List.init n_out (fun i -> "out" ^ string_of_int i) in
      (* [let out0, .. = r__r a0 .. in ..] *)
      let pat_out_ml =
        match ids_out_ml with
        | [ id_out_ml ] -> Ml.VarP id_out_ml
        | _ -> Ml.TupleP (List.map (fun s -> Ml.VarP s) ids_out_ml)
      in
      (* [Run.Pass [marshal__.. (out0); ..]] *)
      let exprs_marshal_out_ml =
        List.mapi
          (fun i typ ->
            let converter = Interface.Converter.resolve_ground typ in
            Ml.AppE (converter.marshal, [ Ml.VarE ("out" ^ string_of_int i) ]))
          typs_output
      in
      let expr_pass_ml =
        Ml.AppE (Ml.LitE "Run.Pass", [ Ml.ListE exprs_marshal_out_ml ])
      in
      let expr_bind_ml = Ml.LetE (pat_out_ml, expr_call_ml, expr_pass_ml) in
      List.fold_right
        (fun (var, expr_ml) expr_cont_ml ->
          Ml.LetE (Ml.VarP var, expr_ml, expr_cont_ml))
        bindings_arg_ml expr_bind_ml

let compile_rel_arm (id : Sl.id) (typs_input : Sl.typ list)
    (typs_output : Sl.typ list) : Ml.arm =
  (* Pattern matching on relation name ["r" -> ..] *)
  let pat_ml = Ml.LitP ("\"" ^ id.it ^ "\"") in
  let id_rel_ml = Names.rel id in
  let expr_func_ml = compile_rel_arm_body id_rel_ml typs_input typs_output in
  (pat_ml, expr_func_ml)

let compile_rel_arms (spec : Sl.spec) : Ml.arm list =
  List.filter_map
    (fun def ->
      match def.it with
      | RelD (id, (nottyp, inputs), _, _, _, _)
      | ExternRelD (id, (nottyp, inputs), _, _) ->
          let typs_rel = Mixfix.args nottyp.it in
          let typs_input, typs_output = Hints.Input.split inputs typs_rel in
          let arm_ml = compile_rel_arm id typs_input typs_output in
          Some arm_ml
      | _ -> None)
    spec

let compile_eval_rel (ctx : Ctx.t) (spec : Sl.spec) : Ml.funcdef =
  ignore ctx;
  let params_ml =
    [
      ("name__", Some (Ml.NameT "string"));
      ("args__", Some (Ml.AppT ("list", [ Ml.NameT "Value.t" ])));
    ]
  in
  let arms_ml = compile_rel_arms spec in
  compile_dispatcher "eval_rel" params_ml (Ml.NameT "Run.rel_result")
    "eval_rel: unknown relation: %s" arms_ml []
