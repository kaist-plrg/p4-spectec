open Lang
open Sl
open Util.Source

(* Parameters *)

(* Value parameter: [typ exp]

   [(param__i : typ_ml)], plus a bind chain that destructures [exp]'s
   pattern out of the [param__i] stub *)

let compile_exp_param ~(index : int option) ~(tparams : string list)
    (ctx : Ctx.t) (typ : typ) (exp : exp) : Ctx.t * Ml.param * Chain.t =
  let id_stub_ml =
    "param__" ^ (index |> Option.map string_of_int |> Option.value ~default:"")
  in
  let expr_stub_ml = Ml.VarE id_stub_ml in
  let typ_ml = Type.compile_typ ~tparams typ in
  let param_ml = (id_stub_ml, Some typ_ml) in
  let ctx, chain = Bind.compile ~tparams ctx expr_stub_ml exp in
  (ctx, param_ml, chain)

(* Def parameter: [def $g(t1, .., tn) : tret]

   [(g_id : (t1_ml -> .. -> tn_ml -> tret_ml))] *)

let compile_def_param ~(tparams : string list) (ctx : Ctx.t) (id : id)
    (params : param list) (typ_ret : typ) : Ctx.t * Ml.param * Chain.t =
  let id_ml = Names.func id in
  let ctx = Ctx.add_binding ctx (id, []) id_ml in
  let typs_param =
    List.filter_map
      (fun (param : param) ->
        match param.it with ExpP (typ, _) -> Some typ | _ -> None)
      params
  in
  let typs_arrow_ml =
    List.map (Type.compile_typ ~tparams) typs_param
    @ [ Type.compile_typ ~tparams typ_ret ]
  in
  let typ_arrow_ml =
    match List.rev typs_arrow_ml with
    | typ_ret_ml :: typs_param_ml_rev ->
        List.fold_left
          (fun typ_cod typ_dom -> Ml.FuncT (typ_dom, typ_cod))
          typ_ret_ml typs_param_ml_rev
    | [] -> assert false
  in
  let param_ml = (id_ml, Some typ_arrow_ml) in
  let chain = Chain.nop in
  (ctx, param_ml, chain)

let compile_param ~(index : int option) ~(tparams : string list) (ctx : Ctx.t)
    (param : param) : Ctx.t * Ml.param * Chain.t =
  match param.it with
  | ExpP (typ, exp) -> compile_exp_param ~index ~tparams ctx typ exp
  | DefP (id, _, params, typ_ret) ->
      compile_def_param ~tparams ctx id params typ_ret

let compile_params ~(tparams : string list) (ctx : Ctx.t) (params : param list)
    : Ctx.t * Ml.param list * Chain.t =
  params
  |> List.mapi (fun idx param -> (idx, param))
  |> List.fold_left
       (fun (ctx, params_ml, chain_acc) (idx, param) ->
         let ctx, param_ml, chain =
           compile_param ~index:(Some idx) ~tparams ctx param
         in
         let params_ml = params_ml @ [ param_ml ] in
         let chain = Chain.connect [ chain_acc; chain ] in
         (ctx, params_ml, chain))
       (ctx, [], Chain.nop)

(* Type parameters

   Each type parameter becomes an OCaml type variable ('x), and for each one it
   takes an extra pair of parameters — a converter — [marshal__x : 'x -> Value.t]
   / [unmarshal__x : Value.t -> 'x] — converting between that abstract type and
   the runtime's uniform [Value.t]

   At a call site, [Interface.Converter.resolve] resolves each callee type
   parameter's converter *)

let compile_tparams (tparams_ml : Ml.tparam list) : Ml.param list =
  List.concat_map
    (fun tparam_ml ->
      [
        ( Interface.Converter.name_marshal tparam_ml,
          Some (Ml.FuncT (Ml.VarT tparam_ml, Ml.NameT "Value.t")) );
        ( Interface.Converter.name_unmarshal tparam_ml,
          Some (Ml.FuncT (Ml.NameT "Value.t", Ml.VarT tparam_ml)) );
      ])
    tparams_ml

(* Type arguments

   Reifies a generic call's own tparams as runtime targs: [<X, ..>]

   [[make_typ_var_ "X" []; ..]] *)

let compile_targs (tparams : string list) : Ml.expr =
  Ml.ListE
    (List.map
       (fun tparam ->
         Ml.AppE (Ml.LitE "make_typ_var_", [ Ml.StrE tparam; Ml.ListE [] ]))
       tparams)

(* Extern function: [extern def $f<X, ..>(t1, .., tn) : tret]

   [let f_id (marshal__x, unmarshal__x, ..) (p__0 : t1_ml) .. =
      let v__0 = marshal__0 (p__0) in ..
      match eval_extern_func "f" (compile_targs) [v__0; ..] with
      | Run.Pass v_out__ -> unmarshal__ret (v_out__)
      | Run.Fail (_, msg__) -> raise (Unmatch msg__)] *)

let compile_extern_func (ctx : Ctx.t) (id : id) (tparams : Il.tparam list)
    (params : param list) (typ_ret : typ) : Ctx.t * Ml.funcdef list =
  let id_ml = Names.func id in
  let tparams_ml = List.map Names.tvar tparams in
  let tparams = List.map it tparams in
  let typ_ret_ml = Type.compile_typ ~tparams typ_ret in
  (* Compile parameters *)
  let typs_param =
    List.filter_map
      (fun (param : param) ->
        match param.it with ExpP (typ, _) -> Some typ | _ -> None)
      params
  in
  let n = List.length typs_param in
  let params_ml =
    compile_tparams tparams_ml
    @ List.mapi
        (fun i typ ->
          ("p__" ^ string_of_int i, Some (Type.compile_typ ~tparams typ)))
        typs_param
  in
  (* Marshal each parameter before crossing into the extern *)
  let vars_marshal_ml, exprs_marshal_ml =
    List.mapi
      (fun i typ ->
        ( "v__" ^ string_of_int i,
          Interface.Converter.apply_converter (string_of_int i)
            (Interface.Converter.resolve ctx tparams typ).marshal
            (Ml.VarE ("p__" ^ string_of_int i)) ))
      typs_param
    |> List.split
  in
  let exprs_arg_ml =
    Ml.ListE (List.init n (fun i -> Ml.VarE ("v__" ^ string_of_int i)))
  in
  let exprs_targ_ml = compile_targs tparams in
  (* Call the extern, unmarshal its result *)
  let expr_call_ml =
    Ml.AppE
      ( Common.extern_field "eval_extern_func",
        [ Ml.StrE id.it; exprs_targ_ml; exprs_arg_ml ] )
  in
  let expr_result_ml =
    Ml.MatchE
      ( expr_call_ml,
        [
          ( Ml.VariantP (`Mono ("Run.Pass", [ Ml.VarP "v_out__" ])),
            Interface.Converter.apply_converter "ret__"
              (Interface.Converter.resolve ctx tparams typ_ret).unmarshal
              (Ml.VarE "v_out__") );
          ( Ml.VariantP (`Mono ("Run.Fail", [ Ml.WildP; Ml.VarP "msg__" ])),
            Ml.AppE
              ( Ml.LitE "raise",
                [ Ml.AppE (Ml.LitE "Unmatch", [ Ml.VarE "msg__" ]) ] ) );
        ] )
  in
  (* Chain the marshal lets ahead of the call *)
  let expr_body_ml =
    List.fold_right
      (fun (var_marshal_ml, expr_marshal_ml) expr_body_ml ->
        Ml.LetE (Ml.VarP var_marshal_ml, expr_marshal_ml, expr_body_ml))
      (List.combine vars_marshal_ml exprs_marshal_ml)
      expr_result_ml
  in
  let funcdef_ml =
    ( id_ml,
      tparams_ml,
      params_ml,
      Some typ_ret_ml,
      Common.deref_ctx expr_body_ml )
  in
  (ctx, [ funcdef_ml ])

(* Builtin function: [builtin def $f<X, ..>(t1, .., tn) : tret]

   [let f_id (marshal__x, unmarshal__x, ..) (p__0 : t1_ml) .. =
      let v__0 = marshal__0 (p__0) in ..
      let v_out__ =
        try call_builtin (fun _ -> ()) "f" (compile_targs) [v__0; ..]
        with Util.Error.BuiltinError (_, msg__) -> raise (Unmatch msg__)
      in
      unmarshal__ret (v_out__)] *)

let compile_builtin_func (ctx : Ctx.t) (id : id) (tparams : Il.tparam list)
    (params : param list) (typ_ret : typ) : Ctx.t * Ml.funcdef list =
  let id_ml = Names.func id in
  let tparams_ml = List.map Names.tvar tparams in
  let tparams = List.map it tparams in
  let typ_ret_ml = Type.compile_typ ~tparams typ_ret in
  (* Compile parameters *)
  let typs_param =
    List.filter_map
      (fun (param : param) ->
        match param.it with ExpP (typ, _) -> Some typ | _ -> None)
      params
  in
  let n = List.length typs_param in
  let params_ml =
    compile_tparams tparams_ml
    @ List.mapi
        (fun i typ ->
          ("p__" ^ string_of_int i, Some (Type.compile_typ ~tparams typ)))
        typs_param
  in
  (* Marshal each parameter before crossing into the builtin *)
  let vars_marshal_ml, exprs_marshal_ml =
    List.mapi
      (fun i typ ->
        ( "v__" ^ string_of_int i,
          Interface.Converter.apply_converter (string_of_int i)
            (Interface.Converter.resolve ctx tparams typ).marshal
            (Ml.VarE ("p__" ^ string_of_int i)) ))
      typs_param
    |> List.split
  in
  let exprs_arg_ml =
    Ml.ListE (List.init n (fun i -> Ml.VarE ("v__" ^ string_of_int i)))
  in
  let exprs_targ_ml = compile_targs tparams in
  (* Call the builtin, catching a builtin error as [Unmatch] *)
  let name_orig_lit_ml =
    Ml.LitE (Printf.sprintf "(\"%s\" $ no_region)" (String.escaped id.it))
  in
  let expr_call_ml =
    Ml.AppE
      ( Common.iface_field "call_builtin",
        [
          Ml.LitE "(fun _ -> ())"; name_orig_lit_ml; exprs_targ_ml; exprs_arg_ml;
        ] )
  in
  let expr_try_ml =
    Ml.TryE
      ( expr_call_ml,
        [
          ( Ml.VariantP
              (`Mono ("Util.Error.BuiltinError", [ Ml.WildP; Ml.VarP "msg__" ])),
            Ml.AppE
              ( Ml.LitE "raise",
                [ Ml.AppE (Ml.LitE "Unmatch", [ Ml.VarE "msg__" ]) ] ) );
        ] )
  in
  (* Unmarshal the result *)
  let expr_result_ml =
    Ml.LetE
      ( Ml.VarP "v_out__",
        expr_try_ml,
        Interface.Converter.apply_converter "ret__"
          (Interface.Converter.resolve ctx tparams typ_ret).unmarshal
          (Ml.VarE "v_out__") )
  in
  (* Chain the marshal lets ahead of the call *)
  let expr_body_ml =
    List.fold_right
      (fun (var_marshal_ml, expr_marshal_ml) expr_body_ml ->
        Ml.LetE (Ml.VarP var_marshal_ml, expr_marshal_ml, expr_body_ml))
      (List.combine vars_marshal_ml exprs_marshal_ml)
      expr_result_ml
  in
  let funcdef_ml =
    ( id_ml,
      tparams_ml,
      params_ml,
      Some typ_ret_ml,
      Common.deref_ctx expr_body_ml )
  in
  (ctx, [ funcdef_ml ])

(* Table function: [tbl def $f(t1, .., tn) : tret = { rows }] *)

let rec compile_table_func (ctx : Ctx.t) (id : id) (params : param list)
    (typ_ret : typ) (tablerows : tablerow list) : Ctx.t * Ml.funcdef list =
  let block = List.concat_map (fun (_, _, block_row) -> block_row) tablerows in
  compile_defined_func_body ~tparams:[] ~tparams_ml:[] ctx id params typ_ret
    block None

(* Defined function

   [def $f<X, ..>(t1, .., tn) : tret = { block } [else { elseblock }]]

   [let main__f_id (marshal__x, unmarshal__x, ..) (p__0 : t1_ml) .. =
      <compile_block block>
    and else__f_id .. = <compile_block elseblock>          (* if present *)
    and f_id .. =
      try main__f_id .. with Unmatch _ -> else__f_id ..]   (* if present *) *)

and compile_defined_func_body ~(tparams : string list)
    ~(tparams_ml : string list) (ctx : Ctx.t) (id : id) (params : param list)
    (typ_ret : typ) (block_main : block) (elseblock_opt : block option) :
    Ctx.t * Ml.funcdef list =
  let id_ml = Names.func id in
  let typ_ret_ml = Type.compile_typ ~tparams typ_ret in
  let ctx_outer = ctx in
  (* Compile parameters *)
  let params_converter_ml = compile_tparams tparams_ml in
  let ctx, params_ml, chain = compile_params ~tparams ctx params in
  let params_ml = params_converter_ml @ params_ml in
  let ids_param_ml = List.map (fun (id_param_ml, _) -> id_param_ml) params_ml in
  (* Compile main block *)
  let id_main_ml = "main__" ^ id_ml in
  let ctx, funcdef_main_ml =
    let ctx, expr_block_ml = Instr.compile_block ~tparams ctx block_main in
    let expr_ml = Chain.apply chain expr_block_ml in
    let funcdef_main_ml =
      (id_main_ml, [], params_ml, Some typ_ret_ml, expr_ml)
    in
    (ctx, funcdef_main_ml)
  in
  (* Compile else block *)
  let id_else_ml = "else__" ^ id_ml in
  let ctx, funcdef_else_ml_opt =
    match elseblock_opt with
    | Some elseblock ->
        let ctx, expr_else_ml = Instr.compile_block ~tparams ctx elseblock in
        let expr_ml = Chain.apply chain expr_else_ml in
        let funcdef_else_ml =
          (id_else_ml, [], params_ml, Some typ_ret_ml, expr_ml)
        in
        (ctx, Some funcdef_else_ml)
    | None -> (ctx, None)
  in
  (* Promote preamble to outer context *)
  let ctx = Ctx.promote_preamble ctx ctx_outer in
  (* Base dispatch expression *)
  let exprs_param_ml =
    List.map (fun id_param_ml -> Ml.VarE id_param_ml) ids_param_ml
  in
  let expr_dispatch_ml =
    match funcdef_else_ml_opt with
    | Some _ ->
        Ml.TryE
          ( Ml.AppE (Ml.VarE id_main_ml, exprs_param_ml),
            [
              ( Ml.VariantP (`Mono ("Unmatch", [ Ml.WildP ])),
                Ml.AppE (Ml.VarE id_else_ml, exprs_param_ml) );
            ] )
    | None -> Ml.AppE (Ml.VarE id_main_ml, exprs_param_ml)
  in
  let funcdef_dispatcher_ml =
    (id_ml, [], params_ml, Some typ_ret_ml, expr_dispatch_ml)
  in
  (* Collect function definitions *)
  let funcdefs_ml =
    let funcdefs_else_ml = Option.to_list funcdef_else_ml_opt in
    (funcdef_main_ml :: funcdefs_else_ml) @ [ funcdef_dispatcher_ml ]
  in
  (ctx, funcdefs_ml)

let compile_defined_func (ctx : Ctx.t) (definedfunc : definedfunc) :
    Ctx.t * Ml.funcdef list =
  let id, tparams, params, typ_ret, block_main, elseblock_opt, _ =
    definedfunc
  in
  let tparams_ml = List.map Names.tvar tparams in
  let tparams = List.map it tparams in
  let ctx, funcdefs_ml =
    compile_defined_func_body ~tparams ~tparams_ml ctx id params typ_ret
      block_main elseblock_opt
  in
  let funcdefs_ml =
    List.map
      (fun (name_ml, _, params_ml, typ_ret_ml, expr_body_ml) ->
        (name_ml, tparams_ml, params_ml, typ_ret_ml, expr_body_ml))
      funcdefs_ml
  in
  (ctx, funcdefs_ml)

(* Defs *)

let compile_def (ctx : Ctx.t) (def : def) : Ctx.t * Ml.funcdef list =
  match def.it with
  | ExternDecD (id, tparams, params, typ_ret, _) ->
      compile_extern_func ctx id tparams params typ_ret
  | BuiltinDecD (id, tparams, params, typ_ret, _) ->
      compile_builtin_func ctx id tparams params typ_ret
  | TableDecD (id, params, typ_ret, tablerows, _) ->
      compile_table_func ctx id params typ_ret tablerows
  | FuncDecD definedfunc -> compile_defined_func ctx definedfunc
  | _ -> (ctx, [])

let compile_defs (ctx : Ctx.t) (defs : def list) : Ctx.t * Ml.funcdef list =
  List.fold_left
    (fun (ctx, funcdefs_ml_acc) def ->
      let ctx, funcdefs_ml = compile_def ctx def in
      (ctx, funcdefs_ml_acc @ funcdefs_ml))
    (ctx, []) defs

(* Spec *)

let compile_group (ctx : Ctx.t) (group : def list) : Ctx.t * Ml.funcdef list =
  compile_defs ctx group
