open Lang
open Sl

(* Parameters *)

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
  (* [Ml.typ] has no arrow case; build one as a parenthesized name string. *)
  let str_arrow_ml =
    "(" ^ String.concat " -> " (List.map Ml.Print.print_typ typs_arrow_ml) ^ ")"
  in
  let param_ml = (id_ml, Some (Ml.NameT str_arrow_ml)) in
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

(* Witness parameters for a generic extern/builtin bridge: for each of the
   function's own tparams, a marshal/unmarshal pair the caller must supply. *)
let compile_witness_params (tparams_ml : string list) : Ml.param list =
  List.concat_map
    (fun tv ->
      [
        ( Interface.witness_marshal_name tv,
          Some (Ml.NameT (Printf.sprintf "('%s -> Value.t)" tv)) );
        ( Interface.witness_unmarshal_name tv,
          Some (Ml.NameT (Printf.sprintf "(Value.t -> '%s)" tv)) );
      ])
    tparams_ml

let compile_targ_reify (tvars : string list) : Ml.expr =
  Ml.ListE
    (List.map
       (fun tv ->
         Ml.AppE (Ml.LitE "make_typ_var_", [ Ml.StrE tv; Ml.ListE [] ]))
       tvars)

(* Bind [resolve_expr] before applying — it may be a bare lambda, and Ml's
   printer never parenthesizes an [AppE]'s function-position expression. *)
let apply_witness (tag : string) (resolve_expr : Ml.expr) (arg_expr : Ml.expr) :
    Ml.expr =
  let w_id = "w__" ^ tag in
  Ml.LetE (Ml.VarP w_id, resolve_expr, Ml.AppE (Ml.VarE w_id, [ arg_expr ]))

(* Extern functions: forward tparam witnesses to marshal/unmarshal at the
   boundary, and reify the tparams as the call's own runtime targs. A
   non-generic extern is just [tparams = []], no special case needed. *)

let compile_extern_func (ctx : Ctx.t) (id : id)
    (tparams : Il.tparam list) (params : param list) (typ_ret : typ) :
    Ctx.t * Ml.funcdef list =
  let id_ml = Names.func id in
  let tvars = List.map (fun (tp : Il.tparam) -> tp.it) tparams in
  let tparams_ml = List.map Names.tvar tparams in
  let typ_ret_ml = Type.compile_typ ~tparams:tvars typ_ret in
  let typs_param =
    List.filter_map
      (fun (param : param) ->
        match param.it with ExpP (typ, _) -> Some typ | _ -> None)
      params
  in
  let n = List.length typs_param in
  let params_ml =
    compile_witness_params tparams_ml
    @ List.mapi
        (fun i typ ->
          ("p__" ^ string_of_int i, Some (Type.compile_typ ~tparams:tvars typ)))
        typs_param
  in
  let vars_marshal_ml, exprs_marshal_ml =
    List.mapi
      (fun i typ ->
        ( "v__" ^ string_of_int i,
          apply_witness (string_of_int i)
            (Interface.resolve_marshal ctx tvars typ)
            (Ml.VarE ("p__" ^ string_of_int i)) ))
      typs_param
    |> List.split
  in
  let exprs_arg_ml =
    Ml.ListE (List.init n (fun i -> Ml.VarE ("v__" ^ string_of_int i)))
  in
  let exprs_targ_ml = compile_targ_reify tvars in
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
            apply_witness "ret__"
              (Interface.resolve_unmarshal ctx tvars typ_ret)
              (Ml.VarE "v_out__") );
          ( Ml.VariantP (`Mono ("Run.Fail", [ Ml.WildP; Ml.VarP "msg__" ])),
            Ml.AppE
              ( Ml.LitE "raise",
                [ Ml.AppE (Ml.LitE "Unmatch", [ Ml.VarE "msg__" ]) ] ) );
        ] )
  in
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

(* Builtin functions — mirrors [compile_extern_func] but for
   [call_builtin]'s calling convention. *)

let compile_builtin_func (ctx : Ctx.t) (id : id)
    (tparams : Il.tparam list) (params : param list) (typ_ret : typ) :
    Ctx.t * Ml.funcdef list =
  let id_ml = Names.func id in
  let tvars = List.map (fun (tp : Il.tparam) -> tp.it) tparams in
  let tparams_ml = List.map Names.tvar tparams in
  let typ_ret_ml = Type.compile_typ ~tparams:tvars typ_ret in
  let typs_param =
    List.filter_map
      (fun (param : param) ->
        match param.it with ExpP (typ, _) -> Some typ | _ -> None)
      params
  in
  let n = List.length typs_param in
  let params_ml =
    compile_witness_params tparams_ml
    @ List.mapi
        (fun i typ ->
          ("p__" ^ string_of_int i, Some (Type.compile_typ ~tparams:tvars typ)))
        typs_param
  in
  let vars_marshal_ml, exprs_marshal_ml =
    List.mapi
      (fun i typ ->
        ( "v__" ^ string_of_int i,
          apply_witness (string_of_int i)
            (Interface.resolve_marshal ctx tvars typ)
            (Ml.VarE ("p__" ^ string_of_int i)) ))
      typs_param
    |> List.split
  in
  let exprs_arg_ml =
    Ml.ListE (List.init n (fun i -> Ml.VarE ("v__" ^ string_of_int i)))
  in
  let exprs_targ_ml = compile_targ_reify tvars in
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
  let expr_result_ml =
    Ml.LetE
      ( Ml.VarP "v_out__",
        expr_try_ml,
        apply_witness "ret__"
          (Interface.resolve_unmarshal ctx tvars typ_ret)
          (Ml.VarE "v_out__") )
  in
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

(* Table functions *)

let rec compile_table_func (ctx : Ctx.t) (id : id) (params : param list)
    (typ_ret : typ) (tablerows : tablerow list) : Ctx.t * Ml.funcdef list =
  let block = List.concat_map (fun (_, _, block_row) -> block_row) tablerows in
  compile_defined_func_body ~tparams:[] ~tparams_ml:[] ctx id params typ_ret
    block None

(* Defined functions *)

(* A generic function's body forwards its own witnesses to sibling/boundary
   calls, so its main__/else__/dispatcher helpers must accept them too. *)
and compile_defined_func_body ~(tparams : string list)
    ~(tparams_ml : string list) (ctx : Ctx.t) (id : id) (params : param list)
    (typ_ret : typ) (block_main : block) (elseblock_opt : block option) :
    Ctx.t * Ml.funcdef list =
  let id_ml = Names.func id in
  let typ_ret_ml = Type.compile_typ ~tparams typ_ret in
  let ctx_outer = ctx in
  (* Compile parameters *)
  let witness_params_ml = compile_witness_params tparams_ml in
  let ctx, params_ml, chain = compile_params ~tparams ctx params in
  let params_ml = witness_params_ml @ params_ml in
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
  let dispatch_ml =
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
    (id_ml, [], params_ml, Some typ_ret_ml, dispatch_ml)
  in
  (* Collect function definitions *)
  let funcdefs_ml =
    let else_list = Option.to_list funcdef_else_ml_opt in
    (funcdef_main_ml :: else_list) @ [ funcdef_dispatcher_ml ]
  in
  (ctx, funcdefs_ml)

let compile_defined_func (ctx : Ctx.t) (definedfunc : definedfunc) :
    Ctx.t * Ml.funcdef list =
  let id, tparams, params, typ_ret, block_main, elseblock_opt, _ =
    definedfunc
  in
  let tparams_str = List.map (fun (tp : Il.tparam) -> tp.it) tparams in
  let tparams_ml = List.map Names.tvar tparams in
  let ctx, funcdefs =
    compile_defined_func_body ~tparams:tparams_str ~tparams_ml ctx id params
      typ_ret block_main elseblock_opt
  in
  ( ctx,
    List.map
      (fun (name, _tparams, params, ret, body) ->
        (name, tparams_ml, params, ret, body))
      funcdefs )

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
