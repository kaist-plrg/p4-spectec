open Lang
open Sl

(* Reverse dispatch: mangled_name -> (original_name, concrete_targs) *)

type reverse_dispatch = (string, string * Il.typ list) Hashtbl.t

let build_reverse_dispatch (dispatch_table : Mono.dispatch_table) :
    reverse_dispatch =
  let tbl = Hashtbl.create 32 in
  Hashtbl.iter
    (fun name_orig_ml instances ->
      List.iter
        (fun (inst : Mono.poly_instance) ->
          Hashtbl.replace tbl inst.mangled_name
            (name_orig_ml, inst.concrete_targs))
        instances)
    dispatch_table;
  tbl

let lookup_dispatch_info (reverse_dispatch : reverse_dispatch) (id : id) :
    string * Il.typ list =
  match Hashtbl.find_opt reverse_dispatch id.it with
  | Some info -> info
  | None -> (id.it, [])

(* Parameters *)

let compile_exp_param ~(index : int option) (ctx : Ctx.t) (typ : typ)
    (exp : exp) : Ctx.t * Ml.param * Chain.t =
  let id_stub_ml =
    "param__" ^ (index |> Option.map string_of_int |> Option.value ~default:"")
  in
  let expr_stub_ml = Ml.VarE id_stub_ml in
  let typ_ml = Type.compile_typ ~tparams:[] typ in
  let param_ml = (id_stub_ml, Some typ_ml) in
  let ctx, chain = Bind.compile ctx expr_stub_ml exp in
  (ctx, param_ml, chain)

let compile_def_param (ctx : Ctx.t) (id : id) : Ctx.t * Ml.param * Chain.t =
  let id_ml = Names.func id in
  let ctx = Ctx.add_binding ctx (id, []) id_ml in
  let param_ml = (id_ml, None) in
  let chain = Chain.nop in
  (ctx, param_ml, chain)

let compile_param ~(index : int option) (ctx : Ctx.t) (param : param) :
    Ctx.t * Ml.param * Chain.t =
  match param.it with
  | ExpP (typ, exp) -> compile_exp_param ~index ctx typ exp
  | DefP (id, _, _, _) -> compile_def_param ctx id

let compile_params (ctx : Ctx.t) (params : param list) :
    Ctx.t * Ml.param list * Chain.t =
  params
  |> List.mapi (fun idx param -> (idx, param))
  |> List.fold_left
       (fun (ctx, params_ml, chain_acc) (idx, param) ->
         let ctx, param_ml, chain = compile_param ~index:(Some idx) ctx param in
         let params_ml = params_ml @ [ param_ml ] in
         let chain = Chain.connect [ chain_acc; chain ] in
         (ctx, params_ml, chain))
       (ctx, [], Chain.nop)

(* Extern functions *)

let compile_extern_func (ctx : Ctx.t) (reverse_dispatch : reverse_dispatch)
    (id : id) (params : param list) (typ_ret : typ) :
    Ctx.t * Ml.funcdef list * Ml.id option =
  let id_ml = Names.func id in
  let typ_ret_ml = Type.compile_typ ~tparams:[] typ_ret in
  let name_orig_ml, targs = lookup_dispatch_info reverse_dispatch id in
  let typs_param =
    List.filter_map
      (fun (param : param) ->
        match param.it with ExpP (typ, _) -> Some typ | _ -> None)
      params
  in
  let n = List.length typs_param in
  let params_ml =
    List.mapi
      (fun i typ_param ->
        let typ_param_ml = Type.compile_typ ~tparams:[] typ_param in
        ("p__" ^ string_of_int i, Some typ_param_ml))
      typs_param
  in
  let vars_marshal_ml, exprs_marshal_ml =
    List.mapi
      (fun i typ ->
        ( "v__" ^ string_of_int i,
          Ml.AppE
            ( Ml.VarE ("marshal_" ^ Interface.interface_name typ),
              [ Ml.VarE ("p__" ^ string_of_int i) ] ) ))
      typs_param
    |> List.split
  in
  let exprs_arg_ml =
    Ml.ListE (List.init n (fun i -> Ml.VarE ("v__" ^ string_of_int i)))
  in
  let exprs_targ_ml = Ml.ListE (List.map Interface.typ_make_expr targs) in
  let expr_call_ml =
    Ml.AppE
      ( Ml.LitE "Extern.eval_extern_func",
        [ Ml.StrE name_orig_ml; exprs_targ_ml; exprs_arg_ml ] )
  in
  let expr_result_ml =
    Ml.MatchE
      ( expr_call_ml,
        [
          ( Ml.VariantP (`Mono ("Run.Pass", [ Ml.VarP "v_out__" ])),
            Ml.AppE
              ( Ml.VarE ("unmarshal_" ^ Interface.interface_name typ_ret),
                [ Ml.VarE "v_out__" ] ) );
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
  let funcdef_ml = (id_ml, params_ml, Some typ_ret_ml, expr_body_ml) in
  (ctx, [ funcdef_ml ], None)

(* Builtin functions *)

let compile_builtin_func (ctx : Ctx.t) (reverse_dispatch : reverse_dispatch)
    (id : id) (params : param list) (typ_ret : typ) :
    Ctx.t * Ml.funcdef list * Ml.id option =
  let id_ml = Names.func id in
  let typ_ret_ml = Type.compile_typ ~tparams:[] typ_ret in
  let name_orig_ml, targs = lookup_dispatch_info reverse_dispatch id in
  let typs_param =
    List.filter_map
      (fun (param : param) ->
        match param.it with ExpP (typ, _) -> Some typ | _ -> None)
      params
  in
  let n = List.length typs_param in
  let params_ml =
    List.mapi
      (fun i typ ->
        let typ_param_ml = Type.compile_typ ~tparams:[] typ in
        ("p__" ^ string_of_int i, Some typ_param_ml))
      typs_param
  in
  let vars_marshal_ml, exprs_marshal_ml =
    List.mapi
      (fun i typ ->
        ( "v__" ^ string_of_int i,
          Ml.AppE
            ( Ml.VarE ("marshal_" ^ Interface.interface_name typ),
              [ Ml.VarE ("p__" ^ string_of_int i) ] ) ))
      typs_param
    |> List.split
  in
  let exprs_arg_ml =
    Ml.ListE (List.init n (fun i -> Ml.VarE ("v__" ^ string_of_int i)))
  in
  let exprs_targ_ml = Ml.ListE (List.map Interface.typ_make_expr targs) in
  let name_orig_ml =
    Ml.LitE
      (Printf.sprintf "(\"%s\" $ no_region)" (String.escaped name_orig_ml))
  in
  let expr_call_ml =
    Ml.AppE
      ( Ml.LitE "Interface.call_builtin",
        [ Ml.LitE "(fun _ -> ())"; name_orig_ml; exprs_targ_ml; exprs_arg_ml ]
      )
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
        Ml.AppE
          ( Ml.VarE ("unmarshal_" ^ Interface.interface_name typ_ret),
            [ Ml.VarE "v_out__" ] ) )
  in
  let expr_body_ml =
    List.fold_right
      (fun (var_marshal_ml, expr_marshal_ml) expr_body_ml ->
        Ml.LetE (Ml.VarP var_marshal_ml, expr_marshal_ml, expr_body_ml))
      (List.combine vars_marshal_ml exprs_marshal_ml)
      expr_result_ml
  in
  let ids_param_ml = List.map fst params_ml in
  let cache_id_ml = "cache__" ^ id_ml in
  let key_ml = Common.make_cache_key ids_param_ml in
  let expr_cache_ml =
    Common.make_cache_dispatcher cache_id_ml key_ml expr_body_ml
  in
  let funcdef_ml = (id_ml, params_ml, Some typ_ret_ml, expr_cache_ml) in
  (ctx, [ funcdef_ml ], Some cache_id_ml)

(* Table functions *)

let rec compile_table_func (ctx : Ctx.t) (id : id) (params : param list)
    (typ_ret : typ) (tablerows : tablerow list) :
    Ctx.t * Ml.funcdef list * Ml.id option =
  let block = List.concat_map (fun (_, _, block_row) -> block_row) tablerows in
  compile_defined_func_mono ctx id params typ_ret block None

(* Defined functions *)

and compile_defined_func_mono (ctx : Ctx.t) (id : id) (params : param list)
    (typ_ret : typ) (block_main : block) (elseblock_opt : block option) :
    Ctx.t * Ml.funcdef list * Ml.id option =
  let id_ml = Names.func id in
  let typ_ret_ml = Type.compile_typ ~tparams:[] typ_ret in
  let ctx_outer = ctx in
  (* Compile parameters *)
  let ctx, params_ml, chain = compile_params ctx params in
  let ids_param_ml = List.map (fun (id_param_ml, _) -> id_param_ml) params_ml in
  (* Compile main block *)
  let id_main_ml = "main__" ^ id_ml in
  let ctx, funcdef_main_ml =
    let ctx, expr_block_ml = Instr.compile_block ctx block_main in
    let expr_ml = Chain.apply chain expr_block_ml in
    let funcdef_main_ml = (id_main_ml, params_ml, Some typ_ret_ml, expr_ml) in
    (ctx, funcdef_main_ml)
  in
  (* Compile else block *)
  let id_else_ml = "else__" ^ id_ml in
  let ctx, funcdef_else_ml_opt =
    match elseblock_opt with
    | Some elseblock ->
        let ctx, expr_else_ml = Instr.compile_block ctx elseblock in
        let expr_ml = Chain.apply chain expr_else_ml in
        let funcdef_else_ml =
          (id_else_ml, params_ml, Some typ_ret_ml, expr_ml)
        in
        (ctx, Some funcdef_else_ml)
    | None -> (ctx, None)
  in
  (* Promote preamble to outer context *)
  let ctx = Ctx.promote_preamble ctx ctx_outer in
  (* Check for higher-order params (DefP) — uncacheable *)
  let has_defp =
    List.exists
      (fun (param : param) -> match param.it with DefP _ -> true | _ -> false)
      params
  in
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
  (* Compile dispatcher: with cache unless DefP params present *)
  let funcdef_dispatcher_ml, cache_id_opt =
    if has_defp then ((id_ml, params_ml, Some typ_ret_ml, dispatch_ml), None)
    else
      let cache_id_ml = "cache__" ^ id_ml in
      let key_ml = Common.make_cache_key ids_param_ml in
      let expr_dispatcher_ml =
        Common.make_cache_dispatcher cache_id_ml key_ml dispatch_ml
      in
      ((id_ml, params_ml, Some typ_ret_ml, expr_dispatcher_ml), Some cache_id_ml)
  in
  (* Collect function definitions *)
  let funcdefs_ml =
    let else_list = Option.to_list funcdef_else_ml_opt in
    (funcdef_main_ml :: else_list) @ [ funcdef_dispatcher_ml ]
  in
  (ctx, funcdefs_ml, cache_id_opt)

let compile_defined_func (ctx : Ctx.t) (definedfunc : definedfunc) :
    Ctx.t * Ml.funcdef list * Ml.id option =
  let id, tparams, params, typ_ret, block_main, elseblock_opt, _ =
    definedfunc
  in
  if tparams <> [] then (ctx, [], None)
  else compile_defined_func_mono ctx id params typ_ret block_main elseblock_opt

(* Defs *)

let compile_def (ctx : Ctx.t) (reverse_dispatch : reverse_dispatch) (def : def)
    : Ctx.t * Ml.funcdef list * Ml.id option =
  match def.it with
  | ExternDecD (id, [], params, typ_ret, _) ->
      compile_extern_func ctx reverse_dispatch id params typ_ret
  | BuiltinDecD (id, [], params, typ_ret, _) ->
      compile_builtin_func ctx reverse_dispatch id params typ_ret
  | TableDecD (id, params, typ_ret, tablerows, _) ->
      compile_table_func ctx id params typ_ret tablerows
  | FuncDecD definedfunc -> compile_defined_func ctx definedfunc
  | _ -> (ctx, [], None)

let compile_defs (ctx : Ctx.t) (defs : def list)
    (reverse_dispatch : reverse_dispatch) : Ctx.t * Ml.funcdef list * Ml.id list
    =
  List.fold_left
    (fun (ctx, funcdefs_ml_acc, cache_ids_acc) def ->
      let ctx, funcdefs_ml, cache_id_opt =
        compile_def ctx reverse_dispatch def
      in
      let cache_ids_acc =
        match cache_id_opt with
        | Some id -> cache_ids_acc @ [ id ]
        | None -> cache_ids_acc
      in
      (ctx, funcdefs_ml_acc @ funcdefs_ml, cache_ids_acc))
    (ctx, [], []) defs

(* Spec *)

let compile_group (ctx : Ctx.t) (group : def list)
    (dispatch_table : Mono.dispatch_table) :
    Ctx.t * Ml.funcdef list * Ml.id list =
  let reverse_dispatch = build_reverse_dispatch dispatch_table in
  compile_defs ctx group reverse_dispatch

let compile_spec (ctx : Ctx.t) (spec : spec)
    (dispatch_table : Mono.dispatch_table) :
    Ctx.t * Ml.funcdef list * Ml.id list =
  compile_group ctx spec dispatch_table
