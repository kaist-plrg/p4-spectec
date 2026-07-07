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

(* Extern + builtin functions.

   Both cross a [Value.t]-typed runtime boundary that, under the compiled (ML)
   path, actually carries typed [Obj.t] values. So both [Obj.magic] their args
   in and their result out instead of converting. The only per-kind differences
   are the call field, its extra leading args, and how the result/error is
   shaped — captured by [make_result]. *)

let compile_magic_bridge (ctx : Ctx.t) (reverse_dispatch : reverse_dispatch)
    (id : id) (params : param list) (typ_ret : typ)
    ~(make_result :
       name_orig_ml:string ->
       exprs_targ_ml:Ml.expr ->
       exprs_arg_ml:Ml.expr ->
       typ_ret_ml:Ml.typ ->
       Ml.expr) : Ctx.t * Ml.funcdef list =
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
  (* Pass typed [Obj.t] inputs through the [Value.t list] boundary. *)
  let vars_marshal_ml, exprs_marshal_ml =
    List.mapi
      (fun i _typ ->
        ( "v__" ^ string_of_int i,
          Ml.AppE (Ml.LitE "Obj.magic", [ Ml.VarE ("p__" ^ string_of_int i) ])
        ))
      typs_param
    |> List.split
  in
  let exprs_arg_ml =
    Ml.ListE (List.init n (fun i -> Ml.VarE ("v__" ^ string_of_int i)))
  in
  let exprs_targ_ml = Ml.ListE (List.map Interface.typ_make_expr targs) in
  let expr_result_ml =
    make_result ~name_orig_ml ~exprs_targ_ml ~exprs_arg_ml ~typ_ret_ml
  in
  let expr_body_ml =
    List.fold_right
      (fun (var_marshal_ml, expr_marshal_ml) expr_body_ml ->
        Ml.LetE (Ml.VarP var_marshal_ml, expr_marshal_ml, expr_body_ml))
      (List.combine vars_marshal_ml exprs_marshal_ml)
      expr_result_ml
  in
  let funcdef_ml =
    (id_ml, params_ml, Some typ_ret_ml, Common.deref_ctx expr_body_ml)
  in
  (ctx, [ funcdef_ml ])

(* Extern functions: [extern.eval_extern_func] returns a [Run.func_result]. *)

let compile_extern_func (ctx : Ctx.t) (reverse_dispatch : reverse_dispatch)
    (id : id) (params : param list) (typ_ret : typ) : Ctx.t * Ml.funcdef list =
  compile_magic_bridge ctx reverse_dispatch id params typ_ret
    ~make_result:(fun ~name_orig_ml ~exprs_targ_ml ~exprs_arg_ml ~typ_ret_ml ->
      let expr_call_ml =
        Ml.AppE
          ( Common.extern_field "eval_extern_func",
            [ Ml.StrE name_orig_ml; exprs_targ_ml; exprs_arg_ml ] )
      in
      Ml.MatchE
        ( expr_call_ml,
          [
            ( Ml.VariantP (`Mono ("Run.Pass", [ Ml.VarP "v_out__" ])),
              (* Cast the output back to its OCaml type. *)
              Ml.AnnotE
                ( Ml.AppE (Ml.LitE "Obj.magic", [ Ml.VarE "v_out__" ]),
                  typ_ret_ml ) );
            ( Ml.VariantP (`Mono ("Run.Fail", [ Ml.WildP; Ml.VarP "msg__" ])),
              Ml.AppE
                ( Ml.LitE "raise",
                  [ Ml.AppE (Ml.LitE "Unmatch", [ Ml.VarE "msg__" ]) ] ) );
          ] ))

(* Builtin functions: routed through [iface.call_builtin], which returns the
   result [Value.t] directly (a typed [Obj.t] under ML). [BuiltinError] from the
   underlying builtin propagates and is mapped to [Unmatch] so the else-block
   fallback still works. *)

let compile_builtin_func (ctx : Ctx.t) (reverse_dispatch : reverse_dispatch)
    (id : id) (params : param list) (typ_ret : typ) : Ctx.t * Ml.funcdef list =
  compile_magic_bridge ctx reverse_dispatch id params typ_ret
    ~make_result:(fun ~name_orig_ml ~exprs_targ_ml ~exprs_arg_ml ~typ_ret_ml ->
      let name_lit_ml =
        Ml.LitE
          (Printf.sprintf "(\"%s\" $ no_region)" (String.escaped name_orig_ml))
      in
      let expr_call_ml =
        Ml.AppE
          ( Common.iface_field "call_builtin",
            [
              Ml.LitE "(fun _ -> ())"; name_lit_ml; exprs_targ_ml; exprs_arg_ml;
            ] )
      in
      let expr_try_ml =
        Ml.TryE
          ( expr_call_ml,
            [
              ( Ml.VariantP
                  (`Mono
                    ("Util.Error.BuiltinError", [ Ml.WildP; Ml.VarP "msg__" ])),
                Ml.AppE
                  ( Ml.LitE "raise",
                    [ Ml.AppE (Ml.LitE "Unmatch", [ Ml.VarE "msg__" ]) ] ) );
            ] )
      in
      Ml.LetE
        ( Ml.VarP "v_out__",
          expr_try_ml,
          Ml.AnnotE
            (Ml.AppE (Ml.LitE "Obj.magic", [ Ml.VarE "v_out__" ]), typ_ret_ml)
        ))

(* Table functions *)

let rec compile_table_func (ctx : Ctx.t) (id : id) (params : param list)
    (typ_ret : typ) (tablerows : tablerow list) : Ctx.t * Ml.funcdef list =
  let block = List.concat_map (fun (_, _, block_row) -> block_row) tablerows in
  compile_defined_func_mono ctx id params typ_ret block None

(* Defined functions *)

and compile_defined_func_mono (ctx : Ctx.t) (id : id) (params : param list)
    (typ_ret : typ) (block_main : block) (elseblock_opt : block option) :
    Ctx.t * Ml.funcdef list =
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
    (id_ml, params_ml, Some typ_ret_ml, dispatch_ml)
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
  if tparams <> [] then (ctx, [])
  else compile_defined_func_mono ctx id params typ_ret block_main elseblock_opt

(* Defs *)

let compile_def (ctx : Ctx.t) (reverse_dispatch : reverse_dispatch) (def : def)
    : Ctx.t * Ml.funcdef list =
  match def.it with
  | ExternDecD (id, [], params, typ_ret, _) ->
      compile_extern_func ctx reverse_dispatch id params typ_ret
  | BuiltinDecD (id, [], params, typ_ret, _) ->
      compile_builtin_func ctx reverse_dispatch id params typ_ret
  | TableDecD (id, params, typ_ret, tablerows, _) ->
      compile_table_func ctx id params typ_ret tablerows
  | FuncDecD definedfunc -> compile_defined_func ctx definedfunc
  | _ -> (ctx, [])

let compile_defs (ctx : Ctx.t) (defs : def list)
    (reverse_dispatch : reverse_dispatch) : Ctx.t * Ml.funcdef list =
  List.fold_left
    (fun (ctx, funcdefs_ml_acc) def ->
      let ctx, funcdefs_ml = compile_def ctx reverse_dispatch def in
      (ctx, funcdefs_ml_acc @ funcdefs_ml))
    (ctx, []) defs

(* Spec *)

let compile_group (ctx : Ctx.t) (group : def list)
    (dispatch_table : Mono.dispatch_table) : Ctx.t * Ml.funcdef list =
  let reverse_dispatch = build_reverse_dispatch dispatch_table in
  compile_defs ctx group reverse_dispatch

let compile_spec (ctx : Ctx.t) (spec : spec)
    (dispatch_table : Mono.dispatch_table) : Ctx.t * Ml.funcdef list =
  compile_group ctx spec dispatch_table
