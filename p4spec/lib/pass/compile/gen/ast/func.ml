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

   Each type parameter becomes an OCaml type variable ('x), and for each one
   it takes three extra parameters: a converter — [marshal__x : 'x ->
   Value.t] / [unmarshal__x : Value.t -> 'x] — converting between that
   abstract type and the runtime's uniform [Value.t], plus the caller's real
   reified type, [typ__x : Typ.t].

   At a call site, [Interface.Converter.resolve] resolves each callee type
   parameter's converter, and [Interface.Dynamic_gen.make_typ_expr] reifies
   its real type. *)

let compile_tparams (tparams_ml : Ml.tparam list) : Ml.param list =
  List.concat_map
    (fun tparam_ml ->
      let id_marshal_ml = Interface.Converter.name_marshal tparam_ml in
      let typ_marshal_ml = Ml.FuncT (Ml.VarT tparam_ml, Ml.NameT "Value.t") in
      let id_unmarshal_ml = Interface.Converter.name_unmarshal tparam_ml in
      let typ_unmarshal_ml = Ml.FuncT (Ml.NameT "Value.t", Ml.VarT tparam_ml) in
      let id_typ_ml = Interface.Naming.name_typ tparam_ml in
      let id_hash_ml = Interface.Converter.name_hash tparam_ml in
      let typ_hash_ml = Ml.FuncT (Ml.VarT tparam_ml, Ml.NameT "int") in
      let id_eq_ml = Interface.Converter.name_eq tparam_ml in
      let typ_eq_ml =
        Ml.FuncT (Ml.VarT tparam_ml, Ml.FuncT (Ml.VarT tparam_ml, Ml.BoolT))
      in
      [
        (id_marshal_ml, Some typ_marshal_ml);
        (id_unmarshal_ml, Some typ_unmarshal_ml);
        (id_typ_ml, Some (Ml.NameT "Typ.t"));
        (id_hash_ml, Some typ_hash_ml);
        (id_eq_ml, Some typ_eq_ml);
      ])
    tparams_ml

(* Type arguments: [<X, ..>]

   [[typ__x; ..]] *)

let compile_targs (tparams_ml : Ml.tparam list) : Ml.expr =
  let exprs_targs_ml =
    List.map
      (fun tparam_ml -> Ml.VarE (Interface.Naming.name_typ tparam_ml))
      tparams_ml
  in
  Ml.ListE exprs_targs_ml

(* Extern function: [extern def $f<X, ..>(t1, .., tn) : tret]

   Under the compiled ML backend, [call_builtin]/[eval_extern_func]'s actual
   host implementation (see [backend-sim/make.ml]'s [Interface_sim]) always
   carries a typed [Obj.t] through the [Value.t] surface — it never produces a
   genuinely boxed [Value.t] the way the interpreted (IL/SL) backends do. So
   this crosses the extern boundary with [Obj.magic] box/unbox, not a real
   marshal/unmarshal — mirrors [compile_builtin_func]'s treatment of the same
   boundary, and is why this reads differently from [Interface.Converter]'s
   real conversion used at the (interpreted-args-carrying) [eval_func]/
   [eval_rel] entry points in [gen/dispatch.ml].

   [let f_id (marshal__x, unmarshal__x, ..) (p__0 : t1_ml) .. =
      let v__0 = Obj.magic (p__0) in ..
      match eval_extern_func "f" (compile_targs) [v__0; ..] with
      | Run.Pass v_out__ -> (Obj.magic v_out__ : tret_ml)
      | Run.Fail (_, msg__) -> raise (Unmatch msg__)] *)

let compile_extern_func (_ctx : Ctx.t) (id : id) (tparams : Il.tparam list)
    (params : param list) (typ_ret : typ) : Ml.funcdef =
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
  (* [Obj.magic] each parameter before crossing into the extern. *)
  let vars_magic_ml, exprs_magic_ml =
    List.mapi
      (fun i _typ ->
        ( "v__" ^ string_of_int i,
          Ml.AppE
            (Ml.LitE "Obj.magic", [ Ml.VarE ("p__" ^ string_of_int i) ]) ))
      typs_param
    |> List.split
  in
  let chain_magic =
    List.map2
      (fun var_magic_ml expr_magic_ml ->
        Chain.make_let (Ml.VarP var_magic_ml) expr_magic_ml)
      vars_magic_ml exprs_magic_ml
    |> Chain.connect
  in
  (* Build args list *)
  let exprs_arg_ml =
    Ml.ListE (List.init n (fun i -> Ml.VarE ("v__" ^ string_of_int i)))
  in
  let exprs_targ_ml = compile_targs tparams_ml in
  (* Call the extern, [Obj.magic] its result back out. *)
  let expr_call_ml =
    Ml.AppE
      ( Interface.Trampoline.eval_extern_func,
        [ Ml.StrE id.it; exprs_targ_ml; exprs_arg_ml ] )
  in
  let expr_result_ml =
    Ml.MatchE
      ( expr_call_ml,
        [
          ( Ml.VariantP (`Mono ("Run.Pass", [ Ml.VarP "v_out__" ])),
            Ml.AnnotE
              (Ml.AppE (Ml.LitE "Obj.magic", [ Ml.VarE "v_out__" ]), typ_ret_ml)
          );
          ( Ml.VariantP (`Mono ("Run.Fail", [ Ml.WildP; Ml.VarP "msg__" ])),
            Ml.AppE
              ( Ml.LitE "raise",
                [ Ml.AppE (Ml.LitE "Unmatch", [ Ml.VarE "msg__" ]) ] ) );
        ] )
  in
  (* Chain bindings *)
  let chain = Chain.connect [ Interface.Trampoline.chain; chain_magic ] in
  let expr_body_ml = Chain.apply chain expr_result_ml in
  let funcdef_ml =
    (id_ml, tparams_ml, params_ml, Some typ_ret_ml, expr_body_ml)
  in
  funcdef_ml

(* Builtin function: [builtin def $f<X, ..>(t1, .., tn) : tret]

   Crosses [call_builtin] with [Obj.magic], not a real marshal/unmarshal —
   see [compile_extern_func]'s comment for why.

   [let f_id (marshal__x, unmarshal__x, ..) (p__0 : t1_ml) .. =
      let v__0 = Obj.magic (p__0) in ..
      let v_out__ =
        try call_builtin (fun _ -> ()) "f" (compile_targs) [v__0; ..]
        with Util.Error.BuiltinError (_, msg__) -> raise (Unmatch msg__)
      in
      (Obj.magic v_out__ : tret_ml)] *)

let compile_builtin_func (_ctx : Ctx.t) (id : id) (tparams : Il.tparam list)
    (params : param list) (typ_ret : typ) : Ml.funcdef =
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
  (* [Obj.magic] each parameter before crossing into the builtin. *)
  let vars_magic_ml, exprs_magic_ml =
    List.mapi
      (fun i _typ ->
        ( "v__" ^ string_of_int i,
          Ml.AppE
            (Ml.LitE "Obj.magic", [ Ml.VarE ("p__" ^ string_of_int i) ]) ))
      typs_param
    |> List.split
  in
  let chain_magic =
    List.map2
      (fun var_magic_ml expr_magic_ml ->
        Chain.make_let (Ml.VarP var_magic_ml) expr_magic_ml)
      vars_magic_ml exprs_magic_ml
    |> Chain.connect
  in
  (* Build args list *)
  let exprs_arg_ml =
    Ml.ListE (List.init n (fun i -> Ml.VarE ("v__" ^ string_of_int i)))
  in
  let exprs_targ_ml = compile_targs tparams_ml in
  (* Call the builtin, catching a builtin error as [Unmatch] *)
  let name_orig_lit_ml =
    Ml.LitE (Printf.sprintf "(\"%s\" $ no_region)" (String.escaped id.it))
  in
  let expr_call_ml =
    Ml.AppE
      ( Interface.Trampoline.call_builtin,
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
  (* [Obj.magic] the result back out. *)
  let expr_result_ml =
    Ml.LetE
      ( Ml.VarP "v_out__",
        expr_try_ml,
        Ml.AnnotE
          (Ml.AppE (Ml.LitE "Obj.magic", [ Ml.VarE "v_out__" ]), typ_ret_ml) )
  in
  (* Chain bindings *)
  let chain = Chain.connect [ Interface.Trampoline.chain; chain_magic ] in
  let expr_body_ml = Chain.apply chain expr_result_ml in
  let funcdef_ml =
    (id_ml, tparams_ml, params_ml, Some typ_ret_ml, expr_body_ml)
  in
  funcdef_ml

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
      let funcdef_ml = compile_extern_func ctx id tparams params typ_ret in
      (ctx, [ funcdef_ml ])
  | BuiltinDecD (id, tparams, params, typ_ret, _) ->
      let funcdef_ml = compile_builtin_func ctx id tparams params typ_ret in
      (ctx, [ funcdef_ml ])
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
