open Lang
open Sl

(* Parameters *)

let compile_exp_param ~(index : int option) (ctx : Ctx.t) (typ : typ)
    (exp : exp) : Ctx.t * Ml.param * Chain.t =
  (* Create a stub expression for the parameter *)
  let id_stub_ml =
    "param__" ^ (index |> Option.map string_of_int |> Option.value ~default:"")
  in
  let expr_stub_ml = Ml.VarE id_stub_ml in
  (* Compile the parameter type *)
  let typ_ml = Type.compile_typ ~tparams:[] typ in
  (* Assemble the parameter *)
  let param_ml = (id_stub_ml, Some typ_ml) in
  (* Create bindings for the parameter *)
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

(* Defined functions *)

let compile_defined_func_mono (ctx : Ctx.t) (id : id) (params : param list)
    (typ_ret : typ) (block_main : block) (elseblock_opt : block option) :
    Ctx.t * Ml.funcdef list =
  let id_ml = Names.func id in
  let typ_ret_ml = Type.compile_typ ~tparams:[] typ_ret in
  (* Enter nested scope *)
  let ctx = Ctx.push ctx in
  (* Compile parameters *)
  let ctx, params_ml, chain = compile_params ctx params in
  let ids_param_ml = List.map (fun (id_param_ml, _) -> id_param_ml) params_ml in
  (* Compile main block *)
  let id_main_ml = "main__" ^ id_ml in
  let ctx, funcdef_main_ml =
    let ctx, expr_block_ml = Instr.compile_block ctx block_main in
    let expr_ml = Chain.apply chain expr_block_ml in
    let funcdef_main_ml = (id_main_ml, params_ml, None, expr_ml) in
    (ctx, funcdef_main_ml)
  in
  (* Compile else block *)
  let id_else_ml = "else__" ^ id_ml in
  let ctx, funcdef_else_ml_opt =
    match elseblock_opt with
    | Some elseblock ->
        let ctx, expr_else_ml = Instr.compile_block ctx elseblock in
        let expr_ml = Chain.apply chain expr_else_ml in
        let funcdef_else_ml = (id_else_ml, params_ml, None, expr_ml) in
        (ctx, Some funcdef_else_ml)
    | None -> (ctx, None)
  in
  (* Compile dispatcher *)
  let funcdef_dispatcher_ml =
    let exprs_param_ml =
      List.map (fun id_param_ml -> Ml.VarE id_param_ml) ids_param_ml
    in
    let exp_dispatcher_ml =
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
    (id_ml, params_ml, Some typ_ret_ml, exp_dispatcher_ml)
  in
  (* Exit nested scope *)
  let ctx = Ctx.pop ctx in
  (* Collect function definitions *)
  let funcdefs_ml =
    let else_list = Option.to_list funcdef_else_ml_opt in
    (funcdef_main_ml :: else_list) @ [ funcdef_dispatcher_ml ]
  in
  (ctx, funcdefs_ml)

let compile_defined_func (ctx : Ctx.t) (definedfunc : definedfunc) :
    Ctx.t * Ml.funcdef list =
  (* Unpack the defined function *)
  let id, tparams, params, typ_ret, block_main, elseblock_opt, _ =
    definedfunc
  in
  (* If the function is polymorphic, pass *)
  if tparams <> [] then (ctx, [])
  else compile_defined_func_mono ctx id params typ_ret block_main elseblock_opt

(* Defs *)

let compile_def (ctx : Ctx.t) (def : def) : Ctx.t * Ml.funcdef list =
  match def.it with
  | ExternDecD _ -> (ctx, [])
  | BuiltinDecD _ -> (ctx, [])
  | TableDecD _ -> (ctx, [])
  | FuncDecD definedfunc -> compile_defined_func ctx definedfunc
  | _ -> (ctx, [])

let compile_defs (ctx : Ctx.t) (defs : def list) : Ctx.t * Ml.funcdef list =
  List.fold_left
    (fun (ctx, funcdefs_ml_acc) def ->
      let ctx, funcdefs_ml = compile_def ctx def in
      (ctx, funcdefs_ml_acc @ funcdefs_ml))
    (ctx, []) defs

(* Spec *)

let compile_spec (ctx : Ctx.t) (spec : spec) = compile_defs ctx spec
