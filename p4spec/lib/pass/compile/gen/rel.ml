open Lang
open Sl
open Util.Source

(* Defined relations *)

let compile_defined_rel (ctx : Ctx.t) (id : id)
    ((_nottyp, inputs) : rel_signature) (exps : exp list) (block_main : block)
    (elseblock_opt : block option) : Ctx.t * Ml.funcdef list =
  let id_ml = Names.rel id in
  let ctx_outer = ctx in
  (* Split into input / output expressions *)
  let exps_input, _exps_output = Hints.Input.split inputs exps in
  (* Build param list typed by exp.note *)
  let params_ml =
    List.mapi
      (fun i exp_in ->
        let typ_ml = Type.compile_typ ~tparams:[] (exp_in.note $ no_region) in
        ("param__" ^ string_of_int i, Some typ_ml))
      exps_input
  in
  let ids_param_ml = List.map fst params_ml in
  (* Build binding chain: bind each param stub into ctx *)
  let ctx, chain =
    List.mapi (fun i exp_in -> (i, exp_in)) exps_input
    |> List.fold_left
         (fun (ctx, chain_acc) (i, exp_in) ->
           let ctx, ch =
             Bind.compile ctx (Ml.VarE ("param__" ^ string_of_int i)) exp_in
           in
           (ctx, Chain.connect [ chain_acc; ch ]))
         (ctx, Chain.nop)
  in
  (* Compile main block *)
  let id_main_ml = "main__" ^ id_ml in
  let ctx, funcdef_main_ml =
    let ctx, expr_block_ml = Instr.compile_block ctx block_main in
    let expr_ml = Chain.apply chain expr_block_ml in
    (ctx, (id_main_ml, params_ml, None, expr_ml))
  in
  (* Compile else block *)
  let id_else_ml = "else__" ^ id_ml in
  let ctx, funcdef_else_ml_opt =
    match elseblock_opt with
    | Some elseblock ->
        let ctx, expr_else_ml = Instr.compile_block ctx elseblock in
        let expr_ml = Chain.apply chain expr_else_ml in
        (ctx, Some (id_else_ml, params_ml, None, expr_ml))
    | None -> (ctx, None)
  in
  (* Promote preamble to outer context *)
  let ctx = Ctx.promote_preamble ctx ctx_outer in
  (* Base dispatch expression *)
  let exprs_param_ml = List.map (fun id_p -> Ml.VarE id_p) ids_param_ml in
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
    (id_ml, params_ml, None, Common.prof_wrap id_ml dispatch_ml)
  in
  let funcdefs_ml =
    (funcdef_main_ml :: Option.to_list funcdef_else_ml_opt)
    @ [ funcdef_dispatcher_ml ]
  in
  (ctx, funcdefs_ml)

(* Extern relations *)

let compile_extern_rel (ctx : Ctx.t) (id : id)
    ((nottyp, inputs) : rel_signature) (exps : exp list) :
    Ctx.t * Ml.funcdef list =
  ignore exps;
  let id_ml = Names.rel id in
  (* Derive input/output types from nottyp (authoritative source) *)
  let typs_rel = Domain.Mixfix.args nottyp.it in
  let typs_input, typs_output = Hints.Input.split inputs typs_rel in
  let n = List.length typs_input in
  (* Input params *)
  let params_ml =
    List.mapi
      (fun i typ ->
        let typ_ml = Type.compile_typ ~tparams:[] typ in
        ("param__" ^ string_of_int i, Some typ_ml))
      typs_input
  in
  (* Marshal inputs *)
  let vars_marshal_ml, exprs_marshal_ml =
    List.mapi
      (fun i typ ->
        ( "v__" ^ string_of_int i,
          Ml.AppE
            ( Ml.VarE ("marshal_" ^ Interface.interface_name typ),
              [ Ml.VarE ("param__" ^ string_of_int i) ] ) ))
      typs_input
    |> List.split
  in
  (* Build args list *)
  let exprs_arg_ml =
    Ml.ListE (List.init n (fun i -> Ml.VarE ("v__" ^ string_of_int i)))
  in
  (* Unmarshal outputs from Pass value list *)
  let expr_unmarshal_body_ml =
    let exprs_out_ml =
      List.mapi
        (fun i (typ : Sl.typ) ->
          Ml.AppE
            ( Ml.VarE ("unmarshal_" ^ Interface.interface_name typ),
              [
                Ml.AppE
                  ( Ml.LitE "List.nth",
                    [ Ml.VarE "vs_out__"; Ml.LitE (string_of_int i) ] );
              ] ))
        typs_output
    in
    match exprs_out_ml with [] -> Ml.UnitE | [ e ] -> e | es -> Ml.TupleE es
  in
  (* Call extern relation and match result *)
  let expr_call_ml =
    Ml.AppE
      (Common.extern_field "eval_extern_rel", [ Ml.StrE id.it; exprs_arg_ml ])
  in
  let expr_result_ml =
    Ml.MatchE
      ( expr_call_ml,
        [
          ( Ml.VariantP (`Mono ("Run.Pass", [ Ml.VarP "vs_out__" ])),
            expr_unmarshal_body_ml );
          ( Ml.VariantP (`Mono ("Run.Fail", [ Ml.WildP; Ml.VarP "msg__" ])),
            Ml.AppE
              ( Ml.LitE "raise",
                [ Ml.AppE (Ml.LitE "Unmatch", [ Ml.VarE "msg__" ]) ] ) );
        ] )
  in
  let expr_body_ml =
    List.fold_right
      (fun (var_ml, expr_ml) acc -> Ml.LetE (Ml.VarP var_ml, expr_ml, acc))
      (List.combine vars_marshal_ml exprs_marshal_ml)
      expr_result_ml
  in
  (ctx, [ (id_ml, params_ml, None, Common.prof_wrap id_ml (Common.deref_ctx expr_body_ml)) ])

(* Defs *)

let compile_def (ctx : Ctx.t) (def : def) : Ctx.t * Ml.funcdef list =
  match def.it with
  | RelD (id, rel_sig, exps, block, elseblock_opt, _) ->
      compile_defined_rel ctx id rel_sig exps block elseblock_opt
  | ExternRelD (id, rel_sig, exps, _) -> compile_extern_rel ctx id rel_sig exps
  | _ -> (ctx, [])

let compile_defs (ctx : Ctx.t) (defs : def list) : Ctx.t * Ml.funcdef list =
  List.fold_left
    (fun (ctx, funcdefs_ml_acc) def ->
      let ctx, funcdefs_ml = compile_def ctx def in
      (ctx, funcdefs_ml_acc @ funcdefs_ml))
    (ctx, []) defs

let compile_group (ctx : Ctx.t) (group : def list) : Ctx.t * Ml.funcdef list =
  compile_defs ctx group

let compile_spec (ctx : Ctx.t) (spec : spec) : Ctx.t * Ml.funcdef list =
  compile_group ctx spec
