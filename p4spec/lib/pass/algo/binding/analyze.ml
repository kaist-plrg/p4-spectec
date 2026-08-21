open Domain.Lib
module Mixfix = Domain.Mixfix
open Lang
open Il
open Runtime.Static
open Envs
open Error
open Bind
open Util.Source

(* Binding analysis :

   1. Collect all binding occurrences of variables in IL construct
      - Check that all binding occurrences reside in invertible constructs
   2. Rename multi/parallel binding occurrences
      - e.g., -- let (int, int) = ... becomes
                -- let (int, int') = ..., -- if int = int'
   3. Desugar partial bindings, occurring as either:
      (1) Bound values occurring inside binder patterns
          - e.g., -- let PATTERN (a, 1 + 2) = ... becomes
                  -- let PATTERN (a, int) = ..., -- if int == 1 + 2
      (2) Injection of a variant case
          - e.g., -- let PATTERN (a, int) = pat becomes
                  -- if pat matches PATTERN, -- let PATTERN (a, b) = pat
      (3) Injection of a subtype case
          - e.g., -- let ((typ) child) = parent becomes
                  -- if parent <: child, -- let child = parent as child
   Note. At this point, binder patterns are one of:
      - VarE, TupleE, CaseE of a singleton case, StrE
      - IterE of the above cases *)

let update_venv_multi (venv : VEnv.t) (renv_multi : Multibind.REnv.t) : VEnv.t =
  Multibind.REnv.fold
    (fun id ids_rename venv ->
      let ids_rename = IdSet.elements ids_rename in
      let typ = VEnv.find id venv in
      List.fold_left
        (fun venv id_rename -> VEnv.add id_rename typ venv)
        venv ids_rename)
    renv_multi venv

let update_venv_partial (venv : VEnv.t) (renv_partial : Partialbind.REnv.t) :
    VEnv.t =
  List.fold_left
    (fun venv (to_, _, iterctx) ->
      let id_to, typ_to, iters_to = to_ in
      let iters = iters_to @ Iterctx.iters_of iterctx in
      VEnv.add id_to (typ_to, iters) venv)
    venv renv_partial

(* Expression binding analysis *)

let rec is_pure_exp (exp : exp) : bool =
  match exp.it with
  | BoolE _ | NumE _ | TextE _ | VarE _ -> true
  | UnE (_, _, exp) -> is_pure_exp exp
  | BinE (_, _, exp_l, exp_r) | CmpE (_, _, exp_l, exp_r) ->
      is_pure_exp exp_l && is_pure_exp exp_r
  | UpCastE (_, exp) | DownCastE (_, exp) | SubE (exp, _, _) | MatchE (exp, _)
    ->
      is_pure_exp exp
  | TupleE exps -> List.for_all is_pure_exp exps
  | CaseE notexp -> List.for_all is_pure_exp (Mixfix.args notexp)
  | StrE expfields ->
      let exps = List.map snd expfields in
      List.for_all is_pure_exp exps
  | OptE (Some exp) -> is_pure_exp exp
  | OptE None -> true
  | ListE exps -> List.for_all is_pure_exp exps
  | ConsE (exp_h, exp_t) -> is_pure_exp exp_h && is_pure_exp exp_t
  | CatE (exp_l, exp_r) -> is_pure_exp exp_l && is_pure_exp exp_r
  | MemE (exp_e, exp_s) -> is_pure_exp exp_e && is_pure_exp exp_s
  | LenE exp | DotE (exp, _) -> is_pure_exp exp
  | IdxE (exp_b, exp_i) -> is_pure_exp exp_b && is_pure_exp exp_i
  | SliceE (exp_b, exp_i, exp_n) ->
      is_pure_exp exp_b && is_pure_exp exp_i && is_pure_exp exp_n
  | UpdE (exp_b, path, exp_f) ->
      is_pure_exp exp_b && is_pure_path path && is_pure_exp exp_f
  | CallE _ -> false
  | IterE (exp, _) -> is_pure_exp exp

and is_pure_path (path : path) : bool =
  match path.it with
  | RootP -> true
  | IdxP (path, exp) -> is_pure_path path && is_pure_exp exp
  | SliceP (path, exp_i, exp_n) ->
      is_pure_path path && is_pure_exp exp_i && is_pure_exp exp_n
  | DotP (path, _) -> is_pure_path path

let analyze_exps_as_bind (ctx : Ctx.t) (iterctx : Iterctx.t) (exps : exp list) :
    Ctx.t * VEnv.t * exp list * prem list =
  let binds = Collectbind.collect_exps ctx exps in
  let venv = BEnv.flatten binds in
  let ctx, renv_multi, exps =
    let renv_multi = Multibind.REnv.init binds in
    Multibind.rename_exps ctx renv_multi exps
  in
  let venv = update_venv_multi venv renv_multi in
  let sideconditions_multi =
    Multibind.gen_sideconditions binds iterctx renv_multi
  in
  let ctx, renv_partial, _, exps =
    Partialbind.rename_exps ctx (VEnv.dom venv) Partialbind.REnv.empty
      Iterctx.empty exps
  in
  let venv = update_venv_partial venv renv_partial in
  let prems_partial = Partialbind.gen_prems ctx iterctx renv_partial in
  let prems = prems_partial @ sideconditions_multi in
  (ctx, venv, exps, prems)

let analyze_exp_as_bound (ctx : Ctx.t) (exp : exp) : unit =
  let binds = Collectbind.collect_exp ctx exp in
  if not (BEnv.is_empty binds) then
    error exp.at
      (Format.asprintf "expression has free variable(s): %s"
         (BEnv.to_string binds))

let analyze_exps_as_bound (ctx : Ctx.t) (exps : exp list) : unit =
  List.iter (analyze_exp_as_bound ctx) exps

(* Argument binding analysis *)

let analyze_args_as_bind (ctx : Ctx.t) (args : arg list) :
    Ctx.t * VEnv.t * arg list * prem list =
  let binds = Collectbind.collect_args ctx args in
  let venv = BEnv.flatten binds in
  let ctx, renv_multi, args =
    let renv_multi = Multibind.REnv.init binds in
    Multibind.rename_args ctx renv_multi args
  in
  let venv = update_venv_multi venv renv_multi in
  let sideconditions_multi =
    Multibind.gen_sideconditions binds Iterctx.empty renv_multi
  in
  let ctx, renv_partial, _, args =
    Partialbind.rename_args ctx (VEnv.dom venv) Partialbind.REnv.empty
      Iterctx.empty args
  in
  let venv = update_venv_partial venv renv_partial in
  let prems_partial = Partialbind.gen_prems ctx Iterctx.empty renv_partial in
  let prems = prems_partial @ sideconditions_multi in
  (ctx, venv, args, prems)

let analyze_args_as_bind_shallow (ctx : Ctx.t) (args : arg list) :
    Ctx.t * VEnv.t * arg list * prem list =
  check
    (Shallowbind.check_shallow_args args)
    (List.hd args).at
    (Format.asprintf "bindings are not shallow: %s"
       (Il.Print.string_of_args args));
  let binds = Collectbind.collect_args ctx args in
  let venv = BEnv.flatten binds in
  let ctx, renv_multi, args =
    let renv_multi = Multibind.REnv.init binds in
    Multibind.rename_args ctx renv_multi args
  in
  let venv = update_venv_multi venv renv_multi in
  let sideconditions_multi =
    Multibind.gen_sideconditions binds Iterctx.empty renv_multi
  in
  check
    (List.is_empty sideconditions_multi)
    (List.hd args).at
    (Format.asprintf
       "shallow binding should not generate sideconditions, but got: %s"
       (List.map Il.Print.string_of_prem sideconditions_multi
       |> String.concat ", "));
  let ctx, renv_partial, _, args =
    Partialbind.rename_args ctx (VEnv.dom venv) Partialbind.REnv.empty
      Iterctx.empty args
  in
  let venv = update_venv_partial venv renv_partial in
  let prems_partial = Partialbind.gen_prems ctx Iterctx.empty renv_partial in
  let prems = prems_partial in
  (ctx, venv, args, prems)

let analyze_arg_as_bound_shallow (ctx : Ctx.t) (arg : arg) : unit =
  check
    (Shallowbind.check_shallow_arg arg)
    arg.at
    (Format.asprintf "bindings are not shallow: %s"
       (Il.Print.string_of_arg arg));
  let binds = Collectbind.collect_arg ctx arg in
  if not (BEnv.is_empty binds) then
    error arg.at
      (Format.asprintf "argument has free variable(s): %s"
         (BEnv.to_string binds))

let analyze_args_as_bound_shallow (ctx : Ctx.t) (args : arg list) : unit =
  List.iter (analyze_arg_as_bound_shallow ctx) args

(* Premise binding analysis *)

let rec is_pure_prem (prem : prem) : bool =
  match prem.it with
  | RulePr _ | IfPr _ | IfHoldPr _ | IfNotHoldPr _ -> false
  | LetPr (_, exp_r) -> is_pure_exp exp_r
  | IterPr (prem, _) -> is_pure_prem prem
  | DebugPr exp -> is_pure_exp exp

let check_prems_in_else (at : region) (prems : prem list) : unit =
  check
    (List.for_all is_pure_prem prems)
    at "cannot have non-pure premises alongside an otherwise premise"

let rec analyze_prem (ctx : Ctx.t) (iterctx : Iterctx.t) (prem : prem) :
    Ctx.t * VEnv.t * prem * prem list =
  match prem.it with
  | RulePr (id, notexp, inputs) ->
      analyze_rule_prem ctx iterctx prem.at id notexp inputs
  | IfPr exp -> analyze_if_prem ctx iterctx prem.at exp
  | IfHoldPr (id, notexp) -> analyze_if_hold_prem ctx iterctx prem.at id notexp
  | IfNotHoldPr (id, notexp) ->
      analyze_if_not_hold_prem ctx iterctx prem.at id notexp
  | LetPr _ ->
      error prem.at "let premise should appear only after bind analysis"
  | IterPr (prem, (iter, vars_bound, [])) ->
      analyze_iter_prem ctx iterctx prem iter vars_bound
  | IterPr _ ->
      error prem.at
        "iterated premise vars_bind should be empty before binding analysis"
  | DebugPr exp -> analyze_debug_prem ctx iterctx prem.at exp

and analyze_rule_prem (ctx : Ctx.t) (iterctx : Iterctx.t) (at : region)
    (id : id) (notexp : notexp) (inputs : Hints.Input.t) :
    Ctx.t * VEnv.t * prem * prem list =
  let mixop, exps = Mixfix.split notexp in
  let exps_input, exps_output = Hints.Input.split inputs exps in
  analyze_exps_as_bound ctx exps_input;
  let ctx, venv, exps_output, sideconditions =
    analyze_exps_as_bind ctx iterctx exps_output
  in
  let exps = Hints.Input.combine inputs exps_input exps_output in
  let notexp = Mixfix.fill mixop exps in
  let prem = RulePr (id, notexp, inputs) $ at in
  let venv_bound = Dimension.infer_exps exps_input in
  let iterctx =
    iterctx
    |> Iterctx.filter_bound (fun var ->
           let id, typ, iters = var in
           VEnv.find_opt id venv_bound
           |> Option.map (fun (typ_bound, iters_bound) ->
                  Typdim.sub (typ_bound, iters_bound) (typ, iters))
           |> Option.value ~default:false)
    |> Iterctx.add_vars_bind (Dimension.infer_exps exps_output)
  in
  Iterctx.validate at iterctx;
  let prem = Iterctx.iterate_prem iterctx prem in
  (ctx, venv, prem, sideconditions)

and analyze_if_eq_prem (ctx : Ctx.t) (iterctx : Iterctx.t) (at_prem : region)
    (at : region) (note : typ') (optyp : optyp) (exp_l : exp) (exp_r : exp) :
    Ctx.t * VEnv.t * prem * prem list =
  let binds_l = Collectbind.collect_exp ctx exp_l in
  let binds_r = Collectbind.collect_exp ctx exp_r in
  match (BEnv.is_empty binds_l, BEnv.is_empty binds_r) with
  | true, true ->
      let prem =
        IfPr (CmpE (`EqOp, optyp, exp_l, exp_r) $$ (at, note)) $ at_prem
      in
      let prem = Iterctx.iterate_prem iterctx prem in
      (ctx, VEnv.empty, prem, [])
  | false, true -> analyze_let_prem ctx at_prem iterctx exp_l binds_l exp_r
  | true, false -> analyze_let_prem ctx at_prem iterctx exp_r binds_r exp_l
  | false, false ->
      error at
        (Format.asprintf
           "cannot bind on both sides of an equality: (left) %s, (right) %s"
           (BEnv.to_string binds_l) (BEnv.to_string binds_r))

and analyze_if_prem (ctx : Ctx.t) (iterctx : Iterctx.t) (at : region)
    (exp : exp) : Ctx.t * VEnv.t * prem * prem list =
  match exp.it with
  | CmpE (`EqOp, optyp, exp_l, exp_r) ->
      let ctx, venv, prem, prems =
        analyze_if_eq_prem ctx iterctx at exp.at exp.note optyp exp_l exp_r
      in
      (ctx, venv, prem, prems)
  | _ ->
      analyze_exp_as_bound ctx exp;
      let prem = IfPr exp $ at in
      let prem = Iterctx.iterate_prem iterctx prem in
      (ctx, VEnv.empty, prem, [])

and analyze_if_hold_prem (ctx : Ctx.t) (iterctx : Iterctx.t) (at : region)
    (id : id) (notexp : notexp) : Ctx.t * VEnv.t * prem * prem list =
  let exps = Mixfix.args notexp in
  analyze_exps_as_bound ctx exps;
  let prem = IfHoldPr (id, notexp) $ at in
  let prem = Iterctx.iterate_prem iterctx prem in
  (ctx, VEnv.empty, prem, [])

and analyze_if_not_hold_prem (ctx : Ctx.t) (iterctx : Iterctx.t) (at : region)
    (id : id) (notexp : notexp) : Ctx.t * VEnv.t * prem * prem list =
  let exps = Mixfix.args notexp in
  analyze_exps_as_bound ctx exps;
  let prem = IfNotHoldPr (id, notexp) $ at in
  let prem = Iterctx.iterate_prem iterctx prem in
  (ctx, VEnv.empty, prem, [])

and analyze_let_prem (ctx : Ctx.t) (at : region) (iterctx : Iterctx.t)
    (exp_l : exp) (binds_l : BEnv.t) (exp_r : exp) :
    Ctx.t * VEnv.t * prem * prem list =
  let venv = BEnv.flatten binds_l in
  let ctx, renv_multi, exp_l =
    let renv_multi = Multibind.REnv.init binds_l in
    Multibind.rename_exp ctx renv_multi exp_l
  in
  let venv = update_venv_multi venv renv_multi in
  let sideconditions_multi =
    Multibind.gen_sideconditions binds_l iterctx renv_multi
  in
  let ctx, renv_partial, _, exp_l =
    Partialbind.rename_exp ctx (VEnv.dom venv) Partialbind.REnv.empty
      Iterctx.empty exp_l
  in
  let venv = update_venv_partial venv renv_partial in
  let prems_partial = Partialbind.gen_prems ctx iterctx renv_partial in
  let prems = prems_partial @ sideconditions_multi in
  let prem = LetPr (exp_l, exp_r) $ at in
  let venv_l = Dimension.infer_exp exp_l in
  let venv_r = Dimension.infer_exp exp_r in
  let iterctx =
    iterctx
    |> Iterctx.filter_bound (fun var ->
           let id, typ, iters = var in
           VEnv.find_opt id venv_r
           |> Option.map (fun (typ_r, iters_r) ->
                  Typdim.sub (typ_r, iters_r) (typ, iters))
           |> Option.value ~default:false)
    |> Iterctx.add_vars_bind venv_l
  in
  Iterctx.validate at iterctx;
  let prem = Iterctx.iterate_prem iterctx prem in
  (ctx, venv, prem, prems)

and analyze_iter_prem (ctx : Ctx.t) (iterctx : Iterctx.t) (prem : prem)
    (iter : iter) (vars : var list) : Ctx.t * VEnv.t * prem * prem list =
  let iterctx = (iter, vars, []) :: iterctx in
  analyze_prem ctx iterctx prem

and analyze_debug_prem (ctx : Ctx.t) (iterctx : Iterctx.t) (at : region)
    (exp : exp) : Ctx.t * VEnv.t * prem * prem list =
  analyze_exp_as_bound ctx exp;
  let prem = DebugPr exp $ at in
  let prem = Iterctx.iterate_prem iterctx prem in
  (ctx, VEnv.empty, prem, [])

let analyze_prems (ctx : Ctx.t) (prems : prem list) : Ctx.t * prem list =
  List.fold_left
    (fun (ctx, prems_acc) prem ->
      let ctx, venv, prem, sideconditions =
        analyze_prem ctx Iterctx.empty prem
      in
      let ctx = Ctx.add_bounds ctx venv in
      (ctx, prems_acc @ [ prem ] @ sideconditions))
    (ctx, []) prems

(* Rule binding analysis *)

let analyze_rulematch (ctx : Ctx.t) (ctxs_local : Ctx.t list)
    (exps_input_group : exp list list) :
    Ctx.t list * Al.rulematch * Al.prem list list =
  let ctx_local_unified =
    let frees =
      ctxs_local
      |> List.map (fun (ctx_local : Ctx.t) -> ctx_local.frees)
      |> List.fold_left IdSet.union IdSet.empty
    in
    Ctx.add_frees ctx frees
  in
  let ctx_local_unified, exps_input_unified, prems_unified_group =
    Antiunify.antiunify ctx_local_unified exps_input_group
  in
  let ctx_local_unified, venv, exps_input_unified_match, prems_match =
    analyze_exps_as_bind ctx_local_unified Iterctx.empty exps_input_unified
  in
  let ctx_local_unified = Ctx.add_bounds ctx_local_unified venv in
  analyze_exps_as_bound ctx_local_unified exps_input_unified;
  let ctxs_local =
    List.map
      (fun (ctx_local : Ctx.t) ->
        {
          ctx_local with
          frees = ctx_local_unified.frees;
          venv = ctx_local_unified.venv;
        })
      ctxs_local
  in
  let ctxs_local, prems_unified_group =
    List.map2 analyze_prems ctxs_local prems_unified_group |> List.split
  in
  let rulematch = (exps_input_unified, exps_input_unified_match, prems_match) in
  (ctxs_local, rulematch, prems_unified_group)

let analyze_rulepath ?(is_else : bool = false) (ctx_local : Ctx.t)
    (id_rule : id) (prems_unified : prem list) (prems : prem list)
    (exps_output : exp list) : Al.rulepath =
  let ctx_local, prems = analyze_prems ctx_local prems in
  let prems = prems_unified @ prems in
  if is_else then check_prems_in_else id_rule.at prems;
  analyze_exps_as_bound ctx_local exps_output;
  (id_rule, prems, exps_output)

let analyze_rulepaths ?(is_else : bool = false) (ctxs_local : Ctx.t list)
    (id_rule_group : id list) (prems_unified_group : prem list list)
    (prems_group : prem list list) (exps_output_group : exp list list) :
    Al.rulepath list =
  ctxs_local
  |> List.map (analyze_rulepath ~is_else)
  |> List.map2
       (fun id_rule analyze_rulepath -> analyze_rulepath id_rule)
       id_rule_group
  |> List.map2
       (fun prems_unified analyze_rulepath -> analyze_rulepath prems_unified)
       prems_unified_group
  |> List.map2
       (fun prems analyze_rulepath -> analyze_rulepath prems)
       prems_group
  |> List.map2
       (fun exps_output analyze_rulepath -> analyze_rulepath exps_output)
       exps_output_group

let analyze_rulegroup ?(is_else : bool = false) (ctx : Ctx.t)
    (inputs : Hints.Input.t) (rulegroup : rulegroup) : Al.rulegroup =
  let id, rules = rulegroup.it in
  let ctxs_local =
    List.map (fun rule -> Free.free_rule rule |> Ctx.add_frees ctx) rules
  in
  let id_rule_group, (notexps, prems_group) =
    List.map
      (fun rule ->
        let id_rule, notexp, prems = rule.it in
        (id_rule, (notexp, prems)))
      rules
    |> List.split
    |> fun (l, r) -> (l, List.split r)
  in
  let exps_input_group, exps_output_group =
    List.map
      (fun notexp ->
        let exps = Mixfix.args notexp in
        Hints.Input.split inputs exps)
      notexps
    |> List.split
  in
  let ctxs_local, rulematch, prems_unified_group =
    analyze_rulematch ctx ctxs_local exps_input_group
  in
  let rulepaths =
    analyze_rulepaths ~is_else ctxs_local id_rule_group prems_unified_group
      prems_group exps_output_group
  in
  (id, rulematch, rulepaths) $ rulegroup.at

let analyze_elsegroup (ctx : Ctx.t) (inputs : Hints.Input.t)
    (elsegroup : elsegroup) : Al.elsegroup =
  let id, rule = elsegroup.it in
  let rulegroup = (id, [ rule ]) $ elsegroup.at in
  let rulegroup_il = analyze_rulegroup ~is_else:true ctx inputs rulegroup in
  let id, rulematch, rulepaths = rulegroup_il.it in
  let rulepath = List.hd rulepaths in
  (id, rulematch, rulepath) $ elsegroup.at

(* Clause binding analysis *)

let analyze_clause ?(is_else : bool = false) (ctx : Ctx.t) (clause : clause) :
    clause =
  let args, exp, prems = clause.it in
  let ctx =
    let frees = Free.free_clause clause in
    Ctx.add_frees ctx frees
  in
  let ctx, venv, args, sideconditions = analyze_args_as_bind ctx args in
  let ctx = Ctx.add_bounds ctx venv in
  let ctx, prems = analyze_prems ctx prems in
  analyze_exp_as_bound ctx exp;
  let prems = sideconditions @ prems in
  if is_else then check_prems_in_else clause.at prems;
  (args, exp, prems) $ clause.at

let analyze_elseclause (ctx : Ctx.t) (elseclause : elseclause) : elseclause =
  analyze_clause ~is_else:true ctx elseclause

(* Table row binding analysis *)

let pattern_set_covered_by_typ (ctx : Ctx.t) (typ : typ) : Pattern.PatternSet.t
    =
  match typ.it with
  | VarT (tid, _) -> (
      let td = Ctx.find_typdef ctx tid in
      match td with
      | Defined (_, deftyp) -> (
          match deftyp.it with
          | VariantT typcases ->
              typcases
              |> List.map (fun (nottyp, _, _) -> nottyp)
              |> Pattern.PatternSet.of_list
          | _ ->
              error typ.at
                ("non-variant type not supported in patterns: "
               ^ Print.string_of_typ typ))
      | _ ->
          error typ.at
            ("non-variant type not supported in patterns: "
           ^ Print.string_of_typ typ))
  | _ -> error typ.at "expected variable type"

let pattern_set_covered_by_exp (ctx : Ctx.t) (exp : exp) : Pattern.PatternSet.t
    =
  match exp.it with
  | VarE _ -> pattern_set_covered_by_typ ctx (exp.note $ exp.at)
  | UpCastE (_, { it = VarE _; note; at }) ->
      pattern_set_covered_by_typ ctx (note $ at)
  | UpCastE (_, { it = CaseE notexp; at; _ }) ->
      let mixop, exps = Mixfix.split notexp in
      [ Mixfix.fill mixop (List.map (fun exp -> exp.note $ exp.at) exps) $ at ]
      |> Pattern.PatternSet.of_list
  | _ -> assert false

let check_valid_match_tablerows (ctx : Ctx.t) (at : region)
    (typs_match : typ list) (tablerows : Al.tablerow list) : unit =
  (* Split the last wildcard row (a "closer") if it exists *)
  let split_last_wildcard_tablerows tablerows =
    let rec split_last_wildcard_tablerows' tablerows_rev = function
      | [] -> (None, tablerows)
      | [ tablerow ] ->
          let exps_signature, _, _, _ = tablerow.it in
          if
            List.for_all
              (fun exp_signature ->
                match exp_signature.it with
                | VarE id when Id.is_underscored id -> true
                | _ -> false)
              exps_signature
          then (Some tablerow, List.rev tablerows_rev)
          else (None, tablerows)
      | tablerow_h :: tablerows_t ->
          split_last_wildcard_tablerows'
            (tablerow_h :: tablerows_rev)
            tablerows_t
    in
    split_last_wildcard_tablerows' [] tablerows
  in
  let closer_opt, tablerows = split_last_wildcard_tablerows tablerows in
  (* Check that table rows have exclusive patterns *)
  let pattern_sets_tablerows =
    List.map
      (fun tablerow ->
        let exps_signature, _, _, _ = tablerow.it in
        List.map (pattern_set_covered_by_exp ctx) exps_signature)
      tablerows
  in
  let pattern_set_overlap_opt = Pattern.find_overlap pattern_sets_tablerows in
  check
    (Option.is_none pattern_set_overlap_opt)
    at
    (Format.asprintf "table rows have overlapping patterns: %s"
       (match pattern_set_overlap_opt with
       | Some (pattern_sets_l, pattern_sets_r) ->
           Pattern.PatternSets.to_string pattern_sets_l
           ^ " and "
           ^ Pattern.PatternSets.to_string pattern_sets_r
       | None -> ""));
  (* Check that table rows are exhaustive *)
  let pattern_sets_total =
    List.map (pattern_set_covered_by_typ ctx) typs_match
  in
  let pattern_sets_group_missing =
    Pattern.find_missing pattern_sets_total pattern_sets_tablerows
  in
  check
    (Option.is_some closer_opt || pattern_sets_group_missing = [])
    at
    (Format.asprintf "table rows are missing patterns: %s"
       (String.concat ", "
          (List.map Pattern.PatternSets.to_string pattern_sets_group_missing)))

let analyze_tablerow (ctx : Ctx.t) (tablerow : tablerow) : Al.tablerow =
  let args, exp = tablerow.it in
  let ctx =
    let frees = Free.free_tablerow tablerow in
    Ctx.add_frees ctx frees
  in
  let ctx, venv, args_input, sideconditions =
    analyze_args_as_bind_shallow ctx args
  in
  let ctx = Ctx.add_bounds ctx venv in
  analyze_args_as_bound_shallow ctx args;
  let exps_signature =
    List.map
      (fun arg -> match arg.it with ExpA exp -> exp | _ -> assert false)
      args
  in
  analyze_exp_as_bound ctx exp;
  (exps_signature, args_input, exp, sideconditions) $ tablerow.at

let analyze_tablerows (ctx : Ctx.t) (at : region) (params : param list)
    (tablerows : tablerow list) : Al.tablerow list =
  let tablerows = List.map (analyze_tablerow ctx) tablerows in
  let typs_match =
    params
    |> List.map (fun param ->
           match param.it with ExpP typ_il -> typ_il | _ -> assert false)
  in
  check_valid_match_tablerows ctx at typs_match tablerows;
  tablerows

(* Definition binding analysis *)

let analyze_def (ctx : Ctx.t) (def : def) : Al.def =
  let at = def.at in
  match def.it with
  | ExternTypD (id, hints) -> Al.ExternTypD (id, hints) $ at
  | TypD (id, tparams, deftyp, hints) ->
      Al.TypD (id, tparams, deftyp, hints) $ at
  | VarD (id, typ, hints) -> Al.VarD (id, typ, hints) $ at
  | ExternRelD (id, nottyp, inputs, hints) ->
      Al.ExternRelD (id, nottyp, inputs, hints) $ at
  | RelD (id, nottyp, inputs, rulegroups, elsegroup_opt, hints) ->
      let rulegroups = List.map (analyze_rulegroup ctx inputs) rulegroups in
      let elsegroup_opt =
        Option.map (analyze_elsegroup ctx inputs) elsegroup_opt
      in
      Al.RelD (id, nottyp, inputs, rulegroups, elsegroup_opt, hints) $ at
  | ExternDecD (id, tparams, params, typ, hints) ->
      Al.ExternDecD (id, tparams, params, typ, hints) $ at
  | BuiltinDecD (id, tparams, params, typ, hints) ->
      Al.BuiltinDecD (id, tparams, params, typ, hints) $ at
  | TableDecD (id, params, typ, tablerows, hints) ->
      let tablerows = analyze_tablerows ctx at params tablerows in
      Al.TableDecD (id, params, typ, tablerows, hints) $ at
  | FuncDecD (id, tparams, params, typ, clauses, elseclause_opt, hints) ->
      let clauses = List.map (analyze_clause ctx) clauses in
      let elseclause_opt = Option.map (analyze_elseclause ctx) elseclause_opt in
      Al.FuncDecD (id, tparams, params, typ, clauses, elseclause_opt, hints)
      $ at

let analyze_spec (spec : spec) : Al.spec =
  let ctx = Ctx.init () in
  let ctx = Ctx.load_spec ctx spec in
  List.map (analyze_def ctx) spec
