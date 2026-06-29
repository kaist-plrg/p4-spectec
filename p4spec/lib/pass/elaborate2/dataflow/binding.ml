open Domain.Lib
module Mixfix = Domain.Mixfix
open Lang
open Il2
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

let analyze_exps_as_bind (dctx : Dctx.t) (iterctx : Iterctx.t) (exps : exp list)
    : Dctx.t * VEnv.t * exp list * prem list =
  let binds = Collectbind.collect_exps dctx exps in
  let venv = BEnv.flatten binds in
  let dctx, renv_multi, exps =
    let renv_multi = Multibind.REnv.init binds in
    Multibind.rename_exps dctx renv_multi exps
  in
  let venv = update_venv_multi venv renv_multi in
  let sideconditions_multi =
    Multibind.gen_sideconditions binds iterctx renv_multi
  in
  let dctx, renv_partial, _, exps =
    Partialbind.rename_exps dctx (VEnv.dom venv) Partialbind.REnv.empty
      Iterctx.empty exps
  in
  let venv = update_venv_partial venv renv_partial in
  let prems_partial = Partialbind.gen_prems dctx iterctx renv_partial in
  let prems = prems_partial @ sideconditions_multi in
  (dctx, venv, exps, prems)

let analyze_exp_as_bound (dctx : Dctx.t) (exp : exp) : unit =
  let binds = Collectbind.collect_exp dctx exp in
  if not (BEnv.is_empty binds) then
    error exp.at
      (Format.asprintf "expression has free variable(s): %s"
         (BEnv.to_string binds))

let analyze_exps_as_bound (dctx : Dctx.t) (exps : exp list) : unit =
  List.iter (analyze_exp_as_bound dctx) exps

(* Argument binding analysis *)

let analyze_args_as_bind (dctx : Dctx.t) (args : arg list) :
    Dctx.t * VEnv.t * arg list * prem list =
  let binds = Collectbind.collect_args dctx args in
  let venv = BEnv.flatten binds in
  let dctx, renv_multi, args =
    let renv_multi = Multibind.REnv.init binds in
    Multibind.rename_args dctx renv_multi args
  in
  let venv = update_venv_multi venv renv_multi in
  let sideconditions_multi =
    Multibind.gen_sideconditions binds Iterctx.empty renv_multi
  in
  let dctx, renv_partial, _, args =
    Partialbind.rename_args dctx (VEnv.dom venv) Partialbind.REnv.empty
      Iterctx.empty args
  in
  let venv = update_venv_partial venv renv_partial in
  let prems_partial = Partialbind.gen_prems dctx Iterctx.empty renv_partial in
  let prems = prems_partial @ sideconditions_multi in
  (dctx, venv, args, prems)

let analyze_args_as_bind_shallow (dctx : Dctx.t) (args : arg list) :
    Dctx.t * VEnv.t * arg list * prem list =
  let binds = Collectbind.collect_args dctx args in
  let venv = BEnv.flatten binds in
  let dctx, renv_multi, args =
    let renv_multi = Multibind.REnv.init binds in
    Multibind.rename_args dctx renv_multi args
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
       (List.map Il2.Print.string_of_prem sideconditions_multi
       |> String.concat ", "));
  let dctx, renv_partial, _, args =
    Partialbind.rename_args dctx (VEnv.dom venv) Partialbind.REnv.empty
      Iterctx.empty args
  in
  let venv = update_venv_partial venv renv_partial in
  let prems_partial = Partialbind.gen_prems dctx Iterctx.empty renv_partial in
  let prems = prems_partial in
  (dctx, venv, args, prems)

let analyze_arg_as_bound_shallow (dctx : Dctx.t) (arg : arg) : unit =
  check
    (Shallowbind.check_shallow_arg arg)
    arg.at
    (Format.asprintf "bindings are not shallow: %s"
       (Il.Print.string_of_arg arg));
  let binds = Collectbind.collect_arg dctx arg in
  if not (BEnv.is_empty binds) then
    error arg.at
      (Format.asprintf "argument has free variable(s): %s"
         (BEnv.to_string binds))

let analyze_args_as_bound_shallow (dctx : Dctx.t) (args : arg list) : unit =
  List.iter (analyze_arg_as_bound_shallow dctx) args

(* Premise binding analysis *)

let rec analyze_prem (dctx : Dctx.t) (iterctx : Iterctx.t) (prem : prem) :
    Dctx.t * VEnv.t * prem * prem list =
  let open Il in
  match prem.it with
  | RulePr (id, notexp, inputs) ->
      analyze_rule_prem dctx iterctx prem.at id notexp inputs
  | IfPr exp -> analyze_if_prem dctx iterctx prem.at exp
  | IfHoldPr (id, notexp) -> analyze_if_hold_prem dctx iterctx prem.at id notexp
  | IfNotHoldPr (id, notexp) ->
      analyze_if_not_hold_prem dctx iterctx prem.at id notexp
  | LetPr _ ->
      error prem.at "let premise should appear only after bind analysis"
  | IterPr (prem, (iter, vars_bound, [])) ->
      analyze_iter_prem dctx iterctx prem iter vars_bound
  | IterPr _ ->
      error prem.at
        "iterated premise vars_bind should be empty before binding analysis"
  | DebugPr exp -> analyze_debug_prem dctx iterctx prem.at exp

and analyze_rule_prem (dctx : Dctx.t) (iterctx : Iterctx.t) (at : region)
    (id : id) (notexp : notexp) (inputs : Hints.Input.t) :
    Dctx.t * VEnv.t * prem * prem list =
  let open Il in
  let mixop, exps = Mixfix.split notexp in
  let exps_input, exps_output = Hints.Input.split inputs exps in
  analyze_exps_as_bound dctx exps_input;
  let dctx, venv, exps_output, sideconditions =
    analyze_exps_as_bind dctx iterctx exps_output
  in
  let exps = Hints.Input.combine inputs exps_input exps_output in
  let notexp = Mixfix.fill mixop exps in
  let prem = RulePr (id, notexp, inputs) $ at in
  let iterctx =
    iterctx
    |> Iterctx.filter_bound (fun id _ _ -> not (VEnv.mem id venv))
    |> Iterctx.add_vars_bind venv
  in
  let prem = Iterctx.iterate_prem iterctx prem in
  (dctx, venv, prem, sideconditions)

and analyze_if_eq_prem (dctx : Dctx.t) (iterctx : Iterctx.t) (at_prem : region)
    (at : region) (note : typ') (optyp : optyp) (exp_l : exp) (exp_r : exp) :
    Dctx.t * VEnv.t * prem * prem list =
  let open Il in
  let binds_l = Collectbind.collect_exp dctx exp_l in
  let binds_r = Collectbind.collect_exp dctx exp_r in
  match (BEnv.is_empty binds_l, BEnv.is_empty binds_r) with
  | true, true ->
      let prem =
        IfPr (CmpE (`EqOp, optyp, exp_l, exp_r) $$ (at, note)) $ at_prem
      in
      let prem = Iterctx.iterate_prem iterctx prem in
      (dctx, VEnv.empty, prem, [])
  | false, true -> analyze_let_prem dctx at_prem iterctx exp_l binds_l exp_r
  | true, false -> analyze_let_prem dctx at_prem iterctx exp_r binds_r exp_l
  | false, false ->
      error at
        (Format.asprintf
           "cannot bind on both sides of an equality: (left) %s, (right) %s"
           (BEnv.to_string binds_l) (BEnv.to_string binds_r))

and analyze_if_prem (dctx : Dctx.t) (iterctx : Iterctx.t) (at : region)
    (exp : exp) : Dctx.t * VEnv.t * prem * prem list =
  let open Il in
  match exp.it with
  | CmpE (`EqOp, optyp, exp_l, exp_r) ->
      let dctx, venv, prem, prems =
        analyze_if_eq_prem dctx iterctx at exp.at exp.note optyp exp_l exp_r
      in
      (dctx, venv, prem, prems)
  | _ ->
      analyze_exp_as_bound dctx exp;
      let prem = IfPr exp $ at in
      let prem = Iterctx.iterate_prem iterctx prem in
      (dctx, VEnv.empty, prem, [])

and analyze_if_hold_prem (dctx : Dctx.t) (iterctx : Iterctx.t) (at : region)
    (id : id) (notexp : notexp) : Dctx.t * VEnv.t * prem * prem list =
  let open Il in
  let exps = Mixfix.args notexp in
  analyze_exps_as_bound dctx exps;
  let prem = IfHoldPr (id, notexp) $ at in
  let prem = Iterctx.iterate_prem iterctx prem in
  (dctx, VEnv.empty, prem, [])

and analyze_if_not_hold_prem (dctx : Dctx.t) (iterctx : Iterctx.t) (at : region)
    (id : id) (notexp : notexp) : Dctx.t * VEnv.t * prem * prem list =
  let open Il in
  let exps = Mixfix.args notexp in
  analyze_exps_as_bound dctx exps;
  let prem = IfNotHoldPr (id, notexp) $ at in
  let prem = Iterctx.iterate_prem iterctx prem in
  (dctx, VEnv.empty, prem, [])

and analyze_let_prem (dctx : Dctx.t) (at : region) (iterctx : Iterctx.t)
    (exp_l : exp) (binds_l : BEnv.t) (exp_r : exp) :
    Dctx.t * VEnv.t * prem * prem list =
  let open Il in
  let venv = BEnv.flatten binds_l in
  let dctx, renv_multi, exp_l =
    let renv_multi = Multibind.REnv.init binds_l in
    Multibind.rename_exp dctx renv_multi exp_l
  in
  let venv = update_venv_multi venv renv_multi in
  let sideconditions_multi =
    Multibind.gen_sideconditions binds_l iterctx renv_multi
  in
  let dctx, renv_partial, _, exp_l =
    Partialbind.rename_exp dctx (VEnv.dom venv) Partialbind.REnv.empty
      Iterctx.empty exp_l
  in
  let venv = update_venv_partial venv renv_partial in
  let prems_partial = Partialbind.gen_prems dctx iterctx renv_partial in
  let prems = prems_partial @ sideconditions_multi in
  let prem = LetPr (exp_l, exp_r) $ at in
  let venv_l =
    Dimension.infer_exp exp_l [] Dimension.Dimctx.empty
    |> Dimension.Dimctx.infer
  in
  let venv_r =
    Dimension.infer_exp exp_r [] Dimension.Dimctx.empty
    |> Dimension.Dimctx.infer
  in
  let iterctx =
    iterctx
    |> Iterctx.filter_bound (fun id typ iters ->
           VEnv.find_opt id venv_r
           |> Option.map (fun (typ_r, iters_r) ->
                  Typdim.sub (typ_r, iters_r) (typ, iters))
           |> Option.value ~default:false)
    |> Iterctx.add_vars_bind venv_l
  in
  let prem = Iterctx.iterate_prem iterctx prem in
  (dctx, venv, prem, prems)

and analyze_iter_prem (dctx : Dctx.t) (iterctx : Iterctx.t) (prem : prem)
    (iter : iter) (vars : var list) : Dctx.t * VEnv.t * prem * prem list =
  let iterctx = (iter, vars, []) :: iterctx in
  analyze_prem dctx iterctx prem

and analyze_debug_prem (dctx : Dctx.t) (iterctx : Iterctx.t) (at : region)
    (exp : exp) : Dctx.t * VEnv.t * prem * prem list =
  let open Il in
  analyze_exp_as_bound dctx exp;
  let prem = DebugPr exp $ at in
  let prem = Iterctx.iterate_prem iterctx prem in
  (dctx, VEnv.empty, prem, [])

let analyze_prems (dctx : Dctx.t) (prems : prem list) : Dctx.t * prem list =
  List.fold_left
    (fun (dctx, prems_acc) prem ->
      let dctx, venv, prem, sideconditions =
        analyze_prem dctx Iterctx.empty prem
      in
      let dctx = Dctx.add_bounds dctx venv in
      (dctx, prems_acc @ [ prem ] @ sideconditions))
    (dctx, []) prems

(* Rule binding analysis *)

let analyze_rulematch (dctx : Dctx.t) (dctxs_local : Dctx.t list)
    (exps_input_group : exp list list) :
    Dctx.t list * Il.rulematch * Il.prem list list =
  let dctx_local_unified =
    let frees =
      dctxs_local
      |> List.map (fun (dctx_local : Dctx.t) -> dctx_local.frees)
      |> List.fold_left IdSet.union IdSet.empty
    in
    Dctx.add_frees dctx frees
  in
  let dctx_local_unified, exps_input_unified, prems_unified_group =
    Antiunify.antiunify dctx_local_unified exps_input_group
  in
  let dctx_local_unified, venv, exps_input_unified_match, prems_match =
    analyze_exps_as_bind dctx_local_unified Iterctx.empty exps_input_unified
  in
  let dctx_local_unified = Dctx.add_bounds dctx_local_unified venv in
  analyze_exps_as_bound dctx_local_unified exps_input_unified;
  let dctxs_local =
    List.map
      (fun (dctx_local : Dctx.t) ->
        {
          dctx_local with
          frees = dctx_local_unified.frees;
          bounds = dctx_local_unified.bounds;
        })
      dctxs_local
  in
  let dctxs_local, prems_unified_group =
    List.map2 analyze_prems dctxs_local prems_unified_group |> List.split
  in
  let rulematch = (exps_input_unified, exps_input_unified_match, prems_match) in
  (dctxs_local, rulematch, prems_unified_group)

let analyze_rulepath (dctx_local : Dctx.t) (id_rule : id)
    (prems_unified : Il.prem list) (prems : prem list) (exps_output : exp list)
    : Il.rulepath =
  let dctx_local, prems = analyze_prems dctx_local prems in
  let prems = prems_unified @ prems in
  analyze_exps_as_bound dctx_local exps_output;
  (id_rule, prems, exps_output)

let analyze_rulepaths (dctxs_local : Dctx.t list) (id_rule_group : id list)
    (prems_unified_group : Il.prem list list) (prems_group : prem list list)
    (exps_output_group : exp list list) : Il.rulepath list =
  dctxs_local |> List.map analyze_rulepath
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

let analyze_rulegroup (dctx : Dctx.t) (inputs : Hints.Input.t)
    (rulegroup : rulegroup) : Il.rulegroup =
  let id, rules = rulegroup.it in
  let dctxs_local =
    List.map (fun rule -> Free.free_rule rule |> Dctx.add_frees dctx) rules
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
  let dctxs_local, rulematch, prems_unified_group =
    analyze_rulematch dctx dctxs_local exps_input_group
  in
  let rulepaths =
    analyze_rulepaths dctxs_local id_rule_group prems_unified_group prems_group
      exps_output_group
  in
  (id, rulematch, rulepaths) $ rulegroup.at

let analyze_elsegroup (dctx : Dctx.t) (inputs : Hints.Input.t)
    (elsegroup : elsegroup) : Il.elsegroup =
  let id, rule = elsegroup.it in
  let rulegroup = (id, [ rule ]) $ elsegroup.at in
  let rulegroup_il = analyze_rulegroup dctx inputs rulegroup in
  let id, rulematch, rulepaths = rulegroup_il.it in
  let rulepath = List.hd rulepaths in
  (id, rulematch, rulepath) $ elsegroup.at

(* Clause binding analysis *)

let analyze_clause (dctx : Dctx.t) (clause : clause) : clause =
  let args, exp, prems = clause.it in
  let dctx =
    let frees = Free.free_clause clause in
    Dctx.add_frees dctx frees
  in
  let dctx, venv, args, sideconditions = analyze_args_as_bind dctx args in
  let dctx = Dctx.add_bounds dctx venv in
  let dctx, prems = analyze_prems dctx prems in
  analyze_exp_as_bound dctx exp;
  let prems = sideconditions @ prems in
  (args, exp, prems) $ clause.at

(* Table row binding analysis *)

let analyze_tablerow (dctx : Dctx.t) (tablerow : tablerow) : Il.tablerow =
  let args, exp = tablerow.it in
  let dctx =
    let frees = Free.free_tablerow tablerow in
    Dctx.add_frees dctx frees
  in
  let dctx, venv, args_input, sideconditions =
    analyze_args_as_bind_shallow dctx args
  in
  let dctx = Dctx.add_bounds dctx venv in
  analyze_args_as_bound_shallow dctx args;
  let exps_signature =
    List.map
      (fun arg -> match arg.it with Il.ExpA exp -> exp | _ -> assert false)
      args
  in
  analyze_exp_as_bound dctx exp;
  (exps_signature, args_input, exp, sideconditions) $ tablerow.at

(* Definition binding analysis *)

let analyze_def (dctx : Dctx.t) (def : def) : Il.def =
  let at = def.at in
  match def.it with
  | ExternTypD (id, hints) -> Il.ExternTypD (id, hints) $ at
  | TypD (id, tparams, deftyp, hints) ->
      Il.TypD (id, tparams, deftyp, hints) $ at
  | VarD (id, typ, hints) -> Il.VarD (id, typ, hints) $ at
  | ExternRelD (id, nottyp, inputs, hints) ->
      Il.ExternRelD (id, nottyp, inputs, hints) $ at
  | RelD (id, nottyp, inputs, rulegroups, elsegroup_opt, hints) ->
      let rulegroups = List.map (analyze_rulegroup dctx inputs) rulegroups in
      let elsegroup_opt =
        Option.map (analyze_elsegroup dctx inputs) elsegroup_opt
      in
      Il.RelD (id, nottyp, inputs, rulegroups, elsegroup_opt, hints) $ at
  | ExternDecD (id, tparams, params, typ, hints) ->
      Il.ExternDecD (id, tparams, params, typ, hints) $ at
  | BuiltinDecD (id, tparams, params, typ, hints) ->
      Il.BuiltinDecD (id, tparams, params, typ, hints) $ at
  | TableDecD (id, params, typ, tablerows, hints) ->
      let tablerows_il = List.map (analyze_tablerow dctx) tablerows in
      Il.TableDecD (id, params, typ, tablerows_il, hints) $ at
  | FuncDecD (id, tparams, params, typ, clauses, elseclause_opt, hints) ->
      let clauses = List.map (analyze_clause dctx) clauses in
      let elseclause_opt = Option.map (analyze_clause dctx) elseclause_opt in
      Il.FuncDecD (id, tparams, params, typ, clauses, elseclause_opt, hints)
      $ at

let analyze_spec (spec : spec) : Il.spec =
  let dctx = Dctx.init spec in
  List.map (analyze_def dctx) spec
