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

(* Clause binding analysis *)

let analyze_clause (dctx : Dctx.t) (clause : clause) : clause =
  let args, exp, prems = clause.it in
  let frees = Free.free_clause clause in
  let dctx = Dctx.add_frees dctx frees in
  let dctx, venv_args, args, prems_from_args = analyze_args_as_bind dctx args in
  let dctx = Dctx.add_bounds dctx venv_args in
  let dctx, prems_analyzed =
    List.fold_left
      (fun (dctx, acc) prem ->
        let dctx, venv, prem, extra_prems =
          analyze_prem dctx Iterctx.empty prem
        in
        let dctx = Dctx.add_bounds dctx venv in
        (dctx, acc @ [ prem ] @ extra_prems))
      (dctx, []) prems
  in
  analyze_exp_as_bound dctx exp;
  let final_prems = prems_from_args @ prems_analyzed in
  (args, exp, final_prems) $ clause.at

let analyze_def (dctx : Dctx.t) (def : def) : def =
  match def.it with
  | FuncDecD (id, tparams, params, typ, clauses, elseclause_opt, hints) ->
      let clauses = List.map (analyze_clause dctx) clauses in
      let elseclause_opt = Option.map (analyze_clause dctx) elseclause_opt in
      let def =
        FuncDecD (id, tparams, params, typ, clauses, elseclause_opt, hints)
        $ def.at
      in
      def
  | _ -> def

let analyze_spec (spec : spec) : spec =
  let dctx = Dctx.init spec in
  List.map (analyze_def dctx) spec
