open Domain.Lib
open Il.Ast
module IEnv = Runtime_static.Envs.IEnv
module TDEnv = Runtime_dynamic_sl.Envs.TDEnv
open Util.Source

(* Structuring premises *)

let rec internalize_iter ?(iterexps : iterexp list = []) (prem : prem) :
    prem * iterexp list =
  match prem.it with
  | IterPr (prem, iterexp) ->
      internalize_iter ~iterexps:(iterexp :: iterexps) prem
  | _ -> (prem, iterexps)

let rec struct_prems (prems : prem list) (instr_ret : Ol.Ast.instr) :
    Ol.Ast.instr list =
  let prems_internalized = List.map internalize_iter prems in
  struct_prems' prems_internalized instr_ret

and struct_prems' (prems_internalized : (prem * iterexp list) list)
    (instr_ret : Ol.Ast.instr) : Ol.Ast.instr list =
  match prems_internalized with
  | [] -> [ instr_ret ]
  | [ ({ it = ElsePr; at; _ }, []) ] ->
      let instr = Ol.Ast.OtherwiseI instr_ret $ at in
      [ instr ]
  | (prem_h, iterexps_h) :: prems_internalized_t -> (
      let at = prem_h.at in
      match prem_h.it with
      | RulePr (id, notexp) ->
          let instr_h = Ol.Ast.RuleI (id, notexp, iterexps_h) $ at in
          let instrs_t = struct_prems' prems_internalized_t instr_ret in
          instr_h :: instrs_t
      | IfPr exp ->
          let instrs_t = struct_prems' prems_internalized_t instr_ret in
          let instr_h = Ol.Ast.IfI (exp, iterexps_h, instrs_t) $ at in
          [ instr_h ]
      | IfHoldPr (id, notexp) ->
          let instrs_t = struct_prems' prems_internalized_t instr_ret in
          let instr_h =
            Ol.Ast.HoldI (id, notexp, iterexps_h, instrs_t, []) $ at
          in
          [ instr_h ]
      | IfNotHoldPr (id, notexp) ->
          let instrs_t = struct_prems' prems_internalized_t instr_ret in
          let instr_h =
            Ol.Ast.HoldI (id, notexp, iterexps_h, [], instrs_t) $ at
          in
          [ instr_h ]
      | LetPr (exp_l, exp_r) ->
          let instr_h = Ol.Ast.LetI (exp_l, exp_r, iterexps_h) $ at in
          let instrs_t = struct_prems' prems_internalized_t instr_ret in
          instr_h :: instrs_t
      | DebugPr exp ->
          let instr_h = Ol.Ast.DebugI exp $ at in
          let instrs_t = struct_prems' prems_internalized_t instr_ret in
          instr_h :: instrs_t
      | _ -> assert false)

(* Structuring rules *)

let struct_rule_matches (frees : IdSet.t)
    (exps_match_impl_group : exp list list) (prems_match_group : prem list list)
    : exp list * prem list list =
  let exps_match_unified, prems_match_unified_group =
    Antiunify.antiunify_rule_match_group frees exps_match_impl_group
  in
  let prems_match_group =
    List.map2 ( @ ) prems_match_unified_group prems_match_group
  in
  (exps_match_unified, prems_match_group)

let struct_rule_paths (prems_path : prem list) (exps_output : exp list) :
    Ol.Ast.instr list =
  let at = exps_output |> List.map Util.Source.at |> over_region in
  let instr_res = Ol.Ast.ResultI exps_output $ at in
  struct_prems prems_path instr_res

let struct_rule_group (prems_match : prem list) (id_rulegroup : id)
    (exps_match_expl : exp list) (rulepaths : rulepath list) : Ol.Ast.instr list
    =
  let instrs_path =
    List.map
      (fun (_, prems_path, exps_output) ->
        struct_rule_paths prems_path exps_output)
      rulepaths
    |> List.concat
  in
  let instr_group =
    Ol.Ast.GroupI (id_rulegroup, exps_match_expl, instrs_path) $ id_rulegroup.at
  in
  struct_prems prems_match instr_group

(* Structuring clauses *)

let struct_clause_path ((prems, exp_output) : prem list * exp) :
    Ol.Ast.instr list =
  let at = exp_output.at in
  let instr_ret = Ol.Ast.ReturnI exp_output $ at in
  struct_prems prems instr_ret

(* Structuring definitions *)

let rec struct_def (ienv : IEnv.t) (tdenv : TDEnv.t) (def : def) : Sl.Ast.def =
  let at = def.at in
  match def.it with
  | TypD (id, tparams, deftyp) -> Sl.Ast.TypD (id, tparams, deftyp) $ at
  | RelD (id, nottyp, inputs, rulegroups, hints) ->
      struct_rel_def ienv tdenv at id nottyp inputs rulegroups hints
  | DecD (id, tparams, _params, typ, clauses, hints) ->
      struct_dec_def ienv tdenv at id tparams typ clauses hints

(* Structuring relation definitions *)

and struct_rel_def (ienv : IEnv.t) (tdenv : TDEnv.t) (at : region) (id_rel : id)
    (nottyp : nottyp) (inputs : int list) (rulegroups : rulegroup list)
    (hints : hint list) : Sl.Ast.def =
  let mixop, _ = nottyp.it in
  let frees = Il.Free.free_rulegroups rulegroups in
  let rulegroups, exps_match_impl_group, prems_match_group =
    List.fold_left
      (fun (rulegroups, exps_match_impl_group, prems_match_group) rulegroup ->
        let id_rulegroup, rulematch, rulepaths = rulegroup.it in
        let exps_match_expl, exps_match_impl, prems_match = rulematch in
        let rulegroups =
          rulegroups @ [ (id_rulegroup, exps_match_expl, rulepaths) ]
        in
        let exps_match_impl_group =
          exps_match_impl_group @ [ exps_match_impl ]
        in
        let prems_match_group = prems_match_group @ [ prems_match ] in
        (rulegroups, exps_match_impl_group, prems_match_group))
      ([], [], []) rulegroups
  in
  let exps_match_unified, prems_match_group =
    struct_rule_matches frees exps_match_impl_group prems_match_group
  in
  let instrs =
    List.map2
      (fun prems_match (id_rulegroup, exps_match_expl, rulepaths) ->
        struct_rule_group prems_match id_rulegroup exps_match_expl rulepaths)
      prems_match_group rulegroups
    |> List.concat
  in
  let instrs =
    instrs |> Optimize.optimize ienv tdenv |> Instrument.instrument tdenv
  in
  Sl.Ast.RelD (id_rel, (mixop, inputs), exps_match_unified, instrs, hints) $ at

(* Structuring declaration definitions *)

and struct_dec_def (ienv : IEnv.t) (tdenv : TDEnv.t) (at : region) (id_dec : id)
    (tparams : tparam list) (typ : typ) (clauses : clause list)
    (hints : hint list) : Sl.Ast.def =
  let args_input, paths = Antiunify.antiunify_clauses clauses in
  let instrs = List.concat_map struct_clause_path paths in
  let instrs = Optimize.optimize ienv tdenv instrs in
  let instrs = Instrument.instrument tdenv instrs in
  Sl.Ast.DecD (id_dec, tparams, args_input, typ, instrs, hints) $ at

(* Load type definitions *)

let load_def (ienv : IEnv.t) (tdenv : TDEnv.t) (def : def) : IEnv.t * TDEnv.t =
  match def.it with
  | TypD (id, tparams, deftyp) ->
      let typdef = (tparams, deftyp) in
      let tdenv = TDEnv.add id typdef tdenv in
      (ienv, tdenv)
  | RelD (id, _, inputs, _, _hints) ->
      let ienv = IEnv.add id inputs ienv in
      (ienv, tdenv)
  | _ -> (ienv, tdenv)

let load_spec (ienv : IEnv.t) (tdenv : TDEnv.t) (spec : spec) : IEnv.t * TDEnv.t
    =
  List.fold_left
    (fun (ienv, tdenv) def -> load_def ienv tdenv def)
    (ienv, tdenv) spec

(* Structuring a spec *)

let struct_spec (spec : spec) : Sl.Ast.spec =
  let ienv, tdenv = load_spec IEnv.empty TDEnv.empty spec in
  List.map (struct_def ienv tdenv) spec
