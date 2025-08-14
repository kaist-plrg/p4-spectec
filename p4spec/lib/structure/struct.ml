open Domain.Lib
open Il.Ast
module HEnv = Runtime_static.Envs.HEnv
module TDEnv = Runtime_dynamic_sl.Envs.TDEnv
open Util.Source

(* Structuring premises *)

let rec internalize_iter ?(iterexps : iterexp list = []) (prem : prem) :
    prem * iterexp list =
  match prem.it with
  | IterPr (prem, iterexp) ->
      internalize_iter ~iterexps:(iterexps @ [ iterexp ]) prem
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

let struct_rule_match_group (henv : HEnv.t) (tdenv : TDEnv.t) (frees : IdSet.t)
    (rule_match_group : (id * exp list * prem list) list) : Sl.Ast.relmatch =
  let id_rulegroup_group, exps_match_group, prems_match_group =
    List.fold_left
      (fun (id_rulegroup_group, exps_match_group, prems_match_group)
           (id_rulegroup, exps_match, prems_match) ->
        ( id_rulegroup_group @ [ id_rulegroup ],
          exps_match_group @ [ exps_match ],
          prems_match_group @ [ prems_match ] ))
      ([], [], []) rule_match_group
  in
  let exps_match_unified, prems_match_unified_group =
    Antiunify.antiunify_rule_match_group frees exps_match_group
  in
  let prems_match_group =
    List.map2
      (fun prems_match_unified prems_match -> prems_match_unified @ prems_match)
      prems_match_unified_group prems_match_group
  in
  let instrs_match =
    List.map2
      (fun id_rulegroup prems_match ->
        let instr_try = Ol.Ast.TryI id_rulegroup $ id_rulegroup.at in
        struct_prems prems_match instr_try)
      id_rulegroup_group prems_match_group
    |> List.concat
  in
  let instrs_match =
    instrs_match |> Optimize.optimize henv tdenv |> Instrument.instrument tdenv
  in
  (exps_match_unified, instrs_match)

let struct_rule_path (rule_path : id * prem list * exp list) : Ol.Ast.instr list
    =
  let _, prems, exps_output = rule_path in
  let instr_ret =
    let at = exps_output |> List.map at |> over_region in
    Ol.Ast.ResultI exps_output $ at
  in
  struct_prems prems instr_ret

let struct_rule_paths (henv : HEnv.t) (tdenv : TDEnv.t)
    (rule_paths : (id * prem list * exp list) list) : Sl.Ast.instr list =
  let instrs_path = List.concat_map struct_rule_path rule_paths in
  instrs_path |> Optimize.optimize henv tdenv |> Instrument.instrument tdenv

let struct_rule_paths_group (henv : HEnv.t) (tdenv : TDEnv.t)
    (rule_paths_group : (id * exp list * rulepath list) list) :
    Sl.Ast.relpath list =
  let id_rulegroup_group, exps_input_group, rulepaths_group =
    List.fold_left
      (fun (id_rulegroup_group, exps_input_group, rulepaths_group)
           (id_rulegroup, exps_input, rulepaths) ->
        ( id_rulegroup_group @ [ id_rulegroup ],
          exps_input_group @ [ exps_input ],
          rulepaths_group @ [ rulepaths ] ))
      ([], [], []) rule_paths_group
  in
  let instrs_path_group =
    List.map (struct_rule_paths henv tdenv) rulepaths_group
  in
  List.combine id_rulegroup_group exps_input_group
  |> List.map2
       (fun instrs_path (id_rulegroup, exps_input) ->
         (id_rulegroup, exps_input, instrs_path))
       instrs_path_group

(* Structuring clauses *)

let struct_clause_path ((prems, exp_output) : prem list * exp) :
    Ol.Ast.instr list =
  let at = exp_output.at in
  let instr_ret = Ol.Ast.ReturnI exp_output $ at in
  struct_prems prems instr_ret

(* Structuring definitions *)

let rec struct_def (henv : HEnv.t) (tdenv : TDEnv.t) (def : def) : Sl.Ast.def =
  let at = def.at in
  match def.it with
  | TypD (id, tparams, deftyp) -> Sl.Ast.TypD (id, tparams, deftyp) $ at
  | RelD (id, nottyp, inputs, rulegroups) ->
      struct_rel_def henv tdenv at id nottyp inputs rulegroups
  | DecD (id, tparams, _params, _typ, clauses) ->
      struct_dec_def henv tdenv at id tparams clauses

(* Structuring relation definitions *)

and struct_rel_def (henv : HEnv.t) (tdenv : TDEnv.t) (at : region) (id_rel : id)
    (nottyp : nottyp) (inputs : int list) (rulegroups : rulegroup list) :
    Sl.Ast.def =
  let mixop, _ = nottyp.it in
  let rule_match_group, rule_paths_group =
    List.fold_left
      (fun (rule_match_group, rule_paths_group) rulegroup ->
        let id_rulegroup, rulematch, rulepaths = rulegroup.it in
        let exps_input_expl, exps_input_impl, prems_input_impl = rulematch in
        ( rule_match_group
          @ [ (id_rulegroup, exps_input_impl, prems_input_impl) ],
          rule_paths_group @ [ (id_rulegroup, exps_input_expl, rulepaths) ] ))
      ([], []) rulegroups
  in
  let frees =
    let frees_match =
      rule_match_group
      |> List.map (fun (_, exps_input_impl, prems_input_impl) ->
             IdSet.union
               (Il.Free.free_exps exps_input_impl)
               (Il.Free.free_prems prems_input_impl))
      |> List.fold_left IdSet.union IdSet.empty
    in
    let frees_path =
      rule_paths_group
      |> List.map (fun (_, exps_input_expl, rulepaths) ->
             rulepaths
             |> List.map (fun (_, prems, exps_output) ->
                    IdSet.union (Il.Free.free_prems prems)
                      (Il.Free.free_exps exps_output))
             |> List.fold_left IdSet.union (Il.Free.free_exps exps_input_expl))
      |> List.fold_left IdSet.union IdSet.empty
    in
    IdSet.union frees_match frees_path
  in
  let relmatch = struct_rule_match_group henv tdenv frees rule_match_group in
  let relpaths = struct_rule_paths_group henv tdenv rule_paths_group in
  Sl.Ast.RelD (id_rel, (mixop, inputs), relmatch, relpaths) $ at

(* Structuring declaration definitions *)

and struct_dec_def (henv : HEnv.t) (tdenv : TDEnv.t) (at : region) (id_dec : id)
    (tparams : tparam list) (clauses : clause list) : Sl.Ast.def =
  let args_input, paths = Antiunify.antiunify_clauses clauses in
  let instrs = List.concat_map struct_clause_path paths in
  let instrs = Optimize.optimize henv tdenv instrs in
  let instrs = Instrument.instrument tdenv instrs in
  Sl.Ast.DecD (id_dec, tparams, args_input, instrs) $ at

(* Load type definitions *)

let load_def (henv : HEnv.t) (tdenv : TDEnv.t) (def : def) : HEnv.t * TDEnv.t =
  match def.it with
  | TypD (id, tparams, deftyp) ->
      let typdef = (tparams, deftyp) in
      let tdenv = TDEnv.add id typdef tdenv in
      (henv, tdenv)
  | RelD (id, _, inputs, _) ->
      let henv = HEnv.add id inputs henv in
      (henv, tdenv)
  | _ -> (henv, tdenv)

let load_spec (henv : HEnv.t) (tdenv : TDEnv.t) (spec : spec) : HEnv.t * TDEnv.t
    =
  List.fold_left
    (fun (henv, tdenv) def -> load_def henv tdenv def)
    (henv, tdenv) spec

(* Structuring a spec *)

let struct_spec (spec : spec) : Sl.Ast.spec =
  let henv, tdenv = load_spec HEnv.empty TDEnv.empty spec in
  List.map (struct_def henv tdenv) spec
