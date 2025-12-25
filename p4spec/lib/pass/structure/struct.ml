open Domain.Lib
open Lang
open Il
module IEnv = Runtime.Static.Envs.IEnv
module TypDef = Runtime.Dynamic_Sl.Typdef
module TDEnv = Runtime.Dynamic_Sl.Envs.TDEnv
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
    (exps_match_input_group : exp list list)
    (prems_match_group : prem list list) : exp list * prem list list =
  let exps_match_input_unified, prems_match_unified_group =
    Antiunify.antiunify_rule_match_group frees exps_match_input_group
  in
  let prems_match_group =
    List.map2 ( @ ) prems_match_unified_group prems_match_group
  in
  (exps_match_input_unified, prems_match_group)

let struct_rule_paths (prems_path : prem list) (exps_output : exp list) :
    Ol.Ast.instr list =
  let at = exps_output |> List.map Util.Source.at |> over_region in
  let instr_res = Ol.Ast.ResultI exps_output $ at in
  struct_prems prems_path instr_res

let struct_rule_group (prems_match : prem list) (id_rulegroup : id)
    (exps_signature : exp list) (rulepaths : rulepath list) : Ol.Ast.instr list
    =
  let instrs_path =
    List.map
      (fun (_, prems_path, exps_output) ->
        struct_rule_paths prems_path exps_output)
      rulepaths
    |> Merge.merge_blocks
  in
  let instr_group =
    Ol.Ast.GroupI (id_rulegroup, exps_signature, instrs_path) $ id_rulegroup.at
  in
  struct_prems prems_match instr_group

(* Structuring clauses *)

let struct_clause_path ((prems, exp_output) : prem list * exp) :
    Ol.Ast.instr list =
  let at = exp_output.at in
  let instr_ret = Ol.Ast.ReturnI exp_output $ at in
  struct_prems prems instr_ret

(* Structuring table rows *)

let struct_tablerow_path ((prems, exp_output) : prem list * exp) :
    Ol.Ast.instr list =
  let at = exp_output.at in
  let instr_ret = Ol.Ast.ReturnI exp_output $ at in
  struct_prems prems instr_ret

(* Structuring definitions *)

let rec struct_def (ienv : IEnv.t) (tdenv : TDEnv.t) (def : def) : Sl.def =
  let at = def.at in
  match def.it with
  | ExternTypD (id, hints) -> Sl.ExternTypD (id, hints) $ at
  | TypD (id, tparams, deftyp, hints) ->
      Sl.TypD (id, tparams, deftyp, hints) $ at
  | ExternRelD (id, nottyp, inputs, hints) ->
      struct_extern_rel_def at id nottyp inputs hints
  | RelD (id, nottyp, inputs, rulegroups, hints) ->
      struct_defined_rel_def ienv tdenv at id nottyp inputs rulegroups hints
  | ExternDecD (id, tparams, params, typ, hints) ->
      struct_extern_dec_def at id tparams params typ hints
  | BuiltinDecD (id, tparams, params, typ, hints) ->
      struct_builtin_dec_def at id tparams params typ hints
  | TableDecD (id, _params, typ, tablerows, hints) ->
      struct_table_dec_def ienv tdenv at id tablerows typ hints
  | FuncDecD (id, tparams, _params, typ, clauses, hints) ->
      struct_func_dec_def ienv tdenv at id tparams typ clauses hints

(* Structuring relation definitions *)

and struct_extern_rel_def (at : region) (id_rel : id) (nottyp : nottyp)
    (inputs : int list) (hints : hint list) : Sl.def =
  let mixop, typs = nottyp.it in
  let typs_match = List.map (fun i -> List.nth typs i) inputs in
  let exps_match, _ =
    List.fold_left
      (fun (exps_match, frees) typ_match ->
        let exp_match, frees =
          Elaborate.Fresh.fresh_exp_from_typ frees typ_match
        in
        (exps_match @ [ exp_match ], frees))
      ([], IdSet.empty) typs_match
  in
  let externrel = (id_rel, (mixop, inputs), exps_match, hints) in
  Sl.ExternRelD externrel $ at

and struct_defined_rel_def (ienv : IEnv.t) (tdenv : TDEnv.t) (at : region)
    (id_rel : id) (nottyp : nottyp) (inputs : int list)
    (rulegroups : rulegroup list) (hints : hint list) : Sl.def =
  let mixop, _ = nottyp.it in
  let frees = Il.Free.free_rulegroups rulegroups in
  let rulegroups, exps_match_group, prems_match_group =
    List.fold_left
      (fun (rulegroups, exps_match_input_group, prems_match_group) rulegroup ->
        let id_rulegroup, rulematch, rulepaths = rulegroup.it in
        let exps_match_signature, exps_match_input, prems_match = rulematch in
        let rulegroups =
          rulegroups @ [ (id_rulegroup, exps_match_signature, rulepaths) ]
        in
        let exps_match_input_group =
          exps_match_input_group @ [ exps_match_input ]
        in
        let prems_match_group = prems_match_group @ [ prems_match ] in
        (rulegroups, exps_match_input_group, prems_match_group))
      ([], [], []) rulegroups
  in
  let exps_match_unified, prems_match_group =
    match rulegroups with
    | [] ->
        let _, typs = nottyp.it in
        let typs_match = List.map (fun i -> List.nth typs i) inputs in
        let exps_match, _ =
          List.fold_left
            (fun (exps_match, frees) typ_match ->
              let exp_match, frees =
                Elaborate.Fresh.fresh_exp_from_typ frees typ_match
              in
              (exps_match @ [ exp_match ], frees))
            ([], IdSet.empty) typs_match
        in
        (exps_match, [])
    | _ -> struct_rule_matches frees exps_match_group prems_match_group
  in
  let instrs =
    List.map2
      (fun prems_match (id_rulegroup, exps_match_signature, rulepaths) ->
        struct_rule_group prems_match id_rulegroup exps_match_signature
          rulepaths)
      prems_match_group rulegroups
    |> Merge.merge_blocks
  in
  let instrs = Optimize.optimize ienv tdenv instrs in
  let exps_match_unified, instrs =
    Pretty.pretty_rel exps_match_unified instrs
  in
  let instrs = Instrument.instrument tdenv instrs in
  Sl.RelD (id_rel, (mixop, inputs), exps_match_unified, instrs, hints) $ at

(* Structuring declaration definitions *)

and struct_extern_dec_def (at : region) (id_dec : id) (tparams : tparam list)
    (params : param list) (typ : typ) (hints : hint list) : Sl.def =
  let args_input, _ =
    List.fold_left
      (fun (args_input, frees) param ->
        let arg_input, frees =
          match param.it with
          | ExpP typ ->
              let exp_input, frees =
                Elaborate.Fresh.fresh_exp_from_typ frees typ
              in
              let arg_input = ExpA exp_input $ param.at in
              (arg_input, frees)
          | DefP (id_def, _, _, _) ->
              let arg_input = DefA id_def $ param.at in
              (arg_input, frees)
        in
        (args_input @ [ arg_input ], frees))
      ([], IdSet.empty) params
  in
  let externfunc = (id_dec, tparams, args_input, typ, hints) in
  Sl.ExternDecD externfunc $ at

and struct_builtin_dec_def (at : region) (id_dec : id) (tparams : tparam list)
    (params : param list) (typ : typ) (hints : hint list) : Sl.def =
  let args_input, _ =
    List.fold_left
      (fun (args_input, frees) param ->
        let arg_input, frees =
          match param.it with
          | ExpP typ ->
              let exp_input, frees =
                Elaborate.Fresh.fresh_exp_from_typ frees typ
              in
              let arg_input = ExpA exp_input $ param.at in
              (arg_input, frees)
          | DefP (id_def, _, _, _) ->
              let arg_input = DefA id_def $ param.at in
              (arg_input, frees)
        in
        (args_input @ [ arg_input ], frees))
      ([], IdSet.empty) params
  in
  let builtinfunc = (id_dec, tparams, args_input, typ, hints) in
  Sl.BuiltinDecD builtinfunc $ at

and struct_table_dec_def (ienv : IEnv.t) (tdenv : TDEnv.t) (at : region)
    (id_dec : id) (tablerows : tablerow list) (typ : typ) (hints : hint list) :
    Sl.def =
  let exps_signature_group, clauses =
    tablerows
    |> List.map (fun tablerow ->
           let exps_signature, args, exp_output, prems = tablerow.it in
           let clause = (args, exp_output, prems) $ tablerow.at in
           (exps_signature, clause))
    |> List.split
  in
  let args_input, paths = Antiunify.antiunify_clauses clauses in
  let instrs_tablerows_group =
    paths
    |> List.map struct_tablerow_path
    |> List.map (Optimize.optimize ienv tdenv)
    |> List.map (Instrument.instrument tdenv)
  in
  let exp_output_group = paths |> List.split |> snd in
  let tablerows =
    List.combine exps_signature_group exp_output_group
    |> List.map2
         (fun instrs_tablerows (exps_signature, exp_output) ->
           (exps_signature, exp_output, instrs_tablerows))
         instrs_tablerows_group
  in
  let tablefunc = (id_dec, args_input, typ, tablerows, hints) in
  Sl.TableDecD tablefunc $ at

and struct_func_dec_def (ienv : IEnv.t) (tdenv : TDEnv.t) (at : region)
    (id_dec : id) (tparams : tparam list) (typ : typ) (clauses : clause list)
    (hints : hint list) : Sl.def =
  let args_input, paths = Antiunify.antiunify_clauses clauses in
  let instrs = paths |> List.map struct_clause_path |> Merge.merge_blocks in
  let instrs = Optimize.optimize ienv tdenv instrs in
  let args_input, instrs = Pretty.pretty_func args_input instrs in
  let instrs = Instrument.instrument tdenv instrs in
  let func = (id_dec, tparams, args_input, typ, instrs, hints) in
  Sl.FuncDecD func $ at

(* Load type definitions *)

let load_def (ienv : IEnv.t) (tdenv : TDEnv.t) (def : def) : IEnv.t * TDEnv.t =
  match def.it with
  | ExternTypD (id, _hints) ->
      let td = TypDef.Extern in
      let tdenv = TDEnv.add id td tdenv in
      (ienv, tdenv)
  | TypD (id, tparams, deftyp, _hints) ->
      let td = TypDef.Defined (tparams, deftyp) in
      let tdenv = TDEnv.add id td tdenv in
      (ienv, tdenv)
  | ExternRelD (id, _, inputs, _) | RelD (id, _, inputs, _, _) ->
      let ienv = IEnv.add id inputs ienv in
      (ienv, tdenv)
  | _ -> (ienv, tdenv)

let load_spec (ienv : IEnv.t) (tdenv : TDEnv.t) (spec : spec) : IEnv.t * TDEnv.t
    =
  List.fold_left
    (fun (ienv, tdenv) def -> load_def ienv tdenv def)
    (ienv, tdenv) spec

(* Structuring a spec *)

let struct_spec (spec : spec) : Sl.spec =
  let ienv, tdenv = load_spec IEnv.empty TDEnv.empty spec in
  List.map (struct_def ienv tdenv) spec
