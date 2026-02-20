open Domain.Lib
open Lang
open Il
open Runtime.Dynamic_Sl
open Envs
open Error
open Util.Source

(* Structuring parameters *)

let struct_param (frees : IdSet.t) (param : param) : IdSet.t * Sl.param =
  let at = param.at in
  match param.it with
  | ExpP typ ->
      let exp_input, frees = Fresh.fresh_exp_from_typ frees typ in
      let param = Sl.ExpP (typ, exp_input) $ at in
      (frees, param)
  | DefP (id_def, _, _, _) ->
      let param = Sl.DefP id_def $ at in
      (frees, param)

let struct_params (params : param list) : Sl.param list =
  params
  |> List.fold_left
       (fun (frees, params) param ->
         let frees, param = struct_param frees param in
         (frees, params @ [ param ]))
       (IdSet.empty, [])
  |> snd

let struct_params_from_args (params : param list) (args_input : arg list) :
    Sl.param list =
  List.map2
    (fun param arg_input ->
      let at = param.at in
      match (param.it, arg_input.it) with
      | ExpP typ, ExpA exp -> Sl.ExpP (typ, exp) $ at
      | DefP (id_def, _, _, _), DefA id_def_arg when Id.eq id_def id_def_arg ->
          Sl.DefP id_def $ at
      | _ -> assert false)
    params args_input

(* Structuring premises *)

let rec internalize_iter ?(iterprems : iterprem list = []) (prem : prem) :
    prem * iterprem list =
  match prem.it with
  | IterPr (prem, iterprem) ->
      internalize_iter ~iterprems:(iterprem :: iterprems) prem
  | _ -> (prem, iterprems)

let rec struct_prems (prems : prem list) (instr_ret : Ol.Ast.instr) :
    Ol.Ast.instr list =
  let prems_internalized = List.map internalize_iter prems in
  struct_prems' prems_internalized instr_ret

and struct_prems' (prems_internalized : (prem * iterprem list) list)
    (instr_ret : Ol.Ast.instr) : Ol.Ast.instr list =
  match prems_internalized with
  | [] -> [ instr_ret ]
  | (prem_h, iterprems_h) :: prems_internalized_t -> (
      let at = prem_h.at in
      match prem_h.it with
      | RulePr (id, notexp, inputs) ->
          let instrs_t = struct_prems' prems_internalized_t instr_ret in
          let instr_h =
            Ol.Ast.RuleI (id, notexp, inputs, iterprems_h, instrs_t) $ at
          in
          [ instr_h ]
      | IfPr exp ->
          let iterexps_h =
            List.map
              (function
                | iter, vars_bound, [] -> (iter, vars_bound)
                | _ -> error at "an if premise should not have bindings")
              iterprems_h
          in
          let instrs_t = struct_prems' prems_internalized_t instr_ret in
          let instr_h = Ol.Ast.IfI (exp, iterexps_h, instrs_t) $ at in
          [ instr_h ]
      | IfHoldPr (id, notexp) ->
          let iterexps_h =
            List.map
              (function
                | iter, vars_bound, [] -> (iter, vars_bound)
                | _ -> error at "an if holds premise should not have bindings")
              iterprems_h
          in
          let instrs_t = struct_prems' prems_internalized_t instr_ret in
          let instr_h =
            Ol.Ast.HoldI (id, notexp, iterexps_h, instrs_t, []) $ at
          in
          [ instr_h ]
      | IfNotHoldPr (id, notexp) ->
          let iterexps_h =
            List.map
              (function
                | iter, vars_bound, [] -> (iter, vars_bound)
                | _ ->
                    error at "an if not holds premise should not have bindings")
              iterprems_h
          in
          let instrs_t = struct_prems' prems_internalized_t instr_ret in
          let instr_h =
            Ol.Ast.HoldI (id, notexp, iterexps_h, [], instrs_t) $ at
          in
          [ instr_h ]
      | LetPr (exp_l, exp_r) ->
          let instrs_t = struct_prems' prems_internalized_t instr_ret in
          let instr_h =
            Ol.Ast.LetI (exp_l, exp_r, iterprems_h, instrs_t) $ at
          in
          [ instr_h ]
      | DebugPr exp ->
          let instr_h = Ol.Ast.DebugI exp $ at in
          let instrs_t = struct_prems' prems_internalized_t instr_ret in
          instr_h :: instrs_t
      | _ -> assert false)

(* Structuring rules *)

let struct_rule_matches (frees : IdSet.t)
    (exps_match_input_group : exp list list)
    (prems_match_group : prem list list) (exps_match_else_opt : exp list option)
    (prems_match_else_opt : prem list option) :
    exp list * prem list list * prem list option =
  let ( exps_match_input_unified,
        prems_match_unified_group,
        prems_match_unified_else_opt ) =
    Antiunify.antiunify_rule_match_group frees exps_match_input_group
      exps_match_else_opt
  in
  let prems_match_group =
    List.map2 ( @ ) prems_match_unified_group prems_match_group
  in
  let prems_match_else_opt =
    match (prems_match_unified_else_opt, prems_match_else_opt) with
    | Some prems_match_unified_else, Some prems_match_else ->
        Some (prems_match_unified_else @ prems_match_else)
    | _ -> None
  in
  (exps_match_input_unified, prems_match_group, prems_match_else_opt)

let struct_rule_paths (rel_signature : Ol.Ast.rel_signature)
    (prems_path : prem list) (exps_output : exp list) : Ol.Ast.block =
  let at = exps_output |> List.map Util.Source.at |> over_region in
  let instr_res = Ol.Ast.ResultI (rel_signature, exps_output) $ at in
  struct_prems prems_path instr_res

let struct_rule_group (rel_signature : Ol.Ast.rel_signature)
    (prems_match : prem list) (id_rulegroup : id) (exps_signature : exp list)
    (rulepaths : rulepath list) : Ol.Ast.block =
  let block_path =
    List.map
      (fun (_, prems_path, exps_output) ->
        struct_rule_paths rel_signature prems_path exps_output)
      rulepaths
    |> Merge.merge_blocks
  in
  let instr_group =
    Ol.Ast.GroupI (id_rulegroup, rel_signature, exps_signature, block_path)
    $ id_rulegroup.at
  in
  struct_prems prems_match instr_group

let struct_else_group (rel_signature : Ol.Ast.rel_signature)
    (prems_match : prem list) (id_rulegroup : id) (exps_signature : exp list)
    (rulepath : rulepath) : Ol.Ast.block =
  let _, prems_path, exps_output = rulepath in
  let block_path = struct_rule_paths rel_signature prems_path exps_output in
  let instr_group =
    Ol.Ast.GroupI (id_rulegroup, rel_signature, exps_signature, block_path)
    $ id_rulegroup.at
  in
  struct_prems prems_match instr_group

(* Structuring clauses *)

let struct_clause_path ((prems, exp_output) : prem list * exp) :
    Ol.Ast.instr list =
  let at = exp_output.at in
  let instr_ret = Ol.Ast.ReturnI exp_output $ at in
  struct_prems prems instr_ret

let struct_elseclause_path ((prems, exp_output) : prem list * exp) :
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

let rec struct_def (tdenv : TDEnv.t) (def : def) : Sl.def =
  let at = def.at in
  match def.it with
  | ExternTypD (id, hints) -> Sl.ExternTypD (id, hints) $ at
  | TypD (id, tparams, deftyp, hints) ->
      Sl.TypD (id, tparams, deftyp, hints) $ at
  | ExternRelD (id, nottyp, inputs, hints) ->
      struct_extern_rel_def at id nottyp inputs hints
  | RelD (id, nottyp, inputs, rulegroups, elsegroup_opt, hints) ->
      struct_defined_rel_def tdenv at id nottyp inputs rulegroups elsegroup_opt
        hints
  | ExternDecD (id, tparams, params, typ, hints) ->
      struct_extern_dec_def at id tparams params typ hints
  | BuiltinDecD (id, tparams, params, typ, hints) ->
      struct_builtin_dec_def at id tparams params typ hints
  | TableDecD (id, params, typ, tablerows, hints) ->
      struct_table_dec_def tdenv at id params tablerows typ hints
  | FuncDecD (id, tparams, params, typ, clauses, elseclause_opt, hints) ->
      struct_func_dec_def tdenv at id tparams params typ clauses elseclause_opt
        hints

(* Structuring relation definitions *)

and struct_extern_rel_def (at : region) (id_rel : id) (nottyp : nottyp)
    (inputs : int list) (hints : hint list) : Sl.def =
  let _, typs = nottyp.it in
  let typs_match = List.map (fun i -> List.nth typs i) inputs in
  let exps_match, _ =
    List.fold_left
      (fun (exps_match, frees) typ_match ->
        let exp_match, frees = Fresh.fresh_exp_from_typ frees typ_match in
        (exps_match @ [ exp_match ], frees))
      ([], IdSet.empty) typs_match
  in
  let externrel = (id_rel, (nottyp, inputs), exps_match, hints) in
  Sl.ExternRelD externrel $ at

and struct_defined_rel_def (tdenv : TDEnv.t) (at : region) (id_rel : id)
    (nottyp : nottyp) (inputs : int list) (rulegroups : rulegroup list)
    (elsegroup_opt : elsegroup option) (hints : hint list) : Sl.def =
  let frees =
    IdSet.union
      (Il.Free.free_rulegroups rulegroups)
      (Il.Free.free_elsegroup_opt elsegroup_opt)
  in
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
  let elsegroup_opt, exps_match_else_opt, prems_match_else_opt =
    match elsegroup_opt with
    | Some elsegroup ->
        let id_rulegroup, rulematch, rulepath = elsegroup.it in
        let exps_match_signature, exps_match_input, prems_match = rulematch in
        let elsegroup = (id_rulegroup, exps_match_signature, rulepath) in
        (Some elsegroup, Some exps_match_input, Some prems_match)
    | None -> (None, None, None)
  in
  let exps_match_unified, prems_match_group, prems_match_else_opt =
    match (rulegroups, elsegroup_opt) with
    | [], None ->
        let _, typs = nottyp.it in
        let typs_match = List.map (fun i -> List.nth typs i) inputs in
        let exps_match, _ =
          List.fold_left
            (fun (exps_match, frees) typ_match ->
              let exp_match, frees = Fresh.fresh_exp_from_typ frees typ_match in
              (exps_match @ [ exp_match ], frees))
            ([], IdSet.empty) typs_match
        in
        (exps_match, [], None)
    | _ ->
        let exps_match_unified, prems_match_group, prems_match_else_opt =
          struct_rule_matches frees exps_match_group prems_match_group
            exps_match_else_opt prems_match_else_opt
        in
        (exps_match_unified, prems_match_group, prems_match_else_opt)
  in
  let rel_signature = (nottyp, inputs) in
  let block =
    List.map2
      (fun prems_match (id_rulegroup, exps_match_signature, rulepaths) ->
        struct_rule_group rel_signature prems_match id_rulegroup
          exps_match_signature rulepaths)
      prems_match_group rulegroups
    |> Merge.merge_blocks
  in
  let elseblock_opt =
    match (prems_match_else_opt, elsegroup_opt) with
    | Some prems_match_else, Some (id_rulegroup, exps_match_signature, rulepath)
      ->
        let elseblock =
          struct_else_group rel_signature prems_match_else id_rulegroup
            exps_match_signature rulepath
        in
        Some elseblock
    | _ -> None
  in
  let block, elseblock_opt =
    Optimize.optimize_with_else tdenv block elseblock_opt
  in
  let block, elseblock_opt = Totalize.totalize tdenv block elseblock_opt in
  let exps_match_unified, block, elseblock_opt =
    Prettify.pretty_rel exps_match_unified block elseblock_opt
  in
  let block, elseblock_opt = Instrument.instrument block elseblock_opt in
  Sl.RelD
    (id_rel, rel_signature, exps_match_unified, block, elseblock_opt, hints)
  $ at

(* Structuring declaration definitions *)

and struct_extern_dec_def (at : region) (id_dec : id) (tparams : tparam list)
    (params : param list) (typ : typ) (hints : hint list) : Sl.def =
  let params = struct_params params in
  let externfunc = (id_dec, tparams, params, typ, hints) in
  Sl.ExternDecD externfunc $ at

and struct_builtin_dec_def (at : region) (id_dec : id) (tparams : tparam list)
    (params : param list) (typ : typ) (hints : hint list) : Sl.def =
  let params = struct_params params in
  let builtinfunc = (id_dec, tparams, params, typ, hints) in
  Sl.BuiltinDecD builtinfunc $ at

and struct_table_dec_def (tdenv : TDEnv.t) (at : region) (id_dec : id)
    (params : param list) (tablerows : tablerow list) (typ : typ)
    (hints : hint list) : Sl.def =
  let exps_signature_group, clauses =
    tablerows
    |> List.map (fun tablerow ->
           let exps_signature, args, exp_output, prems = tablerow.it in
           let clause = (args, exp_output, prems) $ tablerow.at in
           (exps_signature, clause))
    |> List.split
  in
  let args_input, paths, _ = Antiunify.antiunify_clauses clauses None in
  let params = struct_params_from_args params args_input in
  let blocks_tablerows =
    paths
    |> List.map struct_tablerow_path
    |> List.map (Optimize.optimize_without_else tdenv)
    |> List.map (Totalize.totalize_without_else tdenv)
    |> List.map Instrument.instrument_without_else
  in
  let exp_output_group = paths |> List.split |> snd in
  let tablerows =
    List.combine exps_signature_group exp_output_group
    |> List.map2
         (fun instrs_tablerows (exps_signature, exp_output) ->
           (exps_signature, exp_output, instrs_tablerows))
         blocks_tablerows
  in
  let tablefunc = (id_dec, params, typ, tablerows, hints) in
  Sl.TableDecD tablefunc $ at

and struct_func_dec_def (tdenv : TDEnv.t) (at : region) (id_dec : id)
    (tparams : tparam list) (params : param list) (typ : typ)
    (clauses : clause list) (elseclause_opt : clause option) (hints : hint list)
    : Sl.def =
  let args_input, paths, path_else_opt =
    Antiunify.antiunify_clauses clauses elseclause_opt
  in
  let block = paths |> List.map struct_clause_path |> Merge.merge_blocks in
  let elseblock_opt = Option.map struct_elseclause_path path_else_opt in
  let block, elseblock_opt =
    Optimize.optimize_with_else tdenv block elseblock_opt
  in
  let block, elseblock_opt = Totalize.totalize tdenv block elseblock_opt in
  let args_input, block, elseblock_opt =
    Prettify.pretty_func args_input block elseblock_opt
  in
  let params = struct_params_from_args params args_input in
  let block, elseblock_opt = Instrument.instrument block elseblock_opt in
  let func = (id_dec, tparams, params, typ, block, elseblock_opt, hints) in
  Sl.FuncDecD func $ at

(* Load type definitions *)

let load_def (tdenv : TDEnv.t) (def : def) : TDEnv.t =
  match def.it with
  | ExternTypD (id, _hints) ->
      let td = Typdef.Extern in
      TDEnv.add id td tdenv
  | TypD (id, tparams, deftyp, _hints) ->
      let td = Typdef.Defined (tparams, deftyp) in
      TDEnv.add id td tdenv
  | _ -> tdenv

let load_spec (tdenv : TDEnv.t) (spec : spec) : TDEnv.t =
  List.fold_left load_def tdenv spec

(* Entry point *)

let struct_spec (spec : spec) : Sl.spec =
  let tdenv = load_spec TDEnv.empty spec in
  List.map (struct_def tdenv) spec
