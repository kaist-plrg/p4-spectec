open Error
open Util.Source
module SyntaxMap = Map.Make (Kinds.SyntaxId)
module RelationMap = Map.Make (Kinds.RelationId)
module RuleGroupMap = Map.Make (Kinds.RuleGroupId)
module RuleProseMap = Map.Make (Kinds.RuleGroupId)

type t = {
  mutable syntax : Kinds.syntax SyntaxMap.t;
  mutable relation : Kinds.relation RelationMap.t;
  mutable rulegroup : Kinds.rulegroup RuleGroupMap.t;
  mutable ruleprose : Kinds.ruleprose RuleProseMap.t;
}

(* Initialization *)

let init_el_def (ctx : t) (def_el : El.Ast.def) : unit =
  match def_el.it with
  | TypD (id_syntax, tparams, deftyp, hints) ->
      let syntax = (tparams, deftyp, hints) in
      ctx.syntax <- SyntaxMap.add id_syntax.it syntax ctx.syntax
  | RelD (id_rel, nottyp, hints) ->
      let relation = (nottyp, hints) in
      ctx.relation <- RelationMap.add id_rel.it relation ctx.relation
  | RuleGroupD (id_rel, id_rulegroup, rules) ->
      let rulegroup = rules in
      ctx.rulegroup <-
        RuleGroupMap.add (id_rel.it, id_rulegroup.it) rulegroup ctx.rulegroup
  | _ -> ()

let init_el (ctx : t) (spec_el : El.Ast.spec) : unit =
  List.iter (init_el_def ctx) spec_el

let rec init_sl_rule_instr (ctx : t) (id_rel : Sl.Ast.id) (mixop : Sl.Ast.mixop)
    (inputs : int list) (instr : Sl.Ast.instr) : unit =
  match instr.it with
  | IfI (_, _, instrs_then, _) ->
      init_sl_rule_instrs ctx id_rel mixop inputs instrs_then
  | HoldI (_, _, _, holdcase) -> (
      match holdcase with
      | BothH (instrs_hold, instrs_nothold) ->
          init_sl_rule_instrs ctx id_rel mixop inputs instrs_hold;
          init_sl_rule_instrs ctx id_rel mixop inputs instrs_nothold
      | HoldH (instrs_hold, _) ->
          init_sl_rule_instrs ctx id_rel mixop inputs instrs_hold
      | NotHoldH (instrs_nothold, _) ->
          init_sl_rule_instrs ctx id_rel mixop inputs instrs_nothold)
  | CaseI (_, cases, _) ->
      let instrs_group = cases |> List.map snd in
      List.iter (init_sl_rule_instrs ctx id_rel mixop inputs) instrs_group
  | GroupI (id_rulegroup, exps_input, instrs_group) ->
      let ruleprose = (mixop, inputs, exps_input, instrs_group) in
      ctx.ruleprose <-
        RuleProseMap.add (id_rel.it, id_rulegroup.it) ruleprose ctx.ruleprose
  | _ -> ()

and init_sl_rule_instrs (ctx : t) (id_rel : Sl.Ast.id) (mixop : Sl.Ast.mixop)
    (inputs : int list) (instrs : Sl.Ast.instr list) : unit =
  List.iter (init_sl_rule_instr ctx id_rel mixop inputs) instrs

let init_sl_def (ctx : t) (def_sl : Sl.Ast.def) : unit =
  match def_sl.it with
  | RelD (id_rel, (mixop, inputs), _, instrs) ->
      init_sl_rule_instrs ctx id_rel mixop inputs instrs
  | _ -> ()

let init_sl (ctx : t) (spec_sl : Sl.Ast.spec) : unit =
  List.iter (init_sl_def ctx) spec_sl

let init (spec_el : El.Ast.spec) (spec_sl : Sl.Ast.spec) : t =
  let ctx =
    {
      syntax = SyntaxMap.empty;
      relation = RelationMap.empty;
      rulegroup = RuleGroupMap.empty;
      ruleprose = RuleProseMap.empty;
    }
  in
  init_el ctx spec_el;
  init_sl ctx spec_sl;
  ctx

(* Finders *)

let find_syntax (ctx : t) (id : Kinds.SyntaxId.t) : Kinds.syntax =
  match SyntaxMap.find_opt id ctx.syntax with
  | Some syntax -> syntax
  | None -> error no_region ("syntax " ^ id ^ " was not found")

let find_relation (ctx : t) (id : Kinds.RelationId.t) : Kinds.relation =
  match RelationMap.find_opt id ctx.relation with
  | Some relation -> relation
  | None -> error no_region ("relation " ^ id ^ " was not found")

let find_rulegroup (ctx : t) (id : Kinds.RuleGroupId.t) : Kinds.rulegroup =
  match RuleGroupMap.find_opt id ctx.rulegroup with
  | Some rulegroup -> rulegroup
  | None ->
      let id_rel, id_rulegroup = id in
      error no_region
        ("rulegroup " ^ id_rel ^ "/" ^ id_rulegroup ^ " was not found")

let find_ruleprose (ctx : t) (id : Kinds.RuleGroupId.t) : Kinds.ruleprose =
  match RuleProseMap.find_opt id ctx.ruleprose with
  | Some ruleprose -> ruleprose
  | None ->
      let id_rel, id_rulegroup = id in
      error no_region
        ("ruleprose " ^ id_rel ^ "/" ^ id_rulegroup ^ " was not found")
