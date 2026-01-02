open Lang
open Error
open Util.Source

(* Context *)

module SyntaxMap = Map.Make (Kinds.SyntaxId)
module RelationMap = Map.Make (Kinds.RelationId)
module RuleGroupMap = Map.Make (Kinds.RuleGroupId)
module RuleProseMap = Map.Make (Kinds.RuleProseId)
module FuncProseMap = Map.Make (Kinds.FuncProseId)
module TableMap = Map.Make (Kinds.TableId)

type t = {
  filename : string;
  mutable syntax : Kinds.syntax SyntaxMap.t;
  mutable relation : Kinds.relation RelationMap.t;
  mutable rulegroup : Kinds.rulegroup RuleGroupMap.t;
  mutable ruleprose : Kinds.ruleprose RuleProseMap.t;
  mutable funcprose : Kinds.funcprose FuncProseMap.t;
  mutable tables : Kinds.table TableMap.t;
}

(* Initialization *)

let init_el_def (ctx : t) (def_el : El.def) : unit =
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

let init_el (ctx : t) (spec_el : El.spec) : unit =
  List.iter (init_el_def ctx) spec_el

let init_pl_def (ctx : t) (def_sl : Pl.def) : unit =
  match def_sl.it with
  | RelD (rel_title, rulegroups) ->
      let id_rel =
        match rel_title with
        | Pl.ProseRelTitle (`Hold (id_rel, _, _))
        | Pl.ProseRelTitle (`Yield (id_rel, _, _, _, _))
        | Pl.MathRelTitle (id_rel, _, _) ->
            id_rel
      in
      List.iter
        (fun rulegroup ->
          let rulegroup_title, _ = rulegroup in
          let id_rulegroup =
            match rulegroup_title with
            | Pl.ProseRuleTitle (id_rulegroup, _, _)
            | Pl.MathRuleTitle (id_rulegroup, _, _) ->
                id_rulegroup
          in
          ctx.ruleprose <-
            RuleProseMap.add
              (id_rel.it, id_rulegroup.it)
              rulegroup ctx.ruleprose)
        rulegroups
  | TableDecD tablefunc ->
      let id_def, _, _, _ = tablefunc in
      ctx.tables <- TableMap.add id_def.it tablefunc ctx.tables
  | FuncDecD func ->
      let id_def, _, _, _, _ = func in
      ctx.funcprose <- FuncProseMap.add id_def.it func ctx.funcprose
  | _ -> ()

let init_pl (ctx : t) (spec_pl : Pl.spec) : unit =
  List.iter (init_pl_def ctx) spec_pl

let init (spec_el : El.spec) (spec_pl : Pl.spec) (filename : string) : t =
  let ctx =
    {
      filename;
      syntax = SyntaxMap.empty;
      relation = RelationMap.empty;
      rulegroup = RuleGroupMap.empty;
      ruleprose = RuleProseMap.empty;
      funcprose = FuncProseMap.empty;
      tables = TableMap.empty;
    }
  in
  init_el ctx spec_el;
  init_pl ctx spec_pl;
  ctx

(* Finders *)

let find_syntax (ctx : t) (id : Kinds.SyntaxId.t) : Kinds.syntax =
  match SyntaxMap.find_opt id ctx.syntax with
  | Some syntax -> syntax
  | None ->
      error no_region ("syntax " ^ id ^ " was not found in " ^ ctx.filename)

let find_relation (ctx : t) (id : Kinds.RelationId.t) : Kinds.relation =
  match RelationMap.find_opt id ctx.relation with
  | Some relation -> relation
  | None ->
      error no_region ("relation " ^ id ^ " was not found in " ^ ctx.filename)

let find_rulegroup (ctx : t) (id : Kinds.RuleGroupId.t) : Kinds.rulegroup =
  match RuleGroupMap.find_opt id ctx.rulegroup with
  | Some rulegroup -> rulegroup
  | None ->
      let id_rel, id_rulegroup = id in
      error no_region
        ("rulegroup " ^ id_rel ^ "/" ^ id_rulegroup ^ " was not found in "
       ^ ctx.filename)

let find_ruleprose (ctx : t) (id : Kinds.RuleProseId.t) : Kinds.ruleprose =
  match RuleProseMap.find_opt id ctx.ruleprose with
  | Some ruleprose -> ruleprose
  | None ->
      let id_rel, id_rulegroup = id in
      error no_region
        ("ruleprose " ^ id_rel ^ "/" ^ id_rulegroup ^ " was not found in "
       ^ ctx.filename)

let find_funcprose (ctx : t) (id : Kinds.FuncProseId.t) : Kinds.funcprose =
  match FuncProseMap.find_opt id ctx.funcprose with
  | Some funcprose -> funcprose
  | None ->
      error no_region ("funcprose " ^ id ^ " was not found in " ^ ctx.filename)

let find_table (ctx : t) (id : Kinds.TableId.t) : Kinds.table =
  match TableMap.find_opt id ctx.tables with
  | Some table -> table
  | None -> error no_region ("table " ^ id ^ " was not found in " ^ ctx.filename)
