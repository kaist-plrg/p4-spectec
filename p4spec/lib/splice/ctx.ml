open Error
open Util.Source

(* Context for splicer *)

module SyntaxId = String
module SyntaxMap = Map.Make (SyntaxId)
module RelationId = String
module RelationMap = Map.Make (RelationId)

module RuleGroupId = struct
  type t = string * string

  let compare (id_rel_a, id_rulegroup_a) (id_rel_b, id_rulegroup_b) =
    let c = String.compare id_rel_a id_rel_b in
    if c <> 0 then c else String.compare id_rulegroup_a id_rulegroup_b
end

module RuleGroupMap = Map.Make (RuleGroupId)

type syntax = El.Ast.tparam list * El.Ast.deftyp * El.Ast.hint list * region
type relation = El.Ast.nottyp * El.Ast.hint list * region
type rulegroup = El.Ast.rule list * region

type t = {
  mutable syntax : syntax SyntaxMap.t;
  mutable relation : relation RelationMap.t;
  mutable rulegroup : rulegroup RuleGroupMap.t;
  anchors : Anchor.t list;
}

(* Initialization *)

let init_el (ctx : t) (spec_el : El.Ast.spec) : unit =
  let init_el' (def_el : El.Ast.def) : unit =
    match def_el.it with
    | TypD (id_syntax, tparams, deftyp, hints) ->
        let syntax = (tparams, deftyp, hints, def_el.at) in
        ctx.syntax <- SyntaxMap.add id_syntax.it syntax ctx.syntax
    | RelD (id_rel, nottyp, hints) ->
        let relation = (nottyp, hints, def_el.at) in
        ctx.relation <- RelationMap.add id_rel.it relation ctx.relation
    | RuleGroupD (id_rel, id_rulegroup, rules) ->
        let rulegroup = (rules, def_el.at) in
        ctx.rulegroup <-
          RuleGroupMap.add (id_rel.it, id_rulegroup.it) rulegroup ctx.rulegroup
    | _ -> ()
  in
  List.iter init_el' spec_el

let init (spec_el : El.Ast.spec) : t =
  let anchors = [ Anchor.syntax; Anchor.relation; Anchor.rulegroup ] in
  let ctx =
    {
      syntax = SyntaxMap.empty;
      relation = RelationMap.empty;
      rulegroup = RuleGroupMap.empty;
      anchors;
    }
  in
  init_el ctx spec_el;
  ctx

(* Finders *)

let find_syntax_defs (ctx : t) (ids : SyntaxId.t list) : El.Ast.def list =
  let find_syntax_def (id : SyntaxId.t) : El.Ast.def =
    match SyntaxMap.find_opt id ctx.syntax with
    | Some (tparams, deftyp, hints, at) ->
        El.Ast.TypD (id $ no_region, tparams, deftyp, hints) $ at
    | None -> error no_region ("syntax " ^ id ^ " was not found")
  in
  List.map find_syntax_def ids

let find_relation_defs (ctx : t) (ids : RelationId.t list) : El.Ast.def list =
  let find_relation_def (id : RelationId.t) : El.Ast.def =
    match RelationMap.find_opt id ctx.relation with
    | Some (nottyp, hints, at) ->
        El.Ast.RelD (id $ no_region, nottyp, hints) $ at
    | None -> error no_region ("relation " ^ id ^ " was not found")
  in
  List.map find_relation_def ids

let find_rulegroup_defs (ctx : t) (ids : RuleGroupId.t list) : El.Ast.def list =
  let find_rulegroup_def (id : RuleGroupId.t) : El.Ast.def =
    let id_rel, id_rulegroup = id in
    match RuleGroupMap.find_opt id ctx.rulegroup with
    | Some (rules, at) ->
        El.Ast.RuleGroupD (id_rel $ no_region, id_rulegroup $ no_region, rules)
        $ at
    | None ->
        error no_region
          ("rulegroup " ^ id_rel ^ "/" ^ id_rulegroup ^ " was not found")
  in
  List.map find_rulegroup_def ids
