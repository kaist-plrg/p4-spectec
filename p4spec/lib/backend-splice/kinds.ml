open Lang

(* Syntax *)

module SyntaxId = String

type syntax = El.tparam list * El.deftyp * El.hint list

(* Rule groups *)

module RuleGroupId = struct
  type t = string * string

  let compare (id_rel_a, id_rulegroup_a) (id_rel_b, id_rulegroup_b) =
    let c = String.compare id_rel_a id_rel_b in
    if c <> 0 then c else String.compare id_rulegroup_a id_rulegroup_b
end

type rulegroup = El.rule list

(* Relation prose *)

module RelationId = String

type relationprose = Pl.rel_title

(* Rule prose *)

module RuleProseId = RuleGroupId

type ruleprose = Pl.rulegroup

(* Function prose *)

module FuncProseId = String

type funcprose = Pl.func

(* Table *)

module TableId = String

type table = Pl.tablefunc
