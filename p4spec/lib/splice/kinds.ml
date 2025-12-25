open Lang

(* Syntax *)

module SyntaxId = String

type syntax = El.tparam list * El.deftyp * El.hint list

(* Relations *)

module RelationId = String

type relation = El.nottyp * El.hint list

(* Rule groups *)

module RuleGroupId = struct
  type t = string * string

  let compare (id_rel_a, id_rulegroup_a) (id_rel_b, id_rulegroup_b) =
    let c = String.compare id_rel_a id_rel_b in
    if c <> 0 then c else String.compare id_rulegroup_a id_rulegroup_b
end

type rulegroup = El.rule list

(* Rule prose *)

module RuleProseId = RuleGroupId

type ruleprose = Sl.mixop * int list * Sl.exp list * Sl.instr list

(* Function prose *)

module FuncProseId = String

type funcprose = Sl.tparam list * Sl.arg list * Sl.typ * Sl.instr list

(* Table *)

module TableId = String

type table = Sl.arg list * Sl.typ * Sl.tablerow list
