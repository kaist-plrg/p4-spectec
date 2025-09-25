(* Syntax *)

module SyntaxId = String

type syntax = El.Ast.tparam list * El.Ast.deftyp * El.Ast.hint list

(* Relations *)

module RelationId = String

type relation = El.Ast.nottyp * El.Ast.hint list

(* Rule groups *)

module RuleGroupId = struct
  type t = string * string

  let compare (id_rel_a, id_rulegroup_a) (id_rel_b, id_rulegroup_b) =
    let c = String.compare id_rel_a id_rel_b in
    if c <> 0 then c else String.compare id_rulegroup_a id_rulegroup_b
end

type rulegroup = El.Ast.rule list

(* Rule prose *)

module RuleProseId = RuleGroupId

type ruleprose = Sl.Ast.mixop * int list * Sl.Ast.exp list * Sl.Ast.instr list
