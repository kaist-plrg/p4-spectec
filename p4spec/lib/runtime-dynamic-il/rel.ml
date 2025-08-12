open Il.Ast
open Il.Print

(* Relation *)

type t = Runtime_static.Rel.Hint.t * rulegroup list

let to_string (inputs, rulegroups) =
  "rel "
  ^ Runtime_static.Rel.Hint.to_string inputs
  ^ "\n"
  ^ String.concat "\n   " (List.map string_of_rulegroup rulegroups)
