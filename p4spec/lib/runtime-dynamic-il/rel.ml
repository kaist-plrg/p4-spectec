open Il.Ast
open Il.Print

(* Relation *)

type t = Runtime_static.Rel.InputHint.t * rule list

let to_string (inputs, rules) =
  "rel "
  ^ Runtime_static.Rel.InputHint.to_string inputs
  ^ "\n"
  ^ String.concat "\n   " (List.map string_of_rule rules)
