open Il.Ast
open Il.Print

(* Relation *)

type t = nottyp * Runtime_static.Rel.InputHint.t * rulegroup list

let to_string (nottyp, inputs, rulegroups) =
  "relation:\n\n" ^ string_of_rulegroups nottyp inputs rulegroups
