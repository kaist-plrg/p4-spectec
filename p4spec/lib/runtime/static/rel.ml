open Lang
open Il

(* Relation *)

type t =
  | Extern of nottyp * Hints.Input.t
  | Defined of nottyp * Hints.Input.t * rulegroup list * elsegroup option

let to_string = function
  | Extern (nottyp_il, inputs) ->
      Hints.Input.to_string inputs
      ^ " = extern "
      ^ Il.Print.string_of_nottyp nottyp_il
  | Defined (nottyp_il, inputs, rulegroups, elsegroup_opt) ->
      Hints.Input.to_string inputs
      ^ " =\n\n"
      ^ Il.Print.string_of_rulegroups nottyp_il inputs rulegroups
      ^ Il.Print.string_of_elsegroup_opt nottyp_il inputs elsegroup_opt
