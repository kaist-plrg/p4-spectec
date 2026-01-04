open Lang

(* Relation *)

type t =
  | Extern of El.nottyp * Il.nottyp * Hints.Input.t
  | Defined of El.nottyp * Il.nottyp * Hints.Input.t * Il.rulegroup list

let to_string = function
  | Extern (nottyp, nottyp_il, inputs) ->
      El.Print.string_of_nottyp nottyp
      ^ " "
      ^ Hints.Input.to_string inputs
      ^ " = extern "
      ^ Il.Print.string_of_nottyp nottyp_il
  | Defined (nottyp, nottyp_il, inputs, rulegroups) ->
      El.Print.string_of_nottyp nottyp
      ^ " "
      ^ Hints.Input.to_string inputs
      ^ " =\n\n"
      ^ Il.Print.string_of_rulegroups nottyp_il inputs rulegroups
