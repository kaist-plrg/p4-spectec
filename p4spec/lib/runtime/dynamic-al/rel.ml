open Lang
open Al

(* Relation *)

type t =
  | Extern of nottyp * Hints.Input.t
  | Defined of nottyp * Hints.Input.t * rulegroup list * elsegroup option

let to_string = function
  | Extern _ -> "extern relation"
  | Defined _ -> "defined relation"

let get_signature = function
  | Extern (nottyp, inputs) -> (nottyp, inputs)
  | Defined (nottyp, inputs, _, _) -> (nottyp, inputs)
