open Lang
open Il

(* Relation *)

type t = Extern of Hints.Input.t | Defined of Hints.Input.t * rulegroup list

let to_string = function
  | Extern _ -> "extern relation"
  | Defined _ -> "defined relation"
