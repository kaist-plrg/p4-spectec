open Lang
open Il
module InputHint = Runtime_static.Rel.InputHint

(* Relation *)

type t = Extern of InputHint.t | Defined of InputHint.t * rulegroup list

let to_string = function
  | Extern _ -> "extern relation"
  | Defined _ -> "defined relation"
