open Lang
open Il

(* Relation *)

type t = Extern | Defined of rulegroup list * elsegroup option

let to_string = function
  | Extern -> "extern relation"
  | Defined _ -> "defined relation"
