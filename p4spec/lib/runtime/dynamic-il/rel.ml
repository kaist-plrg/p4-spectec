open Lang
open Il

(* Relation *)

type t = Extern | Defined of rulegroup list

let to_string = function
  | Extern -> "extern relation"
  | Defined _ -> "defined relation"
