open Lang
open Sl

(* Relation *)

type t = Extern | Defined of exp list * block * elseblock option

let to_string = function
  | Extern -> "extern relation"
  | Defined _ -> "defined relation"
