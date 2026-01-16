open Lang
open Sl

(* Relation *)

type t = Extern | Defined of exp list * instr list

let to_string = function
  | Extern -> "extern relation"
  | Defined _ -> "defined relation"
