open Lang
open Sl

(* Relation *)

type t =
  | Extern of Hints.Input.t
  | Defined of Hints.Input.t * exp list * instr list

let to_string = function
  | Extern _ -> "extern relation"
  | Defined _ -> "defined relation"
