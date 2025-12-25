module InputHint = Runtime_static.Rel.InputHint
open Lang
open Sl

(* Relation *)

type t =
  | Extern of InputHint.t
  | Defined of InputHint.t * exp list * instr list

let to_string = function
  | Extern _ -> "extern relation"
  | Defined _ -> "defined relation"
