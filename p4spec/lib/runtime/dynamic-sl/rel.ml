open Lang
open Sl
module InputHint = Static.Rel.InputHint

(* Relation *)

type t =
  | Extern of InputHint.t
  | Defined of InputHint.t * exp list * instr list

let to_string = function
  | Extern _ -> "extern relation"
  | Defined _ -> "defined relation"
