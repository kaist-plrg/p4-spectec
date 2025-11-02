module InputHint = Runtime_static.Rel.InputHint
open Sl.Ast
open Xl

(* Relation *)

type t =
  | Extern of InputHint.t
  | Defined of Mixop.t * InputHint.t * exp list * instr list

let to_string = function
  | Extern _ -> "extern relation"
  | Defined _ -> "defined relation"
