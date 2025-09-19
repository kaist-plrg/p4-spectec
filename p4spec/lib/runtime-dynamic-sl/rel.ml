open Sl.Ast
open Sl.Print

(* Relation *)

type t = mixop * Runtime_static.Rel.Hint.t * exp list * instr list

let to_string (mixop, inputs, _exps_match, instrs) =
  "relation:\n\n"
  ^ string_of_instrs ~verbose:true
      ~signature:(Some (mixop, inputs))
      ~level:2 instrs
