open Sl.Ast

(* Function *)

type t = Builtin | Defined of tparam list * arg list * instr list

let to_string = function
  | Builtin -> "builtin function"
  | Defined _ -> "defined function"
