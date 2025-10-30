open Il.Ast

(* Function *)

type t = Builtin | Defined of tparam list * clause list

let to_string = function
  | Builtin -> "builtin function"
  | Defined _ -> "defined function"
