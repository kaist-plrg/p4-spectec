open Il.Ast

(* Function *)

type t = Extern | Builtin | Defined of tparam list * clause list

let to_string = function
  | Extern -> "extern function"
  | Builtin -> "builtin function"
  | Defined _ -> "defined function"
