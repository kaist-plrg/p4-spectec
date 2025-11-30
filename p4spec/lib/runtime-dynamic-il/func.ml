open Il.Ast

(* Function *)

type t =
  | Extern
  | Builtin
  | Defined of tparam list * clause list
  | Table of param list * tablerow list

let to_string = function
  | Extern -> "extern function"
  | Builtin -> "builtin function"
  | Table _ -> "table function"
  | Defined _ -> "defined function"
