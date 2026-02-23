open Lang
open Il

(* Function *)

type t =
  | Extern
  | Builtin
  | Table of param list * tablerow list
  | Defined of tparam list * clause list * elseclause option

let to_string = function
  | Extern -> "extern function"
  | Builtin -> "builtin function"
  | Table _ -> "table function"
  | Defined _ -> "defined function"
