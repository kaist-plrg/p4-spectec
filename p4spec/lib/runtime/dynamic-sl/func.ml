open Lang
open Sl

(* Function *)

type t =
  | Extern
  | Builtin
  | Table of param list * tablerow list
  | Defined of tparam list * param list * block * elseblock option

let to_string = function
  | Extern -> "extern function"
  | Builtin -> "builtin function"
  | Table _ -> "table function"
  | Defined _ -> "defined function"
