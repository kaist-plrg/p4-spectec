open Lang
open Sl

(* Function *)

type t =
  | Extern of tparam list * param list
  | Builtin of tparam list * param list
  | Table of param list * tablerow list
  | Defined of tparam list * param list * block * elseblock option

let to_string = function
  | Extern _ -> "extern function"
  | Builtin _ -> "builtin function"
  | Table _ -> "table function"
  | Defined _ -> "defined function"
