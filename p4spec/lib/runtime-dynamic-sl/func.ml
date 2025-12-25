open Lang
open Sl

(* Function *)

type t =
  | Extern
  | Builtin
  | Table of arg list * tablerow list
  | Defined of tparam list * arg list * instr list

let to_string = function
  | Extern -> "extern function"
  | Builtin -> "builtin function"
  | Table _ -> "table function"
  | Defined _ -> "defined function"
