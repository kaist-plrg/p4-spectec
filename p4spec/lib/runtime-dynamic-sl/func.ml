open Sl.Ast

(* Function *)

type t =
  | Extern
  | Builtin
  | Table of arg list * tablerow list
  | Plain of tparam list * arg list * instr list

let to_string = function
  | Extern -> "extern function"
  | Builtin -> "builtin function"
  | Table _ -> "table function"
  | Plain _ -> "plain function"
