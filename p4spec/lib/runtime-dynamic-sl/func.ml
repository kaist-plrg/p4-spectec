open Sl.Ast

(* Function *)

type t = Extern | Builtin | Defined of tparam list * arg list * instr list

let to_string = function
  | Extern -> "extern function"
  | Builtin -> "builtin function"
  | Defined _ -> "defined function"
