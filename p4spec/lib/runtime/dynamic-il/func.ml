open Lang
open Il

(* Function *)

type t =
  | Extern of tparam list * param list
  | Builtin of tparam list * param list
  | Table of param list * tablerow list
  | Defined of tparam list * param list * clause list * elseclause option

let to_string = function
  | Extern _ -> "extern function"
  | Builtin _ -> "builtin function"
  | Table _ -> "table function"
  | Defined _ -> "defined function"

let get_tparams = function
  | Extern (tparams, _) -> tparams
  | Builtin (tparams, _) -> tparams
  | Table _ -> []
  | Defined (tparams, _, _, _) -> tparams

let get_params = function
  | Extern (_, params) -> params
  | Builtin (_, params) -> params
  | Table (params, _) -> params
  | Defined (_, params, _, _) -> params
