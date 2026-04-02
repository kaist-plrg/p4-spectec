open Lang
open Sl

(* Function *)

type t =
  | Extern of tparam list * param list * typ
  | Builtin of tparam list * param list * typ
  | Table of param list * typ * tablerow list
  | Defined of tparam list * param list * typ * block * elseblock option

let to_string = function
  | Extern _ -> "extern function"
  | Builtin _ -> "builtin function"
  | Table _ -> "table function"
  | Defined _ -> "defined function"

let get_signature = function
  | Extern (tparams, params, typ) -> (tparams, Typ.typs_of_params params, typ)
  | Builtin (tparams, params, typ) -> (tparams, Typ.typs_of_params params, typ)
  | Table (params, typ, _) -> ([], Typ.typs_of_params params, typ)
  | Defined (tparams, params, typ, _, _) ->
      (tparams, Typ.typs_of_params params, typ)
