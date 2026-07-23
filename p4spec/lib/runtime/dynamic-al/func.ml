open Lang
open Al

(* Function *)

type t =
  | Extern of tparam list * param list * typ
  | Builtin of tparam list * param list * typ
  | Table of param list * typ * tablerow list
  | Defined of tparam list * param list * typ * clause list * elseclause option

let to_string = function
  | Extern _ -> "extern function"
  | Builtin _ -> "builtin function"
  | Table _ -> "table function"
  | Defined _ -> "defined function"

let get_signature (func : t) : tparam list * typ list * typ =
  match func with
  | Extern (tparams, params, typ) ->
      (tparams, Type.Typ.Make.of_params_il params, typ)
  | Builtin (tparams, params, typ) ->
      (tparams, Type.Typ.Make.of_params_il params, typ)
  | Table (params, typ, _) -> ([], Type.Typ.Make.of_params_il params, typ)
  | Defined (tparams, params, typ, _, _) ->
      (tparams, Type.Typ.Make.of_params_il params, typ)
