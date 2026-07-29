open Lang
open Sl

(* Function *)

type t =
  | Extern of tparam list * param list * typ
  | Builtin of tparam list * param list * typ
  | Defined of tparam list * param list * typ * block * elseblock option

let to_string = function
  | Extern _ -> "extern function"
  | Builtin _ -> "builtin function"
  | Defined _ -> "defined function"

let get_signature (func : t) : tparam list * typ list * typ =
  match func with
  | Extern (tparams, params, typ) ->
      (tparams, Type.Typ.Make.of_params_sl params, typ)
  | Builtin (tparams, params, typ) ->
      (tparams, Type.Typ.Make.of_params_sl params, typ)
  | Defined (tparams, params, typ, _, _) ->
      (tparams, Type.Typ.Make.of_params_sl params, typ)
