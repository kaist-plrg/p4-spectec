open Lang
open Il
open Error
open Util.Source

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

let get_signature =
  let typ_of_param (param : param) : typ =
    match param.it with
    | ExpP typ -> typ
    | DefP _ -> error no_region "typ of DefP parameter not implemented"
  in
  let typs_of_params (params : param list) : typ list =
    List.map typ_of_param params
  in
  function
  | Extern (tparams, params, typ) -> (tparams, typs_of_params params, typ)
  | Builtin (tparams, params, typ) -> (tparams, typs_of_params params, typ)
  | Table (params, typ, _) -> ([], typs_of_params params, typ)
  | Defined (tparams, params, typ, _, _) -> (tparams, typs_of_params params, typ)
