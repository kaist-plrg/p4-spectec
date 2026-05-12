open Lang
open Pl
open Util.Source

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

(* PL drops the function-type info from DefP (it carries only the id).
   Higher-order parameters cannot be type-checked at the call boundary;
   we substitute a placeholder type. If real specs ever use higher-order
   params, PL's AST needs to grow the dropped fields back. *)
let placeholder_typ : typ = Il.BoolT $ no_region

let of_param (param : param) : typ =
  match param.it with ExpP (typ, _) -> typ | DefP _ -> placeholder_typ

let of_params (params : param list) : typ list = List.map of_param params

let get_signature (func : t) : tparam list * typ list * typ =
  match func with
  | Extern (tparams, params, typ) -> (tparams, of_params params, typ)
  | Builtin (tparams, params, typ) -> (tparams, of_params params, typ)
  | Table (params, typ, _) -> ([], of_params params, typ)
  | Defined (tparams, params, typ, _, _) -> (tparams, of_params params, typ)
