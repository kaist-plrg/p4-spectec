open Lang
open Il
open Il.Print

(* Function *)

type t =
  | Extern of tparam list * param list * typ
  | Builtin of tparam list * param list * typ
  | Defined of tparam list * param list * typ * clause list * elseclause option

let to_string = function
  | Extern (tparams, params, typ) ->
      "extern def " ^ string_of_tparams tparams ^ string_of_params params
      ^ " : " ^ string_of_typ typ
  | Builtin (tparams, params, typ) ->
      "builtin def " ^ string_of_tparams tparams ^ string_of_params params
      ^ " : " ^ string_of_typ typ
  | Defined (tparams, params, typ, clauses, elseclause_opt) ->
      "def " ^ string_of_tparams tparams ^ string_of_params params ^ " : "
      ^ string_of_typ typ ^ " =\n" ^ string_of_clauses clauses
      ^ string_of_elseclause_opt elseclause_opt
