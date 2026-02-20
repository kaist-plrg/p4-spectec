open Lang
open El
open El.Print

(* Function *)

type t =
  | Extern of tparam list * param list * plaintyp
  | Builtin of tparam list * param list * plaintyp
  | Defined of
      tparam list
      * param list
      * plaintyp
      * Il.clause list
      * Il.elseclause option

let to_string = function
  | Extern (tparams, params, plaintyp) ->
      "extern def " ^ string_of_tparams tparams ^ string_of_params params
      ^ " : "
      ^ string_of_plaintyp plaintyp
  | Builtin (tparams, params, plaintyp) ->
      "builtin def " ^ string_of_tparams tparams ^ string_of_params params
      ^ " : "
      ^ string_of_plaintyp plaintyp
  | Defined (tparams, params, plaintyp, clauses, elseclause_opt) ->
      "def " ^ string_of_tparams tparams ^ string_of_params params ^ " : "
      ^ string_of_plaintyp plaintyp
      ^ " =\n"
      ^ Il.Print.string_of_clauses clauses
      ^ Il.Print.string_of_elseclause_opt elseclause_opt
