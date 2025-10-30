open El.Ast
open El.Print

(* Function *)

type t =
  | Builtin of tparam list * param list * plaintyp
  | Defined of tparam list * param list * plaintyp * Il.Ast.clause list

let to_string = function
  | Builtin (tparams, params, plaintyp) ->
      "def " ^ string_of_tparams tparams ^ string_of_params params ^ " : "
      ^ string_of_plaintyp plaintyp
  | Defined (tparams, params, plaintyp, clauses) ->
      "def " ^ string_of_tparams tparams ^ string_of_params params ^ " : "
      ^ string_of_plaintyp plaintyp
      ^ " =\n"
      ^ String.concat "\n"
          (List.mapi
             (fun idx clause -> Il.Print.string_of_clause idx clause)
             clauses)
