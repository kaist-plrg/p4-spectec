open El.Ast
open El.Print

(* Function *)

type t =
  | Extern of tparam list * param list * plaintyp
  | Builtin of tparam list * param list * plaintyp
  | Table of param list * plaintyp * Il.Ast.tablerow list
  | Defined of tparam list * param list * plaintyp * Il.Ast.clause list

let to_string = function
  | Extern (tparams, params, plaintyp) ->
      "extern def " ^ string_of_tparams tparams ^ string_of_params params
      ^ " : "
      ^ string_of_plaintyp plaintyp
  | Builtin (tparams, params, plaintyp) ->
      "builtin def " ^ string_of_tparams tparams ^ string_of_params params
      ^ " : "
      ^ string_of_plaintyp plaintyp
  | Table (params, plaintyp, tablerows) ->
      "table def " ^ string_of_params params ^ " : "
      ^ string_of_plaintyp plaintyp
      ^ " =\n"
      ^ String.concat "\n"
          (List.map
             (fun clause -> Il.Print.string_of_tablerow clause)
             tablerows)
  | Defined (tparams, params, plaintyp, clauses) ->
      "def " ^ string_of_tparams tparams ^ string_of_params params ^ " : "
      ^ string_of_plaintyp plaintyp
      ^ " =\n"
      ^ String.concat "\n"
          (List.mapi
             (fun idx clause -> Il.Print.string_of_clause idx clause)
             clauses)
