open Lang
open Iil
open Iil.Print

(* Function *)

type t =
  | Extern of tparam list * param list * typ
  | Builtin of tparam list * param list * typ
  | Table of param list * typ * tablerow list
  | Defined of tparam list * param list * typ * clause list * elseclause option

let to_string = function
  | Extern (tparams, params, typ) ->
      "extern def " ^ string_of_tparams tparams ^ string_of_params params
      ^ " : " ^ string_of_typ typ
  | Builtin (tparams, params, typ) ->
      "builtin def " ^ string_of_tparams tparams ^ string_of_params params
      ^ " : " ^ string_of_typ typ
  | Table (params, typ, tablerows) ->
      "table def " ^ string_of_params params ^ " : " ^ string_of_typ typ
      ^ " =\n"
      ^ String.concat "\n"
          (List.map (fun clause -> string_of_tablerow clause) tablerows)
  | Defined (tparams, params, typ, clauses, elseclause_opt) ->
      "def " ^ string_of_tparams tparams ^ string_of_params params ^ " : "
      ^ string_of_typ typ ^ " =\n" ^ string_of_clauses clauses
      ^ string_of_elseclause_opt elseclause_opt
