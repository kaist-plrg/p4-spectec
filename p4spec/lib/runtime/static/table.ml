open Lang
open Il
open Il.Print

(* Table *)

type t = param list * typ * tablerow list

let to_string ((params, typ, tablerows) : t) =
  "table def " ^ string_of_params params ^ " : " ^ string_of_typ typ ^ " =\n"
  ^ String.concat "\n"
      (List.map (fun clause -> string_of_tablerow clause) tablerows)
