open Domain.Lib
open Lang
open Il
open Il.Print

(* Table *)

type t = param list * typ * tablerow list

(* A table group holds its single parameter, its return type, its columns
   (id + hints), and each column's rows (keyed by column id, empty at
   declaration time). *)
type group = {
  param : param;
  typ : typ;
  cols : (Id.t * hint list) list;
  rows : tablerow list IdMap.t;
}

let to_string ((params, typ, tablerows) : t) =
  "table def " ^ string_of_params params ^ " : " ^ string_of_typ typ ^ " =\n"
  ^ String.concat "\n"
      (List.map (fun clause -> string_of_tablerow clause) tablerows)
