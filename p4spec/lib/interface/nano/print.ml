module Value = Runtime.Value
open Lang.Il
module Mixfix = Domain.Mixfix

let cyan s = "\027[36m" ^ s ^ "\027[0m"
let magenta s = "\027[35m" ^ s ^ "\027[0m"
let yellow s = "\027[33m" ^ s ^ "\027[0m"

let strip_outer_backticks s =
  let n = String.length s in
  if n >= 2 && s.[0] = '`' && s.[n - 1] = '`' then String.sub s 1 (n - 2) else s

let print_tree (value : Value.t) =
  let rec print_tree' prefix child_prefix (value : value) =
    match value.it with
    | CaseV notval ->
        let args = Mixfix.args notval in
        let mixop_str =
          strip_outer_backticks (Mixfix.to_string (Mixfix.to_mixop notval))
        in
        let typ_name =
          match value.note.typ with VarT (id, _) -> id.it | _ -> mixop_str
        in
        let label =
          if mixop_str = "" then cyan typ_name
          else cyan typ_name ^ " " ^ magenta mixop_str
        in
        Format.printf "%s%s\n" prefix label;
        let n = List.length args in
        List.iteri
          (fun i child ->
            let is_last = i = n - 1 in
            let connector = if is_last then "└── " else "├── " in
            let next_prefix = if is_last then "    " else "│   " in
            print_tree' (child_prefix ^ connector)
              (child_prefix ^ next_prefix)
              child)
          args
    | NumV n -> Format.printf "%s%s\n" prefix (yellow (Print.string_of_num n))
    | TextV s -> Format.printf "%s%s\n" prefix (yellow ("\"" ^ s ^ "\""))
    | BoolV b -> Format.printf "%s%s\n" prefix (yellow (string_of_bool b))
    | _ -> Format.printf "%s%s\n" prefix (yellow (Print.string_of_value value))
  in
  print_tree' "" "" value
