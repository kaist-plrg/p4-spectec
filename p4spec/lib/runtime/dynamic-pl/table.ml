open Lang
open Pl

(* Table *)

type t = param list * typ * tablerow list

let to_string (_ : t) = "table function"

let get_signature ((params, typ, _) : t) : typ list * typ =
  (Type.Typ.Make.of_params_pl params, typ)
