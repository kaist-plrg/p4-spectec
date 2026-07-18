open Lang
open Util.Source

(* Type-to-name mapping for generated function suffixes *)

(* Mirrored at runtime by [interface_name_] in [Template.Split.interface_name_fn]
   — keep both in sync; see that function's own cross-reference comment. *)

let rec name (typ : Sl.typ) : string =
  match typ.it with
  | BoolT -> "bool"
  | NumT `NatT -> "nat"
  | NumT `IntT -> "int"
  | TextT -> "text"
  | VarT (id, []) -> Names.var_of_id id
  | VarT (id, targs) ->
      Names.var_of_id id ^ "__" ^ String.concat "__" (List.map name targs)
  | TupleT typs -> String.concat "_" (List.map name typs) ^ "_tup"
  | IterT (t, Il.Opt) -> name t ^ "__opt"
  | IterT (t, Il.List) -> name t ^ "__list"
  | FuncT _ -> "func"

(* OCaml binding name for a type parameter's runtime [Typ.t] dictionary
   entry: [x] -> [typ__x] *)

let name_typ (tvar : string) = "typ__" ^ tvar
