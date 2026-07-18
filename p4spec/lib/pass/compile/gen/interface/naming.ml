open Lang
open Util.Source

(* Type-to-name mapping for generated function suffixes:
   [marshal_<name>], [unmarshal_<name>] *)

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

(* OCaml binding name for a type parameter's runtime [Typ.t] entry
   [x] -> [typ__x] *)

let name_typ (tvar : string) = "typ__" ^ tvar
