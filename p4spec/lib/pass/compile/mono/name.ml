open Lang

(* ===== Type-to-name fragment ===== *)

(* Convert an Il.typ to a flat name fragment for mangling.
   Uses raw VarT id strings; no camelCase conversion here (that happens in gen/). *)
let rec typ_to_name (typ : Il.typ) : string =
  match typ.it with
  | Il.BoolT -> "bool"
  | Il.NumT `NatT -> "nat"
  | Il.NumT `IntT -> "int"
  | Il.TextT -> "text"
  | Il.VarT (id, []) -> id.it
  | Il.VarT (id, typs) ->
      id.it ^ "_" ^ String.concat "_" (List.map typ_to_name typs)
  | Il.TupleT typs -> "tup_" ^ String.concat "_" (List.map typ_to_name typs)
  | Il.IterT (typ, Il.List) -> typ_to_name typ ^ "_list"
  | Il.IterT (typ, Il.Opt) -> typ_to_name typ ^ "_opt"
  | Il.FuncT _ -> "func"

(* ===== Name mangling ===== *)

(* Mangle a function id with concrete targs.
   "__" separator cannot appear in normal WatSup identifiers. *)
let mangle (func_id : string) (targs : Il.typ list) : string =
  match targs with
  | [] -> func_id
  | _ -> func_id ^ "__" ^ String.concat "__" (List.map typ_to_name targs)
