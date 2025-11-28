open Domain.Lib
open Util.Source

let fresh_id (ids : IdSet.t) (id : Id.t) : Id.t =
  let ids =
    IdSet.filter
      (fun id_e ->
        let id = Xl.Var.strip_var_suffix id in
        let id_e = Xl.Var.strip_var_suffix id_e in
        id.it = id_e.it)
      ids
  in
  let rec fresh_id' (id : Id.t) : Id.t =
    if IdSet.mem id ids then fresh_id' (id.it ^ "'" $ id.at) else id
  in
  fresh_id' id

(* Generate a fresh variable name from a type.
   For tuple types, we generate names like "t1_t2" instead of "(t1, t2)"
   to avoid confusion in the prose. If tuple elements have the same type name,
   we number them to distinguish: "t_1_t_2" *)
let rec name_of_typ (typ : Il.Ast.typ) : string =
  match typ.it with
  | Il.Ast.BoolT -> "bool"
  | Il.Ast.NumT numtyp -> Xl.Num.string_of_typ numtyp
  | Il.Ast.TextT -> "text"
  | Il.Ast.VarT (typid, targs) ->
      typid.it ^ name_of_targs targs
  | Il.Ast.TupleT typs ->
      name_of_tuple_typs typs
  | Il.Ast.IterT (typ, _) ->
      (* Iterator is handled separately in fresh_from_typ *)
      name_of_typ typ
  | Il.Ast.FuncT -> "func"

and name_of_targs (targs : Il.Ast.targ list) : string =
  match targs with
  | [] -> ""
  | targs ->
      "<" ^ String.concat ", " (List.map name_of_targ targs) ^ ">"

and name_of_targ (targ : Il.Ast.targ) : string =
  name_of_typ (targ.it $ targ.at)

and name_of_tuple_typs (typs : Il.Ast.typ list) : string =
  (* Generate names for each element, then number duplicates *)
  let names = List.map name_of_typ typs in
  let numbered = number_duplicates names in
  String.concat "_" numbered

(* Number duplicate names to distinguish them.
   e.g., ["a"; "b"; "a"] -> ["a_1"; "b"; "a_2"] *)
and number_duplicates (names : string list) : string list =
  (* Count occurrences of each name *)
  let counts =
    List.fold_left
      (fun acc name ->
        let count = try List.assoc name acc with Not_found -> 0 in
        (name, count + 1) :: List.remove_assoc name acc)
      [] names
  in
  (* Only number names that appear more than once *)
  let needs_numbering =
    List.filter (fun (_, count) -> count > 1) counts
    |> List.map fst
  in
  (* Number the duplicates *)
  let _, result =
    List.fold_left
      (fun (counters, acc) name ->
        if List.mem name needs_numbering then
          let idx = try List.assoc name counters with Not_found -> 1 in
          let counters = (name, idx + 1) :: List.remove_assoc name counters in
          (counters, acc @ [name ^ "_" ^ string_of_int idx])
        else
          (counters, acc @ [name]))
      ([], []) names
  in
  result

let rec fresh_from_typ (at : region) (typ : Il.Ast.typ) :
    Id.t * Il.Ast.typ * Il.Ast.iter list =
  match typ.it with
  | IterT (typ, iter) ->
      let id, typ, iters = fresh_from_typ at typ in
      (id, typ, iters @ [ iter ])
  | _ ->
      let id = name_of_typ typ $ at in
      (id, typ, [])

let fresh_from_exp ?(wildcard = false) (ids : IdSet.t) (exp : Il.Ast.exp) :
    Id.t * Il.Ast.typ * Il.Ast.iter list =
  let id, typ, iters = fresh_from_typ exp.at (exp.note $ exp.at) in
  let id = if wildcard then "_" ^ id.it $ id.at else id in
  let id = fresh_id ids id in
  (id, typ, iters)

let fresh_from_plaintyp ?(wildcard = false) (ids : IdSet.t)
    (plaintyp : El.Ast.plaintyp) : Id.t =
  let id = El.Print.string_of_plaintyp plaintyp $ plaintyp.at in
  let id = if wildcard then "_" ^ id.it $ id.at else id in
  fresh_id ids id
