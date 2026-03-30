open Lang
open Xl
open Il
open Il.Print
open Util.Source

(* Type with dimension *)

type t = typ * iter list

let to_string (typ, iters) =
  string_of_typ typ ^ String.concat "" (List.map string_of_iter iters)

let compare (typ_a, iters_a) (typ_b, iters_b) =
  let rec compare_typ (typ_a : typ) (typ_b : typ) =
    let tag (typ : typ) =
      match typ.it with
      | BoolT -> 0
      | NumT _ -> 1
      | TextT -> 2
      | VarT _ -> 3
      | TupleT _ -> 4
      | IterT _ -> 5
      | FuncT -> 6
    in
    match (typ_a.it, typ_b.it) with
    | NumT n_a, NumT n_b -> Num.compare_typ n_a n_b
    | VarT (id_a, typs_a), VarT (id_b, typs_b) ->
        let cmp_id = String.compare id_a.it id_b.it in
        if cmp_id <> 0 then cmp_id else compares_typ typs_a typs_b
    | TupleT typs_a, TupleT typs_b -> compares_typ typs_a typs_b
    | IterT (typ_a, iter_a), IterT (typ_b, iter_b) ->
        let cmp_typ = compare_typ typ_a typ_b in
        if cmp_typ <> 0 then cmp_typ else compare iter_a iter_b
    | _ -> Int.compare (tag typ_a) (tag typ_b)
  and compares_typ (typs_a : typ list) (typs_b : typ list) =
    match (typs_a, typs_b) with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | typ_a :: typs_a, typ_b :: typs_b ->
        let cmp = compare_typ typ_a typ_b in
        if cmp <> 0 then cmp else compares_typ typs_a typs_b
  in
  let cmp = compare_typ typ_a typ_b in
  if cmp <> 0 then cmp else compare iters_a iters_b

let equiv (typ_a, iters_a) (typ_b, iters_b) =
  Il.Eq.eq_typ typ_a typ_b
  && List.length iters_a = List.length iters_b
  && List.for_all2 ( = ) iters_a iters_b

let sub (typ_a, iters_a) (typ_b, iters_b) =
  Il.Eq.eq_typ typ_a typ_b
  && List.length iters_a <= List.length iters_b
  && List.for_all2 ( = ) iters_a
       (List.filteri (fun idx _ -> idx < List.length iters_a) iters_b)

let add_iter (iter : iter) (typ, iters) = (typ, iters @ [ iter ])
