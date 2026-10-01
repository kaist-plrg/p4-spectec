open Lang
open Il
open Il.Print
open Util.Source

(* Type with dimension *)

type t = typ * iter list

let to_string (typ, iters) =
  string_of_typ typ ^ String.concat "" (List.map string_of_iter iters)

let compare (typ_a, iters_a) (typ_b, iters_b) =
  let cmp = Type.Typ.compare typ_a typ_b in
  if cmp <> 0 then cmp else Stdlib.compare iters_a iters_b

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
