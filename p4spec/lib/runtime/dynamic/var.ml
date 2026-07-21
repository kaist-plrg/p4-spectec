open Lang
open Il
open Il.Print
open Util.Source

type t = id * iter list

let to_string (id, iters) =
  string_of_varid id ^ String.concat "" (List.map string_of_iter iters)

let compare_iter (iter_a : iter) (iter_b : iter) =
  match (iter_a, iter_b) with
  | Opt, Opt | List, List -> 0
  | Opt, List -> -1
  | List, Opt -> 1

let rec compare_iters iters_a iters_b =
  match (iters_a, iters_b) with
  | [], [] -> 0
  | [], _ :: _ -> -1
  | _ :: _, [] -> 1
  | iter_a :: iters_a, iter_b :: iters_b ->
      let c = compare_iter iter_a iter_b in
      if c <> 0 then c else compare_iters iters_a iters_b

(* Compare variables by id, then by iters. *)
let compare (id_a, iters_a) (id_b, iters_b) =
  let cmp_id = String.compare id_a.it id_b.it in
  if cmp_id = 0 then compare_iters iters_a iters_b else cmp_id
