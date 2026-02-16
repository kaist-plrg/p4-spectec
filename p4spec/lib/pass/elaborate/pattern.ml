open Domain
open Lang
open Il
open Util.Source

module Nottyp = struct
  type t = nottyp

  let compare (t_a : t) (t_b : t) : int =
    let mixop_a, typs_a = t_a.it in
    let mixop_b, typs_b = t_b.it in
    let compare_mixop = Mixop.compare mixop_a mixop_b in
    if compare_mixop <> 0 then compare_mixop
    else
      let typs_a = List.map it typs_a in
      let typs_b = List.map it typs_b in
      List.compare compare typs_a typs_b
end

(* A Pattern is a set of notation types. *)

include Set.Make (Nottyp)

let to_string (pattern : t) : string =
  "{"
  ^ (elements pattern
    |> List.map Il.Print.string_of_nottyp
    |> String.concat " | ")
  ^ "}"

(* A Pattern.Tuple matches a tuple of patterns, corresponding to a table row. *)

module Tuple = struct
  type nonrec t = t list

  let to_string (tuple : t) : string =
    "(" ^ (tuple |> List.map to_string |> String.concat ", ") ^ ")"
end

(* Exclusiveness check *)

(* Two Tuples overlap if the overlap in every position *)
let has_overlap (tup1 : Tuple.t) (tup2 : Tuple.t) : bool =
  List.for_all2 (fun tup1 tup2 -> inter tup1 tup2 |> is_empty |> not) tup1 tup2

(* Check whether a list of Tuples contains an overlap and returns the
   first pair of overlapping Tuples if it exists. *)
let find_overlap (tuples : Tuple.t list) : (Tuple.t * Tuple.t) option =
  let rec find_overlap' = function
    | [] -> None
    | tuple :: rest -> (
        match List.find_opt (has_overlap tuple) rest with
        | Some tuple_conflict -> Some (tuple, tuple_conflict)
        | None -> find_overlap' rest)
  in
  find_overlap' tuples

(* Exhaustiveness check *)

(* Subtracting a Tuple from another Tuple to compute a list of
   fragments. The sum of the fragments define the subtracted set. *)
let subtract ~(from : Tuple.t) (what : Tuple.t) : Tuple.t list =
  if not (has_overlap from what) then [ from ]
  else
    (* F × F' − W × W' =  (F - W) × F'  U  (F ∩ W) × (F' - W') *)
    let rec subtract' (from : Tuple.t) (what : Tuple.t) (prefix : Tuple.t) :
        Tuple.t list =
      match (from, what) with
      | [], [] -> []
      | from :: from_rest, what :: what_rest ->
          (* F - W *)
          let f_diff_w = diff from what in
          (* F ∩ W *)
          let f_inter_w = inter from what in
          (* (F - W) × F' *)
          let fragment =
            if is_empty f_diff_w then []
            else [ List.rev (f_diff_w :: prefix) @ from_rest ]
          in
          (* (F ∩ W) × (F' - W) *)
          if is_empty f_inter_w then fragment
          else fragment @ subtract' from_rest what_rest (f_inter_w :: prefix)
      | _ -> assert false
    in
    subtract' from what []

(* Repeatedly subtracts each row from the total set to find
   uncovered fragments. If the result is empty, the pattern is
   exhaustive. *)
let find_missing (total : Tuple.t) (rows : Tuple.t list) : Tuple.t list =
  List.fold_left
    (fun fragments row ->
      List.concat_map (fun fragment -> subtract ~from:fragment row) fragments)
    [ total ] rows
