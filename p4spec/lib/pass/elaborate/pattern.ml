open Domain
open Lang
open Il
open Util.Source

(* Pattern is a set of notation types *)

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

module PatternSet = struct
  include Set.Make (Nottyp)

  let to_string (pattern_set : t) : string =
    "{"
    ^ (elements pattern_set
      |> List.map Il.Print.string_of_nottyp
      |> String.concat " | ")
    ^ "}"
end

module PatternSets = struct
  type t = PatternSet.t list

  let to_string (pattern_sets : t) : string =
    "("
    ^ (pattern_sets |> List.map PatternSet.to_string |> String.concat ", ")
    ^ ")"
end

(* Stringifier *)

(* Exclusiveness check *)

let has_overlap (pattern_sets_a : PatternSets.t)
    (pattern_sets_b : PatternSets.t) : bool =
  List.for_all2
    (fun pattern_set_a pattern_set_b ->
      PatternSet.inter pattern_set_a pattern_set_b |> PatternSet.is_empty |> not)
    pattern_sets_a pattern_sets_b

let find_overlap (pattern_sets_group : PatternSets.t list) :
    (PatternSets.t * PatternSets.t) option =
  let rec find_overlap' = function
    | [] -> None
    | pattern_sets_h :: pattern_sets_group_t -> (
        match
          List.find_opt (has_overlap pattern_sets_h) pattern_sets_group_t
        with
        | Some pattern_sets_conflict ->
            Some (pattern_sets_h, pattern_sets_conflict)
        | None -> find_overlap' pattern_sets_group_t)
  in
  find_overlap' pattern_sets_group

(* Exhaustiveness check *)

let subtract (pattern_sets_total : PatternSets.t) (pattern_sets : PatternSets.t)
    : PatternSets.t list =
  if not (has_overlap pattern_sets_total pattern_sets) then
    [ pattern_sets_total ]
  else
    (* F × F' − W × W' =  (F - W) × F'  U  (F ∩ W) × (F' - W') *)
    let rec subtract' (pattern_sets_from : PatternSets.t)
        (pattern_sets : PatternSets.t) (pattern_sets_prefix : PatternSets.t) :
        PatternSets.t list =
      match (pattern_sets_from, pattern_sets) with
      | [], [] -> []
      | ( pattern_set_from_h :: pattern_sets_from_t,
          pattern_set_h :: pattern_sets_t ) ->
          let pattern_set_diff =
            PatternSet.diff pattern_set_from_h pattern_set_h
          in
          let pattern_set_inter =
            PatternSet.inter pattern_set_from_h pattern_set_h
          in
          (* (F - W) × F' *)
          let pattern_sets_group_fragment =
            if PatternSet.is_empty pattern_set_diff then []
            else
              [
                List.rev (pattern_set_diff :: pattern_sets_prefix)
                @ pattern_sets_from_t;
              ]
          in
          (* (F ∩ W) × (F' - W) *)
          if PatternSet.is_empty pattern_set_inter then
            pattern_sets_group_fragment
          else
            pattern_sets_group_fragment
            @ subtract' pattern_sets_from_t pattern_sets_t
                (pattern_set_inter :: pattern_sets_prefix)
      | _ -> assert false
    in
    subtract' pattern_sets_total pattern_sets []

let find_missing (pattern_sets_total : PatternSets.t)
    (pattern_sets_group : PatternSets.t list) : PatternSets.t list =
  List.fold_left
    (fun pattern_sets_total pattern_sets ->
      List.concat_map
        (fun pattern_sets_total -> subtract pattern_sets_total pattern_sets)
        pattern_sets_total)
    [ pattern_sets_total ] pattern_sets_group
