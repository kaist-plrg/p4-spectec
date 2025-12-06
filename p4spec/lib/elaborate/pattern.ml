open Il.Ast
open Xl
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

module PatternSet = Set.Make (Nottyp)

(* Stringifier *)

let to_string (patterns : PatternSet.t list) : string =
  let spatterns =
    List.map
      (fun pattern ->
        "{"
        ^ String.concat "|"
            (pattern |> PatternSet.elements
            |> List.map Il.Print.string_of_nottyp)
        ^ "}")
      patterns
    |> String.concat ", "
  in
  "(" ^ spatterns ^ ")"

(* Exclusiveness check *)

let has_overlap (patterns_a : PatternSet.t list)
    (patterns_b : PatternSet.t list) : bool =
  try
    List.for_all2
      (fun pattern_a pattern_b ->
        PatternSet.inter pattern_a pattern_b |> PatternSet.is_empty |> not)
      patterns_a patterns_b
  with Invalid_argument _ ->
    failwith "Pattern.has_overlap: patterns have different lengths"

let find_overlap (patterns : PatternSet.t list list) :
    (PatternSet.t list * PatternSet.t list) option =
  let rec find_overlap' = function
    | [] -> None
    | pattern_h :: patterns_t -> (
        match List.find_opt (has_overlap pattern_h) patterns_t with
        | Some pattern_conflict -> Some (pattern_h, pattern_conflict)
        | None -> find_overlap' patterns_t)
  in
  find_overlap' patterns

(* Exhaustiveness check *)

let subtract (patterns_total : PatternSet.t list) (patterns : PatternSet.t list)
    : PatternSet.t list list =
  if not (has_overlap patterns_total patterns) then [ patterns_total ]
  else
    (* F × F' − W × W' =  (F - W) × F'  U  (F ∩ W) × (F' - W') *)
    let rec subtract' (from_rest : PatternSet.t list)
        (what_rest : PatternSet.t list) (prefix : PatternSet.t list) :
        PatternSet.t list list =
      match (from_rest, what_rest) with
      | [], [] -> [] (* subtraction complete *)
      | from_cur :: from_next, what_cur :: what_next ->
          let f_minus_w = PatternSet.diff from_cur what_cur in
          let f_and_w = PatternSet.inter from_cur what_cur in
          (* (F - W) × F' *)
          let fragment =
            if PatternSet.is_empty f_minus_w then []
            else [ List.rev (f_minus_w :: prefix) @ from_next ]
          in
          (* (F ∩ W) × (F' - W) *)
          if PatternSet.is_empty f_and_w then fragment
          else fragment @ subtract' from_next what_next (f_and_w :: prefix)
      | _ -> failwith "Pattern.subtract: tuple patterns have different lengths"
    in
    subtract' patterns_total patterns []

let find_missing (patterns_total : PatternSet.t list)
    (patterns : PatternSet.t list list) : PatternSet.t list list =
  List.fold_left
    (fun patterns_total pattern ->
      List.concat_map
        (fun patterns_total -> subtract patterns_total pattern)
        patterns_total)
    [ patterns_total ] patterns
