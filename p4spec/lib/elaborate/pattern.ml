open Il.Ast
open Util.Source
open Xl

module Nottyp = struct
  type t = nottyp

  let compare (t1 : t) (t2 : t) : int =
    let m1, ts1 = t1.it in
    let m2, ts2 = t2.it in
    let c_m = Mixop.compare m1 m2 in
    if c_m <> 0 then c_m
    else
      let ts1' = List.map it ts1 in
      let ts2' = List.map it ts2 in
      List.compare compare ts1' ts2'
end

module PatternSet = Set.Make (Nottyp)

type tuple_pattern = PatternSet.t list

(* Two tuple patterns overlap if they overlap in every position *)

let has_overlap (tupat1 : tuple_pattern) (tupat2 : tuple_pattern) : bool =
  try
    List.for_all2
      (fun pat1 pat2 -> PatternSet.is_empty (PatternSet.inter pat1 pat2) |> not)
      tupat1 tupat2
  with Invalid_argument _ ->
    failwith "Pattern.has_overlap: tuple patterns have different lengths"

let is_mutually_exclusive (tupats : tuple_pattern list) : bool =
  let rec is_mutually_exclusive' = function
    | [] -> true
    | pat :: pats ->
        if List.exists (fun pat' -> has_overlap pat pat') pats then false
        else is_mutually_exclusive' pats
  in
  is_mutually_exclusive' tupats

let find_overlap (rows : tuple_pattern list) :
    (tuple_pattern * tuple_pattern) option =
  let rec find_overlap' = function
    | [] -> None
    | pat :: pats -> (
        match List.find_opt (fun pat' -> has_overlap pat pat') pats with
        | Some pat_conflict -> Some (pat, pat_conflict)
        | None -> find_overlap' pats)
  in
  find_overlap' rows

let subtract ~(from : tuple_pattern) (what : tuple_pattern) : tuple_pattern list
    =
  if not (has_overlap from what) then [ from ]
  else
    (* F × F' − W × W' =  (F - W) × F'  U  (F ∩ W) × (F' - W') *)
    let rec subtract' (from_rest : tuple_pattern) (what_rest : tuple_pattern)
        (prefix : tuple_pattern) : tuple_pattern list =
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
    subtract' from what []

let find_missing (rows : tuple_pattern list) (total : tuple_pattern) :
    tuple_pattern list =
  List.fold_left
    (fun missing_regions row ->
      List.concat_map (fun region -> subtract ~from:region row) missing_regions)
    [ total ] rows

let tuple_pattern_to_string (row : tuple_pattern) : string =
  let col_strs =
    List.map
      (fun s ->
        "{"
        ^ String.concat "|"
            (s |> PatternSet.elements |> List.map Il.Print.string_of_nottyp)
        ^ "}")
      row
  in
  "(" ^ String.concat ", " col_strs ^ ")"
