open Domain
open Lang

(* A Pattern is a set of mixops.
   Allows for an accurate representation of which mixop variants are
   allowed, including wildcards. *)

include Set.Make (Mixop)

let to_string (pattern : t) : string =
  "{"
  ^ (elements pattern
    |> List.map Il.Print.string_of_mixop
    |> String.concat " | ")
  ^ "}"

(* Exclusiveness check *)

let has_overlap (pat1 : t) (pat2 : t) : bool =
  inter pat1 pat2 |> is_empty |> not

(* Check whether a list of patterns contains an overlap and returns the
   first pair of overlapping patterns if it exists. *)
let find_overlap (patterns : t list) : (t * t) option =
  let rec find_overlap' = function
    | [] -> None
    | pat :: rest -> (
        match List.find_opt (has_overlap pat) rest with
        | Some pat_conflict -> Some (pat, pat_conflict)
        | None -> find_overlap' rest)
  in
  find_overlap' patterns

(* Exhaustiveness check *)

(* Repeatedly subtracts each row from the total set to find
   uncovered fragments. If the result is empty, the pattern is
   exhaustive. *)
let find_missing ~(total : t) (rows : t list) : t =
  List.fold_left (fun remainder row -> diff remainder row) total rows

(* Refinement: compute minimal set of non-overlapping patterns that
   can handle all supplied columns.
   This preserves sets that are already split, and repeatedly splits
*)
let refine_rows ~(total : t) (columns : t list list) : t list =
  (* Make the wildcards explicit and append to each column. *)
  let complete_columns =
    List.map
      (fun column ->
        (* First, remove any trailing wildcard expanded as the total set *)
        let explicit_column =
          match List.rev column with
          | last_row :: rest when last_row = total -> List.rev rest
          | _ -> column
        in
        (* Compute the exact wildcard set and append to the column *)
        let wildcard = find_missing ~total explicit_column in
        explicit_column @ [ wildcard ])
      columns
  in
  List.fold_left
    (fun refinement column ->
      List.concat_map
        (fun refined_row ->
          List.filter_map
            (fun column ->
              let intersection = inter refined_row column in
              if is_empty intersection then None else Some intersection)
            column)
        refinement)
    [ total ] complete_columns
