open El
open Util.Source

(* Input hints for relations *)

type t = int list [@@deriving yojson]
type t_phrase = int phrase list

let to_string t =
  Format.asprintf "hint(input %s)"
    (String.concat " " (List.map (fun idx -> "%" ^ string_of_int idx) t))

(* Equivalence of hints *)

let eq (hint_a : t) (hint_b : t) : bool =
  List.length hint_a = List.length hint_b && List.for_all2 ( = ) hint_a hint_b

(* Creating hints *)

let init (hintexp : Hint.t) : t_phrase option =
  match hintexp.it with
  | SeqE hintexps ->
      List.fold_left
        (fun hint hintexp ->
          match hint with
          | Some hint -> (
              match hintexp.it with
              | HoleE (`Num idx) -> Some ((idx $ hintexp.at) :: hint)
              | _ -> None)
          | None -> None)
        (Some []) hintexps
      |> Option.map List.rev
  | HoleE (`Num idx) -> Some [ idx $ hintexp.at ]
  | _ -> None

(* Validating hints *)

type invalid =
  | Empty
  | Duplicate_index of int * region * region
  | Out_of_bounds of int * region

let validate (hint : t_phrase) (arity : int) : (t, invalid) result =
  let rec find_duplicate seen = function
    | [] -> None
    | idx :: idxs -> (
        match List.find_opt (fun idx_seen -> idx_seen.it = idx.it) seen with
        | Some idx_first -> Some (idx.it, idx_first.at, idx.at)
        | None -> find_duplicate (idx :: seen) idxs)
  in
  match hint with
  | [] -> Error Empty
  | _ -> (
      match find_duplicate [] hint with
      | Some (idx, at_first, at_duplicate) ->
          Error (Duplicate_index (idx, at_first, at_duplicate))
      | None -> (
          match
            List.find_opt (fun idx -> idx.it < 0 || idx.it >= arity) hint
          with
          | Some idx -> Error (Out_of_bounds (idx.it, idx.at))
          | None -> Ok (List.map it hint)))

(* Splitting and combining expressions based on input hints *)

let split (hint : t) (items : 'a list) : 'a list * 'a list =
  let rec go idx items_input_rev items_output_rev = function
    | [] -> (List.rev items_input_rev, List.rev items_output_rev)
    | item :: items ->
        if List.memq idx hint then
          go (idx + 1) (item :: items_input_rev) items_output_rev items
        else go (idx + 1) items_input_rev (item :: items_output_rev) items
  in
  go 0 [] [] items

let combine (hint : t) (items_input : 'a list) (items_output : 'a list) :
    'a list =
  let len = List.length items_input + List.length items_output in
  let idxs_input, idxs_output =
    List.init len Fun.id |> List.partition (fun idx -> List.memq idx hint)
  in
  let items_input_indexed = List.combine idxs_input items_input in
  let items_output_indexed = List.combine idxs_output items_output in
  items_input_indexed @ items_output_indexed
  |> List.sort (fun (idx_a, _) (idx_b, _) -> Int.compare idx_a idx_b)
  |> List.map snd

(* Checking if a hint is conditional *)

let is_conditional (hint : t) (items : 'a list) : bool =
  let _, items_output = split hint items in
  List.length items_output = 0
