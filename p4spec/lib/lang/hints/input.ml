open El
open Util.Checks
open Util.Source

(* Input hints for relations *)

type t = int list [@@deriving yojson]

let to_string t =
  Format.asprintf "hint(input %s)"
    (String.concat " " (List.map (fun idx -> "%" ^ string_of_int idx) t))

(* Equivalence of hints *)

let eq (hint_a : t) (hint_b : t) : bool =
  List.length hint_a = List.length hint_b && List.for_all2 ( = ) hint_a hint_b

(* Creating hints *)

let init (hintexp : Hint.t) : t option =
  match hintexp.it with
  | SeqE hintexps ->
      List.fold_left
        (fun hint hintexp ->
          match hint with
          | Some hint -> (
              match hintexp.it with
              | HoleE (`Num idx) -> Some (hint @ [ idx ])
              | _ -> None)
          | None -> None)
        (Some []) hintexps
  | HoleE (`Num idx) -> Some [ idx ]
  | _ -> None

(* Validating hints *)

let validate (hint : t) (arity : int) : (unit, string) result =
  if hint = [] then Error "input hint is empty"
  else if not (distinct ( = ) hint) then
    Error "input hint contains duplicate indices"
  else if List.exists (fun idx -> idx < 0 || idx >= arity) hint then
    Error "input hint contains out-of-bounds indices"
  else Ok ()

(* Splitting and combining expressions based on input hints *)

let split (hint : t) (items : 'a list) : 'a list * 'a list =
  items
  |> List.mapi (fun idx item -> (idx, item))
  |> List.partition (fun (idx, _) -> List.mem idx hint)
  |> fun (item_input, item_output) ->
  (List.map snd item_input, List.map snd item_output)

let combine (hint : t) (items_input : 'a list) (items_output : 'a list) :
    'a list =
  let len = List.length items_input + List.length items_output in
  let idxs_input, idxs_output =
    List.init len Fun.id |> List.partition (fun idx -> List.mem idx hint)
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
