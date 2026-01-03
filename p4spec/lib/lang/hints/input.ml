open El
open Util.Checks
open Util.Source

(* Input hints for relations *)

type t = int list

let to_string t =
  Format.asprintf "hint(input %s)"
    (String.concat " " (List.map (fun idx -> "%" ^ string_of_int idx) t))

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

let split (hint : t) (items : 'a list) : (int * 'a) list * (int * 'a) list =
  items
  |> List.mapi (fun idx item -> (idx, item))
  |> List.partition (fun (idx, _) -> List.mem idx hint)

let split_without_idx (hint : t) (items : 'a list) : 'a list * 'a list =
  items
  |> List.mapi (fun idx item -> (idx, item))
  |> List.partition (fun (idx, _) -> List.mem idx hint)
  |> fun (item_input, item_output) ->
  (List.map snd item_input, List.map snd item_output)

let combine (items_input : (int * 'a) list) (items_output : (int * 'a) list) :
    'a list =
  items_input @ items_output
  |> List.sort (fun (idx_i, _) (idx_o, _) -> compare idx_i idx_o)
  |> List.map snd

(* Checking if a hint is conditional *)

let is_conditional (hint : t) (items : 'a list) : bool =
  let _, items_output = split hint items in
  List.length items_output = 0
