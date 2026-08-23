open El
open Util.Source

(* Field hints *)

type t = text list
type located = text phrase list

let to_string (hint : t) : string =
  Format.asprintf "hint(fields %s)"
    (hint |> List.map Print.string_of_text |> String.concat " ")

(* Creating hints *)

let init_located (hintexp : Hint.t) : located option =
  match hintexp.it with
  | TextE text -> Some [ text $ hintexp.at ]
  | SeqE hintexps ->
      List.fold_left
        (fun hint hintexp ->
          match hint with
          | Some hint -> (
              match hintexp.it with
              | TextE text -> Some (hint @ [ text $ hintexp.at ])
              | _ -> None)
          | None -> None)
        (Some []) hintexps
  | _ -> None

let unlocate (hint : located) : t = List.map (fun text -> text.it) hint

let locations (hint : located) : region list =
  List.map (fun text -> text.at) hint

let init (hintexp : Hint.t) : t option =
  Option.map unlocate (init_located hintexp)

(* Validating hints *)

type arity_mismatch = { expected : int; actual : int }

let validate (hint : t) (arity : int) : (unit, arity_mismatch) result =
  let actual = List.length hint in
  if actual = arity then Ok () else Error { expected = arity; actual }
