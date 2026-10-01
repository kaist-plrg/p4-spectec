open El
open Util.Source

(* Field hints *)

type t = text phrase list

let to_string (hint : t) : string =
  Format.asprintf "hint(fields %s)"
    (hint |> List.map it |> List.map Print.string_of_text |> String.concat " ")

(* Creating hints *)

let init (hintexp : Hint.t) : t option =
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

(* Validating hints *)

type invalid_arity = { expected : int; actual : int }

let validate (hint : t) (arity : int) : (unit, invalid_arity) result =
  let actual = List.length hint in
  if actual = arity then Ok () else Error { expected = arity; actual }
