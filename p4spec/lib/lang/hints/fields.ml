open El
open Util.Source

(* Field hints *)

type t = text list

let to_string (hint : t) : string =
  Format.asprintf "hint(fields %s)"
    (hint |> List.map Print.string_of_text |> String.concat " ")

(* Creating hints *)

let init (hintexp : Hint.t) : t option =
  match hintexp.it with
  | TextE text -> Some [ text ]
  | SeqE hintexps ->
      List.fold_left
        (fun hint hintexp ->
          match hint with
          | Some hint -> (
              match hintexp.it with TextE s -> Some (hint @ [ s ]) | _ -> None)
          | None -> None)
        (Some []) hintexps
  | _ -> None

(* Validating hints *)

let validate (hint : t) (arity : int) : (unit, string) result =
  if List.length hint = arity then Ok ()
  else
    Error
      (Printf.sprintf "expected %d strings, but got %d." arity
         (List.length hint))
