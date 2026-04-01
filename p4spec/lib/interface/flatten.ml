open Domain
open Lang
open Il
open Util.Source

let flatten_case_v_opt (value : value) :
    (string * string list * value list) option =
  match (value.note.typ, value.it) with
  | VarT (id, _), CaseV (mixop, values) ->
      let atoms =
        mixop |> Mixop.atoms
        |> List.map (fun atom -> Atom.string_of_atom atom.it)
      in
      Some (id.it, atoms, values)
  | _ -> None
