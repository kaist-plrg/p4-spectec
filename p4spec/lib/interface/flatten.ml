open Domain
module Mixfix = Domain.Mixfix
open Lang
open Il
open Util.Source

let flatten_case_v_opt (value : value) :
    (string * string list * value list) option =
  match (value.note.typ, value.it) with
  | VarT (id, _), CaseV valuecase ->
      let mixop, values = Mixfix.split valuecase in
      let atoms =
        mixop |> Mixop.atoms
        |> List.map (fun atom -> Atom.string_of_atom atom.it)
      in
      Some (id.it, atoms, values)
  | _ -> None
