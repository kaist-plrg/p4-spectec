open Il.Ast
open Xl.Atom
open Util.Source

let flatten_case_v_opt (value : value) : (string list list * value list) option
    =
  match value.it with
  | CaseV (mixop, values) ->
      let mixop = List.map (List.map (fun p -> string_of_atom p.it)) mixop in
      Some (mixop, values)
  | _ -> None
