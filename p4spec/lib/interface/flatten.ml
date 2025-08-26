open Il.Ast
open Xl.Atom
open Util.Source

let flatten_case_v (value : value) : string list list * value list =
  match value.it with
  | CaseV (mixop, values) ->
      let mixop = List.map (List.map (fun p -> string_of_atom p.it)) mixop in
      (mixop, values)
  | _ ->
      failwith
        ("Expected a CaseV value but got " ^ Il.Print.string_of_value value)
