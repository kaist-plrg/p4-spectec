(* open Domain.Atom *)
open Lang
open Il
(* open Util.Source *)

let flatten_case_v_opt (_value : value) :
    (string * string list list * value list) option =
  failwith "TODO"
(* match (value.note.typ, value.it) with *)
(* | VarT (id, _), CaseV (mixop, values) -> *)
(*     let mixop = List.map (List.map (fun p -> string_of_atom p.it)) mixop in *)
(*     Some (id.it, mixop, values) *)
(* | _ -> None *)
