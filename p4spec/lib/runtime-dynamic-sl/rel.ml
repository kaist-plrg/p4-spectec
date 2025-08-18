open Sl.Ast
open Sl.Print

(* Relation *)

type t = Runtime_static.Rel.Hint.t * relmatch * relpath list

let to_string (inputs, relmatch, relpaths) =
  Runtime_static.Rel.Hint.to_string inputs
  ^ string_of_relmatch relmatch
  ^ "\n\n"
  ^ string_of_relpaths relpaths
