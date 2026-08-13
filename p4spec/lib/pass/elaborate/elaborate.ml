open Lang
open Util.Source

(* Errors *)

type error = { at : region; msg : string }

let to_region_msg { at; msg } = (at, msg)

(* Entry point *)

let elab_spec (spec : El.spec) : (Il.spec, error) result =
  try Ok (Elab.elab_spec spec)
  with Error.ElabError (at, msg) -> Error { at; msg }
