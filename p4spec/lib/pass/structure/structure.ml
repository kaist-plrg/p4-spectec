open Lang
open Util.Source

(* Errors *)

type error = { at : region; msg : string }

let to_region_msg { at; msg } = (at, msg)

(* Entry point *)

let struct_spec ~(final : bool) (spec : Al.spec) : (Sl.spec, error) result =
  try Ok (Struct.struct_spec ~final spec)
  with Error.StructError (at, msg) -> Error { at; msg }
