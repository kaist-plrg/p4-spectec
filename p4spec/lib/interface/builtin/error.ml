open Util.Source

(* Error *)

exception BuiltinError of region * string

let error (at : region) (msg : string) = raise (BuiltinError (at, msg))
let warn (at : region) (msg : string) = Util.Error.warn at "builtin" msg

(* Checks *)

let check (b : bool) (at : region) (msg : string) : unit =
  if not b then error at msg
