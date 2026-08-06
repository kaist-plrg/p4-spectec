open Util.Source

(* Error *)

exception RuntimeError of region * string

let error (at : region) (msg : string) = raise (RuntimeError (at, msg))
let warn (at : region) (msg : string) = Util.Error.warn at "runtime" msg

(* Checks *)

let check (b : bool) (at : region) (msg : string) : unit =
  if not b then error at msg
