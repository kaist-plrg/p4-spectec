open Util.Source

(* Error *)

exception AlgoError of region * string

let error (at : region) (msg : string) = raise (AlgoError (at, msg))
let warn (at : region) (msg : string) = Util.Error.warn at "algo" msg

(* Checks *)

let check (b : bool) (at : region) (msg : string) : unit =
  if not b then error at msg
