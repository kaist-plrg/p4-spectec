open Util.Source

(* Error *)

exception InterpError of region * string

let error (at : region) (msg : string) = raise (InterpError (at, msg))
let warn (at : region) (msg : string) = Util.Error.warn at "interp" msg

(* Check *)

let check (b : bool) (at : region) (msg : string) : unit =
  if not b then error at msg

let guard (b : bool) (at : region) (msg : string) : unit =
  if not b then warn at msg
