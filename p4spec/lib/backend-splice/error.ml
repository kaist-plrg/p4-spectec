open Util.Source

(* Error *)

exception SpliceError of region * string

let error (at : region) (msg : string) = raise (SpliceError (at, msg))
let warn (at : region) (msg : string) = Util.Error.warn at "splice" msg

(* Check *)

let check (b : bool) (at : region) (msg : string) : unit =
  if not b then error at msg

let guard (b : bool) (at : region) (msg : string) : unit =
  if not b then warn at msg
