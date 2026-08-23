open Util.Source

(* Error *)

exception InterpError of region * string
exception BacktrackError of Util.Attempt.failtrace list

let error (at : region) (msg : string) = raise (InterpError (at, msg))
let error_with_failtraces failtraces = raise (BacktrackError failtraces)
let warn (at : region) (msg : string) = Diagnostic.warn ~source:"interp" at msg

(* Check *)

let check (b : bool) (at : region) (msg : string) : unit =
  if not b then error at msg

let guard (b : bool) (at : region) (msg : string) : unit =
  if not b then warn at msg
