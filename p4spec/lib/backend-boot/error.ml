module Run = Runtime.Dynamic_Runner.Signature
open Util.Source

type code = Unsupported_interface

let render_code = function
  | Unsupported_interface -> "boot/unsupported-interface"

(* Error *)

let error (at : region) (msg : string) =
  raise (Run.ExternError (Run.abort ~source:"boot" at msg))

let error_no_region (msg : string) = error no_region msg

(* Check *)

let check (b : bool) (at : region) (msg : string) : unit =
  if not b then error at msg
