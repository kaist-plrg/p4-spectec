module Run = Runtime.Dynamic_Runner.Signature
open Util.Source

type code = Unsupported_architecture

let render_code = function
  | Unsupported_architecture -> "sim/unsupported-architecture"

(* Error *)

let error (at : region) (msg : string) =
  raise (Run.ExternError (Run.abort ~source:"sim" at msg))

let error_no_region (msg : string) = error no_region msg
let error_stf (msg : string) = Stf.Error.error msg

(* Check *)

let check (b : bool) (at : region) (msg : string) : unit =
  if not b then error at msg
