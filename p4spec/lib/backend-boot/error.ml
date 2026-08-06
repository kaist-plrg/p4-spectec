open Util.Source

(* Error *)

let error (at : region) (msg : string) =
  raise (Runtime.Dynamic_Runner.Signature.ExternError (at, msg))

let error_no_region (msg : string) = error no_region msg
let warn (at : region) (msg : string) = Util.Error.warn at "extern" msg
let warn_no_region (msg : string) = warn no_region msg

(* Check *)

let check (b : bool) (at : region) (msg : string) : unit =
  if not b then error at msg
