open Util.Source

type code = Unsupported_architecture

let render_code = function
  | Unsupported_architecture -> "sim/unsupported-architecture"

(* Error *)

let error (at : region) (msg : string) =
  let open Runtime.Dynamic_Runner.Signature in
  raise (ExternError (diagnostic_failure ~source:"sim" at msg))

let error_stf (msg : string) = Stf.Error.error msg
let error_no_region (msg : string) = error no_region msg

(* Check *)

let check (b : bool) (at : region) (msg : string) : unit =
  if not b then error at msg
