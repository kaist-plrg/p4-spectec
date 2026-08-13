open Util.Source

(* Error *)

let error (at : region) (msg : string) = Interp_common.Error.error at msg
let warn (at : region) (msg : string) = Interp_common.Error.warn at msg

(* Check *)

let check (b : bool) (at : region) (msg : string) : unit =
  if not b then error at msg

let guard (b : bool) (at : region) (msg : string) : unit =
  if not b then warn at msg
