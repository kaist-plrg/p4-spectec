open Util.Error
open Util.Source

(* Error *)

let error (at : region) (msg : string) = error_algo at msg
let warn (at : region) (msg : string) = warn_algo at msg

(* Checks *)

let check (b : bool) (at : region) (msg : string) : unit =
  if not b then error at msg
