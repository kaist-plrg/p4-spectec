open Util.Error
open Util.Source

(* Error *)

let error (at : region) (msg : string) = error_extern at msg
let error_stf (msg : string) = error_stf msg
let error_no_region (msg : string) = error_extern no_region msg
let warn (at : region) (msg : string) = warn_extern at msg
let warn_no_region (msg : string) = warn_extern no_region msg

(* Check *)

let check (b : bool) (at : region) (msg : string) : unit =
  if not b then error at msg
