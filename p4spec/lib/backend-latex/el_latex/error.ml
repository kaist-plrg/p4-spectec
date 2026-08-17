open Util.Source

(* Error *)

exception LatexError of region * string

let error (at : region) (msg : string) = raise (LatexError (at, msg))
let error_no_region (msg : string) = error no_region msg

(* Error checks *)

let check (condition : bool) (at : region) (msg : string) : unit =
  if not condition then error at msg

let check_no_region (condition : bool) (msg : string) : unit =
  check condition no_region msg
