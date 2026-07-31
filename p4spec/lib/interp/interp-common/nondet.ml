open Util.Source

(* Nondeterminism *)

exception Nondet of region

let nondet (at : region) = raise (Nondet at)
