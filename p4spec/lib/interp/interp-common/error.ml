open Util.Source

(* Diagnostics *)

let error (at : region) (msg : string) : Diagnostic.t =
  Diagnostic.error ~source:"interp" at msg

let warn (at : region) (msg : string) = Diagnostic.warn ~source:"interp" at msg

(* Guard *)

let guard (b : bool) (at : region) (msg : string) : unit =
  if not b then warn at msg
