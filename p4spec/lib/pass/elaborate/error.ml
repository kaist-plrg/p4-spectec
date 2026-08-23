open Util.Source

(* Error *)

exception ElabError of Diagnostic.t

let error (at : region) (msg : string) =
  raise (ElabError (Diagnostic.error ~source:"elab" at msg))

let warn (at : region) (msg : string) = Diagnostic.warn at "elab" msg

(* Checks *)

let check (b : bool) (at : region) (msg : string) : unit =
  if not b then error at msg
