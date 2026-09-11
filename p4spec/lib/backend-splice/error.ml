open Util.Source

(* Error *)

exception SpliceError of Diagnostic.t

let error (at : region) (msg : string) =
  raise (SpliceError (Diagnostic.error ~source:"splice" at msg))

let warn ?(detail : string option) (at : region) (msg : string) =
  Diagnostic.warn ?detail ~source:"splice" at msg

(* Check *)

let check (b : bool) (at : region) (msg : string) : unit =
  if not b then error at msg

let guard (b : bool) (at : region) (msg : string) : unit =
  if not b then warn at msg
