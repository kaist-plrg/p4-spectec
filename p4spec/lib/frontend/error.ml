open Util.Source

exception ParseError of Diagnostic.t

let error (at : region) (msg : string) =
  raise (ParseError (Diagnostic.error ~source:"parse" at msg))
