open Util.Source

exception ProseError of Diagnostic.t

let error (at : region) (msg : string) =
  raise (ProseError (Diagnostic.error ~source:"prose" at msg))
