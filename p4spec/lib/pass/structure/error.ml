open Util.Source

exception StructError of Diagnostic.t

let error (at : region) (msg : string) =
  raise (StructError (Diagnostic.error ~source:"structure" at msg))
