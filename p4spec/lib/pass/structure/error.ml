open Util.Source

exception StructError of region * string

let error (at : region) (msg : string) = raise (StructError (at, msg))
