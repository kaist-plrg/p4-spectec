open Util.Source

exception ParseError of region * string

let error (at : region) (msg : string) = raise (ParseError (at, msg))
