open Util.Source

exception ProseError of region * string

let error (at : region) (msg : string) = raise (ProseError (at, msg))
