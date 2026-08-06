exception StfError of string

let error (msg : string) = raise (StfError msg)
