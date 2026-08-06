exception UnparseError of string

let error (msg : string) = raise (UnparseError msg)
