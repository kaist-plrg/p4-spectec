open El

type t = string

let to_string (s : t) : string = s

let init (hintexp : Hint.t) : t option =
  match hintexp.it with TextE s -> Some s | _ -> None
