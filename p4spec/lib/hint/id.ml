type t = ProseIn | ProseOut | ProseTrue | ProseFalse | Prose

let compare = Stdlib.compare

(* This is for your parser/elaborator *)
let of_string = function
  | "prose_in" -> Ok ProseIn
  | "prose_out" -> Ok ProseOut
  | "prose_true" -> Ok ProseTrue
  | "prose_false" -> Ok ProseFalse
  | "prose" -> Ok Prose
  | s -> Error ("Unknown hint name: " ^ s)
