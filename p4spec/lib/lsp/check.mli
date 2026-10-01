(* Sibling contents come from disk. *)
(* Positions use UTF-16 unless the caller selects UTF-8. *)
val run :
  ?position_encoding:[ `UTF8 | `UTF16 ] ->
  path:string ->
  string ->
  Linol_eio.Diagnostic.t list
