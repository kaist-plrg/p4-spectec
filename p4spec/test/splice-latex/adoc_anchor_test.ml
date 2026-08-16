open Lang
module Adoc = Lang__Pl__Render__Adoc

let semantic_anchor = function
  | Adoc.Function "present_function" -> Some "function_prose_present_function"
  | Adoc.Relation "Present_relation" -> Some "relation_prose_Present_relation"
  | _ -> None

let print label value = Printf.printf "[%s]\n%s\n" label value

let () =
  let open Adoc in
  link_subject_code (Function "present_function") (token "present_function")
  |> ser_code ~anchor:semantic_anchor
  |> print "present-function";
  link_subject_code (Function "missing_function") (token "missing_function")
  |> ser_code ~anchor:semantic_anchor
  |> print "missing-function";
  link_subject_prose (Relation "Present_relation") (text "present relation")
  |> ser_prose ~anchor:semantic_anchor
  |> print "present-relation";
  link_subject_prose (Relation "Missing_relation") (text "missing relation")
  |> ser_prose ~anchor:semantic_anchor
  |> print "missing-relation";
  link_prose ~target:"direct-arm" (text "direct arm")
  |> ser_prose ~anchor:semantic_anchor
  |> print "direct"
