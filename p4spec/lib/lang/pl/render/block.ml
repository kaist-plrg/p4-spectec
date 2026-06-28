(* Block document

   A structured representation of block-level asciidoc (bulleted list items with
   nesting, sibling lines, and literal scaffolding), serialized once by
   [serialize]. Inline content is carried as [Doc.t]; the serializer recomputes
   list bullets from the current [level] exactly as [render_instr] did, so block
   structure stops being assembled by hand with [F.asprintf]/[^]. *)

type t =
  | Inline of Doc.t (* inline-only content (titles, table cells): no bullet *)
  | Line of Doc.t (* one list item at the current level *)
  | Nest of Doc.t * t list (* item head + children rendered at level+1 *)
  | Vseq of t list (* siblings joined by "\n" at the current level *)
  | Raw of string (* literal scaffolding: "--", "|===", try anchors *)
  | Empty

let inline (d : Doc.t) : t = Inline d
let line (d : Doc.t) : t = Line d
let nest (head : Doc.t) (children : t list) : t = Nest (head, children)
let vseq (ts : t list) : t = Vseq ts
let raw (s : string) : t = Raw s
let empty : t = Empty

let bullet_at ~(ordered : bool) (level : int) : string =
  if ordered then Adoc.adoc_ordered_bullet level
  else Adoc.adoc_unordered_bullet level

let rec serialize ~(level : int) ~(ordered : bool) (b : t) : string =
  match b with
  | Empty -> ""
  | Inline d -> Doc.to_adoc d
  | Raw s -> s
  | Line d -> bullet_at ~ordered level ^ Doc.to_adoc d
  | Vseq ts ->
      String.concat "\n" (List.map (serialize ~level ~ordered) ts)
  | Nest (head, children) ->
      let head_line = bullet_at ~ordered level ^ Doc.to_adoc head in
      let body =
        children
        |> List.map (serialize ~level:(level + 1) ~ordered)
        |> String.concat "\n"
      in
      if children = [] then head_line else head_line ^ "\n" ^ body
