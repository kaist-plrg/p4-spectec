(* Block document

   A structured representation of block-level asciidoc: inline [Doc.t] sentences
   joined by literal scaffolding (bullets, newlines, attach blocks, tables). It
   replaces the hand-rolled [F.asprintf]/[^] assembly in the instruction and
   definition renderers, so each logical line is a single [Doc.t] serialized
   once, and block structure is composed as a typed tree rather than concatenated
   strings.

   Bullets and other literal asciidoc markup are carried as [Raw] (they hold no
   inline rendering context to lose); rendered content flows through [Inline]. *)

type t =
  | Inline of Doc.prose (* one inline sentence *)
  | Raw of string (* literal scaffolding: bullets, "\n", "--", "|===", anchors *)
  | Concat of t list (* children concatenated with no separator *)
  | Vseq of t list (* children joined by "\n" *)
  | Empty

let inline (d : Doc.prose) : t = Inline d
let raw (s : string) : t = Raw s
let concat (ts : t list) : t = Concat ts
let vseq (ts : t list) : t = Vseq ts
let empty : t = Empty

let rec serialize (b : t) : string =
  match b with
  | Empty -> ""
  | Inline d -> Doc.to_adoc d
  | Raw s -> s
  | Concat ts -> String.concat "" (List.map serialize ts)
  | Vseq ts -> String.concat "\n" (List.map serialize ts)
