open Doc

(* Link ownership

   Renderer uses [link_unowned_doc] to preserve existing links
   while assigning a target to unowned regions.

   Layout uses [normalize_after_layout] to
   distribute links over generated rows and cells.

   Serialize uses [strip_links] only for invisible geometry copies.

   link_unowned_doc a (Concat [x; Link (b, y)])
   -> Concat [Link (a, x); Link (b, y)] *)

(* Link targets

   "Eval_0" -> Target "Eval_0" *)

let is_target_char (char : char) : bool =
  match char with
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
  | _ -> false

let target_of_string (s_target : string) : target =
  if String.length s_target > 0 && String.for_all is_target_char s_target then
    Target s_target
  else Error.error_no_region "invalid LaTeX link target"

(* Existing-link analysis

   Concat [x; Link (a, y)] -> true *)

let rec has_link_doc (doc : t) : bool =
  match doc with
  | Empty | Styled _ | Badge _ | Decimal _ | Hexadecimal _ | Fixed _ | Space
  | ThinSpace | Quad ->
      false
  | Concat docs -> has_link_docs docs
  | Group doc | Mathbin doc | Mathrel doc | Displaystyle doc -> has_link_doc doc
  | Delimited (_, doc) -> has_link_doc doc
  | Subscript (base, sub) | Superscript (base, sub) ->
      has_link_doc base || has_link_doc sub
  | Subsup (base, sub, sup) ->
      has_link_doc base || has_link_doc sub || has_link_doc sup
  | Fraction (numerator, denominator) ->
      has_link_doc numerator || has_link_doc denominator
  | Link _ -> true
  | SoftBreak _ -> false
  | LayoutGroup doc | Nest (_, doc) -> has_link_doc doc
  | Fill (_, separator, docs) -> has_link_doc separator || has_link_docs docs
  | Aligned rows -> List.exists has_link_docs rows
  | Grid (_, rows) -> List.exists has_link_row rows
  | Stacked docs | LeftStack docs | Numbered docs -> has_link_docs docs
  | Gathered blocks -> List.exists has_link_block blocks

and has_link_docs (docs : t list) : bool = List.exists has_link_doc docs

and has_link_row (row : row) : bool =
  match row with
  | Cells docs -> has_link_docs docs
  | Spanning doc -> has_link_doc doc
  | RowGap -> false

and has_link_block (block : block) : bool =
  match block with Line doc -> has_link_doc doc | Gap -> false

(* Link-boundary analysis

   Link, Numbered, and Fill prevent one fallback link from owning their
   complete enclosing region. *)

let rec has_boundary_doc (doc : t) : bool =
  match doc with
  | Empty | Styled _ | Badge _ | Decimal _ | Hexadecimal _ | Fixed _ | Space
  | ThinSpace | Quad ->
      false
  | Concat docs -> has_boundary_docs docs
  | Group doc | Mathbin doc | Mathrel doc | Displaystyle doc ->
      has_boundary_doc doc
  | Delimited (_, doc) -> has_boundary_doc doc
  | Subscript (base, sub) | Superscript (base, sub) ->
      has_boundary_doc base || has_boundary_doc sub
  | Subsup (base, sub, sup) ->
      has_boundary_doc base || has_boundary_doc sub || has_boundary_doc sup
  | Fraction (numerator, denominator) ->
      has_boundary_doc numerator || has_boundary_doc denominator
  | Link _ -> true
  | SoftBreak _ -> false
  | LayoutGroup doc | Nest (_, doc) -> has_boundary_doc doc
  | Fill _ -> true
  | Aligned rows -> List.exists has_boundary_docs rows
  | Grid (_, rows) -> List.exists has_boundary_row rows
  | Stacked docs | LeftStack docs -> has_boundary_docs docs
  | Numbered _ -> true
  | Gathered blocks -> List.exists has_boundary_block blocks

and has_boundary_docs (docs : t list) : bool = List.exists has_boundary_doc docs

and has_boundary_row (row : row) : bool =
  match row with
  | Cells docs -> has_boundary_docs docs
  | Spanning doc -> has_boundary_doc doc
  | RowGap -> false

and has_boundary_block (block : block) : bool =
  match block with Line doc -> has_boundary_doc doc | Gap -> false

(* Fallback-link insertion

   A boundary-free document receives one link. Documents containing explicit
   links, numbered premises, or fills are traversed into smaller regions. *)

let rec link_unowned_doc (target : target) (doc : t) : t =
  if not (has_boundary_doc doc) then Doc.link target doc
  else
    match doc with
    | Empty | Styled _ | Badge _ | Decimal _ | Hexadecimal _ | Fixed _ | Space
    | ThinSpace | Quad ->
        Doc.link target doc
    | Concat docs -> link_unowned_concat target docs
    | Group doc ->
        let doc = link_unowned_doc target doc in
        Group doc
    | Mathbin doc ->
        let doc = link_unowned_doc target doc in
        Mathbin doc
    | Mathrel doc ->
        let doc = link_unowned_doc target doc in
        Mathrel doc
    | Displaystyle doc ->
        let doc = link_unowned_doc target doc in
        Displaystyle doc
    | Delimited (delimiter, doc) ->
        let doc = link_unowned_doc target doc in
        Delimited (delimiter, doc)
    | Subscript (base, sub) ->
        let base = link_unowned_doc target base in
        let sub = link_unowned_doc target sub in
        Subscript (base, sub)
    | Superscript (base, sup) ->
        let base = link_unowned_doc target base in
        let sup = link_unowned_doc target sup in
        Superscript (base, sup)
    | Subsup (base, sub, sup) ->
        let base = link_unowned_doc target base in
        let sub = link_unowned_doc target sub in
        let sup = link_unowned_doc target sup in
        Subsup (base, sub, sup)
    | Fraction (numerator, denominator) ->
        let numerator = link_unowned_doc target numerator in
        let denominator = link_unowned_doc target denominator in
        Fraction (numerator, denominator)
    | Link (target_existing, doc_linked) as doc ->
        if has_link_doc doc_linked then
          link_unowned_doc target_existing doc_linked
        else doc
    | SoftBreak _ -> Doc.link target doc
    | LayoutGroup doc ->
        let doc = link_unowned_doc target doc in
        LayoutGroup doc
    | Nest (indent, doc) ->
        let doc = link_unowned_doc target doc in
        Nest (indent, doc)
    | Fill (indent, separator, docs) ->
        let docs = link_unowned_docs target docs in
        Fill (indent, separator, docs)
    | Aligned rows ->
        let rows = List.map (link_unowned_docs target) rows in
        Aligned rows
    | Grid (alignments, rows) ->
        let rows = List.map (link_unowned_row target) rows in
        Grid (alignments, rows)
    | Stacked docs ->
        let docs = link_unowned_docs target docs in
        Stacked docs
    | LeftStack docs ->
        let docs = link_unowned_docs target docs in
        LeftStack docs
    | Numbered docs ->
        let docs = link_unowned_docs target docs in
        Numbered docs
    | Gathered blocks ->
        let blocks = List.map (link_unowned_block target) blocks in
        Gathered blocks

(* Concatenated regions

   Concat [x; y; Link (b, z); w]
   -> Concat [Link (a, Concat [x; y]); Link (b, z); Link (a, w)] *)

and link_unowned_concat (target : target) (docs : t list) : t =
  link_unowned_concat_acc target [] [] docs

and link_unowned_concat_acc (target : target) (docs_unowned : t list)
    (docs_linked : t list) (docs : t list) : t =
  match docs with
  | [] ->
      let docs_linked =
        link_unowned_concat_flush target docs_unowned docs_linked
      in
      concat (List.rev docs_linked)
  | doc :: docs when has_boundary_doc doc ->
      let docs_linked =
        link_unowned_concat_flush target docs_unowned docs_linked
      in
      let doc = link_unowned_doc target doc in
      link_unowned_concat_acc target [] (doc :: docs_linked) docs
  | doc :: docs ->
      link_unowned_concat_acc target (doc :: docs_unowned) docs_linked docs

and link_unowned_concat_flush (target : target) (docs_unowned : t list)
    (docs_linked : t list) : t list =
  match docs_unowned with
  | [] -> docs_linked
  | docs_unowned ->
      let doc = concat (List.rev docs_unowned) in
      Doc.link target doc :: docs_linked

and link_unowned_docs (target : target) (docs : t list) : t list =
  List.map (link_unowned_doc target) docs

and link_unowned_row (target : target) (row : row) : row =
  match row with
  | Cells docs ->
      let docs = link_unowned_docs target docs in
      Cells docs
  | Spanning doc ->
      let doc = link_unowned_doc target doc in
      Spanning doc
  | RowGap -> RowGap

and link_unowned_block (target : target) (block : block) : block =
  match block with
  | Line doc ->
      let doc = link_unowned_doc target doc in
      Line doc
  | Gap -> Gap

(* Post-layout normalization

   Link (a, LeftStack [x; y])
   -> LeftStack [Link (a, x); Link (a, y)] *)

let rec normalize_after_layout (target : target) (doc : t) : t =
  match doc with
  | LeftStack docs ->
      let docs = link_unowned_docs target docs in
      LeftStack docs
  | Grid (alignments, rows) ->
      let rows = List.map (normalize_row_after_layout target) rows in
      Grid (alignments, rows)
  | doc -> link_unowned_doc target doc

and normalize_row_after_layout (target : target) (row : row) : row =
  match row with
  | Cells docs ->
      let docs = link_unowned_docs target docs in
      Cells docs
  | Spanning doc ->
      let doc = link_unowned_doc target doc in
      Spanning doc
  | RowGap -> RowGap

(* Link-free geometry copies

   Link (a, x) -> x *)

let rec strip_links (doc : t) : t =
  match doc with
  | ( Empty | Styled _ | Badge _ | Decimal _ | Hexadecimal _ | Fixed _ | Space
    | ThinSpace | Quad ) as doc ->
      doc
  | Concat docs ->
      let docs = strip_links_docs docs in
      Concat docs
  | Group doc ->
      let doc = strip_links doc in
      Group doc
  | Mathbin doc ->
      let doc = strip_links doc in
      Mathbin doc
  | Mathrel doc ->
      let doc = strip_links doc in
      Mathrel doc
  | Displaystyle doc ->
      let doc = strip_links doc in
      Displaystyle doc
  | Delimited (delimiter, doc) ->
      let doc = strip_links doc in
      Delimited (delimiter, doc)
  | Subscript (base, sub) ->
      let base = strip_links base in
      let sub = strip_links sub in
      Subscript (base, sub)
  | Superscript (base, sup) ->
      let base = strip_links base in
      let sup = strip_links sup in
      Superscript (base, sup)
  | Subsup (base, sub, sup) ->
      let base = strip_links base in
      let sub = strip_links sub in
      let sup = strip_links sup in
      Subsup (base, sub, sup)
  | Fraction (numerator, denominator) ->
      let numerator = strip_links numerator in
      let denominator = strip_links denominator in
      Fraction (numerator, denominator)
  | Link (_, doc) -> strip_links doc
  | SoftBreak _ as doc -> doc
  | LayoutGroup doc ->
      let doc = strip_links doc in
      LayoutGroup doc
  | Nest (indent, doc) ->
      let doc = strip_links doc in
      Nest (indent, doc)
  | Fill (indent, separator, docs) ->
      let separator = strip_links separator in
      let docs = strip_links_docs docs in
      Fill (indent, separator, docs)
  | Aligned rows ->
      let rows = List.map strip_links_docs rows in
      Aligned rows
  | Grid (alignments, rows) ->
      let rows = List.map strip_links_row rows in
      Grid (alignments, rows)
  | Stacked docs ->
      let docs = strip_links_docs docs in
      Stacked docs
  | LeftStack docs ->
      let docs = strip_links_docs docs in
      LeftStack docs
  | Numbered docs ->
      let docs = strip_links_docs docs in
      Numbered docs
  | Gathered blocks ->
      let blocks = List.map strip_links_block blocks in
      Gathered blocks

and strip_links_docs (docs : t list) : t list = List.map strip_links docs

and strip_links_row (row : row) : row =
  match row with
  | Cells docs ->
      let docs = strip_links_docs docs in
      Cells docs
  | Spanning doc ->
      let doc = strip_links doc in
      Spanning doc
  | RowGap -> RowGap

and strip_links_block (block : block) : block =
  match block with
  | Line doc ->
      let doc = strip_links doc in
      Line doc
  | Gap -> Gap
