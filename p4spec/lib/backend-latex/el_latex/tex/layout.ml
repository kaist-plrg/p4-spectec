open Doc

(* Width-sensitive layout resolution

   1. [resolve] validates the requested width and starts at column zero.
   2. [resolve_doc] dispatches on each [Doc.t] constructor and propagates its
      current column, continuation column, and same-line suffix width.
   3. [annotate_suffix_widths] pairs each Concat child with the width that must
      remain after it on the same line.
        - [Broken] mode stops at the first soft break;
        - [Flat] mode includes the complete suffix.
   4. [resolve_layout_group] compares [Width.flat doc] plus the suffix width
      with the columns remaining on the current line.
   5. [resolve_in_mode] turns each soft break into
        - a space in [Flat] mode or
        - a new indented line in [Broken] mode.
   6. [resolve_fill] packs items onto the current line and indents items that do
      not fit. [resolve_aligned] and [resolve_grid] stabilize shared column widths.
   7. Serialize receives a [Doc.t] with its width-sensitive choices replaced by
      concrete spaces and line structure.

   Thus f(a, b, c) stays on one line when it fits, and otherwise becomes
   f(a,
      b,
      c). *)

type mode = Flat | Broken

(* Resolved lines

   [x; y] represents two concrete lines and becomes LeftStack [x; y]. *)

type lines = t list

let doc_of_indent (column_next : int) : t =
  let docs_quad = List.init (column_next / 2) (fun _index -> Quad) in
  let docs_space = if column_next mod 2 = 0 then [] else [ Space ] in
  concat (docs_quad @ docs_space)

let lines_of_doc (doc : t) : lines =
  match doc with LeftStack lines -> lines | doc -> [ doc ]

let doc_of_lines (lines : lines) : t =
  match lines with [] -> Empty | [ doc ] -> doc | lines -> LeftStack lines

let concat_lines (lines_l : lines) (lines_r : lines) : lines =
  match (List.rev lines_l, lines_r) with
  | [], lines | lines, [] -> lines
  | line_l :: lines_l, line_r :: lines_r ->
      List.rev lines_l @ (concat [ line_l; line_r ] :: lines_r)

let column_after_lines (column : int) (lines : lines) : int =
  match lines with
  | [] -> column
  | [ doc ] -> column + Width.flat doc
  | lines -> Width.flat (List.hd (List.rev lines))

(* Same-line suffix-width annotation

   annotate_suffix_widths Broken 0 [x; y; SoftBreak SoftSpace; z]
   -> [(x, width y); (y, 0); (SoftBreak SoftSpace, width z); (z, 0)]

   Broken mode reserves only the suffix before its first soft break.
   Flat mode reserves every following document plus the enclosing suffix width. *)

let rec width_before_break (doc : t) : int * bool =
  match doc with
  | Concat docs -> width_before_break_in_docs docs
  | Group doc
  | Mathbin doc
  | Mathrel doc
  | Displaystyle doc
  | Link (_, doc)
  | Nest (_, doc) ->
      width_before_break doc
  | SoftBreak _ -> (0, true)
  | Fill (_, separator, docs) ->
      width_before_break (concat_intersperse separator docs)
  | Empty | Styled _ | Badge _ | Decimal _ | Hexadecimal _ | Fixed _ | Space
  | ThinSpace | Quad | Delimited _ | Subscript _ | Superscript _ | Subsup _
  | Fraction _ | LayoutGroup _ | Aligned _ | Grid _ | Stacked _ | LeftStack _
  | Numbered _ | Gathered _ ->
      (Width.flat doc, false)

and width_before_break_in_docs (docs : t list) : int * bool =
  match docs with
  | [] -> (0, false)
  | doc :: docs ->
      let width, has_break = width_before_break doc in
      if has_break then (width, true)
      else
        let width_rest, has_break = width_before_break_in_docs docs in
        (width + width_rest, has_break)

let width_suffix_of_docs (mode : mode) (width_suffix : int) (docs : t list) :
    int =
  match mode with
  | Flat ->
      List.fold_left (fun width doc -> width + Width.flat doc) width_suffix docs
  | Broken ->
      let width, has_break = width_before_break_in_docs docs in
      width + if has_break then 0 else width_suffix

let rec annotate_suffix_widths (mode : mode) (width_suffix : int)
    (docs : t list) : (t * int) list =
  match docs with
  | [] -> []
  | doc :: docs ->
      let width_suffix_doc = width_suffix_of_docs mode width_suffix docs in
      (doc, width_suffix_doc) :: annotate_suffix_widths mode width_suffix docs

(* Layout entry point

   resolve ~width:80 doc -> resolve_doc at column zero with no reserved suffix *)

let rec resolve ~(width : int) (doc : t) : t =
  Error.check_no_region (width > 0) "Layout.resolve: width must be positive";
  resolve_doc ~width ~column:0 ~column_next:0 ~width_suffix:0 doc

(* Document resolution

   resolve_doc ~width:80 ~column:12 ~column_next:4 ~width_suffix:3 doc means:

   - width: the total line-width budget for doc;
   - column: the column where doc begins on its current line;
   - column_next: the column where continuation lines begin;
   - width_suffix: columns reserved after doc for following content on the same line;
   - doc: the unresolved semantic TeX document.

   Child documents may receive smaller width budgets, adjusted columns, or a
   larger [width_suffix]. *)

and resolve_doc ~(width : int) ~(column : int) ~(column_next : int)
    ~(width_suffix : int) (doc : t) : t =
  match doc with
  | Empty | Styled _ | Badge _ | Decimal _ | Hexadecimal _ | Fixed _ | Space
  | ThinSpace | Quad ->
      resolve_atom doc
  | Concat docs -> resolve_concat ~width ~column ~column_next ~width_suffix docs
  | Group doc -> resolve_group ~width ~column ~column_next ~width_suffix doc
  | Mathbin doc -> resolve_mathbin ~width ~column ~column_next ~width_suffix doc
  | Mathrel doc -> resolve_mathrel ~width ~column ~column_next ~width_suffix doc
  | Displaystyle doc ->
      resolve_displaystyle ~width ~column ~column_next ~width_suffix doc
  | Delimited (delimiter, doc) ->
      resolve_delimited ~width ~column ~column_next ~width_suffix delimiter doc
  | Subscript (base, sub) ->
      resolve_subscript ~width ~column ~column_next ~width_suffix base sub
  | Superscript (base, sup) ->
      resolve_superscript ~width ~column ~column_next ~width_suffix base sup
  | Subsup (base, sub, sup) ->
      resolve_subsup ~width ~column ~column_next ~width_suffix base sub sup
  | Fraction (numerator, denominator) ->
      resolve_fraction ~width numerator denominator
  | Link (target, doc) ->
      resolve_link ~width ~column ~column_next ~width_suffix target doc
  | SoftBreak soft -> resolve_soft_break soft
  | LayoutGroup doc ->
      resolve_layout_group ~width ~column ~column_next ~width_suffix doc
  | Nest (indent_offset, doc) ->
      resolve_nested ~width ~column ~column_next ~width_suffix indent_offset doc
  | Fill (indent_fill, separator, docs) ->
      resolve_fill ~width ~column ~column_next ~width_suffix indent_fill
        separator docs
  | Aligned rows -> resolve_aligned ~width rows
  | Grid (alignments, rows) -> resolve_grid ~width alignments rows
  | Stacked docs -> resolve_stacked ~width docs
  | LeftStack docs -> resolve_left_stack ~width docs
  | Numbered docs -> resolve_numbered ~width docs
  | Gathered blocks -> resolve_gathered ~width blocks

(* Atomic documents

   Styled (Mathsf, "x") -> Styled (Mathsf, "x") *)

and resolve_atom (doc : t) : t = doc

(* Concatenated documents

   Concat [x; y] -> merge the resolved lines of x and y *)

and resolve_concat_docs ~(width : int) ~(column : int) ~(column_next : int)
    (lines : lines) (docs : (t * int) list) : t =
  match docs with
  | [] -> doc_of_lines lines
  | (doc, width_suffix) :: docs ->
      let column_doc = column_after_lines column lines in
      let doc =
        resolve_doc ~width ~column:column_doc ~column_next ~width_suffix doc
      in
      let lines = concat_lines lines (lines_of_doc doc) in
      resolve_concat_docs ~width ~column ~column_next lines docs

and resolve_concat ~(width : int) ~(column : int) ~(column_next : int)
    ~(width_suffix : int) (docs : t list) : t =
  let docs = annotate_suffix_widths Flat width_suffix docs in
  resolve_concat_docs ~width ~column ~column_next [ Empty ] docs

(* Groups

   Group x -> Group (resolve x) *)

and resolve_group ~(width : int) ~(column : int) ~(column_next : int)
    ~(width_suffix : int) (doc : t) : t =
  let doc = resolve_doc ~width ~column ~column_next ~width_suffix doc in
  Group doc

(* Binary classifications

   Mathbin x -> Mathbin (resolve x) *)

and resolve_mathbin ~(width : int) ~(column : int) ~(column_next : int)
    ~(width_suffix : int) (doc : t) : t =
  let doc = resolve_doc ~width ~column ~column_next ~width_suffix doc in
  Mathbin doc

(* Relation classifications

   Mathrel x -> Mathrel (resolve x) *)

and resolve_mathrel ~(width : int) ~(column : int) ~(column_next : int)
    ~(width_suffix : int) (doc : t) : t =
  let doc = resolve_doc ~width ~column ~column_next ~width_suffix doc in
  Mathrel doc

(* Display style

   Displaystyle x -> Displaystyle (resolve x) *)

and resolve_displaystyle ~(width : int) ~(column : int) ~(column_next : int)
    ~(width_suffix : int) (doc : t) : t =
  let doc = resolve_doc ~width ~column ~column_next ~width_suffix doc in
  Displaystyle doc

(* Delimited documents

   Delimited (Paren, x) -> Delimited (Paren, resolve x at width - 2) *)

and resolve_delimited ~(width : int) ~(column : int) ~(column_next : int)
    ~(width_suffix : int) (delimiter : delimiter) (doc : t) : t =
  let width_child = max 1 (width - Width.width_delimiter_margin) in
  let doc =
    resolve_doc ~width:width_child ~column ~column_next ~width_suffix doc
  in
  Delimited (delimiter, doc)

(* Script child budgets

   x_i -> twice the columns remaining after x *)

and width_script_budget ~(width : int) ~(column : int) ~(width_suffix : int)
    (base : t) : int =
  max 1 (2 * (width - column - width_suffix - Width.flat base))

(* Subscripts

   Subscript (x, i) -> resolve x while reserving script i *)

and resolve_subscript ~(width : int) ~(column : int) ~(column_next : int)
    ~(width_suffix : int) (base : t) (sub : t) : t =
  let base =
    resolve_doc ~width ~column ~column_next
      ~width_suffix:(width_suffix + Width.flat_script_width sub)
      base
  in
  let width_child = width_script_budget ~width ~column ~width_suffix base in
  let sub =
    resolve_doc ~width:width_child ~column:0 ~column_next ~width_suffix:0 sub
  in
  Subscript (base, sub)

(* Superscripts

   Superscript (x, n) -> resolve x while reserving script n *)

and resolve_superscript ~(width : int) ~(column : int) ~(column_next : int)
    ~(width_suffix : int) (base : t) (sup : t) : t =
  let base =
    resolve_doc ~width ~column ~column_next
      ~width_suffix:(width_suffix + Width.flat_script_width sup)
      base
  in
  let width_child = width_script_budget ~width ~column ~width_suffix base in
  let sup =
    resolve_doc ~width:width_child ~column:0 ~column_next ~width_suffix:0 sup
  in
  Superscript (base, sup)

(* Paired scripts

   Subsup (x, i, n) -> resolve x while reserving the wider script *)

and resolve_subsup ~(width : int) ~(column : int) ~(column_next : int)
    ~(width_suffix : int) (base : t) (sub : t) (sup : t) : t =
  let width_script =
    max (Width.flat_script_width sub) (Width.flat_script_width sup)
  in
  let base =
    resolve_doc ~width ~column ~column_next
      ~width_suffix:(width_suffix + width_script)
      base
  in
  let width_child = width_script_budget ~width ~column ~width_suffix base in
  let sub =
    resolve_doc ~width:width_child ~column:0 ~column_next ~width_suffix:0 sub
  in
  let sup =
    resolve_doc ~width:width_child ~column:0 ~column_next ~width_suffix:0 sup
  in
  Subsup (base, sub, sup)

(* Fractions

   Fraction (p, q) -> resolve p and q at width - 2 *)

and resolve_fraction ~(width : int) (numerator : t) (denominator : t) : t =
  let width_child = max 1 (width - Width.width_fraction_margin) in
  let numerator =
    resolve_doc ~width:width_child ~column:0 ~column_next:0 ~width_suffix:0
      numerator
  in
  let denominator =
    resolve_doc ~width:width_child ~column:0 ~column_next:0 ~width_suffix:0
      denominator
  in
  Fraction (numerator, denominator)

(* Links

   Link (target, x) -> normalize target around resolved x *)

and resolve_link ~(width : int) ~(column : int) ~(column_next : int)
    ~(width_suffix : int) (target : target) (doc : t) : t =
  let doc = resolve_doc ~width ~column ~column_next ~width_suffix doc in
  Link.normalize_after_layout target doc

(* Soft breaks

   SoftBreak SoftSpace -> Space *)

and resolve_soft_break (soft : soft) : t =
  match soft with SoftCut -> Empty | SoftSpace -> Space

(* Layout groups

   LayoutGroup (x soft-space y) -> flat if it fits, broken otherwise *)

and resolve_layout_group ~(width : int) ~(column : int) ~(column_next : int)
    ~(width_suffix : int) (doc : t) : t =
  let mode =
    if Width.flat doc + width_suffix <= width - column then Flat else Broken
  in
  let lines =
    resolve_in_mode ~mode ~width ~column ~column_next ~width_suffix doc
  in
  doc_of_lines lines

(* Nested documents

   Nest (2, x) -> resolve x with column_next + 2 *)

and resolve_nested ~(width : int) ~(column : int) ~(column_next : int)
    ~(width_suffix : int) (indent_offset : int) (doc : t) : t =
  resolve_doc ~width ~column
    ~column_next:(column_next + indent_offset)
    ~width_suffix doc

(* Fill documents

   Fill (2, comma, [x; y]) -> pack x and y, then indent overflow by 2 *)

and indent_fill_lines (column_next : int) (lines : lines) : lines =
  match lines with
  | [] -> []
  | line_h :: lines -> concat [ doc_of_indent column_next; line_h ] :: lines

and resolve_fill_docs ~(width : int) ~(column : int) ~(column_next : int)
    ~(width_suffix : int) (separator : t) (lines : lines) (docs : t list) : t =
  match docs with
  | [] -> doc_of_lines lines
  | doc :: docs ->
      let width_suffix_doc = match docs with [] -> width_suffix | _ -> 0 in
      let column_current = column_after_lines column lines in
      let width_needed =
        Width.flat separator + Width.flat doc + width_suffix_doc
      in
      let fits = width_needed <= width - column_current in
      let lines =
        if fits then
          let column_doc = column_current + Width.flat separator in
          let doc =
            resolve_doc ~width ~column:column_doc ~column_next
              ~width_suffix:width_suffix_doc doc
          in
          let doc = concat [ separator; doc ] in
          concat_lines lines (lines_of_doc doc)
        else
          let doc =
            resolve_doc ~width ~column:column_next ~column_next
              ~width_suffix:width_suffix_doc doc
          in
          lines @ indent_fill_lines column_next (lines_of_doc doc)
      in
      resolve_fill_docs ~width ~column ~column_next ~width_suffix separator
        lines docs

and resolve_fill ~(width : int) ~(column : int) ~(column_next : int)
    ~(width_suffix : int) (indent_fill : int) (separator : t) (docs : t list) :
    t =
  let column_next_fill = column_next + indent_fill in
  match docs with
  | [] -> Empty
  | doc_h :: docs ->
      let width_suffix_h = match docs with [] -> width_suffix | _ -> 0 in
      let doc_h =
        resolve_doc ~width ~column ~column_next:column_next_fill
          ~width_suffix:width_suffix_h doc_h
      in
      let lines = lines_of_doc doc_h in
      resolve_fill_docs ~width ~column ~column_next:column_next_fill
        ~width_suffix separator lines docs

(* Remaining grid columns

   [3; 4] -> 3 + gap + 4 + gap *)

and width_remaining_columns (widths : int list) : int =
  List.fold_left ( + ) 0 widths
  + (Width.width_intercolumn_spacing * List.length widths)

(* Grid cells

   [lhs; equal; rhs] -> resolve each cell within its column budget *)

and resolve_cells_from ~(width : int) (column_widths : int list)
    (alignments : alignment option list) (docs : t list) (column : int) : t list
    =
  match docs with
  | [] -> []
  | doc :: docs ->
      let width_cell, column_widths =
        match column_widths with
        | width_cell :: column_widths -> (width_cell, column_widths)
        | [] -> (Width.flat doc, List.map Width.flat docs)
      in
      let alignment, alignments =
        match alignments with
        | alignment :: alignments -> (alignment, alignments)
        | [] -> (None, [])
      in
      let padding =
        match alignment with
        | None | Some Left -> 0
        | Some Center -> (width_cell - Width.flat doc) / 2
        | Some Right -> width_cell - Width.flat doc
      in
      let width_remaining = width_remaining_columns column_widths in
      let width_local = max 1 (width - column - width_remaining) in
      let doc =
        resolve_doc ~width:width_local ~column:(max 0 padding) ~column_next:0
          ~width_suffix:0 doc
      in
      let column = column + width_cell + Width.width_intercolumn_spacing in
      doc :: resolve_cells_from ~width column_widths alignments docs column

and resolve_cells ~(width : int) ~(column_widths : int list)
    ~(alignments : alignment list option) (docs : t list) : t list =
  let alignments =
    match alignments with
    | None -> List.map (fun _doc -> None) docs
    | Some alignments -> List.map (fun alignment -> Some alignment) alignments
  in
  resolve_cells_from ~width column_widths alignments docs 0

(* Rows at candidate column widths

   Resolve every row using the same candidate widths *)

and resolve_rows_at_widths ~(width : int) ~(alignments : alignment list option)
    ~(rows : t list list) (column_widths : int list) : t list list =
  List.map (resolve_cells ~width ~column_widths ~alignments) rows

(* Grid-width comparison

   Prefer the candidate with the narrower total aligned width *)

and rows_are_narrower ((widths, _) : int list * t list list)
    ((widths_best, _) : int list * t list list) : bool =
  Width.flat_columns widths < Width.flat_columns widths_best

(* Grid-width stabilization

   Iterate candidate widths; cycles retain the narrowest resolved rows *)

and stabilize_rows ~(width : int) ~(alignments : alignment list option)
    ~(rows : t list list) (seen : int list list)
    (best : (int list * t list list) option) (column_widths : int list) :
    t list list =
  let rows_resolved =
    resolve_rows_at_widths ~width ~alignments ~rows column_widths
  in
  let column_widths_resolved = Width.flat_column_widths rows_resolved in
  let candidate = (column_widths_resolved, rows_resolved) in
  let best =
    match best with
    | None -> candidate
    | Some best when rows_are_narrower candidate best -> candidate
    | Some best -> best
  in
  if column_widths_resolved = column_widths then rows_resolved
  else if List.mem column_widths_resolved seen then snd best
  else
    let seen = column_widths_resolved :: seen in
    stabilize_rows ~width ~alignments ~rows seen (Some best)
      column_widths_resolved

(* Grid widths iterate to a fixed point; cycles keep the narrowest grid *)

and resolve_rows ~(width : int) ~(alignments : alignment list option)
    (rows : t list list) : t list list =
  let column_widths = Width.flat_column_widths rows in
  stabilize_rows ~width ~alignments ~rows [ column_widths ] None column_widths

(* Aligned documents

   Aligned [[lhs; equal; rhs]] -> resolve each shared-width column *)

and resolve_aligned ~(width : int) (rows : t list list) : t =
  let rows = resolve_rows ~width ~alignments:None rows in
  Aligned rows

(* Grids

   Grid (alignments, rows) -> resolve cells at shared column widths *)

and resolve_grid_rows ~(width : int) (rows_cell : t list list) (rows : row list)
    : row list =
  match (rows_cell, rows) with
  | [], [] -> []
  | rows_cell, RowGap :: rows ->
      RowGap :: resolve_grid_rows ~width rows_cell rows
  | rows_cell, Spanning doc :: rows ->
      let doc =
        resolve_doc ~width ~column:0 ~column_next:0 ~width_suffix:0 doc
      in
      Spanning doc :: resolve_grid_rows ~width rows_cell rows
  | docs :: rows_cell, Cells _ :: rows ->
      Cells docs :: resolve_grid_rows ~width rows_cell rows
  | [], Cells _ :: _ | _ :: _, [] ->
      Error.error_no_region "grid rows do not match resolved cell rows"

and resolve_grid ~(width : int) (alignments : alignment list) (rows : row list)
    : t =
  let rows_cell =
    List.filter_map
      (fun row ->
        match row with Cells docs -> Some docs | Spanning _ | RowGap -> None)
      rows
  in
  let rows_cell = resolve_rows ~width ~alignments:(Some alignments) rows_cell in
  let rows = resolve_grid_rows ~width rows_cell rows in
  Grid (alignments, rows)

(* Stacked documents

   Stacked [p; q] -> resolve p and q from column zero *)

and resolve_stacked ~(width : int) (docs : t list) : t =
  let docs =
    List.map (resolve_doc ~width ~column:0 ~column_next:0 ~width_suffix:0) docs
  in
  Stacked docs

(* Left-stacked documents

   LeftStack [x; y] -> LeftStack [resolve x; resolve y] *)

and resolve_left_stack ~(width : int) (docs : t list) : t =
  let docs =
    List.map (resolve_doc ~width ~column:0 ~column_next:0 ~width_suffix:0) docs
  in
  LeftStack docs

(* Numbered documents

   Numbered [p; q] -> resolve p and q after reserving the label gutter *)

and resolve_numbered ~(width : int) (docs : t list) : t =
  let width_body =
    max 1 (width - Width.flat_numbered_gutter (List.length docs))
  in
  let docs =
    List.map
      (resolve_doc ~width:width_body ~column:0 ~column_next:0 ~width_suffix:0)
      docs
  in
  Numbered docs

(* Gathered documents

   Gathered [Line x; Gap] -> Gathered [Line (resolve x); Gap] *)

and resolve_gathered_block ~(width : int) (block : block) : block =
  match block with
  | Line doc ->
      let doc =
        resolve_doc ~width ~column:0 ~column_next:0 ~width_suffix:0 doc
      in
      Line doc
  | Gap -> Gap

and resolve_gathered ~(width : int) (blocks : block list) : t =
  let blocks = List.map (resolve_gathered_block ~width) blocks in
  Gathered blocks

(* Mode-specific dispatch

   SoftBreak SoftSpace in Broken -> [Empty; doc_of_indent column_next] *)

and resolve_in_mode ~(mode : mode) ~(width : int) ~(column : int)
    ~(column_next : int) ~(width_suffix : int) (doc : t) : lines =
  match doc with
  | Concat docs ->
      resolve_concat_in_mode ~mode ~width ~column ~column_next ~width_suffix
        docs
  | Group doc ->
      resolve_group_in_mode ~mode ~width ~column ~column_next ~width_suffix doc
  | Mathbin doc ->
      resolve_mathbin_in_mode ~mode ~width ~column ~column_next ~width_suffix
        doc
  | Mathrel doc ->
      resolve_mathrel_in_mode ~mode ~width ~column ~column_next ~width_suffix
        doc
  | Displaystyle doc ->
      resolve_displaystyle_in_mode ~mode ~width ~column ~column_next
        ~width_suffix doc
  | Delimited (delimiter, doc) ->
      resolve_delimited_in_mode ~mode ~width ~column ~column_next ~width_suffix
        delimiter doc
  | Subscript (base, sub) ->
      resolve_subscript_in_mode ~mode ~width ~column ~column_next ~width_suffix
        base sub
  | Superscript (base, sup) ->
      resolve_superscript_in_mode ~mode ~width ~column ~column_next
        ~width_suffix base sup
  | Subsup (base, sub, sup) ->
      resolve_subsup_in_mode ~mode ~width ~column ~column_next ~width_suffix
        base sub sup
  | Fraction (numerator, denominator) ->
      resolve_fraction_in_mode ~mode ~width numerator denominator
  | Link (target, doc) ->
      resolve_link_in_mode ~mode ~width ~column ~column_next ~width_suffix
        target doc
  | SoftBreak soft -> resolve_soft_break_in_mode ~mode ~column_next soft
  | LayoutGroup doc ->
      resolve_layout_group_in_mode ~width ~column ~column_next ~width_suffix doc
  | Nest (indent_offset, doc) ->
      resolve_nested_in_mode ~mode ~width ~column ~column_next ~width_suffix
        indent_offset doc
  | _ -> resolve_leaf_in_mode ~width ~column ~column_next ~width_suffix doc

and resolve_leaf_in_mode ~(width : int) ~(column : int) ~(column_next : int)
    ~(width_suffix : int) (doc : t) : lines =
  [ resolve_doc ~width ~column ~column_next ~width_suffix doc ]

(* Mode-specific concatenation

   Concat [x; y] -> merge x and y under the selected mode *)

and resolve_concat_docs_in_mode ~(mode : mode) ~(width : int) ~(column : int)
    ~(column_next : int) (lines : lines) (docs : (t * int) list) : lines =
  match docs with
  | [] -> lines
  | (doc, width_suffix) :: docs ->
      let column_doc = column_after_lines column lines in
      let doc =
        resolve_in_mode ~mode ~width ~column:column_doc ~column_next
          ~width_suffix doc
      in
      let lines = concat_lines lines doc in
      resolve_concat_docs_in_mode ~mode ~width ~column ~column_next lines docs

and resolve_concat_in_mode ~(mode : mode) ~(width : int) ~(column : int)
    ~(column_next : int) ~(width_suffix : int) (docs : t list) : lines =
  let docs = annotate_suffix_widths mode width_suffix docs in
  resolve_concat_docs_in_mode ~mode ~width ~column ~column_next [ Empty ] docs

(* Mode-specific groups

   Group x -> map Group over the resolved lines of x *)

and resolve_group_in_mode ~(mode : mode) ~(width : int) ~(column : int)
    ~(column_next : int) ~(width_suffix : int) (doc : t) : lines =
  let lines =
    resolve_in_mode ~mode ~width ~column ~column_next ~width_suffix doc
  in
  List.map group lines

(* Mode-specific binary classifications

   Mathbin x -> map Mathbin over the resolved lines of x *)

and resolve_mathbin_in_mode ~(mode : mode) ~(width : int) ~(column : int)
    ~(column_next : int) ~(width_suffix : int) (doc : t) : lines =
  let lines =
    resolve_in_mode ~mode ~width ~column ~column_next ~width_suffix doc
  in
  List.map mathbin lines

(* Mode-specific relation classifications

   Mathrel x -> map Mathrel over the resolved lines of x *)

and resolve_mathrel_in_mode ~(mode : mode) ~(width : int) ~(column : int)
    ~(column_next : int) ~(width_suffix : int) (doc : t) : lines =
  let lines =
    resolve_in_mode ~mode ~width ~column ~column_next ~width_suffix doc
  in
  List.map mathrel lines

(* Mode-specific display style

   Displaystyle x -> Displaystyle (resolved multiline x) *)

and resolve_displaystyle_in_mode ~(mode : mode) ~(width : int) ~(column : int)
    ~(column_next : int) ~(width_suffix : int) (doc : t) : lines =
  let lines =
    resolve_in_mode ~mode ~width ~column ~column_next ~width_suffix doc
  in
  let doc = doc_of_lines lines in
  [ Displaystyle doc ]

(* Mode-specific delimiters

   Delimited (Paren, x) -> one delimiter around resolved multiline x *)

and resolve_delimited_in_mode ~(mode : mode) ~(width : int) ~(column : int)
    ~(column_next : int) ~(width_suffix : int) (delimiter : delimiter) (doc : t)
    : lines =
  let width_child = max 1 (width - Width.width_delimiter_margin) in
  let lines =
    resolve_in_mode ~mode ~width:width_child ~column ~column_next ~width_suffix
      doc
  in
  let doc = doc_of_lines lines in
  [ Delimited (delimiter, doc) ]

(* Mode-specific script bases and children

   base of x_i -> resolve x while reserving script i
   child i of x_i -> resolve i within the remaining script budget *)

and resolve_script_base_in_mode ~(mode : mode) ~(width : int) ~(column : int)
    ~(column_next : int) ~(width_suffix : int) ~(width_script : int) (base : t)
    : t =
  let width_suffix = width_suffix + width_script in
  let lines =
    resolve_in_mode ~mode ~width ~column ~column_next ~width_suffix base
  in
  doc_of_lines lines

and resolve_script_child_in_mode ~(mode : mode) ~(width : int) ~(column : int)
    ~(column_next : int) ~(width_suffix : int) (base : t) (child : t) : t =
  let width_child = width_script_budget ~width ~column ~width_suffix base in
  let lines =
    resolve_in_mode ~mode ~width:width_child ~column:0 ~column_next
      ~width_suffix:0 child
  in
  doc_of_lines lines

(* Mode-specific subscripts

   Subscript (x, i) -> [Subscript (resolved x, resolved i)] *)

and resolve_subscript_in_mode ~(mode : mode) ~(width : int) ~(column : int)
    ~(column_next : int) ~(width_suffix : int) (base : t) (sub : t) : lines =
  let base =
    resolve_script_base_in_mode ~mode ~width ~column ~column_next ~width_suffix
      ~width_script:(Width.flat_script_width sub)
      base
  in
  let sub =
    resolve_script_child_in_mode ~mode ~width ~column ~column_next ~width_suffix
      base sub
  in
  [ Subscript (base, sub) ]

(* Mode-specific superscripts

   Superscript (x, n) -> [Superscript (resolved x, resolved n)] *)

and resolve_superscript_in_mode ~(mode : mode) ~(width : int) ~(column : int)
    ~(column_next : int) ~(width_suffix : int) (base : t) (sup : t) : lines =
  let base =
    resolve_script_base_in_mode ~mode ~width ~column ~column_next ~width_suffix
      ~width_script:(Width.flat_script_width sup)
      base
  in
  let sup =
    resolve_script_child_in_mode ~mode ~width ~column ~column_next ~width_suffix
      base sup
  in
  [ Superscript (base, sup) ]

(* Mode-specific paired scripts

   Subsup (x, i, n) -> [Subsup (resolved x, resolved i, resolved n)] *)

and resolve_subsup_in_mode ~(mode : mode) ~(width : int) ~(column : int)
    ~(column_next : int) ~(width_suffix : int) (base : t) (sub : t) (sup : t) :
    lines =
  let width_script =
    max (Width.flat_script_width sub) (Width.flat_script_width sup)
  in
  let base =
    resolve_script_base_in_mode ~mode ~width ~column ~column_next ~width_suffix
      ~width_script base
  in
  let sub =
    resolve_script_child_in_mode ~mode ~width ~column ~column_next ~width_suffix
      base sub
  in
  let sup =
    resolve_script_child_in_mode ~mode ~width ~column ~column_next ~width_suffix
      base sup
  in
  [ Subsup (base, sub, sup) ]

(* Mode-specific fractions

   Fraction (p, q) -> [Fraction (resolved p, resolved q)] *)

and resolve_fraction_in_mode ~(mode : mode) ~(width : int) (numerator : t)
    (denominator : t) : lines =
  let width_child = max 1 (width - Width.width_fraction_margin) in
  let lines_numerator =
    resolve_in_mode ~mode ~width:width_child ~column:0 ~column_next:0
      ~width_suffix:0 numerator
  in
  let lines_denominator =
    resolve_in_mode ~mode ~width:width_child ~column:0 ~column_next:0
      ~width_suffix:0 denominator
  in
  let numerator = doc_of_lines lines_numerator in
  let denominator = doc_of_lines lines_denominator in
  [ Fraction (numerator, denominator) ]

(* Mode-specific links

   Link (target, x) -> wrap each resolved line of x *)

and resolve_link_in_mode ~(mode : mode) ~(width : int) ~(column : int)
    ~(column_next : int) ~(width_suffix : int) (target : target) (doc : t) :
    lines =
  let lines =
    resolve_in_mode ~mode ~width ~column ~column_next ~width_suffix doc
  in
  List.map (Link.link_unowned_doc target) lines

(* Mode-specific soft breaks

   SoftBreak SoftSpace in Flat -> [Space] *)

and resolve_soft_break_in_mode ~(mode : mode) ~(column_next : int) (soft : soft)
    : lines =
  match (mode, soft) with
  | Flat, SoftCut -> [ Empty ]
  | Flat, SoftSpace -> [ Space ]
  | Broken, _ -> [ Empty; doc_of_indent column_next ]

(* Mode-specific layout groups

   LayoutGroup x -> independently choose flat or broken lines for x *)

and resolve_layout_group_in_mode ~(width : int) ~(column : int)
    ~(column_next : int) ~(width_suffix : int) (doc : t) : lines =
  let doc =
    resolve_layout_group ~width ~column ~column_next ~width_suffix doc
  in
  lines_of_doc doc

(* Mode-specific nesting

   Nest (2, x) -> resolve x with column_next + 2 in the selected mode *)

and resolve_nested_in_mode ~(mode : mode) ~(width : int) ~(column : int)
    ~(column_next : int) ~(width_suffix : int) (indent_offset : int) (doc : t) :
    lines =
  resolve_in_mode ~mode ~width ~column
    ~column_next:(column_next + indent_offset)
    ~width_suffix doc
