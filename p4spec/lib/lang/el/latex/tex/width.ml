open Doc

(* Flat-width parameters *)

let width_half_rounded_up (width : int) : int = (width + 1) / 2
let width_intercolumn_spacing = 2
let width_delimiter_margin = 2
let width_fraction_margin = 2
let width_numbered_label_margin = 2

(* Flat document width

   flat (Concat [Styled (Mathsf, "x"); Space; Styled (Mathsf, "y")]) -> 3

   Approximates one unresolved line for layout decisions *)

let rec flat (doc : t) : int =
  match doc with
  | Empty -> flat_empty ()
  | Styled (style, text) -> flat_styled style text
  | Badge text -> flat_badge text
  | Decimal n -> flat_decimal n
  | Hexadecimal n -> flat_hexadecimal n
  | Fixed symbol -> flat_fixed symbol
  | Space -> flat_space ()
  | ThinSpace -> flat_thin_space ()
  | Quad -> flat_quad ()
  | Concat docs -> flat_concat docs
  | Group doc -> flat_group doc
  | Mathbin doc -> flat_mathbin doc
  | Mathrel doc -> flat_mathrel doc
  | Displaystyle doc -> flat_displaystyle doc
  | Delimited (delimiter, doc) -> flat_delimited delimiter doc
  | Subscript (base, sub) -> flat_subscript base sub
  | Superscript (base, sup) -> flat_superscript base sup
  | Subsup (base, sub, sup) -> flat_subsup base sub sup
  | Fraction (numerator, denominator) -> flat_fraction numerator denominator
  | Link (target, doc) -> flat_link target doc
  | SoftBreak soft -> flat_soft_break soft
  | LayoutGroup doc -> flat_layout_group doc
  | Nest (indent, doc) -> flat_nest indent doc
  | Fill (indent, separator, docs) -> flat_fill indent separator docs
  | Aligned rows -> flat_aligned rows
  | Grid (alignments, rows) -> flat_grid alignments rows
  | Stacked docs -> flat_stacked docs
  | LeftStack docs -> flat_left_stack docs
  | Numbered docs -> flat_numbered docs
  | Gathered blocks -> flat_gathered blocks

(* Empty documents

   Empty -> 0 *)

and flat_empty () : int = 0

(* Styled documents

   Styled (_, "abc") -> 3 *)

and flat_styled (_style : style) (text : string) : int = String.length text

(* Rule badges

   Badge "rule" -> 4 *)

and flat_badge (text : string) : int = String.length text

(* Decimal numbers

   Decimal 123 -> 3 *)

and flat_decimal (n : Bigint.t) : int = String.length (Bigint.to_string n)

(* Hexadecimal numbers

   Hexadecimal 0xff -> 4 *)

and flat_hexadecimal (n : Bigint.t) : int =
  String.length (Bigint.Hex.to_string n)

(* Fixed symbols

   Fixed Turnstile -> 2 *)

and string_of_symbol (symbol : symbol) : string =
  match symbol with
  | Equal -> "="
  | NotEqual -> "=/="
  | Less -> "<"
  | Greater -> ">"
  | LessEqual -> "<="
  | GreaterEqual -> ">="
  | Plus -> "+"
  | Minus -> "-"
  | Question -> "?"
  | Ast -> "*"
  | Slash -> "/"
  | Comma -> ","
  | Semicolon -> ";"
  | Colon -> ":"
  | DoubleColon -> "::"
  | Cat -> "++"
  | Production -> "::="
  | VerticalBar -> "|"
  | Dot -> "."
  | Dot2 -> ".."
  | Ellipsis -> "..."
  | Epsilon -> "e"
  | In -> "in"
  | Neg -> "~"
  | Land -> "/\\"
  | Lor -> "\\/"
  | Rightarrow -> "=>"
  | Leftrightarrow -> "<=>"
  | Cdot -> "*"
  | Bmod -> "\\"
  | Turnstile -> "|-"
  | Tilesturn -> "-|"
  | To -> "->"
  | Longrightarrow -> "==>"
  | Hookrightarrow -> "~>"
  | Mapsto -> "|->"
  | Sim -> "~~"
  | Setminus -> "\\"
  | EmptySet -> "0"
  | LeftParen -> "("
  | RightParen -> ")"
  | LeftBracket -> "["
  | RightBracket -> "]"
  | LeftBrace -> "{"
  | RightBrace -> "}"

and flat_fixed (symbol : symbol) : int =
  max 1 (String.length (string_of_symbol symbol))

(* Spaces

   Space -> 1 *)

and flat_space () : int = 1

(* Thin spaces

   ThinSpace -> 1 *)

and flat_thin_space () : int = 1

(* Quad spaces

   Quad -> 2 *)

and flat_quad () : int = 2

(* Document sequences

   Concat [x; Space; y] -> width x + 1 + width y *)

and flat_concat (docs : t list) : int =
  List.fold_left (fun width doc -> width + flat doc) 0 docs

(* Groups

   Group x -> width x *)

and flat_group (doc : t) : int = flat doc

(* Binary classifications

   Mathbin x -> width x *)

and flat_mathbin (doc : t) : int = flat doc

(* Relation classifications

   Mathrel x -> width x *)

and flat_mathrel (doc : t) : int = flat doc

(* Display style

   Displaystyle x -> width x *)

and flat_displaystyle (doc : t) : int = flat doc

(* Delimited documents

   Delimited (Paren, x) -> width x + 2 *)

and flat_delimited (_delimiter : delimiter) (doc : t) : int =
  width_delimiter_margin + flat doc

(* Script widths

   flat_script_width abc -> 2 *)

and flat_script_width (doc : t) : int = width_half_rounded_up (flat doc)

(* Subscripts

   Subscript (x, zero) -> width x + width zero / 2 *)

and flat_subscript (base : t) (sub : t) : int =
  flat base + flat_script_width sub

(* Superscripts

   Superscript (x, zero) -> width x + width zero / 2 *)

and flat_superscript (base : t) (sup : t) : int =
  flat base + flat_script_width sup

(* Paired scripts

   Subsup (x, i, n) -> width x + max (script width i) (script width n) *)

and flat_subsup (base : t) (sub : t) (sup : t) : int =
  flat base + max (flat_script_width sub) (flat_script_width sup)

(* Fractions

   Fraction (x, yz) -> 2 + max (width x) (width yz) *)

and flat_fraction (numerator : t) (denominator : t) : int =
  width_fraction_margin + max (flat numerator) (flat denominator)

(* Links

   Link (_, x) -> width x *)

and flat_link (_target : target) (doc : t) : int = flat doc

(* Soft breaks

   SoftBreak SoftCut -> 0 *)

and flat_soft_break (soft : soft) : int =
  match soft with SoftCut -> 0 | SoftSpace -> 1

(* Layout groups

   LayoutGroup x -> width x *)

and flat_layout_group (doc : t) : int = flat doc

(* Nested documents

   Nest (2, x) -> width x *)

and flat_nest (_indent : int) (doc : t) : int = flat doc

(* Filled documents

   Fill (_, comma, [x; y]) -> width "x, y" *)

and flat_fill (_indent : int) (separator : t) (docs : t list) : int =
  flat (concat_intersperse separator docs)

(* Aligned documents

   Aligned [[x; equal; y]] -> width x + width equal + width y + gaps *)

and flat_columns (widths : int list) : int =
  match List.length widths with
  | 0 -> 0
  | columns ->
      List.fold_left ( + ) 0 widths + (width_intercolumn_spacing * (columns - 1))

and flat_column_widths (rows : t list list) : int list =
  let rec merge (widths : int list) (cells : t list) : int list =
    match (widths, cells) with
    | [], [] -> []
    | widths, [] -> widths
    | [], cell :: cells -> flat cell :: merge [] cells
    | width :: widths, cell :: cells ->
        max width (flat cell) :: merge widths cells
  in
  List.fold_left merge [] rows

and flat_aligned (rows : t list list) : int =
  flat_columns (flat_column_widths rows)

(* Widest documents

   [x; longest] -> width longest *)

and flat_widest (docs : t list) : int =
  List.fold_left (fun width doc -> max width (flat doc)) 0 docs

(* Grids

   Grid (_, [Cells [x]; Spanning long]) -> max (width x) (width long) *)

and flat_grid (_alignments : alignment list) (rows : row list) : int =
  let rows_cell, docs_spanning =
    List.fold_left
      (fun (rows_cell, docs_spanning) row ->
        match row with
        | Cells row -> (row :: rows_cell, docs_spanning)
        | Spanning doc -> (rows_cell, doc :: docs_spanning)
        | RowGap -> (rows_cell, docs_spanning))
      ([], []) rows
  in
  max (flat_aligned rows_cell) (flat_widest docs_spanning)

(* Centered stacks

   Stacked [x; long] -> width long *)

and flat_stacked (docs : t list) : int = flat_widest docs

(* Left-aligned stacks

   LeftStack [x; long] -> width long *)

and flat_left_stack (docs : t list) : int = flat_widest docs

(* Numbered documents

   Numbered [p; q] -> gutter width + max (width p) (width q) *)

and flat_numbered_gutter (count : int) : int =
  String.length (string_of_int count)
  + width_numbered_label_margin + width_intercolumn_spacing

and flat_numbered (docs : t list) : int =
  flat_numbered_gutter (List.length docs) + flat_widest docs

(* Gathered documents

   Gathered [Line x; Gap; Line long] -> width long *)

and flat_gathered (blocks : block list) : int =
  List.fold_left flat_block 0 blocks

(* Gathered-block dispatch

   Line long -> update the widest line; Gap -> preserve it *)

and flat_block (width : int) (block : block) : int =
  match block with Line doc -> flat_line width doc | Gap -> flat_gap width

(* Gathered lines

   flat_line 2 long -> max 2 (width long) *)

and flat_line (width : int) (doc : t) : int = max width (flat doc)

(* Gathered gaps

   flat_gap 4 -> 4 *)

and flat_gap (width : int) : int = width
