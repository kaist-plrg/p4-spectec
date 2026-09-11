open Doc

(* TeX serialization

   1. [to_string] creates one output buffer.
   2. [render_doc] dispatches on each [Doc.t] constructor.
   3. Structural renderers append balanced TeX and render their children.
   4. [to_string] returns the completed buffer.

   Fraction (x, y) -> \frac{x}{y} *)

type context_style = Math | Text
type command_style = { command : string; context : context_style }

(* Buffer output *)

let add_string (buffer : Buffer.t) (text : string) : unit =
  Buffer.add_string buffer text

(* Math escaping

   x_i -> x\_i *)

let escape_math_char (buffer : Buffer.t) (char : char) : unit =
  match char with
  | '#' -> add_string buffer "\\#"
  | '$' -> add_string buffer "\\$"
  | '%' -> add_string buffer "\\%"
  | '&' -> add_string buffer "\\&"
  | '_' -> add_string buffer "\\_"
  | '{' -> add_string buffer "\\{"
  | '}' -> add_string buffer "\\}"
  | '\\' -> add_string buffer "\\backslash{}"
  | '^' -> add_string buffer "\\hat{}"
  | '~' -> add_string buffer "\\sim{}"
  | char -> Buffer.add_char buffer char

let escape_math (text : string) : string =
  let buffer = Buffer.create (String.length text) in
  String.iter (escape_math_char buffer) text;
  Buffer.contents buffer

(* Text escaping

   x_i -> x\_i *)

let escape_text_char (buffer : Buffer.t) (char : char) : unit =
  match char with
  | '#' -> add_string buffer "\\#"
  | '$' -> add_string buffer "\\$"
  | '%' -> add_string buffer "\\%"
  | '&' -> add_string buffer "\\&"
  | '_' -> add_string buffer "\\_"
  | '{' -> add_string buffer "\\{"
  | '}' -> add_string buffer "\\}"
  | char -> Buffer.add_char buffer char

let escape_text (text : string) : string =
  let buffer = Buffer.create (String.length text) in
  String.iter (escape_text_char buffer) text;
  Buffer.contents buffer

(* Styled commands

   Mathsf -> { command = "mathsf"; context = Math } *)

let command_of_style (style : style) : command_style =
  match style with
  | Mathit -> { command = "mathit"; context = Math }
  | Mathrm -> { command = "mathrm"; context = Math }
  | Mathsf -> { command = "mathsf"; context = Math }
  | Mathbb -> { command = "mathbb"; context = Math }
  | Mathtt -> { command = "mathtt"; context = Math }
  | Text -> { command = "text"; context = Text }
  | Texttt -> { command = "texttt"; context = Text }

(* Commands

   render_command buffer "mathit" "x" -> \mathit{x} *)

let render_command (buffer : Buffer.t) (command : string) (content : string) :
    unit =
  add_string buffer "\\";
  add_string buffer command;
  add_string buffer "{";
  add_string buffer content;
  add_string buffer "}"

(* Text runs

   render_text buffer "texttt" "a\\b" -> \texttt{a}\backslash{}\texttt{b} *)

let render_text_run (buffer : Buffer.t) (command : string)
    (buffer_run : Buffer.t) : unit =
  if Buffer.length buffer_run > 0 then (
    let content = Buffer.contents buffer_run |> escape_text in
    render_command buffer command content;
    Buffer.clear buffer_run)

let render_text_char (buffer : Buffer.t) (command : string)
    (buffer_run : Buffer.t) (char : char) : unit =
  match char with
  | '\\' ->
      render_text_run buffer command buffer_run;
      add_string buffer "\\backslash{}"
  | '^' ->
      render_text_run buffer command buffer_run;
      add_string buffer "\\hat{}"
  | '~' ->
      render_text_run buffer command buffer_run;
      add_string buffer "\\sim{}"
  | char -> Buffer.add_char buffer_run char

let render_text (buffer : Buffer.t) (command : string) (text : string) : unit =
  if text = "" then render_command buffer command ""
  else
    let buffer_run = Buffer.create (String.length text) in
    String.iter (render_text_char buffer command buffer_run) text;
    render_text_run buffer command buffer_run

(* Document serialization *)

let rec render_doc (buffer : Buffer.t) (doc : t) : unit =
  match doc with
  | Empty -> render_empty buffer
  | Styled (style, text) -> render_styled buffer style text
  | Badge text -> render_badge buffer text
  | Decimal n -> render_decimal buffer n
  | Hexadecimal n -> render_hexadecimal buffer n
  | Fixed symbol -> render_fixed buffer symbol
  | Space -> render_space buffer
  | ThinSpace -> render_thin_space buffer
  | Quad -> render_quad buffer
  | Concat docs -> render_concat buffer docs
  | Group doc -> render_group buffer doc
  | Mathbin doc -> render_mathbin buffer doc
  | Mathrel doc -> render_mathrel buffer doc
  | Displaystyle doc -> render_displaystyle buffer doc
  | Delimited (delimiter, doc) -> render_delimited buffer delimiter doc
  | Subscript (base, sub) -> render_subscript buffer base sub
  | Superscript (base, sup) -> render_superscript buffer base sup
  | Subsup (base, sub, sup) -> render_subsup buffer base sub sup
  | Fraction (numerator, denominator) ->
      render_fraction buffer numerator denominator
  | Link (target, doc) -> render_link buffer target doc
  | SoftBreak soft -> render_soft_break buffer soft
  | LayoutGroup doc -> render_layout_group buffer doc
  | Nest (indent, doc) -> render_nest buffer indent doc
  | Fill (indent, separator, docs) -> render_fill buffer indent separator docs
  | Aligned rows -> render_aligned buffer rows
  | Grid (alignments, rows) -> render_grid buffer alignments rows
  | Stacked docs -> render_stacked buffer docs
  | LeftStack docs -> render_left_stack buffer docs
  | Numbered docs -> render_numbered buffer docs
  | Gathered blocks -> render_gathered buffer blocks

and render_enclosed (buffer : Buffer.t) (opening : string) (closing : string)
    (doc : t) : unit =
  add_string buffer opening;
  render_doc buffer doc;
  add_string buffer closing

and render_docs (buffer : Buffer.t) (docs : t list) : unit =
  List.iter (render_doc buffer) docs

(* Empty documents

   Empty -> "" *)

and render_empty (_buffer : Buffer.t) : unit = ()

(* Styled documents

   Styled (Mathsf, "x") -> \mathsf{x} *)

and render_styled (buffer : Buffer.t) (style : style) (text : string) : unit =
  let command_style = command_of_style style in
  match command_style.context with
  | Math ->
      let content = escape_math text in
      render_command buffer command_style.command content
  | Text -> render_text buffer command_style.command text

(* Rule badges

   Badge "R" -> \fcolorbox{black}{...}{\scriptsize\texttt{R}} *)

and render_badge (buffer : Buffer.t) (text : string) : unit =
  add_string buffer "{\\definecolor{ellatexrulelabelbg}{rgb}{0.94,0.94,0.92}";
  add_string buffer "\\fcolorbox{black}{ellatexrulelabelbg}{\\scriptsize ";
  render_text buffer "texttt" text;
  add_string buffer "}}"

(* Decimal numbers

   Decimal 42 -> 42 *)

and render_decimal (buffer : Buffer.t) (n : Bigint.t) : unit =
  let text = Bigint.to_string n in
  add_string buffer text

(* Hexadecimal numbers

   Hexadecimal 255 -> \mathtt{0xff} *)

and render_hexadecimal (buffer : Buffer.t) (n : Bigint.t) : unit =
  add_string buffer "\\mathtt{";
  let text = Bigint.Hex.to_string n in
  add_string buffer text;
  add_string buffer "}"

(* Fixed symbols

   Fixed Turnstile -> \vdash *)

and string_of_symbol (symbol : symbol) : string =
  match symbol with
  | Equal -> "="
  | NotEqual -> "\\ne"
  | Less -> "<"
  | Greater -> ">"
  | LessEqual -> "\\le"
  | GreaterEqual -> "\\ge"
  | Plus -> "+"
  | Minus -> "-"
  | Question -> "?"
  | Ast -> "\\ast"
  | Slash -> "/"
  | Comma -> ","
  | Semicolon -> ";"
  | Colon -> ":"
  | DoubleColon -> "::"
  | Cat -> "+\\!\\!+"
  | Production -> "::="
  | VerticalBar -> "|"
  | Dot -> "."
  | Dot2 -> ".."
  | Ellipsis -> "\\ldots"
  | Epsilon -> "\\epsilon"
  | In -> "\\in"
  | Neg -> "\\neg"
  | Land -> "\\land"
  | Lor -> "\\lor"
  | Rightarrow -> "\\Rightarrow"
  | Leftrightarrow -> "\\Leftrightarrow"
  | Cdot -> "\\cdot"
  | Bmod -> "\\bmod"
  | Turnstile -> "\\vdash"
  | Tilesturn -> "\\dashv"
  | To -> "\\to"
  | Longrightarrow -> "\\Longrightarrow"
  | Hookrightarrow -> "\\hookrightarrow"
  | Mapsto -> "\\mapsto"
  | Sim -> "\\sim"
  | Setminus -> "\\setminus"
  | EmptySet -> "\\varnothing"
  | LeftParen -> "("
  | RightParen -> ")"
  | LeftBracket -> "["
  | RightBracket -> "]"
  | LeftBrace -> "\\{"
  | RightBrace -> "\\}"

and render_fixed (buffer : Buffer.t) (symbol : symbol) : unit =
  let text = string_of_symbol symbol in
  add_string buffer text

(* Spaces

   Space -> " " *)

and render_space (buffer : Buffer.t) : unit = add_string buffer " "

(* Thin spaces

   ThinSpace -> \, *)

and render_thin_space (buffer : Buffer.t) : unit = add_string buffer "\\,"

(* Quad spaces

   Quad -> \quad *)

and render_quad (buffer : Buffer.t) : unit = add_string buffer "\\quad"

(* Concatenated documents

   Concat [x; Space; y] -> x y *)

and render_concat (buffer : Buffer.t) (docs : t list) : unit =
  render_docs buffer docs

(* Groups

   Group x -> {x} *)

and render_group (buffer : Buffer.t) (doc : t) : unit =
  render_enclosed buffer "{" "}" doc

(* Binary classifications

   Mathbin x -> \mathbin{x} *)

and render_mathbin (buffer : Buffer.t) (doc : t) : unit =
  render_enclosed buffer "\\mathbin{" "}" doc

(* Relation classifications

   Mathrel x -> \mathrel{x} *)

and render_mathrel (buffer : Buffer.t) (doc : t) : unit =
  render_enclosed buffer "\\mathrel{" "}" doc

(* Display style

   Displaystyle x -> {\displaystyle x} *)

and render_displaystyle (buffer : Buffer.t) (doc : t) : unit =
  render_enclosed buffer "{\\displaystyle " "}" doc

(* Delimited documents

   Delimited (Paren, x) -> \left(x\right) *)

and strings_of_delimiter (delimiter : delimiter) : string * string =
  match delimiter with
  | Paren -> ("(", ")")
  | Bracket -> ("[", "]")
  | Brace -> ("\\{", "\\}")
  | Angle -> ("\\langle", "\\rangle")
  | Bar -> ("|", "|")

and render_delimited (buffer : Buffer.t) (delimiter : delimiter) (doc : t) :
    unit =
  let s_left, s_right = strings_of_delimiter delimiter in
  add_string buffer "\\left";
  add_string buffer s_left;
  render_doc buffer doc;
  add_string buffer "\\right";
  add_string buffer s_right

(* Subscripts

   Subscript (x, i) -> {x}_{i} *)

and render_subscript (buffer : Buffer.t) (base : t) (sub : t) : unit =
  add_string buffer "{";
  render_doc buffer base;
  add_string buffer "}_{";
  render_doc buffer sub;
  add_string buffer "}"

(* Superscripts

   Superscript (x, n) -> {x}^{n} *)

and render_superscript (buffer : Buffer.t) (base : t) (sup : t) : unit =
  add_string buffer "{";
  render_doc buffer base;
  add_string buffer "}^{";
  render_doc buffer sup;
  add_string buffer "}"

(* Paired scripts

   Subsup (x, i, n) -> {x}_{i}^{n} *)

and render_subsup (buffer : Buffer.t) (base : t) (sub : t) (sup : t) : unit =
  add_string buffer "{";
  render_doc buffer base;
  add_string buffer "}_{";
  render_doc buffer sub;
  add_string buffer "}^{";
  render_doc buffer sup;
  add_string buffer "}"

(* Fractions

   Fraction (p, q) -> \frac{p}{q} *)

and render_fraction (buffer : Buffer.t) (numerator : t) (denominator : t) : unit
    =
  add_string buffer "\\frac{";
  render_doc buffer numerator;
  add_string buffer "}{";
  render_doc buffer denominator;
  add_string buffer "}"

(* Links

   Link (Target "f", x) -> \href{#f}{x} *)

and render_link (buffer : Buffer.t) (target : target) (doc : t) : unit =
  let (Target s_target) = target in
  add_string buffer "\\href{#";
  add_string buffer s_target;
  add_string buffer "}{";
  render_doc buffer doc;
  add_string buffer "}"

(* Soft breaks

   SoftBreak SoftSpace -> " " *)

and render_soft_break (buffer : Buffer.t) (soft : soft) : unit =
  match soft with SoftCut -> () | SoftSpace -> render_space buffer

(* Layout groups

   LayoutGroup x -> x *)

and render_layout_group (buffer : Buffer.t) (doc : t) : unit =
  render_doc buffer doc

(* Nested documents

   Nest (2, x) -> x *)

and render_nest (buffer : Buffer.t) (_indent : int) (doc : t) : unit =
  render_doc buffer doc

(* Fill documents

   Fill (_, comma, [x; y]) -> x, y *)

and render_fill (buffer : Buffer.t) (_indent : int) (separator : t)
    (docs : t list) : unit =
  let doc = Doc.concat_intersperse separator docs in
  render_doc buffer doc

(* TeX arrays

   [Right; Left], [[x; y]] -> \begin{array}{rl}x & y\end{array} *)

and string_of_alignment (alignment : alignment) : string =
  match alignment with Left -> "l" | Center -> "c" | Right -> "r"

and render_cells (buffer : Buffer.t) (docs : t list) : unit =
  match docs with
  | [] -> ()
  | [ doc ] -> render_doc buffer doc
  | doc :: docs ->
      render_doc buffer doc;
      add_string buffer " & ";
      render_cells buffer docs

and render_array_rows (buffer : Buffer.t) (rows : t list list) : unit =
  match rows with
  | [] -> ()
  | [ row ] -> render_cells buffer row
  | row :: rows ->
      render_cells buffer row;
      add_string buffer " \\\\\n";
      render_array_rows buffer rows

and render_array (buffer : Buffer.t) (alignments : alignment list)
    (rows : t list list) : unit =
  let alignment_strings = List.map string_of_alignment alignments in
  let alignment_string = String.concat "" alignment_strings in
  add_string buffer "\\begin{array}{";
  add_string buffer alignment_string;
  add_string buffer "}\n";
  render_array_rows buffer rows;
  add_string buffer "\n\\end{array}"

(* Aligned documents

   Aligned [[x; equal; y]] -> \begin{aligned}x & = & y\end{aligned} *)

and render_aligned_rows (buffer : Buffer.t) (rows : t list list) : unit =
  match rows with
  | [] -> ()
  | [ row ] -> render_cells buffer row
  | row :: rows ->
      render_cells buffer row;
      add_string buffer " \\\\\n";
      render_aligned_rows buffer rows

and render_aligned (buffer : Buffer.t) (rows : t list list) : unit =
  add_string buffer "\\begin{aligned}\n";
  render_aligned_rows buffer rows;
  add_string buffer "\n\\end{aligned}"

(* Grids

   Grid ([Right; Center; Left],
     [Cells [lhs; equal; rhs]; Spanning condition]) ->

     lhs = rhs
     condition

   Cells supplies one document per column.
   Spanning supplies one document for the whole row.
   RowGap adds vertical space between adjacent rows.

   A TeX array cannot make one row ignore its columns.
   A mixed grid is therefore printed twice: a visible copy provides the content,
   and an invisible \hphantom copy gives the grid the width of its widest row.
   Links are removed from the invisible copy so that each link is emitted only once. *)

and docs_of_first_column (rows : t list list) : t list =
  match rows with
  | [] -> []
  | [] :: rows -> docs_of_first_column rows
  | (doc :: _) :: rows -> Link.strip_links doc :: docs_of_first_column rows

and render_grid_empty_cells (buffer : Buffer.t) (columns : int) : unit =
  match columns with
  | columns when columns <= 1 -> ()
  | 2 -> add_string buffer " &"
  | columns ->
      add_string buffer " & ";
      render_grid_empty_cells buffer (columns - 1)

and render_grid_spanning_row (buffer : Buffer.t) (alignments : alignment list)
    (docs_column_first : t list) (doc : t) : unit =
  let columns = List.length alignments in
  match docs_column_first with
  | [] ->
      render_doc buffer doc;
      render_grid_empty_cells buffer columns
  | docs_column_first ->
      let rows_column_first = List.map (fun doc -> [ doc ]) docs_column_first in
      add_string buffer "\\mathrlap{\\displaystyle ";
      render_doc buffer doc;
      add_string buffer "}\\smash{\\hphantom{";
      render_array buffer [ Right ] rows_column_first;
      add_string buffer "}}";
      render_grid_empty_cells buffer columns

and render_grid_row (buffer : Buffer.t) (alignments : alignment list)
    (docs_column_first : t list) (row : row) : unit =
  match row with
  | Cells docs -> render_cells buffer docs
  | Spanning doc ->
      render_grid_spanning_row buffer alignments docs_column_first doc
  | RowGap -> Error.error_no_region "malformed grid row gap"

and render_grid_rows (buffer : Buffer.t) (alignments : alignment list)
    (docs_column_first : t list) (rows : row list) : unit =
  match rows with
  | [] -> ()
  | RowGap :: _ -> Error.error_no_region "malformed grid row gap"
  | [ row ] -> render_grid_row buffer alignments docs_column_first row
  | row :: RowGap :: rows ->
      render_grid_row buffer alignments docs_column_first row;
      add_string buffer " \\\\[1ex]\n";
      render_grid_rows buffer alignments docs_column_first rows
  | row :: rows ->
      render_grid_row buffer alignments docs_column_first row;
      add_string buffer " \\\\\n";
      render_grid_rows buffer alignments docs_column_first rows

and render_grid_array (buffer : Buffer.t) (alignments : alignment list)
    (rows_cell : t list list) (rows : row list) : unit =
  let docs_column_first = docs_of_first_column rows_cell in
  let alignment_strings = List.map string_of_alignment alignments in
  let alignment_string = String.concat "" alignment_strings in
  add_string buffer "\\begin{array}{";
  add_string buffer alignment_string;
  add_string buffer "}\n";
  render_grid_rows buffer alignments docs_column_first rows;
  add_string buffer "\n\\end{array}"

and render_grid_width_spans (buffer : Buffer.t) (docs : t list) : unit =
  match docs with
  | [] -> ()
  | doc :: docs ->
      add_string buffer " \\\\\n";
      render_doc buffer (Link.strip_links doc);
      render_grid_width_spans buffer docs

and render_grid_width_envelope (buffer : Buffer.t) (alignments : alignment list)
    (rows_cell : t list list) (docs_spanning : t list) : unit =
  let rows_cell = List.map (List.map Link.strip_links) rows_cell in
  add_string buffer "\\begin{array}{l}\n";
  render_array buffer alignments rows_cell;
  render_grid_width_spans buffer docs_spanning;
  add_string buffer "\n\\end{array}"

and render_grid_mixed (buffer : Buffer.t) (alignments : alignment list)
    (rows : row list) (rows_cell : t list list) (docs_spanning : t list) : unit
    =
  add_string buffer "\\mathrlap{\\displaystyle ";
  render_grid_array buffer alignments rows_cell rows;
  add_string buffer "}\\smash{\\hphantom{";
  render_grid_width_envelope buffer alignments rows_cell docs_spanning;
  add_string buffer "}}"

and render_grid (buffer : Buffer.t) (alignments : alignment list)
    (rows : row list) : unit =
  let rows_cell, docs_spanning =
    List.fold_right
      (fun row (rows_cell, docs_spanning) ->
        match row with
        | Cells docs -> (docs :: rows_cell, docs_spanning)
        | Spanning doc -> (rows_cell, doc :: docs_spanning)
        | RowGap -> (rows_cell, docs_spanning))
      rows ([], [])
  in
  match (rows_cell, docs_spanning) with
  | [], _ -> render_grid_array buffer [ Left ] [] rows
  | rows_cell, [] -> render_grid_array buffer alignments rows_cell rows
  | rows_cell, docs_spanning ->
      render_grid_mixed buffer alignments rows rows_cell docs_spanning

(* Stacked documents

   Stacked [p; q] -> \begin{aligned}& p\\& q\end{aligned} *)

and render_stacked_docs (buffer : Buffer.t) (docs : t list) : unit =
  match docs with
  | [] -> ()
  | [ doc ] ->
      add_string buffer "& ";
      render_doc buffer doc
  | doc :: docs ->
      add_string buffer "& ";
      render_doc buffer doc;
      add_string buffer " \\\\\n";
      render_stacked_docs buffer docs

and render_stacked (buffer : Buffer.t) (docs : t list) : unit =
  add_string buffer "\\begin{aligned}\n";
  render_stacked_docs buffer docs;
  add_string buffer "\n\\end{aligned}"

(* Left-stacked documents

   LeftStack [x; y] -> \begin{array}{l}x\\y\end{array} *)

and render_left_stack (buffer : Buffer.t) (docs : t list) : unit =
  let rows = List.map (fun doc -> [ doc ]) docs in
  render_array buffer [ Left ] rows

(* Numbered documents

   Numbered [p; LeftStack [q; r]] ->

   (1)  p
   (2)  q
        r

   Each document receives one number.
   A LeftStack expands into multiple array rows,
   but only its first row displays that number;
   continuation rows leave the number column empty. *)

and render_number (buffer : Buffer.t) (number : int option) : unit =
  match number with
  | None -> ()
  | Some number ->
      add_string buffer "{\\scriptstyle\\mathtt{";
      add_string buffer "(";
      let text = string_of_int number in
      add_string buffer text;
      add_string buffer ")}}"

and render_numbered_row (buffer : Buffer.t) (number : int option) (doc : t) :
    unit =
  render_number buffer number;
  add_string buffer " & ";
  render_doc buffer doc

and render_numbered_continuations (buffer : Buffer.t) (docs : t list) : unit =
  match docs with
  | [] -> ()
  | doc :: docs ->
      add_string buffer " \\\\\n";
      render_numbered_row buffer None doc;
      render_numbered_continuations buffer docs

and render_numbered_doc (buffer : Buffer.t) (number : int) (doc : t) : unit =
  match doc with
  | LeftStack [] -> ()
  | LeftStack (doc :: docs) ->
      render_numbered_row buffer (Some number) doc;
      render_numbered_continuations buffer docs
  | doc -> render_numbered_row buffer (Some number) doc

and render_numbered_docs (buffer : Buffer.t) (number : int) (docs : t list) :
    unit =
  match docs with
  | [] -> ()
  | LeftStack [] :: docs -> render_numbered_docs buffer (number + 1) docs
  | [ doc ] -> render_numbered_doc buffer number doc
  | doc :: docs ->
      render_numbered_doc buffer number doc;
      add_string buffer " \\\\\n";
      render_numbered_docs buffer (number + 1) docs

and render_numbered (buffer : Buffer.t) (docs : t list) : unit =
  add_string buffer "\\begin{array}{r@{\\quad}l}\n";
  render_numbered_docs buffer 1 docs;
  add_string buffer "\n\\end{array}"

(* Gathered documents

   Gathered [Line x; Gap; Line y] -> x\\[1ex]y *)

and render_gathered_blocks (buffer : Buffer.t) (blocks : block list) : unit =
  match blocks with
  | [] -> ()
  | [ Line doc ] -> render_doc buffer doc
  | Line doc :: Gap :: blocks ->
      render_doc buffer doc;
      add_string buffer " \\\\[1ex]\n";
      render_gathered_blocks buffer blocks
  | Line doc :: blocks ->
      render_doc buffer doc;
      add_string buffer " \\\\\n";
      render_gathered_blocks buffer blocks
  | Gap :: _ -> Error.error_no_region "malformed gathered document"

and render_gathered (buffer : Buffer.t) (blocks : block list) : unit =
  add_string buffer "\\begin{gathered}\n";
  render_gathered_blocks buffer blocks;
  add_string buffer "\n\\end{gathered}"

(* Entry point *)

let to_string (doc : t) : string =
  let buffer = Buffer.create 256 in
  render_doc buffer doc;
  Buffer.contents buffer
