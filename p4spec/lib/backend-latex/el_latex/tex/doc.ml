(* TeX document model *)

(* Font and escaping context for styled text

   Styled (Mathsf, "TC_0") -> \mathsf{TC\_0}
   Styled (Texttt, "rule_1") -> \texttt{rule\_1} *)

type style = Mathit | Mathrm | Mathsf | Mathbb | Mathtt | Text | Texttt

(* Balanced delimiter pairs

   Delimited (Paren, Styled (Mathsf, "x"))
   -> \left(\mathsf{x}\right)

   Delimited (Angle, Styled (Mathbb, "N"))
   -> \left\langle\mathbb{N}\right\rangle *)

type delimiter = Paren | Bracket | Brace | Angle | Bar

(* Column alignment for Grid

   Grid ([Right; Center; Left], [Cells [x; equal; y]])
   -> \begin{array}{rcl}x & = & y\end{array} *)

type alignment = Left | Center | Right

(* Break opportunity and its flat representation

   SoftBreak SoftCut -> ""
   SoftBreak SoftSpace -> " "

   LayoutGroup (Concat [x; SoftBreak SoftSpace; y]) stays [x y] when it fits
   and resolves to two lines when it exceeds the layout width *)

type soft = SoftCut | SoftSpace

(* Fixed mathematical vocabulary

   Fixed Plus -> +
   Fixed Turnstile -> \vdash
   Fixed Rightarrow -> \Rightarrow

   Unlike Styled text, these atoms serialize to predefined TeX commands *)

type symbol =
  | Equal
  | NotEqual
  | Less
  | Greater
  | LessEqual
  | GreaterEqual
  | Plus
  | Minus
  | Question
  | Ast
  | Slash
  | Comma
  | Semicolon
  | Colon
  | DoubleColon
  | Cat
  | Production
  | VerticalBar
  | Dot
  | Dot2
  | Ellipsis
  | Epsilon
  | In
  | Neg
  | Land
  | Lor
  | Rightarrow
  | Leftrightarrow
  | Cdot
  | Bmod
  | Turnstile
  | Tilesturn
  | To
  | Longrightarrow
  | Hookrightarrow
  | Mapsto
  | Sim
  | Setminus
  | EmptySet
  | LeftParen
  | RightParen
  | LeftBracket
  | RightBracket
  | LeftBrace
  | RightBrace

(* Link target in the generated HTML document

   Target "Eval" -> #Eval *)

type target = Target of string

[@@@ocamlformat "disable"]

(* Semantic TeX documents retain mathematical and layout intent
   until Width, Layout, and Serialize interpret them *)

type t =
  (* Atomic documents *)
  | Empty                                      (* "" *)
  | Styled of style * string                   (* Styled (Mathsf, "x") -> \mathsf{x} *)
  | Badge of string                            (* Badge "R" -> boxed rule label R *)
  | Decimal of Bigint.t                        (* Decimal 42 -> 42 *)
  | Hexadecimal of Bigint.t                    (* Hexadecimal 42 -> \mathtt{0x2a} *)
  | Fixed of symbol                            (* Fixed Turnstile -> \vdash *)
  | Space                                      (* " " *)
  | ThinSpace                                  (* \, *)
  | Quad                                       (* \quad *)
  (* Sequential composition and TeX classification *)
  | Concat of t list                           (* Concat [x; Space; y] -> x y *)
  | Group of t                                 (* Group x -> {x} *)
  | Mathbin of t                               (* Mathbin x -> \mathbin{x} *)
  | Mathrel of t                               (* Mathrel x -> \mathrel{x} *)
  | Displaystyle of t                          (* Displaystyle x -> {\displaystyle x} *)
  (* Delimiters and mathematical attachments *)
  | Delimited of delimiter * t                 (* Delimited (Paren, x) -> \left(x\right) *)
  | Subscript of t * t                         (* Subscript (x, i) -> {x}_{i} *)
  | Superscript of t * t                       (* Superscript (x, n) -> {x}^{n} *)
  | Subsup of t * t * t                        (* Subsup (x, i, n) -> {x}_{i}^{n} *)
  | Fraction of t * t                          (* Fraction (p, q) -> \frac{p}{q} *)
  (* Navigation *)
  | Link of target * t                         (* Link (Target "f", x) -> \href{#f}{x} *)
  (* Width-sensitive layout *)
  | SoftBreak of soft                          (* SoftBreak SoftSpace -> space or line break *)
  | LayoutGroup of t                           (* LayoutGroup x -> flat or broken x *)
  | Nest of int * t                            (* Nest (2, x) -> indent continuations by 2 *)
  | Fill of int * t * t list                   (* Fill (2, comma, xs) -> packed xs *)
  (* Multi-row layout *)
  | Aligned of t list list                     (* Aligned [[x; equal; y]] -> x & = & y *)
  | Grid of alignment list * row list          (* Grid ([Right; Left], rows) -> array{rl} *)
  | Stacked of t list                          (* Stacked [p; q] -> vertically centered p, q *)
  | LeftStack of t list                        (* LeftStack [p; q] -> left-aligned p, q *)
  | Numbered of t list                         (* Numbered [p; q] -> premises (1), (2) *)
  | Gathered of block list                     (* Gathered [Line x; Gap; Line y] -> x \\[1ex] y *)

(* Grid rows *)

and row =
  | Cells of t list                            (* Cells [x; equal; y] -> x & = & y *)
  | Spanning of t                              (* Spanning x -> full-width row x *)
  | RowGap                                     (* RowGap -> \\[1ex] *)

(* Gathered blocks *)

and block =
  | Line of t                                  (* Line x -> row x *)
  | Gap                                        (* Gap -> \\[1ex] *)
[@@@ocamlformat "enable"]

(* Emptiness checker *)

let rec is_empty (doc : t) : bool =
  match doc with
  | Empty -> true
  | Concat docs -> are_empty docs
  | Styled _ | Badge _ | Decimal _ | Hexadecimal _ | Fixed _ | Space | ThinSpace
  | Quad | Group _ | Mathbin _ | Mathrel _ | Delimited _ | Subscript _
  | Superscript _ | Subsup _ | Fraction _ | Aligned _ | Gathered _ | Link _
  | SoftBreak _ | Grid _ ->
      false
  | Displaystyle doc | LayoutGroup doc | Nest (_, doc) -> is_empty doc
  | Stacked docs | LeftStack docs | Numbered docs -> are_empty docs
  | Fill (_, _, docs) -> are_empty docs

and are_empty (docs : t list) : bool = List.for_all is_empty docs

(* Document constructors *)

(* Atomic documents *)

let empty : t = Empty
let styled_mathit (text : string) : t = Styled (Mathit, text)
let styled_mathrm (text : string) : t = Styled (Mathrm, text)
let styled_mathsf (text : string) : t = Styled (Mathsf, text)
let styled_mathbb (text : string) : t = Styled (Mathbb, text)
let styled_mathtt (text : string) : t = Styled (Mathtt, text)
let styled_text (text : string) : t = Styled (Text, text)
let styled_texttt (text : string) : t = Styled (Texttt, text)

let badge (text : string) : t =
  if String.equal text "" then Empty else Badge text

let decimal (n : Bigint.t) : t = Decimal n
let hexadecimal (n : Bigint.t) : t = Hexadecimal n
let fixed (symbol : symbol) : t = Fixed symbol
let space : t = Space
let thin_space : t = ThinSpace
let quad : t = Quad

(* Sequential composition *)

let concat (docs : t list) : t =
  let rec flatten (acc : t list) (docs : t list) : t list =
    match docs with
    | [] -> List.rev acc
    | Empty :: docs -> flatten acc docs
    | Concat nested :: docs -> flatten acc (nested @ docs)
    | doc :: docs -> flatten (doc :: acc) docs
  in
  match flatten [] docs with
  | [] -> Empty
  | [ doc ] -> doc
  | docs -> Concat docs

let concat_intersperse (separator : t) (docs : t list) : t =
  let docs = List.filter (Fun.negate is_empty) docs in
  let rec separate (docs : t list) : t list =
    match docs with
    | [] -> []
    | [ doc ] -> [ doc ]
    | doc :: docs -> doc :: separator :: separate docs
  in
  concat (separate docs)

let concat_spaced (docs : t list) : t = concat_intersperse Space docs

let concat_comma_separated (docs : t list) : t =
  let separator = concat [ fixed Comma; Space ] in
  concat_intersperse separator docs

let concat_juxtaposed (docs : t list) : t = concat_intersperse ThinSpace docs

(* TeX classification *)

let group (doc : t) : t = Group doc
let mathbin (doc : t) : t = Mathbin doc
let mathrel (doc : t) : t = Mathrel doc

let displaystyle (doc : t) : t =
  if is_empty doc then Empty else Displaystyle doc

let delimited (delimiter : delimiter) (doc : t) : t = Delimited (delimiter, doc)
let subscript (base : t) (sub : t) : t = Subscript (base, sub)
let superscript (base : t) (sup : t) : t = Superscript (base, sup)
let subsup (base : t) (sub : t) (sup : t) : t = Subsup (base, sub, sup)

let fraction (numerator : t) (denominator : t) : t =
  Fraction (numerator, denominator)

let link (target : target) (doc : t) : t =
  if is_empty doc then Empty else Link (target, doc)

(* Width-sensitive layout *)

let soft_cut : t = SoftBreak SoftCut
let soft_space : t = SoftBreak SoftSpace
let layout_group (doc : t) : t = if is_empty doc then Empty else LayoutGroup doc

let layout_group_soft_comma_separated (docs : t list) : t =
  let separator = concat [ fixed Comma; soft_space ] in
  layout_group (concat_intersperse separator docs)

let nest (indent : int) (doc : t) : t =
  Error.check_no_region (indent >= 0) "Doc.nest: indent must be non-negative";
  if indent = 0 || is_empty doc then doc else Nest (indent, doc)

let fill ?(indent : int = 0) ~(separator : t) (docs : t list) : t =
  Error.check_no_region (indent >= 0) "Doc.fill: indent must be non-negative";
  match List.filter (Fun.negate is_empty) docs with
  | [] -> Empty
  | [ doc ] -> doc
  | docs -> Fill (indent, separator, docs)

(* Multi-row layout *)

let aligned (rows : t list list) : t = Aligned rows

let grid (alignments : alignment list) (rows : row list) : t =
  match (alignments, rows) with
  | [], [] -> Empty
  | [], _ -> Error.error_no_region "Doc.grid: rows require columns"
  | _, rows ->
      List.iter
        (fun row ->
          match row with
          | Cells cells when List.length cells <> List.length alignments ->
              Error.error_no_region
                "Doc.grid: cell count does not match columns"
          | Cells _ | Spanning _ | RowGap -> ())
        rows;
      let rows =
        List.filter
          (fun row ->
            match row with
            | Cells cells -> List.exists (Fun.negate is_empty) cells
            | Spanning doc -> not (is_empty doc)
            | RowGap -> true)
          rows
      in
      if rows = [] then Empty else Grid (alignments, rows)

let stacked (docs : t list) : t =
  match List.filter (Fun.negate is_empty) docs with
  | [] -> Empty
  | docs -> Stacked docs

let left_stack (docs : t list) : t =
  match List.filter (Fun.negate is_empty) docs with
  | [] -> Empty
  | [ doc ] -> doc
  | docs -> LeftStack docs

let numbered (docs : t list) : t =
  match List.filter (Fun.negate is_empty) docs with
  | [] -> Empty
  | docs -> Numbered docs

(* Gathered block normalization

   [Gap; Line x; Gap; Gap; Line y; Gap] -> [Line x; Gap; Line y] *)

let rec normalize_gathered_blocks (gap_pending : bool) (blocks_rev : block list)
    (blocks : block list) : block list =
  match blocks with
  | [] -> List.rev blocks_rev
  | Gap :: blocks ->
      let gap_pending = blocks_rev <> [] in
      normalize_gathered_blocks gap_pending blocks_rev blocks
  | Line doc :: blocks when is_empty doc ->
      normalize_gathered_blocks gap_pending blocks_rev blocks
  | Line doc :: blocks ->
      let blocks_rev =
        if gap_pending then Line doc :: Gap :: blocks_rev
        else Line doc :: blocks_rev
      in
      normalize_gathered_blocks false blocks_rev blocks

let gathered (blocks : block list) : t =
  Gathered (normalize_gathered_blocks false [] blocks)

(* Row constructors *)

let cells (docs : t list) : row = Cells docs
let spanning (doc : t) : row = Spanning doc
let row_gap : row = RowGap

(* Block constructors *)

let line (doc : t) : block = Line doc
let gap : block = Gap
