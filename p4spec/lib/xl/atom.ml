[@@@ocamlformat "disable"]

type t =
  | Atom of string  (* atomid *)
  | Infinity        (* infinity *)
  | Bot             (* `_|_` *)
  | Top             (* `^|^` *)
  | Dot             (* `.` *)
  | Dot2            (* `..` *)
  | Dot3            (* `...` *)
  | Semicolon       (* `;` *)
  | Backslash       (* `\` *)
  | Mem             (* `<-` *)
  | Arrow           (* `->` *)
  | Arrow2          (* ``=>` *)
  | ArrowSub        (* `->_` *)
  | Arrow2Sub       (* ``=>_` *)
  | Colon           (* `:` *)
  | Sub             (* `<:` *)
  | Sup             (* `:>` *)
  | Assign          (* `:=` *)
  | Equal           (* ``=` *)
  | NotEqual        (* ``=/=` *)
  | Less            (* ``<` *)
  | Greater         (* ``>` *)
  | LessEqual       (* ``<=` *)
  | GreaterEqual    (* ``>=` *)
  | Equiv           (* `==` *)
  | Approx          (* `~~` *)
  | SqArrow         (* `~>` *)
  | SqArrowStar     (* `~>*` *)
  | Prec            (* `<<` *)
  | Succ            (* `>>` *)
  | Turnstile       (* `|-` *)
  | Tilesturn       (* `-|` *)
  | Quest           (* ``?` *)
  | Plus            (* ``+` *)
  | Star            (* ``*` *)
  | Comma           (* ``,` *)
  | Cat             (* ``++` *)
  | Bar             (* ``|` *)
  | BigAnd          (* `(/\)` *)
  | BigOr           (* `(\/)` *)
  | BigAdd          (* `(+)` *)
  | BigMul          (* `( * )` *)
  | BigCat          (* `(++)` *)
  | LParen
  | RParen          (* ``(` `)` *)
  | LBrack
  | RBrack          (* ``[` `]` *)
  | LBrace
  | RBrace          (* ``{` `}` *)
[@@deriving yojson]
[@@@ocamlformat "enable"]

let compare atom_a atom_b = compare atom_a atom_b
let eq atom_a atom_b = compare atom_a atom_b = 0

let string_of_atom = function
  | Atom "_" -> "_"
  | Atom id -> id
  | Infinity -> "infinity"
  | Bot -> "_|_"
  | Top -> "^|^"
  | Dot -> "."
  | Dot2 -> ".."
  | Dot3 -> "..."
  | Semicolon -> ";"
  | Backslash -> "\\"
  | Mem -> "<-"
  | Arrow -> "->"
  | Arrow2 -> "=>"
  | ArrowSub -> "->_"
  | Arrow2Sub -> "=>_"
  | Colon -> ":"
  | Sub -> "<:"
  | Sup -> ":>"
  | Assign -> ":="
  | Equal -> "="
  | NotEqual -> "=/="
  | Less -> "<"
  | Greater -> ">"
  | LessEqual -> "<="
  | GreaterEqual -> ">="
  | Equiv -> "=="
  | Approx -> "~~"
  | SqArrow -> "~>"
  | SqArrowStar -> "~>*"
  | Prec -> "<<"
  | Succ -> ">>"
  | Tilesturn -> "-|"
  | Turnstile -> "|-"
  | Quest -> "?"
  | Plus -> "+"
  | Star -> "*"
  | Comma -> ","
  | Cat -> "++"
  | Bar -> "|"
  | BigAnd -> "(/\\)"
  | BigOr -> "(\\/)"
  | BigAdd -> "(+)"
  | BigMul -> "(*)"
  | BigCat -> "(++)"
  | LParen -> "("
  | LBrack -> "["
  | LBrace -> "{"
  | RParen -> ")"
  | RBrack -> "]"
  | RBrace -> "}"

(* The following mostly correspond to Latex names except where noted;
   where noted, a respective macro is expected to be defined *)

let name = function
  | Atom s -> s
  | Infinity -> "infty"
  | Bot -> "bot"
  | Top -> "top"
  | Dot -> "dot" (* Latex: . *)
  | Dot2 -> "dotdot" (* Latex: .. *)
  | Dot3 -> "dots" (* Latex: \ldots *)
  | Semicolon -> "semicolon" (* Latex: ; *)
  | Backslash -> "setminus"
  | Mem -> "in"
  | Arrow -> "arrow" (* Latex: \rightarrow *)
  | Arrow2 -> "darrow" (* Latex: \Rightarrow *)
  | ArrowSub -> "arrow_" (* Latex: \rightarrow with subscript *)
  | Arrow2Sub -> "darrow_" (* Latex: \Rightarrow with subscript *)
  | Colon -> "colon" (* Latex: : *)
  | Sub -> "sub" (* Latex: \leq or <: *)
  | Sup -> "sup" (* Latex: \geq or :> *)
  | Assign -> "assign" (* Latex: := *)
  | Equal -> "eq"
  | NotEqual -> "neq" (* Latex: \neq *)
  | Less -> "lt" (* Latex: < *)
  | Greater -> "gt" (* Latex: > *)
  | LessEqual -> "leq" (* Latex: \leq *)
  | GreaterEqual -> "geq" (* Latex: \geq *)
  | Equiv -> "equiv"
  | Approx -> "approx"
  | SqArrow -> "sqarrow" (* Latex: \hookrightarrow *)
  | SqArrowStar -> "sqarrowstar" (* Latex: \hookrightarrow^\ast *)
  | Prec -> "prec"
  | Succ -> "succ"
  | Tilesturn -> "dashv"
  | Turnstile -> "vdash"
  | Quest -> "quest" (* Latex: ? *)
  | Plus -> "plus" (* Latex: + *)
  | Star -> "ast"
  | Comma -> "comma" (* Latex: , *)
  | Cat -> "cat" (* Latex: \oplus *)
  | Bar -> "bar" (* Latex: | *)
  | BigAnd -> "bigand" (* Latex: \bigwedge *)
  | BigOr -> "bigor" (* Latex: \bigvee *)
  | BigAdd -> "bigadd" (* Latex: \Sigma *)
  | BigMul -> "bigmul" (* Latex: \Pi *)
  | BigCat -> "bigcat" (* Latex: \bigoplus *)
  | LParen -> "lparen" (* Latex: brackets... *)
  | LBrack -> "lbrack"
  | LBrace -> "lbrace"
  | RParen -> "rparen"
  | RBrack -> "rbrack"
  | RBrace -> "rbrace"
