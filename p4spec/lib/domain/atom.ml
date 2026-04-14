[@@@ocamlformat "disable"]

type t =
  | Atom of string       (* atomid *)
  | SilentAtom of string (* `atomid *)
  | Sub                  (* `<:` *)
  | Sup                  (* `:>` *)
  | Turnstile            (* `|-` *)
  | Tilesturn            (* `-|` *)
  | Tick                 (* ```` *)
  | DoubleQuote          (* ``''` *)
  | Underscore           (* ``_` *)
  | Arrow                (* `->` *)
  | ArrowSub             (* `->_` *)
  | DoubleArrow          (* ``=>` *)
  | DoubleArrowSub       (* ``=>_` *)
  | DoubleArrowLong      (* ``==>` *)
  | SqArrow              (* `~>` *)
  | SqArrowStar          (* `~>*` *)
  | Dot                  (* ``.` *)
  | Dot2                 (* ``..` *)
  | Dot3                 (* ``...` *)
  | Comma                (* ``,` *)
  | Semicolon            (* ``;` *)
  | Colon                (* `:` *)
  | ColonEq              (* `:=` *)
  | Hash                 (* ``#` *)
  | Dollar               (* ``$` *)
  | At                   (* ``@` *)
  | Quest                (* ``?` *)
  | Bang                 (* ``!` *)
  | BangEq               (* ``!=` *)
  | Tilde                (* ``~` *)
  | Tilde2               (* `~~` *)
  | LAngle               (* ``<` *)
  | LAngle2              (* `<<` *)
  | LAngleEq             (* ``<=` *)
  | LAngle2Eq            (* `<<=` *)
  | RAngle               (* ``>` *)
  | RAngle2              (* `>>` *)
  | RAngleEq             (* ``>=` *)
  | RAngle2Eq            (* `>>=` *)
  | LParen               (* ``(` *)
  | RParen               (* ``)` *)
  | LBrack               (* ``[` *)
  | RBrack               (* ``]` *)
  | LBrace               (* ``{` *)
  | LBraceHashRBrace     (* `{#}` *)
  | RBrace               (* ``}` *)
  | Plus                 (* ``+` *)
  | Plus2                (* ``++` *)
  | PlusEq               (* ``+=` *)
  | PlusColon            (* ``+:` *)
  | Minus                (* ``-` *)
  | MinusEq              (* ``-=` *)
  | Star                 (* ``*` *)
  | StarEq               (* ``*=` *)
  | Slash                (* ``/` *)
  | SlashEq              (* ``/=` *)
  | Backslash            (* ``\` *)
  | Percent              (* ``%` *)
  | PercentEq            (* ``%=` *)
  | Eq                   (* ``=` *)
  | Eq2                  (* `==` *)
  | Amp                  (* ``&` *)
  | Amp2                 (* ``&&` *)
  | Amp3                 (* ``&&&` *)
  | AmpEq                (* ``&=` *)
  | Up                   (* ``^` *)
  | UpEq                 (* ``^=` *)
  | Bar                  (* ``|` *)
  | Bar2                 (* ``||` *)
  | BarEq                (* ``|=` *)
  | SPlus                (* ``|+|` *)
  | SPlusEq              (* ``|+|=` *)
  | SMinus               (* ``|-|` *)
  | SMinusEq             (* ``|-|=` *)
[@@deriving yojson]
[@@@ocamlformat "enable"]

let compare atom_a atom_b = compare atom_a atom_b
let eq atom_a atom_b = atom_a = atom_b

let string_of_atom = function
  | Atom id -> id
  | SilentAtom id -> "`" ^ id
  | Sub -> "<:"
  | Sup -> ":>"
  | Turnstile -> "|-"
  | Tilesturn -> "-|"
  | Tick -> "`"
  | DoubleQuote -> "\""
  | Underscore -> "_"
  | Arrow -> "->"
  | ArrowSub -> "->_"
  | DoubleArrow -> "=>"
  | DoubleArrowSub -> "=>_"
  | DoubleArrowLong -> "==>"
  | SqArrow -> "~>"
  | SqArrowStar -> "~>*"
  | Dot -> "."
  | Dot2 -> ".."
  | Dot3 -> "..."
  | Comma -> ","
  | Semicolon -> ";"
  | Colon -> ":"
  | ColonEq -> ":="
  | Hash -> "#"
  | Dollar -> "$"
  | At -> "@"
  | Quest -> "?"
  | Bang -> "!"
  | BangEq -> "!="
  | Tilde -> "~"
  | Tilde2 -> "~~"
  | LAngle -> "<"
  | LAngle2 -> "<<"
  | LAngleEq -> "<="
  | LAngle2Eq -> "<<="
  | RAngle -> ">"
  | RAngle2 -> ">>"
  | RAngleEq -> ">="
  | RAngle2Eq -> ">>="
  | LParen -> "("
  | RParen -> ")"
  | LBrack -> "["
  | RBrack -> "]"
  | LBrace -> "{"
  | LBraceHashRBrace -> "{#}"
  | RBrace -> "}"
  | Plus -> "+"
  | Plus2 -> "++"
  | PlusEq -> "+="
  | PlusColon -> "+:"
  | Minus -> "-"
  | MinusEq -> "-="
  | Star -> "*"
  | StarEq -> "*="
  | Slash -> "/"
  | SlashEq -> "/="
  | Backslash -> "\\"
  | Percent -> "%"
  | PercentEq -> "%="
  | Eq -> "="
  | Eq2 -> "=="
  | Amp -> "&"
  | Amp2 -> "&&"
  | Amp3 -> "&&&"
  | AmpEq -> "&="
  | Up -> "^"
  | UpEq -> "^="
  | Bar -> "|"
  | Bar2 -> "||"
  | BarEq -> "|="
  | SPlus -> "|+|"
  | SPlusEq -> "|+|="
  | SMinus -> "|-|"
  | SMinusEq -> "|-|="

let atom_of_string = function
  | "<:" -> Sub
  | ":>" -> Sup
  | "|-" -> Turnstile
  | "-|" -> Tilesturn
  | "`" -> Tick
  | "\"" -> DoubleQuote
  | "_" -> Underscore
  | "->" -> Arrow
  | "->_" -> ArrowSub
  | "=>" -> DoubleArrow
  | "=>_" -> DoubleArrowSub
  | "==>" -> DoubleArrowLong
  | "~>" -> SqArrow
  | "~>*" -> SqArrowStar
  | "." -> Dot
  | ".." -> Dot2
  | "..." -> Dot3
  | "," -> Comma
  | ";" -> Semicolon
  | ":" -> Colon
  | ":=" -> ColonEq
  | "#" -> Hash
  | "$" -> Dollar
  | "@" -> At
  | "?" -> Quest
  | "!" -> Bang
  | "!=" -> BangEq
  | "~" -> Tilde
  | "~~" -> Tilde2
  | "<" -> LAngle
  | "<<" -> LAngle2
  | "<=" -> LAngleEq
  | "<<=" -> LAngle2Eq
  | ">" -> RAngle
  | ">>" -> RAngle2
  | ">=" -> RAngleEq
  | ">>=" -> RAngle2Eq
  | "(" -> LParen
  | ")" -> RParen
  | "[" -> LBrack
  | "]" -> RBrack
  | "{" -> LBrace
  | "{#}" -> LBraceHashRBrace
  | "}" -> RBrace
  | "+" -> Plus
  | "++" -> Plus2
  | "+=" -> PlusEq
  | "+:" -> PlusColon
  | "-" -> Minus
  | "-=" -> MinusEq
  | "*" -> Star
  | "*=" -> StarEq
  | "/" -> Slash
  | "/=" -> SlashEq
  | "\\" -> Backslash
  | "%" -> Percent
  | "%=" -> PercentEq
  | "=" -> Eq
  | "==" -> Eq2
  | "&" -> Amp
  | "&&" -> Amp2
  | "&&&" -> Amp3
  | "&=" -> AmpEq
  | "^" -> Up
  | "^=" -> UpEq
  | "|" -> Bar
  | "||" -> Bar2
  | "|=" -> BarEq
  | "|+|" -> SPlus
  | "|+|=" -> SPlusEq
  | "|-|" -> SMinus
  | "|-|=" -> SMinusEq
  | id when String.starts_with ~prefix:"`" id ->
      SilentAtom (String.sub id 1 (String.length id - 1))
  | id -> Atom id

let render_atom = function
  | Atom id -> id
  | SilentAtom "EMPTY" -> "/* empty */"
  | SilentAtom id -> "`" ^ id
  | Sub -> "<:"
  | Sup -> ":>"
  | Turnstile -> "|-"
  | Tilesturn -> "-|"
  | Tick -> "`"
  | DoubleQuote -> "\""
  | Underscore -> "_"
  | Arrow -> "->"
  | ArrowSub -> "->_"
  | DoubleArrow -> "=>"
  | DoubleArrowSub -> "=>_"
  | DoubleArrowLong -> "==>"
  | SqArrow -> "~>"
  | SqArrowStar -> "~>*"
  | Dot -> "."
  | Dot2 -> ".."
  | Dot3 -> "..."
  | Comma -> ","
  | Semicolon -> ";"
  | Colon -> ":"
  | ColonEq -> ":="
  | Hash -> "#"
  | Dollar -> "$"
  | At -> "@"
  | Quest -> "?"
  | Bang -> "!"
  | BangEq -> "!="
  | Tilde -> "~"
  | Tilde2 -> "~~"
  | LAngle -> "<"
  | LAngle2 -> "<<"
  | LAngleEq -> "<="
  | LAngle2Eq -> "<<="
  | RAngle -> ">"
  | RAngle2 -> ">>"
  | RAngleEq -> ">="
  | RAngle2Eq -> ">>="
  | LParen -> "("
  | RParen -> ")"
  | LBrack -> "["
  | RBrack -> "]"
  | LBrace -> "{"
  | LBraceHashRBrace -> "{#}"
  | RBrace -> "}"
  | Plus -> "+"
  | Plus2 -> "++"
  | PlusEq -> "+="
  | PlusColon -> "+:"
  | Minus -> "-"
  | MinusEq -> "-="
  | Star -> "*"
  | StarEq -> "*="
  | Slash -> "/"
  | SlashEq -> "/="
  | Backslash -> "\\"
  | Percent -> "%"
  | PercentEq -> "%="
  | Eq -> "="
  | Eq2 -> "=="
  | Amp -> "&"
  | Amp2 -> "&&"
  | Amp3 -> "&&&"
  | AmpEq -> "&="
  | Up -> "^"
  | UpEq -> "^="
  | Bar -> "|"
  | Bar2 -> "||"
  | BarEq -> "|="
  | SPlus -> "|+|"
  | SPlusEq -> "|+|="
  | SMinus -> "|-|"
  | SMinusEq -> "|-|="
