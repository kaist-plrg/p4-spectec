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

let compare atom_a atom_b =
  let tag = function
    | Atom _ -> 0
    | SilentAtom _ -> 1
    | Sub -> 2
    | Sup -> 3
    | Turnstile -> 4
    | Tilesturn -> 5
    | Tick -> 6
    | DoubleQuote -> 7
    | Underscore -> 8
    | Arrow -> 9
    | ArrowSub -> 10
    | DoubleArrow -> 11
    | DoubleArrowSub -> 12
    | DoubleArrowLong -> 13
    | SqArrow -> 14
    | SqArrowStar -> 15
    | Dot -> 16
    | Dot2 -> 17
    | Dot3 -> 18
    | Comma -> 19
    | Semicolon -> 20
    | Colon -> 21
    | Hash -> 22
    | Dollar -> 23
    | At -> 24
    | Quest -> 25
    | Bang -> 26
    | BangEq -> 27
    | Tilde -> 28
    | Tilde2 -> 29
    | LAngle -> 30
    | LAngle2 -> 31
    | LAngleEq -> 32
    | LAngle2Eq -> 33
    | RAngle -> 34
    | RAngle2 -> 35
    | RAngleEq -> 36
    | RAngle2Eq -> 37
    | LParen -> 38
    | RParen -> 39
    | LBrack -> 40
    | RBrack -> 41
    | LBrace -> 42
    | LBraceHashRBrace -> 43
    | RBrace -> 44
    | Plus -> 45
    | Plus2 -> 46
    | PlusEq -> 47
    | Minus -> 48
    | MinusEq -> 49
    | Star -> 50
    | StarEq -> 51
    | Slash -> 52
    | SlashEq -> 53
    | Backslash -> 54
    | Percent -> 55
    | PercentEq -> 56
    | Eq -> 57
    | Eq2 -> 58
    | Amp -> 59
    | Amp2 -> 60
    | Amp3 -> 61
    | AmpEq -> 62
    | Up -> 63
    | UpEq -> 64
    | Bar -> 65
    | Bar2 -> 66
    | BarEq -> 67
    | SPlus -> 68
    | SPlusEq -> 69
    | SMinus -> 70
    | SMinusEq -> 71
  in
  let c = compare (tag atom_a) (tag atom_b) in
  if c <> 0 then c
  else
    match (atom_a, atom_b) with
    | Atom id_a, Atom id_b -> String.compare id_a id_b
    | SilentAtom id_a, SilentAtom id_b -> String.compare id_a id_b
    | _ -> 0

let eq atom_a atom_b = compare atom_a atom_b = 0

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
