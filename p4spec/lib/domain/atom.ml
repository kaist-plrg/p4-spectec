[@@@ocamlformat "disable"]

type t =
  | Atom of string                  (* atomid *)
  | SilentAtom of string            (* `atomid *)
  | Sub                             (* `<:` *)
  | Sup                             (* `:>` *)
  | Turnstile                       (* `|-` *)
  | Tilesturn                       (* `-|` *)
  | Tick                            (* ```` *)
  | DoubleQuote                     (* ``''` *)
  | Underscore                      (* ``_` *)
  | Arrow of [ `Plain | `Tick ]     (* `->` or -> *)
  | ArrowSub                        (* `->_` *)
  | DoubleArrow                     (* ``=>` *)
  | DoubleArrowSub                  (* ``=>_` *)
  | DoubleArrowLong                 (* ``==>` *)
  | SqArrow                         (* `~>` *)
  | SqArrowStar                     (* `~>*` *)
  | Dot of [ `Plain | `Tick ]       (* ``.` or . *)
  | Dot2 of [ `Plain | `Tick ]      (* ``..` or .. *)
  | Dot3 of [ `Plain | `Tick ]      (* ``...` or ... *)
  | Comma                           (* ``,` *)
  | Semicolon of [ `Plain | `Tick ] (* ``;` or ; *)
  | Colon of [ `Plain | `Tick ]     (* `:` or : *)
  | ColonEq of [ `Plain | `Tick ]   (* `:=` or := *)
  | Hash                            (* ``#` *)
  | Dollar                          (* ``$` *)
  | At                              (* ``@` *)
  | Quest                           (* ``?` *)
  | Bang                            (* ``!` *)
  | BangEq                          (* ``!=` *)
  | Tilde                           (* ``~` *)
  | Tilde2 of [ `Plain | `Tick ]    (* `~~` or ~~ *)
  | LAngle of [ `Tick | `Tick2 ]    (* ``<` or ```<` *)
  | LAngle2                         (* `<<` *)
  | LAngleEq                        (* ``<=` *)
  | LAngle2Eq                       (* `<<=` *)
  | RAngle of [ `Plain | `Tick2 ]   (* > or ```>` *)
  | RAngle2                         (* `>>` *)
  | RAngleEq                        (* ``>=` *)
  | RAngle2Eq                       (* `>>=` *)
  | LParen                          (* ``(` *)
  | RParen                          (* ``)` *)
  | LBrack of [ `Tick | `Tick2 ]    (* ``[` or ```[` *)
  | RBrack of [ `Plain | `Tick2 ]   (* ] or ```]` *)
  | LBrace of [ `Tick | `Tick2 ]    (* ``{` or ```{` *)
  | LBraceHashRBrace                (* `{#}` *)
  | RBrace of [ `Plain | `Tick2 ]   (* } or ```}` *)
  | Plus                            (* ``+` *)
  | Plus2                           (* ``++` *)
  | PlusEq                          (* ``+=` *)
  | PlusColon                       (* ``+:` *)
  | Minus                           (* ``-` *)
  | MinusEq                         (* ``-=` *)
  | Star                            (* ``*` *)
  | StarEq                          (* ``*=` *)
  | Slash                           (* ``/` *)
  | SlashEq                         (* ``/=` *)
  | Backslash                       (* ``\` *)
  | Percent                         (* ``%` *)
  | PercentEq                       (* ``%=` *)
  | Eq                              (* ``=` *)
  | Eq2                             (* `==` *)
  | Amp                             (* ``&` *)
  | Amp2                            (* ``&&` *)
  | Amp3                            (* ``&&&` *)
  | AmpEq                           (* ``&=` *)
  | Up                              (* ``^` *)
  | UpEq                            (* ``^=` *)
  | Bar                             (* ``|` *)
  | Bar2                            (* ``||` *)
  | BarEq                           (* ``|=` *)
  | SPlus                           (* ``|+|` *)
  | SPlusEq                         (* ``|+|=` *)
  | SMinus                          (* ``|-|` *)
  | SMinusEq                        (* ``|-|=` *)
[@@deriving yojson]
[@@@ocamlformat "enable"]

let compare atom_a atom_b = compare atom_a atom_b

let eq (atom_a : t) (atom_b : t) : bool =
  match (atom_a, atom_b) with
  | Atom id_a, Atom id_b | SilentAtom id_a, SilentAtom id_b ->
      String.equal id_a id_b
  | _ -> atom_a = atom_b

let string_of_atom = function
  | Atom id -> id
  | SilentAtom id -> "`" ^ id
  | Sub -> "<:"
  | Sup -> ":>"
  | Turnstile -> "|-"
  | Tilesturn -> "-|"
  | Tick -> "``"
  | DoubleQuote -> "`\""
  | Underscore -> "`_"
  | Arrow `Plain -> "->"
  | Arrow `Tick -> "`->"
  | ArrowSub -> "->_"
  | DoubleArrow -> "`=>"
  | DoubleArrowSub -> "=>_"
  | DoubleArrowLong -> "==>"
  | SqArrow -> "~>"
  | SqArrowStar -> "~>*"
  | Dot `Plain -> "."
  | Dot `Tick -> "`."
  | Dot2 `Plain -> ".."
  | Dot2 `Tick -> "`.."
  | Dot3 `Plain -> "..."
  | Dot3 `Tick -> "`..."
  | Comma -> "`,"
  | Semicolon `Plain -> ";"
  | Semicolon `Tick -> "`;"
  | Colon `Plain -> ":"
  | Colon `Tick -> "`:"
  | ColonEq `Plain -> ":="
  | ColonEq `Tick -> "`:="
  | Hash -> "`#"
  | Dollar -> "`$"
  | At -> "`@"
  | Quest -> "`?"
  | Bang -> "`!"
  | BangEq -> "`!="
  | Tilde -> "`~"
  | Tilde2 `Plain -> "~~"
  | Tilde2 `Tick -> "`~~"
  | LAngle `Tick -> "`<"
  | LAngle `Tick2 -> "``<"
  | LAngle2 -> "`<<"
  | LAngleEq -> "`<="
  | LAngle2Eq -> "`<<="
  | RAngle `Plain -> ">"
  | RAngle `Tick2 -> "``>"
  | RAngle2 -> "`>>"
  | RAngleEq -> "`>="
  | RAngle2Eq -> "`>>="
  | LParen -> "`("
  | RParen -> ")"
  | LBrack `Tick -> "`["
  | LBrack `Tick2 -> "``["
  | RBrack `Plain -> "]"
  | RBrack `Tick2 -> "``]"
  | LBrace `Tick -> "`{"
  | LBrace `Tick2 -> "``{"
  | LBraceHashRBrace -> "`{#}"
  | RBrace `Plain -> "}"
  | RBrace `Tick2 -> "``}"
  | Plus -> "`+"
  | Plus2 -> "`++"
  | PlusEq -> "`+="
  | PlusColon -> "`+:"
  | Minus -> "`-"
  | MinusEq -> "`-="
  | Star -> "`*"
  | StarEq -> "`*="
  | Slash -> "`/"
  | SlashEq -> "`/="
  | Backslash -> "\\"
  | Percent -> "`%"
  | PercentEq -> "`%="
  | Eq -> "`="
  | Eq2 -> "`=="
  | Amp -> "`&"
  | Amp2 -> "`&&"
  | Amp3 -> "`&&&"
  | AmpEq -> "`&="
  | Up -> "`^"
  | UpEq -> "`^="
  | Bar -> "`|"
  | Bar2 -> "`||"
  | BarEq -> "`|="
  | SPlus -> "`|+|"
  | SPlusEq -> "`|+|="
  | SMinus -> "`|-|"
  | SMinusEq -> "`|-|="

let atom_of_string = function
  | "<:" -> Sub
  | ":>" -> Sup
  | "|-" -> Turnstile
  | "-|" -> Tilesturn
  | "``" -> Tick
  | "`\"" -> DoubleQuote
  | "`_" -> Underscore
  | "->" -> Arrow `Plain
  | "`->" -> Arrow `Tick
  | "->_" -> ArrowSub
  | "`=>" -> DoubleArrow
  | "=>_" -> DoubleArrowSub
  | "==>" -> DoubleArrowLong
  | "~>" -> SqArrow
  | "~>*" -> SqArrowStar
  | "." -> Dot `Plain
  | "`." -> Dot `Tick
  | ".." -> Dot2 `Plain
  | "`.." -> Dot2 `Tick
  | "..." -> Dot3 `Plain
  | "`..." -> Dot3 `Tick
  | "`," -> Comma
  | ";" -> Semicolon `Plain
  | "`;" -> Semicolon `Tick
  | ":" -> Colon `Plain
  | "`:" -> Colon `Tick
  | ":=" -> ColonEq `Plain
  | "`:=" -> ColonEq `Tick
  | "`#" -> Hash
  | "`$" -> Dollar
  | "`@" -> At
  | "`?" -> Quest
  | "`!" -> Bang
  | "`!=" -> BangEq
  | "`~" -> Tilde
  | "~~" -> Tilde2 `Plain
  | "`~~" -> Tilde2 `Tick
  | "`<" -> LAngle `Tick
  | "``<" -> LAngle `Tick2
  | "`<<" -> LAngle2
  | "`<=" -> LAngleEq
  | "`<<=" -> LAngle2Eq
  | ">" -> RAngle `Plain
  | "``>" -> RAngle `Tick2
  | "`>>" -> RAngle2
  | "`>=" -> RAngleEq
  | "`>>=" -> RAngle2Eq
  | "`(" -> LParen
  | ")" -> RParen
  | "`[" -> LBrack `Tick
  | "``[" -> LBrack `Tick2
  | "]" -> RBrack `Plain
  | "``]" -> RBrack `Tick2
  | "{" -> LBrace `Tick
  | "``{" -> LBrace `Tick2
  | "`{#}" -> LBraceHashRBrace
  | "}" -> RBrace `Plain
  | "``}" -> RBrace `Tick2
  | "`+" -> Plus
  | "`++" -> Plus2
  | "`+=" -> PlusEq
  | "`+:" -> PlusColon
  | "`-" -> Minus
  | "`-=" -> MinusEq
  | "`*" -> Star
  | "`*=" -> StarEq
  | "`/" -> Slash
  | "`/=" -> SlashEq
  | "\\" -> Backslash
  | "`%" -> Percent
  | "`%=" -> PercentEq
  | "`=" -> Eq
  | "`==" -> Eq2
  | "`&" -> Amp
  | "`&&" -> Amp2
  | "`&&&" -> Amp3
  | "`&=" -> AmpEq
  | "`^" -> Up
  | "`^=" -> UpEq
  | "`|" -> Bar
  | "`||" -> Bar2
  | "`|=" -> BarEq
  | "`|+|" -> SPlus
  | "`|+|=" -> SPlusEq
  | "`|-|" -> SMinus
  | "`|-|=" -> SMinusEq
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
  | Arrow _ -> "->"
  | ArrowSub -> "->_"
  | DoubleArrow -> "=>"
  | DoubleArrowSub -> "=>_"
  | DoubleArrowLong -> "==>"
  | SqArrow -> "~>"
  | SqArrowStar -> "~>*"
  | Dot _ -> "."
  | Dot2 _ -> ".."
  | Dot3 _ -> "..."
  | Comma -> ","
  | Semicolon _ -> ";"
  | Colon _ -> ":"
  | ColonEq _ -> ":="
  | Hash -> "#"
  | Dollar -> "$"
  | At -> "@"
  | Quest -> "?"
  | Bang -> "!"
  | BangEq -> "!="
  | Tilde -> "~"
  | Tilde2 _ -> "~~"
  | LAngle _ -> "<"
  | LAngle2 -> "<<"
  | LAngleEq -> "<="
  | LAngle2Eq -> "<<="
  | RAngle _ -> ">"
  | RAngle2 -> ">>"
  | RAngleEq -> ">="
  | RAngle2Eq -> ">>="
  | LParen -> "("
  | RParen -> ")"
  | LBrack _ -> "["
  | RBrack _ -> "]"
  | LBrace _ -> "{"
  | LBraceHashRBrace -> "{#}"
  | RBrace _ -> "}"
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
