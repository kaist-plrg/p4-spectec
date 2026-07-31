type upid = string [@@deriving yojson]
type optext = string [@@deriving yojson]

[@@@ocamlformat "disable"]

type t =
  | Keyword of upid                 (* concrete object word: INT *)
  | Tag of upid                     (* silent meta case label: _NUM *)
  | Operator of optext              (* concrete operator: '+', '->', '#' *)
  | Sub                             (* <: *)
  | Sup                             (* :> *)
  | Turnstile                       (* |- *)
  | Tilesturn                       (* -| *)
  | Arrow                           (* -> *)
  | ArrowSub                        (* ->_ *)
  | DoubleArrowSub                  (* =>_ *)
  | DoubleArrowLong                 (* ==> *)
  | SqArrow                         (* ~> *)
  | SqArrowStar                     (* ~>* *)
  | Dot                             (* . *)
  | Dot2                            (* .. *)
  | Dot3                            (* ... *)
  | Semicolon                       (* ; *)
  | Colon                           (* : *)
  | ColonEq                         (* := *)
  | Tilde2                          (* ~~ *)
  | Backslash                       (* \ *)
  | LAngle                          (* `< *)
  | RAngle                          (* `> *)
  | LParen                          (* `( *)
  | RParen                          (* `) *)
  | LBrack                          (* `[ *)
  | RBrack                          (* `] *)
  | LBrace                          (* `{ *)
  | RBrace                          (* `} *)
[@@deriving yojson]
[@@@ocamlformat "enable"]

let compare (atom_a : t) (atom_b : t) = Stdlib.compare atom_a atom_b
let eq (atom_a : t) (atom_b : t) : bool = compare atom_a atom_b = 0

(* Parse-faithful: round-trips through atom_of_string *)
let string_of_atom = function
  | Keyword id -> id
  | Tag id -> "_" ^ id
  | Operator s -> "'" ^ s ^ "'"
  | Sub -> "<:"
  | Sup -> ":>"
  | Turnstile -> "|-"
  | Tilesturn -> "-|"
  | Arrow -> "->"
  | ArrowSub -> "->_"
  | DoubleArrowSub -> "=>_"
  | DoubleArrowLong -> "==>"
  | SqArrow -> "~>"
  | SqArrowStar -> "~>*"
  | Dot -> "."
  | Dot2 -> ".."
  | Dot3 -> "..."
  | Semicolon -> ";"
  | Colon -> ":"
  | ColonEq -> ":="
  | Tilde2 -> "~~"
  | Backslash -> "\\"
  | LAngle -> "`<"
  | RAngle -> "`>"
  | LParen -> "`("
  | RParen -> "`)"
  | LBrack -> "`["
  | RBrack -> "`]"
  | LBrace -> "`{"
  | RBrace -> "`}"

let atom_of_string = function
  | "<:" -> Sub
  | ":>" -> Sup
  | "|-" -> Turnstile
  | "-|" -> Tilesturn
  | "->" -> Arrow
  | "->_" -> ArrowSub
  | "=>_" -> DoubleArrowSub
  | "==>" -> DoubleArrowLong
  | "~>" -> SqArrow
  | "~>*" -> SqArrowStar
  | "." -> Dot
  | ".." -> Dot2
  | "..." -> Dot3
  | ";" -> Semicolon
  | ":" -> Colon
  | ":=" -> ColonEq
  | "~~" -> Tilde2
  | "\\" -> Backslash
  | "`<" -> LAngle
  | "`>" -> RAngle
  | "`(" -> LParen
  | "`)" -> RParen
  | "`[" -> LBrack
  | "`]" -> RBrack
  | "`{" -> LBrace
  | "`}" -> RBrace
  | s
    when String.length s >= 2 && s.[0] = '\'' && s.[String.length s - 1] = '\''
    ->
      Operator (String.sub s 1 (String.length s - 2))
  | s when String.length s >= 2 && s.[0] = '_' ->
      Tag (String.sub s 1 (String.length s - 1))
  | id -> Keyword id

(* Lossy display glyph *)
let render_atom = function
  | Keyword id -> id
  | Tag "EMPTY" -> "/* empty */"
  | Tag id -> "_" ^ id
  | Operator s -> s
  | Sub -> "<:"
  | Sup -> ":>"
  | Turnstile -> "|-"
  | Tilesturn -> "-|"
  | Arrow -> "->"
  | ArrowSub -> "->_"
  | DoubleArrowSub -> "=>_"
  | DoubleArrowLong -> "==>"
  | SqArrow -> "~>"
  | SqArrowStar -> "~>*"
  | Dot -> "."
  | Dot2 -> ".."
  | Dot3 -> "..."
  | Semicolon -> ";"
  | Colon -> ":"
  | ColonEq -> ":="
  | Tilde2 -> "~~"
  | Backslash -> "\\"
  | LAngle -> "<"
  | RAngle -> ">"
  | LParen -> "("
  | RParen -> ")"
  | LBrack -> "["
  | RBrack -> "]"
  | LBrace -> "{"
  | RBrace -> "}"

let is_upid (s : string) : bool =
  String.length s > 0
  && (match s.[0] with 'A' .. 'Z' -> true | _ -> false)
  && String.for_all
       (function
         | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '\'' -> true
         | _ -> false)
       s

let is_operator atom s =
  match atom with Operator o -> String.equal o s | _ -> false

(* Constructors *)

let keyword (s : string) : t = Keyword s

let tag (s : string) : t =
  if is_upid s then Tag s else invalid_arg ("Atom.tag: expected upid: " ^ s)

let operator (s : string) : t =
  if String.contains s '\'' || String.contains s '\n' then
    invalid_arg ("Atom.operator: unquotable operator: " ^ s)
  else Operator s
