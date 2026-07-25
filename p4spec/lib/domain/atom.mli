(* Atoms: terminal tokens of notation, shared across EL, IL, SL. Keyword/Tag/
   Operator carry private payloads built only through the validating builders *)

type upid = private string
type optext = private string

[@@@ocamlformat "disable"]
type t =
  | Keyword of upid                 (* INT *)
  | Tag of upid                     (* _NUM *)
  | Operator of optext              (* '+', '#' *)
  | Sub | Sup | Turnstile | Tilesturn
  | Arrow | ArrowSub | DoubleArrowSub | DoubleArrowLong | SqArrow | SqArrowStar
  | Dot | Dot2 | Dot3 | Semicolon | Colon | ColonEq | Tilde2 | Backslash
  | LAngle | RAngle | LParen | RParen | LBrack | RBrack | LBrace | RBrace
[@@deriving yojson]
[@@@ocamlformat "enable"]

val compare : t -> t -> int
val eq : t -> t -> bool

(* Parse-faithful; round-trips with atom_of_string for boot/unboot *)
val string_of_atom : t -> string
val atom_of_string : string -> t
(* Lossy display glyph *)
val render_atom : t -> string

(* keyword takes any lexer-produced object identifier; tag and operator raise
   Invalid_argument unless the payload matches what the lexer can read *)
val keyword : string -> t
val tag : string -> t
val operator : string -> t
val is_operator : t -> string -> bool
