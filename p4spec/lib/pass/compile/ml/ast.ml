[@@@ocamlformat "disable"]

(* Names *)

type id = string

(* Constructors and fields *)

type ctor = string
type field = string

(* Types *)

type typ =
  (* `unit` *)
  | UnitT
  (* `bool` *)
  | BoolT
  (* `string` *)
  | StringT
  (* `Bigint.t` *)
  | BigintT
  (* id *)
  | NameT of id
  (* `id *)
  | VarT of id
  (* id list(typ, ' ') *)
  | AppT of id * typ list
  (* `(` list(typ, `,`) `)` *)
  | TupleT of typ list
  (* `[>` list(`ctor of typ1 * ..., `|`) `]` *)
  | OpenRowT of typrow list

and typrow = ctor * typ list

(* Type parameters *)

type tparam = string

(* Type definitions *)

type typfield = field * typ
type typcase = ctor * typ list

type deftyp =
  | AliasTD of typ
  | RecordTD of typfield list
  | VariantTD of typcase list

type typdef = tparam list * id * deftyp

(* Patterns *)

type pat =
  (* `_` *)
  | WildP
  (* id *)
  | VarP of id
  (* literal *)
  | LitP of string
  (* `(` list(pat, `,`) `)` *)
  | TupleP of pat list
  (* `[` list(pat, `;`) `]` *)
  | ListP of pat list
  (* pat `::` pat *)
  | ConsP of pat * pat
  (* `None` or `Some` pat *)
  | OptP of pat option
  (* `#`id *)
  | OpenP of id
  | VariantP of
    [
      (* `ctor `(` list(pat, `,`) `)` *)
      | `Poly of ctor * pat list
      (* ctor `(` list(pat, `,`) `)` *)
      | `Mono of ctor * pat list
    ]
  (* `(` pat `as` id `)` *)
  | AsP of pat * id
  (* `(` list(pat, `|`) `)` — or-pattern *)
  | OrP of pat list

(* Operators *)

type unop = string
type binop = string

(* Expressions *)

type expr =
  (* `()` *)
  | UnitE
  (* `true` or `false` *)
  | BoolE of bool
  (* `Bigint.of_string` num *)
  | BigintE of string
  (* `"` string `"` *)
  | StrE of string
  (* literal *)
  | LitE of string
  (* id *)
  | VarE of id
  (* expr binop expr *)
  | BinopE of binop * expr * expr
  (* unop expr *)
  | UnopE of unop * expr
  (* `(` list(expr, `,`) `)` *)
  | TupleE of expr list
  (* `[` list(expr, `;`) `]` *)
  | ListE of expr list
  (* expr `::` expr *)
  | ConsE of expr * expr
  (* `None` or `Some` expr *)
  | OptE of expr option
  (* `ctor `(` list(expr, `,`) `)` *)
  | VariantE of ctor * expr list
  (* `{` list(field = expr, `;`) `}` *)
  | RecordE of (field * expr) list
  (* `{` expr `with` list(field = expr, `;`) `}` *)
  | RecordUpdateE of expr * (field * expr) list
  (* expr `.` field *)
  | FieldE of expr * field
  (* expr list(expr, ` `) *)
  | AppE of expr * expr list
  (* `if` expr `then` expr `else` expr *)
  | IfE of expr * expr * expr option
  (* `match` expr `with` list(pattern = expr, `;`) `end` *)
  | MatchE of expr * (pat * expr) list
  (* `let` pattern `=` expr `in` expr *)
  | LetE of pat * expr * expr
  (* `try` expr `with` list(pattern -> expr, `|`) `end` *)
  | TryE of expr * arm list
  (* `fun` pattern `->` expr *)
  | FunE of pat list * expr
  (* expr `;` expr *)
  | SeqE of expr list
  (* `(` expr `:` typ `)` *)
  | AnnotE of expr * typ
  (* `(` expr `:>` typ `)` *)
  | CoerceE of expr * typ

and arm = pat * expr

(* Parameters *)

type param = id * typ option

(* Function definitions *)

type funcdef = id * param list * typ option * expr

(* Top-level items *)

type toplevel =
  | Raw of string
  | TypeRec of typdef list
  | Let of id * expr
  | LetRec of funcdef list

(* Files *)

type file = toplevel list
