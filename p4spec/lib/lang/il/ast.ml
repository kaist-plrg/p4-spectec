open Domain
open Xl
open Util.Source

[@@@ocamlformat "disable"]

(* Numbers *)

type num = Num.t [@@deriving yojson]

(* Texts *)

type text = string [@@deriving yojson]

(* Identifiers *)

type id = id' phrase [@@deriving yojson]
and id' = string [@@deriving yojson]

(* Atoms *)

type atom = atom' phrase [@@deriving yojson]
and atom' = Atom.t [@@deriving yojson]

(* Mixfix operators *)

type mixop = Mixfix.mixop [@@deriving yojson]

(* Iterators *)

type iter =
  | Opt       (* `?` *)
  | List      (* `*` *)
[@@deriving yojson]

(* Variables *)

type var = id * typ * iter list

(* Types *)

and typ = typ' phrase
and typ' =
  | BoolT                                 (* `bool` *)
  | NumT of Num.typ                       (* numtyp *)
  | TextT                                 (* `text` *)
  | VarT of id * targ list                (* id (`<` list(targ, `,`) `>`)? *)
  | TupleT of typ list                    (* `(` list(typ, `,`) `)` *)
  | IterT of typ * iter                   (* typ iter *)
  | FuncT of tparam list * typ list * typ (* `<` list(tparam, `,`) `>` `(` list(typ, `,`) `)` `:` typ *)
[@@deriving yojson]

(* Defined types *)

and nottyp = nottyp' phrase
[@@deriving yojson]
and nottyp' = typ Mixfix.t
[@@deriving yojson]

and deftyp = deftyp' phrase
and deftyp' =
  | PlainT of typ
  | StructT of typfield list
  | VariantT of typcase list

and typfield = atom * typ
and typorigin = typorigin' phrase
and typorigin' = id * targ list
and typcase = nottyp * typorigin * hint list

(* Values *)

(* Invariant: vid is a globally unique identifier.
   Consequently, equal vids imply structural equality. Breaking this invariant
   would cause silent bugs in the implementation of value equality, since it
   would cause id aliasing between structurally distinct values. *)

and vid = int
and vnote = { vid : vid; typ : typ'; vhash : int } [@@deriving yojson]

and value = (value', vnote) note_phrase [@@deriving yojson]
and value' =
  | BoolV of bool
  | NumV of Num.t
  | TextV of string
  | StructV of valuefield list
  | CaseV of valuecase
  | TupleV of value list
  | OptV of value option
  | ListV of value list
  | FuncV of id
  | ExternV of Yojson.Safe.t
[@@deriving yojson]

and valuefield = atom * value
[@@deriving yojson]
and valuecase = value Mixfix.t
[@@deriving yojson]

(* Operators *)

and numop = [ `DecOp | `HexOp ] [@@deriving yojson]
and unop = [ Bool.unop | Num.unop ]
and binop = [ Bool.binop | Num.binop ]
and cmpop = [ Bool.cmpop | Num.cmpop ]
and optyp = [ Bool.typ | Num.typ ]

(* Subtype checks *)

and subcheck =
  | SkipSC
  | MixopSC of mixop list
  | TupleSC of subcheck list
  | IterSC of iter * subcheck
  | RecurseSC of typ
[@@deriving yojson]

(* Expressions *)

and exp = (exp', typ') note_phrase
and exp' =
  | BoolE of bool                         (* bool *)
  | NumE of num                           (* num *)
  | TextE of text                         (* text *)
  | VarE of id                            (* varid *)
  | UnE of unop * optyp * exp             (* unop exp *)
  | BinE of binop * optyp * exp * exp     (* exp binop exp *)
  | CmpE of cmpop * optyp * exp * exp     (* exp cmpop exp *)
  | UpCastE of typ * exp                  (* exp as typ *)
  | DownCastE of typ * exp                (* exp as typ *)
  | SubE of exp * typ * subcheck          (* exp `<:` typ *)
  | MatchE of exp * pattern               (* exp `matches` pattern *)
  | TupleE of exp list                    (* `(` exp* `)` *)
  | CaseE of notexp                       (* notexp *)
  | StrE of (atom * exp) list             (* { expfield* } *)
  | OptE of exp option                    (* exp? *)
  | ListE of exp list                     (* `[` exp* `]` *)
  | ConsE of exp * exp                    (* exp `::` exp *)
  | CatE of exp * exp                     (* exp `++` exp *)
  | MemE of exp * exp                     (* exp `<-` exp *)
  | LenE of exp                           (* `|` exp `|` *)
  | DotE of exp * atom                    (* exp.atom *)
  | IdxE of exp * exp                     (* exp `[` exp `]` *)
  | SliceE of exp * exp * exp             (* exp `[` exp `:` exp `]` *)
  | UpdE of exp * path * exp              (* exp `[` path `=` exp `]` *)
  | CallE of id * targ list * arg list    (* $id`<` targ* `>``(` arg* `)` *)
  | IterE of exp * iterexp                (* exp iterexp *)

and notexp = exp Mixfix.t
and iterexp = iter * var list

(* Patterns *)

and pattern =
  | CaseP of mixop
  | ListP of [ `Cons | `Fixed of int | `Nil ]
  | OptP of [ `Some | `None ]

(* Path *)

and path = (path', typ') note_phrase
and path' =
  | RootP                        (*  *)
  | IdxP of path * exp           (* path `[` exp `]` *)
  | SliceP of path * exp * exp   (* path `[` exp `:` exp `]` *)
  | DotP of path * atom          (* path `.` atom *)

(* Parameters *)

and param = param' phrase
and param' =
  (* typ *)
  | ExpP of typ
  (* `def` `$`id ` (`<` list(tparam, `,`) `>`)? (`(` list(param, `,`) `)`)? `:` typ *)
  | DefP of id * tparam list * param list * typ

(* Type parameters *)

and tparam = tparam' phrase
and tparam' = id'

(* Arguments *)

and arg = arg' phrase
and arg' =
  | ExpA of exp   (* exp *)
  | DefA of id    (* `$`id *)

(* Type arguments *)

and targ = targ' phrase [@@deriving yojson]
and targ' = typ' [@@deriving yojson]

(* Premises *)

and prem = prem' phrase
and prem' =
  | RulePr of id * notexp * Hints.Input.t (* id `:` notexp *)
  | IfPr of exp                           (* `if` exp *)
  | IfHoldPr of id * notexp               (* `if` id `:` notexp `holds` *)
  | IfNotHoldPr of id * notexp            (* `if` id `:` notexp `does not hold` *)
  | LetPr of exp * exp                    (* `let` exp `=` exp *)
  | IterPr of prem * iterprem             (* prem iterprem *)
  | DebugPr of exp                        (* `debug` exp *)

and iterprem = iter * var list * var list

(* Rules *)

and rule = rule' phrase
and rule' = id * notexp * prem list

and rulegroup = rulegroup' phrase
and rulegroup' = id * rule list

and elsegroup = elsegroup' phrase
and elsegroup' = id * rule

(* Clauses *)

and clause = clause' phrase
and clause' = arg list * exp * prem list

and elseclause = clause
and elseclause' = clause'

(* Table rows *)

and tablerow = tablerow' phrase
and tablerow' = arg list * exp

(* Hints *)

and hint = El.hint

(* Definitions *)

type def = def' phrase
and def' =
  (* `extern` `syntax` id hint* *)
  | ExternTypD of id * hint list
  (* `syntax` id `<` list(tparam, `,`) `>` hint* `=` deftyp *)
  | TypD of id * tparam list * deftyp * hint list
  (* `var` id `:` typ hint* *)
  | VarD of id * typ * hint list
  (* `extern` `relation` id `:` nottyp `hint(input` `%`int* `)` hint* *)
  | ExternRelD of id * nottyp * Hints.Input.t * hint list
  (* `relation` id `:` nottyp `hint(input` `%`int* `)` rulegroup* hint* *)
  | RelD of id * nottyp * Hints.Input.t * rulegroup list * elsegroup option * hint list
  (* `extern` `dec` id `<` list(tparam, `,`) `>` list(param, `,`) `:` typ hint* *)
  | ExternDecD of id * tparam list * param list * typ * hint list
  (* `builtin` `dec` id `<` list(tparam, `,`) `>` list(param, `,`) `:` typ hint* *)
  | BuiltinDecD of id * tparam list * param list * typ * hint list
  (* `table` `dec` id list(param, `,`) `:` typ hint* *)
  | TableDecD of id * param list * typ * tablerow list * hint list
  (* `dec` id `<` list(tparam, `,`) `>` list(param, `,`) `:` typ clause* hint* *)
  | FuncDecD of id * tparam list * param list * typ * clause list * elseclause option * hint list

(* Spec *)

type spec = def list
