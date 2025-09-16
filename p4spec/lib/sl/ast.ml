open Util.Source

[@@@ocamlformat "disable"]

(* Numbers *)

type num = Il.Ast.num [@@deriving yojson]

(* Texts *)

type text = Il.Ast.text [@@deriving yojson]

(* Identifiers *)

type id = Il.Ast.id [@@deriving yojson]
type id' = Il.Ast.id'

(* Atoms *)

type atom = Il.Ast.atom [@@deriving yojson]
type atom' = Il.Ast.atom'

(* Mixfix operators *)

type mixop = Il.Ast.mixop [@@deriving yojson]

(* Iterators *)

type iter = Il.Ast.iter [@@deriving yojson]

(* Variables *)

type var = Il.Ast.var [@@deriving yojson]

(* Types *)

type typ = Il.Ast.typ [@@deriving yojson]
type typ' = Il.Ast.typ'

type nottyp = Il.Ast.nottyp [@@deriving yojson]
type nottyp' = Il.Ast.nottyp'

type deftyp = Il.Ast.deftyp [@@deriving yojson]
type deftyp' = Il.Ast.deftyp'

type typfield = Il.Ast.typfield [@@deriving yojson]
type typcase = Il.Ast.typcase [@@deriving yojson]

(* Values *)

type vid = Il.Ast.vid
type vnote = Il.Ast.vnote

type value = Il.Ast.value [@@deriving yojson]
type value' = Il.Ast.value'

type valuefield = atom * value [@@deriving yojson]
type valuecase = mixop * value list [@@deriving yojson]

(* Operators *)

type numop = Il.Ast.numop [@@deriving yojson]
type unop = Il.Ast.unop [@@deriving yojson]
type binop = Il.Ast.binop [@@deriving yojson]
type cmpop = Il.Ast.cmpop [@@deriving yojson]
type optyp = Il.Ast.optyp [@@deriving yojson]

(* Expressions *)

type exp = Il.Ast.exp [@@deriving yojson]
type exp' = Il.Ast.exp'

type notexp = Il.Ast.notexp [@@deriving yojson]
type iterexp = Il.Ast.iterexp [@@deriving yojson]

(* Patterns *)

type pattern = Il.Ast.pattern
[@@deriving yojson]

(* Path *)

type path = Il.Ast.path [@@deriving yojson]
type path' = Il.Ast.path'

(* Parameters *)

type param = Il.Ast.param [@@deriving yojson]
type param' = Il.Ast.param'

(* Type parameters *)

type tparam = Il.Ast.tparam [@@deriving yojson]
type tparam' = Il.Ast.tparam'

(* Arguments *)

type arg = Il.Ast.arg [@@deriving yojson]
type arg' = Il.Ast.arg'

(* Type arguments *)

type targ = Il.Ast.targ [@@deriving yojson]
type targ' = Il.Ast.targ'

(* Path conditions *)

and pid = int

and phantom = pid * pathcond list
[@@deriving yojson]

and pathcond =
  | ForallC of pathcond * iterexp list
  | ExistsC of pathcond * iterexp list
  | PlainC of exp
  | HoldC of id * notexp
  | NotHoldC of id * notexp
[@@deriving yojson]

(* Holding conditions *)

and holdcase =
  | BothH of instr list * instr list
  | HoldH of instr list * phantom option
  | NotHoldH of instr list * phantom option
[@@deriving yojson]

(* Case analysis *)

and case = guard * instr list
[@@deriving yojson]

and guard =
  | BoolG of bool
  | CmpG of cmpop * optyp * exp
  | SubG of typ
  | MatchG of pattern
  | MemG of exp
[@@deriving yojson]

(* Instructions *)

and instr = instr' phrase
and instr' =
  | IfI of exp * iterexp list * instr list * phantom option
  | HoldI of id * notexp * iterexp list * holdcase
  | CaseI of exp * case list * phantom option 
  | OtherwiseI of instr
  | LetI of exp * exp * iterexp list
  | RuleI of id * notexp * iterexp list
  | ResultI of exp list
  | ReturnI of exp
  | DebugI of exp
[@@deriving yojson]

(* Hints *)

type hint = El.Ast.hint
[@@deriving yojson]

(* Definitions *)

type def = def' phrase
and def' =
  (* `syntax` id `<` list(tparam, `,`) `>` `=` deftyp *)
  | TypD of id * tparam list * deftyp
  (* `relation` id `:` mixop `hint(input` `%`int* `)` list(exp, `,`) `:` instr* *)
  | RelD of id * (mixop * int list) * exp list * instr list * hint list
  (* `dec` id `<` list(tparam, `,`) `>` list(param, `,`) `:` typ instr* *)
  | DecD of id * tparam list * arg list * instr list * hint list
[@@deriving yojson]

(* Spec *)

type spec = def list [@@deriving yojson]
