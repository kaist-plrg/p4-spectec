open Util.Source

[@@@ocamlformat "disable"]

(* Numbers *)

type num = Il.Ast.num

(* Texts *)

type text = Il.Ast.text

(* Identifiers *)

type id = Il.Ast.id [@@deriving yojson]
type id' = Il.Ast.id'

(* Atoms *)

type atom = Il.Ast.atom
type atom' = Il.Ast.atom'

(* Mixfix operators *)

type mixop = Il.Ast.mixop [@@deriving yojson]

(* Iterators *)

type iter = Il.Ast.iter

(* Variables *)

type var = Il.Ast.var

(* Types *)

type typ = Il.Ast.typ [@@deriving yojson]
type typ' = Il.Ast.typ'

type nottyp = Il.Ast.nottyp
type nottyp' = Il.Ast.nottyp'

type deftyp = Il.Ast.deftyp [@@deriving yojson]
type deftyp' = Il.Ast.deftyp'

type typfield = Il.Ast.typfield
type typcase = Il.Ast.typcase

(* Values *)

type vid = Il.Ast.vid
type vnote = Il.Ast.vnote

type value = Il.Ast.value
type value' = Il.Ast.value'

type valuefield = atom * value
type valuecase = mixop * value list

(* Operators *)

type numop = Il.Ast.numop
type unop = Il.Ast.unop
type binop = Il.Ast.binop
type cmpop = Il.Ast.cmpop [@@deriving yojson]
type optyp = Il.Ast.optyp [@@deriving yojson]

(* Expressions *)

type exp = Il.Ast.exp [@@deriving yojson]
type exp' = Il.Ast.exp'

type notexp = Il.Ast.notexp [@@deriving yojson]
type iterexp = Il.Ast.iterexp
[@@deriving yojson]

(* Patterns *)

type pattern = Il.Ast.pattern
[@@deriving yojson]

(* Path *)

type path = Il.Ast.path
type path' = Il.Ast.path'

(* Parameters *)

type param = Il.Ast.param
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

and pathcond =
  | ForallC of exp * iterexp list
  | ExistsC of exp * iterexp list
  | PlainC of exp

(* Case analysis *)

and case = guard * instr list

and guard =
  | BoolG of bool
  | CmpG of cmpop * optyp * exp
  | SubG of typ
  | MatchG of pattern
  | MemG of exp

(* Instructions *)

and instr = instr' phrase [@@deriving yojson]
and instr' =
  | IfI of exp * iterexp list * instr list * phantom option
  | CaseI of exp * case list * phantom option 
  | OtherwiseI of instr
  | LetI of exp * exp * iterexp list
  | RuleI of id * notexp * iterexp list
  | ResultI of exp list
  | ReturnI of exp
[@@deriving yojson]

(* Hints *)

type hint = { hintid : id; hintexp : El.Ast.exp }

(* Definitions *)

type def = def' phrase
and def' =
  (* `syntax` id `<` list(tparam, `,`) `>` `=` deftyp *)
  | TypD of id * tparam list * deftyp
  (* `relation` id `:` mixop `hint(input` `%`int* `)` list(exp, `,`) `:` instr* *)
  | RelD of id * (mixop * int list) * exp list * instr list
  (* `dec` id `<` list(tparam, `,`) `>` list(param, `,`) `:` typ instr* *)
  | DecD of id * tparam list * arg list * instr list
[@@deriving yojson]

(* Spec *)

type spec = def list [@@deriving yojson]
