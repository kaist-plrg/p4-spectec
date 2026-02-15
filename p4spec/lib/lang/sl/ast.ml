open Util.Source

[@@@ocamlformat "disable"]

(* Numbers *)

type num = Il.num [@@deriving yojson]

(* Texts *)

type text = Il.text [@@deriving yojson]

(* Identifiers *)

type id = Il.id [@@deriving yojson]
type id' = Il.id'

(* Atoms *)

type atom = Il.atom [@@deriving yojson]
type atom' = Il.atom'

(* Mixfix operators *)

type mixop = Il.mixop [@@deriving yojson]

(* Iterators *)

type iter = Il.iter [@@deriving yojson]

(* Variables *)

type var = Il.var [@@deriving yojson]

(* Types *)

type typ = Il.typ [@@deriving yojson]
type typ' = Il.typ'

type nottyp = Il.nottyp [@@deriving yojson]
type nottyp' = Il.nottyp'

type deftyp = Il.deftyp [@@deriving yojson]
type deftyp' = Il.deftyp'

type typfield = Il.typfield [@@deriving yojson]
type typcase = Il.typcase [@@deriving yojson]

(* Values *)

type vid = Il.vid
type vnote = Il.vnote

type value = Il.value [@@deriving yojson]
type value' = Il.value'

type valuefield = atom * value [@@deriving yojson]
type valuecase = mixop * value list [@@deriving yojson]

(* Operators *)

type numop = Il.numop [@@deriving yojson]
type unop = Il.unop [@@deriving yojson]
type binop = Il.binop [@@deriving yojson]
type cmpop = Il.cmpop [@@deriving yojson]
type optyp = Il.optyp [@@deriving yojson]

(* Expressions *)

type exp = Il.exp [@@deriving yojson]
type exp' = Il.exp'

type notexp = Il.notexp [@@deriving yojson]
type iterexp = Il.iterexp [@@deriving yojson]

(* Patterns *)

type pattern = Il.pattern
[@@deriving yojson]

(* Path *)

type path = Il.path [@@deriving yojson]
type path' = Il.path'

(* Parameters *)

type param = param' phrase [@@deriving yojson]
and param' =
  | ExpP of typ * exp
  | DefP of id
  [@@deriving yojson]

(* Type parameters *)

type tparam = Il.tparam [@@deriving yojson]
type tparam' = Il.tparam'

(* Arguments *)

type arg = Il.arg [@@deriving yojson]
type arg' = Il.arg'

(* Type arguments *)

type targ = Il.targ [@@deriving yojson]
type targ' = Il.targ'

(* Phantoms *)

and pid = int
and phantom = pid

(* Holding conditions *)

and holdcase =
  | BothH of block * block
  | HoldH of block * phantom option
  | NotHoldH of block * phantom option
[@@deriving yojson]

(* Case analysis *)

and case = guard * block
[@@deriving yojson]

and guard =
  | BoolG of bool
  | CmpG of cmpop * optyp * exp
  | SubG of typ
  | MatchG of pattern
  | MemG of exp
[@@deriving yojson]

(* Instructions *)

and iid = int
and inote = { iid : iid } [@@deriving yojson]

and instr = (instr', inote) note_phrase [@@deriving yojson]
and instr' =
  (* Branching instructions *)
  | IfI of exp * iterexp list * block * phantom option
  | HoldI of id * notexp * iterexp list * holdcase
  | CaseI of exp * case list * phantom option 
  (* Aggregate instructions *)
  | GroupI of id * rel_signature * exp list * block
  (* Binding instructions *)
  | LetI of exp * exp * iterinstr list * block
  | RuleI of id * notexp * Hints.Input.t * iterinstr list * block
  (* Result/Return instructions *)
  | ResultI of rel_signature * exp list
  | ReturnI of exp
  (* Debugging instructions *)
  | DebugI of exp
[@@deriving yojson]

and block = instr list
[@@deriving yojson]

and elseblock = instr list
[@@deriving yojson]

and iterinstr = Il.iterprem
[@@deriving yojson]

(* Hints *)

and hint = El.hint
[@@deriving yojson]

(* Relations *)

(* nottyp `hint(input` `%`int* `)` *)
and rel_signature = nottyp * Hints.Input.t
[@@deriving yojson]

(* id `:` rel_signature exp* hint* *)
type externrel = id * rel_signature * exp list * hint list
[@@deriving yojson]

(* id `:` mixop `hint(input` `%`int* `)` exp* block elseblock? hint* *)
type rel = id * rel_signature * exp list * block * elseblock option * hint list
[@@deriving yojson]

(* Functions *)

(* id `<` list(tparam, `,`) `>` list(param, `,`) `:` hint* *)
type externfunc = id * tparam list * param list * typ * hint list
[@@deriving yojson]

(* id `<` list(tparam, `,`) `>` list(param, `,`) `:` hint* *)
type builtinfunc = id * tparam list * param list * typ * hint list
[@@deriving yojson]

(* `(` list(exp, `,`)* `)` `->` exp block *)
type tablerow = exp list * exp * block
[@@deriving yojson]

(* id `(` list(param, `,`) `)` `:` typ tablerow* hint* *)
type tablefunc = id * param list * typ * tablerow list * hint list
[@@deriving yojson]

(* id `<` list(tparam, `,`) `>` list(arg, `,`) `:` typ block elseblock? hint* *)
type definedfunc = id * tparam list * param list * typ * block * elseblock option * hint list
[@@deriving yojson]

(* Definitions *)

type def = def' phrase
and def' =
  (* `extern` `syntax` id hint* *)
  | ExternTypD of id * hint list
  (* `syntax` id `<` list(tparam, `,`) `>` `=` deftyp hint* *)
  | TypD of id * tparam list * deftyp * hint list
  (* `extern` `relation` rel *)
  | ExternRelD of externrel
  (* `relation` rel *)
  | RelD of rel
  (* `extern `dec` externfunc *)
  | ExternDecD of externfunc
  (* `builtin` `dec` builtinfunc *)
  | BuiltinDecD of builtinfunc
  (* `tbl` `dec` tablefunc *)
  | TableDecD of tablefunc
  (* `dec` func *)
  | FuncDecD of definedfunc
[@@deriving yojson]

(* Spec *)

type spec = def list [@@deriving yojson]
