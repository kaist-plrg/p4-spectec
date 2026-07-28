open Util.Source

[@@@ocamlformat "disable"]

(* Numbers *)

type num = Sl.num

(* Texts *)

type text = Sl.text

(* Identifiers *)

type id = Sl.id

(* Atoms *)

type atom = Sl.atom

(* Mixfix operators *)

type mixop = Sl.mixop

(* Iterators *)

type iter = Sl.iter

(* Variables *)

type var = Sl.var

(* Types *)

type typ = Sl.typ
type typ' = Sl.typ'

type nottyp = Sl.nottyp
type nottyp' = Sl.nottyp'

type deftyp = Sl.deftyp
type deftyp' = Sl.deftyp'

type typfield = Sl.typfield
type typcase = Sl.typcase

(* Values *)

type value = Sl.value

(* Operators *)

type unop = Sl.unop
type binop = Sl.binop
type cmpop = Sl.cmpop
type optyp = Sl.optyp

(* Expressions *)

type exp = ((exp', typ') note_phrase) Annot.t
and exp' =
  | BoolE of bool
  | NumE of num
  | TextE of text
  | VarE of id
  | UnE of unop * optyp * exp
  | BinE of binop * optyp * exp * exp
  | CmpE of cmpop * optyp * exp * exp
  | UpCastE of typ * exp
  | DownCastE of typ * exp
  | SubE of exp * typ
  | MatchE of exp * pattern
  | TupleE of exp list
  | CaseE of notexp
  | StrE of (atom * exp) list
  | OptE of exp option
  | ListE of exp list
  | ConsE of exp * exp
  | CatE of exp * exp
  | MemE of exp * exp
  | LenE of exp
  | DotE of exp * atom
  | IdxE of exp * exp
  | SliceE of exp * exp * exp
  | UpdE of exp * path * exp
  | CallE of id * targ list * arg list
  | IterE of exp * iterexp

and notexp = exp Domain.Mixfix.t
and iterexp = Sl.iterexp

(* Patterns *)

and pattern = Sl.pattern

(* Path *)

and path = (path', typ') note_phrase
and path' =
  | RootP
  | IdxP of path * exp
  | SliceP of path * exp * exp
  | DotP of path * atom

(* Type parameters *)

and tparam = Sl.tparam

(* Parameters *)

and param = param' phrase
and param' =
  | ExpP of typ * exp
  | DefP of id * tparam list * param list * typ
  | TableP of id * param list * typ

(* Type arguments *)

and targ = Sl.targ

(* Arguments *)

and arg = arg' phrase
and arg' =
  | ExpA of exp
  | DefA of id
  | TableA of id

(* Dangling *)

type dangle = Sl.dangle

(* Holding conditions *)

and holdcase =
  | BothH of block * block
  | HoldH of block * dangle
  | NotHoldH of block * dangle

(* Case analysis *)

and case = guard * block

and guard =
  | BoolG of bool
  | CmpG of cmpop * optyp * exp
  | SubG of typ
  | MatchG of pattern
  | MemG of exp
  (* Shorthands *)
  | CheckLetSubG of typ * exp
  | CheckLetMatchG of pattern * exp

(* Instructions *)

and iid = Sl.iid
and inote = Sl.inote

and instr = ((instr', inote) note_phrase) Annot.t
and instr' =
  | IfI of exp * iterexp list * block * dangle
  | HoldI of id * notexp * iterexp list * holdcase
  | CaseI of exp * case list * dangle
  | GroupI of id * id * rel_signature * exp list * block
  | TryI of arm list
  | LetI of exp * exp * iterinstr list
  | RuleI of id * notexp * Hints.Input.t * iterinstr list
  | ResultI of rel_signature * exp list
  | ReturnI of exp
  | DebugI of exp
  (* Shorthands *)
  | DestructI of (string option * exp) list * exp
  | CheckLetSubI of typ * exp * exp * block
  | CheckLetMatchI of pattern * exp * exp * block
  | OptionGetI of exp * exp * block

and block = instr list
and elseblock = instr list

and arm = block

and iterinstr = Sl.iterinstr

(* Relations *)

and rel_signature = Sl.rel_signature

type externrel = id * rel_signature * exp list

type rel = id * rel_signature * exp list * block * elseblock option

(* Functions *)

type externfunc = id * tparam list * param list * typ

type builtinfunc = id * tparam list * param list * typ

type tablerow = exp list * exp * block

type tablefunc = id * param list * typ * tablerow list

type definedfunc =
  id * tparam list * param list * typ * block * elseblock option

(* Definitions *)

type def = (def' phrase) Annot.t
and def' =
  | ExternTypD of id
  | TypD of id * tparam list * deftyp
  | VarD of id * typ
  | ExternRelD of externrel
  | RelD of rel
  | ExternDecD of externfunc
  | BuiltinDecD of builtinfunc
  | TableDecD of tablefunc
  | FuncDecD of definedfunc

(* Spec *)

type spec = def list
