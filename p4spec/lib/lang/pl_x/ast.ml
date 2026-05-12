open Util.Source

[@@@ocamlformat "disable"]

type num = Sl.num
type text = Sl.text
type id = Sl.id
type atom = Sl.atom
type mixop = Sl.mixop
type iter = Sl.iter
type var = Sl.var

type value = Sl.value

type typ = Sl.typ
type typ' = Sl.typ'
type nottyp = Sl.nottyp
type nottyp' = Sl.nottyp'
type deftyp = Sl.deftyp
type deftyp' = Sl.deftyp'
type typfield = Sl.typfield
type typcase = Sl.typcase

type unop = Sl.unop
type binop = Sl.binop
type cmpop = Sl.cmpop
type optyp = Sl.optyp

type pattern = Sl.pattern
type tparam = Sl.tparam
type targ = Sl.targ
type iterexp = Sl.iterexp
type iterinstr = Sl.iterinstr
type dangle = Sl.dangle
type iid = Sl.iid
type inote = Sl.inote
type rel_signature = Sl.rel_signature

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

and notexp = mixop * exp list

(* Paths *)

and path = (path', typ') note_phrase
and path' =
  | RootP
  | IdxP of path * exp
  | SliceP of path * exp * exp
  | DotP of path * atom

(* Arguments *)

and arg = arg' phrase
and arg' =
  | ExpA of exp
  | DefA of id

(* Parameters *)

and param = param' phrase
and param' =
  | ExpP of typ * exp
  | DefP of id

(* Guards and blocks *)

and holdcase =
  | BothH of block * block
  | HoldH of block * dangle
  | NotHoldH of block * dangle

and case = guard * block

and guard =
  | BoolG of bool
  | CmpG of cmpop * optyp * exp
  | SubG of typ
  | MatchG of pattern
  | MemG of exp
  (* Shorthands — only emitted by the shorten pass. *)
  | CheckLetSubG of typ * exp        (* scrut <: typ, bind scrut as exp *)
  | CheckLetMatchG of pattern * exp  (* scrut matches pattern, bind scrut as exp *)

and instr = ((instr', inote) note_phrase) Annot.t
and instr' =
  | IfI of exp * iterexp list * block * dangle
  | HoldI of id * notexp * iterexp list * holdcase
  | CaseI of exp * case list * dangle
  | GroupI of id * id * rel_signature * exp list * block
  | TryI of block list
  | LetI of exp * exp * iterinstr list
  | RuleI of id * notexp * Hints.Input.t * iterinstr list
  | ResultI of rel_signature * exp list
  | ReturnI of exp
  | DebugI of exp
  (* Shorthands *)
  (* DestructI (fields, exp_source): at runtime, destructure [exp_source]
     (a CaseV) positionally against [fields]. Each field is [(name_opt,
     exp_target)]: the renderer formats visible (Some) fields and skips
     underscore (None) ones; the interpreter assigns each [exp_target]
     to the corresponding CaseV value regardless. *)
  | DestructI of (string option * exp) list * exp
  (* CheckLetI (exp_target, exp_source, body): bind [exp_target] to
     [exp_source] after a subtype-or-match check on [exp_source], then
     run [body]. *)
  | CheckLetI of exp * exp * block
  (* OptionGetI (exp_target, exp_source): bind [exp_target] to the
     inner value of [exp_source], asserting [exp_source] is [Some _]. *)
  | OptionGetI of exp * exp

and block = instr list
and elseblock = instr list

(* Definitions *)

type externrel = id * rel_signature * exp list
type rel = id * rel_signature * exp list * block * elseblock option

type externfunc = id * tparam list * param list * typ
type builtinfunc = id * tparam list * param list * typ
type tablerow = exp list * exp * block
type tablefunc = id * param list * typ * tablerow list
type definedfunc =
  id * tparam list * param list * typ * block * elseblock option


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

type spec = def list
