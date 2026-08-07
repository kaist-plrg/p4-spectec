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

(* Type arguments *)

and targ = Sl.targ

(* Arguments *)

and arg = arg' phrase
and arg' =
  | ExpA of exp
  | DefA of id

(* Dangling *)

type dangle = Sl.dangle

(* Holding conditions -- shared control-flow, parametric over the tier's own instruction *)

and 'tier holdcase =
  | BothH of 'tier block * 'tier block
  | HoldH of 'tier block * dangle
  | NotHoldH of 'tier block * dangle

(* Case analysis *)

and 'tier case = guard * 'tier block

and guard =
  | BoolG of bool
  | CmpG of cmpop * optyp * exp
  | SubG of typ
  | MatchG of pattern
  | MemG of exp
  (* Shorthands *)
  | CheckLetSubG of typ * exp
  | CheckLetMatchG of pattern * exp

(* Backtracking *)

and 'tier arm = 'tier block

(* Instructions -- shared control-flow shape common to both tiers. The
   tier-specific instructions (rule group / result / return / rule, and the
   backtracking-or-routing [BlockI]) are carried by [TierI]. *)

and iid = Sl.iid
and fallthrough = FallGroup of id | FallNext | FallElse | FallFail
and inote = { iid : iid; fallthrough : fallthrough option }

and 'tier instr = (('tier instr', inote) note_phrase) Annot.t
and 'tier instr' =
  | IfI of exp * iterexp list * 'tier block * dangle
  | HoldI of id * notexp * iterexp list * 'tier holdcase
  | CaseI of exp * 'tier case list * dangle
  | LetI of exp * exp * iterinstr list
  | DebugI of exp
  (* Shorthands *)
  | DestructI of (string option * exp) list * exp
  | CheckLetSubI of typ * exp * exp * 'tier block
  | CheckLetMatchI of pattern * exp * exp * 'tier block
  | OptionGetI of exp * exp * 'tier block
  (* Tier-specific instruction *)
  | TierI of 'tier

and 'tier block = 'tier instr list

and iterinstr = Sl.iterinstr

(* Relations *)

and rel_signature = Sl.rel_signature

(* Group-body tier: result/return/rule-application, or a backtracking block
   whose arms are group blocks; no GroupI reachable *)

type instr_group =
  | ResultI of rel_signature * exp list
  | ReturnI of exp
  | RuleI of id * notexp * Hints.Input.t * iterinstr list
  | BlockI of block_group list

and block_group = instr_group block

(* Dispatch tier: rule groups (a group's body is a group block, so groups
   never nest), or a routing block whose arms are dispatch blocks *)

type instr_dispatch =
  | GroupI of id * id * rel_signature * exp list * block_group
  | BlockI of block_dispatch list

and block_dispatch = instr_dispatch block

type externrel = id * rel_signature * exp list

type rel = id * rel_signature * exp list * block_dispatch * block_dispatch option

(* Functions *)

type externfunc = id * tparam list * param list * typ

type builtinfunc = id * tparam list * param list * typ

type tablerow = exp list * exp * block_group

type tablefunc = id * param list * typ * tablerow list

type definedfunc =
  id * tparam list * param list * typ * block_group * block_group option

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
