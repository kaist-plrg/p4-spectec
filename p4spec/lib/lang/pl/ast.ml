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

(* Mixfix operatros *)

type mixop = Sl.mixop

(* Iterators *)

type iter = Sl.iter

(* Variables *)

type var = Sl.var

(* Types *)

type typ = Sl.typ
type typ' = Sl.typ'

(* Operators *)

type unop = Sl.unop
type binop = Sl.binop
type cmpop = Sl.cmpop

type optyp = Sl.optyp

(* Call prose using hints *)

and func_call = 
  | ProseFuncCall of
    [ `Check of id * Hints.Alter.t * Hints.Alter.t * targ list * arg list
    | `Yield of id * Hints.Alter.t * targ list * arg list ]
  | MathFuncCall of id * targ list * arg list

and rel_call = 
  | ProseRelCall of
    [ `Hold of id * Hints.Alter.t * exp list
    | `Yield of id * Hints.Alter.t * exp list * Hints.Alter.t * exp list ]
  | MathRelCall of id * mixop * exp list

(* Expressions *)

and exp = (exp', typ') note_phrase
and exp' =
  | BoolE of bool                                           (* bool *)
  | NumE of num                                             (* num *)
  | TextE of text                                           (* text *)
  | VarE of id                                              (* id *)
  | UnE of unop * optyp * exp                               (* unop exp *)
  | BinE of binop * optyp * exp * exp                       (* exp binop exp *)
  | CmpE of cmpop * optyp * exp * exp                       (* exp cmpop exp *)
  | UpCastE of typ * exp                                    (* exp as typ *)
  | DownCastE of typ * exp                                  (* exp as typ *)
  | SubE of exp * typ                                       (* exp `<:` typ *)
  | MatchE of exp * pattern                                 (* exp `matches` pattern *)
  | TupleE of exp list                                      (* `(` exp* `)` *)
  | CaseE of id * mixop * exp list * Hints.Alter.t option   (* notexp *)
  | StrE of (atom * exp) list                               (* { (atom exp)* } *)
  | OptE of exp option                                      (* exp? *)
  | ListE of exp list                                       (* `[` exp* `]` *)
  | ConsE of exp * exp                                      (* exp `::` exp *)
  | CatE of exp * exp                                       (* exp `++` exp *)
  | MemE of exp * exp                                       (* exp `<-` exp *)
  | LenE of exp                                             (* `|` exp `|` *)
  | DotE of exp * atom                                      (* exp.atom *)
  | IdxE of exp * exp                                       (* exp `[` exp `]` *)
  | SliceE of exp * exp * exp                               (* exp `[` exp `:` exp `]` *)
  | UpdE of exp * path * exp                                (* exp `[` path `=` exp `]` *)
  | CallE of func_call                                      (* func_call `<` targ* `>` `(` arg* `)` *)
  | IterE of exp * iterexp                                  (* exp iterexp *)

and notexp = mixop * exp list
and iterexp = Sl.iterexp

(* Patterns *)

and pattern = Sl.pattern

(* Paths *)

and path = (path', typ') note_phrase
and path' =
  | RootP                       (* *)
  | IdxP of path * exp          (* path `[` exp `]` *)
  | SliceP of path * exp * exp  (* path `[` exp `:` exp `]` *)
  | DotP of path * atom         (* path `.` atom *)

(* Parameters *)

and param = param' phrase
and param' =
  | ExpP of typ * exp     (* typ exp *)
  | DefP of id            (* `$`id *)

(* Type parameters *)

and tparam = Sl.tparam

(* Arguments *)

and arg = arg' phrase
and arg' =
  | ExpA of exp (* exp *)
  | DefA of id  (* `$`id *)

(* Type arguments *)

and targ = Sl.targ

(* Instructions *)

type branch = If | ElseIf | Else

type cond =
  | ExpCond of exp
  | RelCond of rel_call
  | ForAllCond of cond * var list
  | ForAnyCond of cond * var list

type result =
  | ProseResult of
    [ `Hold | `Yield of Hints.Alter.t * exp list ]
  | MathResult of exp list

type instr = instr' phrase
and instr' =
  (* Iteration instructions *)
  | ForEachI of var list * instr * var list
  (* Branching instructions *)
  | BranchI of branch * cond * block
  | OtherwiseI of block
  | CheckI of cond
  (* Binding instructions *)
  | LetI of exp * exp
  | RuleI of rel_call
  (* Result/Return instructions *)
  | ResultI of result
  | ReturnI of exp
  (* Shorthands *)
  | DestructI of (exp * string) list * exp
  | CheckLetI of exp * exp
  | OptionGetI of exp * exp

and block = instr list

(* Relations *)

type rel_title =
  | ProseRelTitle of
    [ `Hold of id * Hints.Alter.t * exp list
    | `Yield of id * Hints.Alter.t * exp list * Hints.Alter.t * exp list ]
  | MathRelTitle of id * mixop * exp list

type externrel = rel_title

type rulegroup_title =
  | ProseRuleTitle of
    [ `Hold of id * Hints.Alter.t * exp list
    | `Yield of id * Hints.Alter.t * exp list ]
  | MathRuleTitle of id * mixop * exp list

type rulegroup = rulegroup_title * block

type rel = rel_title * rulegroup list

(* Functions *)

type func_title =
  | ProseFuncTitle of
    [ `Check of id * Hints.Alter.t * param list
    | `Yield of id * Hints.Alter.t * param list ]
  | MathFuncTitle of id * tparam list * param list

type externfunc = func_title

type builtinfunc = func_title

type tablerow = exp list * exp * block

type tablefunc = func_title * tablerow list

type func = func_title * block

(* Definitions *)

type def = def' phrase
and def' =
  | ExternRelD of externrel
  | RelD of rel
  | ExternDecD of externfunc
  | BuiltinDecD of builtinfunc
  | TableDecD of tablefunc
  | FuncDecD of func

(* Spec *)

type spec = def list
