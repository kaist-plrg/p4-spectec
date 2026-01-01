open Util.Source

[@@@ocamlformat "disable"]

(* Numbers *)

type num = Il.num

(* Texts *)

type text = Il.text

(* Identifiers *)

type id = Il.id

(* Atoms *)

type atom = Il.atom

(* Mixfix operatros *)

type mixop = Il.mixop

(* Iterators *)

type iter = Il.iter

(* Variables *)

type var = Il.var

(* Types *)

type typ = Il.typ
type typ' = Il.typ'

(* Operators *)

type unop = Il.unop
type binop = Il.binop
type cmpop = Il.cmpop

type optyp = Il.optyp

(* Call prose using hints *)

and hintexp = El.exp

and func_call = 
  | ProseFuncCall of
    [ `Check of id * hintexp * hintexp * targ list * arg list
    | `Yield of id * hintexp * targ list * arg list ]
  | MathFuncCall of id * targ list * arg list

and rel_call = 
  | ProseRelCall of
    [ `Hold of id * hintexp * exp list
    | `Yield of id * hintexp * exp list * exp list ]
  | MathRelCall of id * mixop * exp list

(* Expressions *)

and exp = (exp', typ') note_phrase
and exp' =
  | BoolE of bool                                   (* bool *)
  | NumE of num                                     (* num *)
  | TextE of text                                   (* text *)
  | VarE of id                                      (* id *)
  | UnE of unop * optyp * exp                       (* unop exp *)
  | BinE of binop * optyp * exp * exp               (* exp binop exp *)
  | CmpE of cmpop * optyp * exp * exp               (* exp cmpop exp *)
  | UpCastE of typ * exp                            (* exp as typ *)
  | DownCastE of typ * exp                          (* exp as typ *)
  | SubE of exp * typ                               (* exp `<:` typ *)
  | MatchE of exp * pattern                         (* exp `matches` pattern *)
  | TupleE of exp list                              (* `(` exp* `)` *)
  | CaseE of id * mixop * exp list * hintexp option (* notexp *)
  | StrE of (atom * exp) list                       (* { (atom exp)* } *)
  | OptE of exp option                              (* exp? *)
  | ListE of exp list                               (* `[` exp* `]` *)
  | ConsE of exp * exp                              (* exp `::` exp *)
  | CatE of exp * exp                               (* exp `++` exp *)
  | MemE of exp * exp                               (* exp `<-` exp *)
  | LenE of exp                                     (* `|` exp `|` *)
  | DotE of exp * atom                              (* exp.atom *)
  | IdxE of exp * exp                               (* exp `[` exp `]` *)
  | SliceE of exp * exp * exp                       (* exp `[` exp `:` exp `]` *)
  | UpdE of exp * path * exp                        (* exp `[` path `=` exp `]` *)
  | CallE of func_call                              (* func_call `<` targ* `>` `(` arg* `)` *)
  | IterE of exp * iterexp                          (* exp iterexp *)

and notexp = mixop * exp list
and iterexp = iter * var list

(* Patterns *)

and pattern = Il.pattern

(* Paths *)

and path = (path', typ') note_phrase
and path' =
  | RootP                       (* *)
  | IdxP of path * exp          (* path `[` exp `]` *)
  | SliceP of path * exp * exp  (* path `[` exp `:` exp `]` *)
  | DotP of path * atom         (* path `.` atom *)

(* Type parameters *)

and tparam = Il.tparam

(* Arguments *)

and arg = arg' phrase
and arg' =
  | ExpA of exp (* exp *)
  | DefA of id  (* `$`id *)

(* Type arguments *)

and targ = Il.targ

(* Instructions *)

type branch = If | ElseIf | Else

type cond =
  | ExpCond of exp
  | RelCond of rel_call
  | ForAllCond of cond * var list
  | ForAnyCond of cond * var list

type result =
  | ProseResult of hintexp * exp list
  | MathResult of exp list

type instr = instr' phrase
and instr' =
  (* Iteration instructions *)
  | ForEachI of var list * instr * var list
  (* Branching instructions *)
  | BranchI of branch * cond * instr list
  | OtherwiseI of instr list
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

(* Relations *)

type rel_title =
  | ProseRelTitle of
    [ `Hold of id * hintexp * exp list
    | `Yield of id * hintexp * exp list * hintexp * exp list ]
  | MathRelTitle of id * mixop * exp list

type externrel = rel_title

type rulegroup_title =
  | ProseRuleTitle of id * hintexp * exp list
  | MathRuleTitle of id * mixop * exp list

type rulegroup = rulegroup_title * instr list

type rel = rel_title * rulegroup list

(* Functions *)

type externfunc = id * tparam list * arg list * typ

type builtinfunc = id * tparam list * arg list * typ

type tablerow = exp list * exp * instr list

type tablefunc = id * arg list * typ * tablerow list

type func = id * tparam list * arg list * typ * instr list

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
