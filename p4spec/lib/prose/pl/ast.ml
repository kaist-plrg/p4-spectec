open Util.Source

(* Numbers *)

type num = Il.Ast.num

(* Texts *)

type text = Il.Ast.text

(* Identifiers *)

type id = Il.Ast.id

(* Atoms *)

type atom = Il.Ast.atom

(* Mixfix operatros *)

type mixop = Il.Ast.mixop

(* Iterators *)

type iter = Il.Ast.iter

(* Variables *)

type var = Il.Ast.var
type itervars = var list

(* Types *)

type typ = Il.Ast.typ
type typ' = Il.Ast.typ'

(* Operators *)

type unop = Il.Ast.unop
type binop = Il.Ast.binop
type cmpop = Il.Ast.cmpop

type optyp = Il.Ast.optyp

(* Expressions *)
and exp = (exp', typ') note_phrase

and exp' =
  | BoolE of bool (* bool *)
  | NumE of num (* num *)
  | TextE of text (* text *)
  | VarE of id (* varid *)
  | UnE of unop * optyp * exp (* unop exp *)
  | BinE of binop * optyp * exp * exp (* exp binop exp *)
  | CmpE of cmpop * optyp * exp * exp (* exp cmpop exp *)
  | UpCastE of typ * exp (* exp as typ *)
  | DownCastE of typ * exp (* exp as typ *)
  | SubE of exp * typ (* exp `<:` typ *)
  | MatchE of exp * pattern (* exp `matches` pattern *)
  | TupleE of exp list (* `(` exp* `)` *)
  | CaseE of id * mixop * exp list * hintexp option
  | StrE of (atom * exp) list (* { expfield* } *)
  | OptE of exp option (* exp? *)
  | ListE of exp list (* `[` exp* `]` *)
  | ConsE of exp * exp (* exp `::` exp *)
  | CatE of exp * exp (* exp `++` exp *)
  | MemE of exp * exp (* exp `<-` exp *)
  | LenE of exp (* `|` exp `|` *)
  | DotE of exp * atom (* exp.atom *)
  | IdxE of exp * exp (* exp `[` exp `]` *)
  | SliceE of exp * exp * exp (* exp `[` exp `:` exp `]` *)
  | UpdE of exp * path * exp (* exp `[` path `=` exp `]` *)
  | CallE of funcprose * targ list * arg list
  | IterE of exp * iterexp (* exp iterexp *)

and notexp = mixop * exp list
and iterexp = iter * var list

(* Patterns *)
and pattern = Il.Ast.pattern

(* Path *)
and path = (path', typ') note_phrase

and path' =
  | RootP (* *)
  | IdxP of path * exp (* path `[` exp `]` *)
  | SliceP of path * exp * exp (* path `[` exp `:` exp `]` *)
  | DotP of path * atom (* path `.` atom *)

(* Arguments *)
and arg = arg' phrase

and arg' =
  | ExpA of exp
  (* exp *)
  | DefA of id (* `$`id *)

(* Type arguments *)
and targ = Il.Ast.targ

(* Function Renderers *)
and funcprose =
  (* prose_true, prose_false?, inputs *)
  | BoolProse of id * hintexp * hintexp option
  (* prose_in, inputs *)
  | InputProse of id * hintexp
  (* $def<targs>(args) *)
  | Def of id

and hintexp = El.Ast.exp

and relcall =
  (* prose hint, outputs, inputs *)
  | Prose of hintexp * exp list * exp list
  (* mixop, exps *)
  | Mixop of mixop * exp list

(* Type parameters *)

type tparam = Il.Ast.tparam

(* Branch types *)

type branchtype = If | ElseIf | Else

(* Relation renderer *)
type cond =
  | ExpCond of exp
  | RelCond of relcall * id
  (* %, for all % in % *)
  | ForAllCond of cond * itervars
  (* %, for any % in % *)
  | ForAnyCond of cond * itervars

type partial_bind =
  | Ignore
  | Var of id * string option

type instr = instr' phrase

and instr' =
  (* % %: \n -> % *)
  | BranchI of branchtype * cond * instr list
  (* Otherwise: \n -> % *)
  | OtherwiseI of instr
  (* Check that % *)
  | CheckI of cond
  (* Let %, obtained by repeating %, for each % *)
  | ForEachI of itervars * instr * itervars
  (* Let % be % *)
  | LetI of exp * exp
  (* Let %exps be the result of %renderer(%exps) : %rid *)
  | RelI of relcall * id
  (* Result in %prose_out(%exps) *)
  | ResultI of hintexp option * exp list
  | ReturnI of exp
  | GroupI of id * exp list * instr list  (** Shorthand instructions **)
  | DestructI of partial_bind list * exp
  | CheckLetI of exp * exp
  | OptionGetI of exp * exp

type def = def' phrase

and def' =
  | RelD of id * exp list * instr list
  | DecD of id * tparam list * arg list * typ * instr list

type spec = def list
