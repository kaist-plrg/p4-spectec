open Util.Source

(* Identifiers *)

type rid = Il.Ast.id
type fid = Il.Ast.id

(* Notation *)

type mixop = Il.Ast.mixop

(* Iterators *)

type iter = Il.Ast.iter

(* Variables *)
type var = Il.Ast.var

(* Expressions *)

type exp = Il.Ast.exp

type hintexp = El.Ast.exp

(* Type parameters *)

type tparam = Il.Ast.tparam

(* Arguments *)

type arg = Il.Ast.arg

(* Branch types *)

type branchtype = If | ElseIf | Else

(* Relation renderer *)

type relcall =
  (* prose hint, outputs, inputs *)
  | Prose of hintexp * exp list * exp list
  (* mixop, exps *)
  | Mixop of mixop * exp list

type cond =
  | ExpCond of exp
  | RelCond of relcall * rid
  | ForAllCond of cond * var list
  | ForAnyCond of cond * var list

type instr = instr' phrase
and instr' =
  (* % %: \n -> % *)
  | Branch of branchtype * cond * instr list
  (* % let % be %: \n -> % *)
  | Bind of branchtype * exp * exp * instr list
  (* Otherwise: \n -> % *)
  | Otherwise of instr
  (* Check that % *)
  | Check of cond
  (* Let %, obtained by repeating %, for each % *)
  | ForEach of var list * instr * var list
  (* Let % be % *)
  | Let of exp * exp
  (* Let %exps be the result of %renderer(%exps) : %rid *)
  | Rel of relcall * rid
  (* Result in %prose_out(%exps) *)
  | Result of hintexp option * exp list
  | Return of exp
  | Group of rid * exp list * instr list

type def = def' phrase
and def' =
  | RelD of rid * exp list * instr list
  | DecD of fid * tparam list * arg list * instr list

type spec = def list
