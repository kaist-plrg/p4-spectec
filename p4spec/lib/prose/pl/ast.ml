open Util.Source

(* Identifiers *)

type rid = Il.Ast.id
type fid = Il.Ast.id

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

type cond =
  | ExpCond of exp
  (* prose_true/false, exps *)
  | RelCond of hintexp option * exp list * rid

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
  (* Let %,%,... be the result of %(%,%,...) :% *)
  | Rel of exp list * hintexp option * exp list * rid
  (* Result in %(%) *)
  | Result of hintexp option * exp list


type rel = rid * exp list * instr list

type func = fid * tparam list * arg list * instr list

type def = def' phrase
and def' =
  | RelD of rel
  | DecD of func

type spec = def list
