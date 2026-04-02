open Lang
open Xl
open Il
open Il.Print
open Util.Source

(* Type *)

type t = typ

let to_string t = string_of_typ t

(* Constructor *)

module Make = struct
  let rec iterate (typ : t) (iters : iter list) : t =
    match iters with
    | [] -> typ
    | iter :: iters -> iterate (IterT (typ, iter) $ typ.at) iters

  let bool' : typ' = BoolT
  let bool : typ = bool' $ no_region
  let nat' : typ' = NumT `NatT
  let nat : typ = nat' $ no_region
  let int' : typ' = NumT `IntT
  let int : typ = int' $ no_region
  let num' (numtyp : Num.typ) : typ' = NumT numtyp
  let num (numtyp : Num.typ) : typ = num' numtyp $ no_region
  let text' : typ' = TextT
  let text : typ = text' $ no_region
  let var' (id : id) (targs : targ list) : typ' = VarT (id, targs)
  let var (id : id) (targs : targ list) : typ = var' id targs $ no_region
  let tuple' (typs : typ list) : typ' = TupleT typs
  let tuple (typs : typ list) : typ = tuple' typs $ no_region
  let iter' (typ : typ) (it : iter) : typ' = IterT (typ, it)
  let iter (typ : typ) (it : iter) : typ = iter' typ it $ no_region
  let opt' (typ : typ) : typ' = iter' typ Opt
  let opt (typ : typ) : typ = iter typ Opt
  let list' (typ : typ) : typ' = iter' typ List
  let list (typ : typ) : typ = iter typ List
  let func' : typ' = FuncT
  let func : typ = func' $ no_region
end
