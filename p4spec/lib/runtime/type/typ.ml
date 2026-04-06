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

  let bool : typ = BoolT $ no_region
  let nat : typ = NumT `NatT $ no_region
  let int : typ = NumT `IntT $ no_region
  let num (numtyp : Num.typ) : typ = NumT numtyp $ no_region
  let text : typ = TextT $ no_region
  let var (id : id) (targs : t list) : typ = VarT (id, targs) $ no_region
  let tuple (typs : t list) : t = TupleT typs $ no_region
  let iter (typ : t) (iter : iter) : t = IterT (typ, iter) $ no_region
  let opt (typ : t) : t = iter typ Opt
  let list (typ : t) : t = iter typ List

  let func (tparams : tparam list) (typs_params : typ list) (typ : typ) : t =
    FuncT (tparams, typs_params, typ) $ no_region

  let rec of_param_il (param : Il.param) : t =
    match param.it with
    | ExpP typ -> typ
    | DefP (_, tparams, params, typ) ->
        let typs_params = of_params_il params in
        func tparams typs_params typ

  and of_params_il (params : Il.param list) : t list =
    List.map of_param_il params

  let rec of_param_sl (param : Sl.param) : t =
    match param.it with
    | ExpP (typ, _) -> typ
    | DefP (_, tparams, params, typ) ->
        let typs_params = of_params_sl params in
        func tparams typs_params typ

  and of_params_sl (params : Sl.param list) : t list =
    List.map of_param_sl params
end
