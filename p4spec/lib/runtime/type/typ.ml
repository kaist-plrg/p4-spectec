open Lang
open Xl
open Il
open Il.Print
open Util.Source

(* Type *)

type t = typ

let to_string t = string_of_typ t

(* Comparison *)

let rec compare (typ_a : t) (typ_b : t) : int =
  let tag (typ : t) =
    match typ.it with
    | BoolT -> 0
    | NumT _ -> 1
    | TextT -> 2
    | VarT _ -> 3
    | TupleT _ -> 4
    | IterT _ -> 5
    | FuncT _ -> 6
  in
  match (typ_a.it, typ_b.it) with
  | NumT numtyp_a, NumT numtyp_b -> Num.compare_typ numtyp_a numtyp_b
  | VarT (id_a, targs_a), VarT (id_b, targs_b) ->
      let cmp = String.compare id_a.it id_b.it in
      if cmp <> 0 then cmp else List.compare compare targs_a targs_b
  | TupleT typs_a, TupleT typs_b -> List.compare compare typs_a typs_b
  | IterT (typ_a, iter_a), IterT (typ_b, iter_b) ->
      let cmp = compare typ_a typ_b in
      if cmp <> 0 then cmp else Stdlib.compare iter_a iter_b
  | FuncT (tparams_a, typs_a, typ_a), FuncT (tparams_b, typs_b, typ_b) ->
      let cmp =
        List.compare
          (fun tparam_a tparam_b -> String.compare tparam_a.it tparam_b.it)
          tparams_a tparams_b
      in
      if cmp <> 0 then cmp
      else
        let cmp = List.compare compare typs_a typs_b in
        if cmp <> 0 then cmp else compare typ_a typ_b
  | _ -> Int.compare (tag typ_a) (tag typ_b)

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

  let func' (tparams : tparam list) (typs_params : typ list) (typ : typ) : typ'
      =
    FuncT (tparams, typs_params, typ)

  let func (tparams : tparam list) (typs_params : typ list) (typ : typ) : typ =
    func' tparams typs_params typ $ no_region

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

  let rec of_param_pl (param : Pl.param) : t =
    match param.it with
    | ExpP (typ, _) -> typ
    | DefP (_, tparams, params, typ) ->
        let typs_params = of_params_pl params in
        func tparams typs_params typ

  and of_params_pl (params : Pl.param list) : t list =
    List.map of_param_pl params
end
