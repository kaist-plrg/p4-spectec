module Fresh_ = Fresh
open Domain
open Lib
open Lang
open Xl
open Il
open Error
open Util.Source

(* Type equivalence and subtyping *)

let rec equiv_typ (find_typdef_opt : TId.t -> Typdef.t option) (typ_a : typ)
    (typ_b : typ) : bool =
  let typ_a = Expand.expand_typ find_typdef_opt typ_a in
  let typ_b = Expand.expand_typ find_typdef_opt typ_b in
  match (typ_a.it, typ_b.it) with
  | BoolT, BoolT -> true
  | NumT numtyp_a, NumT numtyp_b -> Num.equiv numtyp_a numtyp_b
  | TextT, TextT -> true
  | VarT (tid_a, targs_a), VarT (tid_b, targs_b) ->
      tid_a.it = tid_b.it
      && List.length targs_a = List.length targs_b
      && List.for_all2 (equiv_typ find_typdef_opt) targs_a targs_b
  | TupleT typs_a, TupleT typs_b ->
      List.length typs_a = List.length typs_b
      && List.for_all2 (equiv_typ find_typdef_opt) typs_a typs_b
  | IterT (typ_a, iter_a), IterT (typ_b, iter_b) ->
      equiv_typ find_typdef_opt typ_a typ_b && iter_a = iter_b
  | _ -> false

and equiv_nottyp (find_typdef_opt : TId.t -> Typdef.t option)
    (nottyp_a : nottyp) (nottyp_b : nottyp) : bool =
  Mixfix.eq ~eq_arg:(equiv_typ find_typdef_opt) nottyp_a.it nottyp_b.it

and equiv_functyp (find_typdef_opt : TId.t -> Typdef.t option) (at : region)
    (tparams_a : tparam list) (typs_params_a : typ list) (typ_a : typ)
    (tparams_b : tparam list) (typs_params_b : typ list) (typ_b : typ) : bool =
  check
    (List.length tparams_a = List.length tparams_b)
    no_region "type parameters do not match";
  let tids_fresh, theta_a, theta_b =
    List.fold_left2
      (fun (tids_fresh, theta_a, theta_b) tparam_a tparam_b ->
        let tid_fresh =
          "__FRESH" ^ string_of_int (Fresh_.fresh ()) $ no_region
        in
        let typ_fresh = VarT (tid_fresh, []) $ no_region in
        let tids_fresh = TIdSet.add tid_fresh tids_fresh in
        let theta_a = TIdMap.add tparam_a typ_fresh theta_a in
        let theta_b = TIdMap.add tparam_b typ_fresh theta_b in
        (tids_fresh, theta_a, theta_b))
      (TIdSet.empty, TIdMap.empty, TIdMap.empty)
      tparams_a tparams_b
  in
  let find_typdef_opt tid =
    if TIdSet.mem tid tids_fresh then Some Typdef.Param else find_typdef_opt tid
  in
  check
    (List.length typs_params_a = List.length typs_params_b)
    at "parameters do not match";
  let typs_params_a = Subst.subst_typs theta_a typs_params_a in
  let typs_params_b = Subst.subst_typs theta_b typs_params_b in
  let typ_a = Subst.subst_typ theta_a typ_a in
  let typ_b = Subst.subst_typ theta_b typ_b in
  List.for_all2 (equiv_typ find_typdef_opt) typs_params_a typs_params_b
  && equiv_typ find_typdef_opt typ_a typ_b
