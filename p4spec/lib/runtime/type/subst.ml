module Fresh_ = Fresh
open Domain.Lib
module Mixfix = Domain.Mixfix
open Lang
open Il
open Error
open Util.Source

(* Substitution of type variables *)

type theta = Typ.t TIdMap.t

let freshen_tparams (tparams : tparam list) : theta * tparam list =
  List.fold_left
    (fun (theta, tids_fresh) tparam ->
      let tid_fresh = "__FRESH" ^ string_of_int (Fresh_.fresh ()) $ no_region in
      let typ_fresh = VarT (tid_fresh, []) $ no_region in
      let theta = TIdMap.add tparam typ_fresh theta in
      (theta, tids_fresh @ [ tid_fresh ]))
    (TIdMap.empty, []) tparams

(* Types *)

let rec subst_typ_inner (theta : theta) (typ : typ) : typ =
  match typ.it with
  | BoolT | NumT _ | TextT -> typ
  | VarT (tid, targs) -> (
      match TIdMap.find_opt tid theta with
      | Some _ when targs <> [] ->
          error typ.at "higher-order substitution is disallowed"
      | Some typ -> typ
      | None ->
          let targs = subst_typs_inner theta targs in
          VarT (tid, targs) $ typ.at)
  | TupleT typs ->
      let typs = subst_typs_inner theta typs in
      TupleT typs $ typ.at
  | IterT (typ, iter) ->
      let typ = subst_typ_inner theta typ in
      IterT (typ, iter) $ typ.at
  | FuncT (tparams, typs_params, typ_ret) ->
      let theta_fresh, tparams = freshen_tparams tparams in
      let typs_params =
        typs_params |> subst_typs_inner theta_fresh |> subst_typs_inner theta
      in
      let typ_ret =
        typ_ret |> subst_typ_inner theta_fresh |> subst_typ_inner theta
      in
      FuncT (tparams, typs_params, typ_ret) $ typ.at

and subst_typs_inner (theta : theta) (typs : typ list) : typ list =
  List.map (subst_typ_inner theta) typs

let subst_typ (theta : theta) (typ : typ) : typ =
  if TIdMap.is_empty theta then typ else subst_typ_inner theta typ

let subst_typs (theta : theta) (typs : typ list) : typ list =
  if TIdMap.is_empty theta then typs else subst_typs_inner theta typs

(* Variant types *)

let subst_nottyp (theta : theta) (nottyp : nottyp) : nottyp =
  if TIdMap.is_empty theta then nottyp
  else Mixfix.map (subst_typ theta) nottyp.it $ nottyp.at

let subst_typcase (theta : theta) (typcase : typcase) : typcase =
  let nottyp, typorigin, hints = typcase in
  let nottyp = subst_nottyp theta nottyp in
  let typorigin =
    let id, targs = typorigin.it in
    let targs = subst_typs theta targs in
    (id, targs) $ typorigin.at
  in
  (nottyp, typorigin, hints)

(* Parameters *)

let rec subst_param (theta : theta) (param : param) : param =
  match param.it with
  | ExpP typ ->
      let typ = subst_typ theta typ in
      ExpP typ $ param.at
  | DefP (id, tparams, params, typ) ->
      let theta_fresh, tparams = freshen_tparams tparams in
      let params = params |> subst_params theta_fresh |> subst_params theta in
      let typ = typ |> subst_typ theta_fresh |> subst_typ theta in
      DefP (id, tparams, params, typ) $ param.at

and subst_params (theta : theta) (params : param list) : param list =
  List.map (subst_param theta) params
