open Domain.Lib
open Lang
open Il
open Il.Print
open Util.Error
open Util.Source

(* Type *)

type t = typ

let to_string t = string_of_typ t

(* Constructor *)

let rec iterate (typ : t) (iters : iter list) : t =
  match iters with
  | [] -> typ
  | iter :: iters -> iterate (IterT (typ, iter) $ typ.at) iters

(* Substitution of type variables *)

type theta = t TIdMap.t

let rec subst_typ_inner (theta : theta) (typ : t) : t =
  match typ.it with
  | BoolT | NumT _ | TextT -> typ
  | VarT (tid, targs) -> (
      match TIdMap.find_opt tid theta with
      | Some _ when targs <> [] ->
          error_interp typ.at "higher-order substitution is disallowed"
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
  | FuncT -> typ

and subst_typs_inner (theta : theta) (typs : t list) : t list =
  List.map (subst_typ_inner theta) typs

let subst_typ (theta : theta) (typ : t) : t =
  if TIdMap.is_empty theta then typ else subst_typ_inner theta typ

let subst_typs (theta : theta) (typs : t list) : t list =
  if TIdMap.is_empty theta then typs else subst_typs_inner theta typs
