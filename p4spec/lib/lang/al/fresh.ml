open Domain.Lib
open Ast
open Util.Source

let id (ids : IdSet.t) (id_ : Id.t) : Id.t =
  let ids =
    IdSet.filter
      (fun id_e ->
        let id_ = Xl.Var.strip_var_suffix id_ in
        let id_e = Xl.Var.strip_var_suffix id_e in
        id_.it = id_e.it)
      ids
  in
  let rec id' (id_ : Id.t) : Id.t =
    if IdSet.mem id_ ids then id' (id_.it ^ "'" $ id_.at) else id_
  in
  id' id_

let var_from_typ ?(wildcard = false) (metavars : typ TIdMap.t) (ids : IdSet.t)
    (at : region) (typ : typ) : Var.t =
  let rec var_from_typ' (typ : typ) : Var.t =
    let vars_alias =
      metavars |> TIdMap.bindings
      |> List.filter_map (fun (tid, typ_alias) ->
             if
               Eq.eq_typ typ typ_alias
               && not (String.equal (Print.string_of_typ typ) tid.it)
             then Some (tid, typ_alias, [])
             else None)
    in
    match vars_alias with [ var_alias ] -> var_alias | _ -> var_from_typ'' typ
  and var_from_typ'' (typ : typ) : Var.t =
    match typ.it with
    | IterT (typ, iter) ->
        let id, typ, iters = var_from_typ' typ in
        (id, typ, iters @ [ iter ])
    | _ ->
        let id = Print.string_of_typ typ $ at in
        (id, typ, [])
  in
  let id_var, typ_var, iters_var = var_from_typ' typ in
  let id_var = if wildcard then "_" ^ id_var.it $ id_var.at else id_var in
  let id_var = id ids id_var in
  (id_var, typ_var, iters_var)

let var_from_exp ?(wildcard = false) (metavars : typ TIdMap.t) (ids : IdSet.t)
    (exp : exp) : Var.t =
  var_from_typ ~wildcard metavars ids exp.at (exp.note $ exp.at)

let exp_from_typ ~(dim : bool) (metavars : typ TIdMap.t) (ids : IdSet.t)
    (typ : typ) : IdSet.t * exp =
  let id, typ, iters = var_from_typ metavars ids typ.at typ in
  let ids = IdSet.add id ids in
  let exp = Var.as_exp ~dim (id, typ, iters) in
  (ids, exp)
