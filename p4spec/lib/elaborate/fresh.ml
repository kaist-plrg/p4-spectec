open Domain.Lib
open Util.Source

let fresh_id (ids : IdSet.t) (id : Id.t) : Id.t =
  let ids =
    IdSet.filter
      (fun id_e ->
        let id = Xl.Var.strip_var_suffix id in
        let id_e = Xl.Var.strip_var_suffix id_e in
        id.it = id_e.it)
      ids
  in
  let rec fresh_id' (id : Id.t) : Id.t =
    if IdSet.mem id ids then fresh_id' (id.it ^ "'" $ id.at) else id
  in
  fresh_id' id

let fresh_id_from_plaintyp ?(wildcard = false) (ids : IdSet.t)
    (plaintyp : El.Ast.plaintyp) : Id.t =
  let id = El.Print.string_of_plaintyp plaintyp $ plaintyp.at in
  let id = if wildcard then "_" ^ id.it $ id.at else id in
  fresh_id ids id

let fresh_var_from_typ ?(wildcard = false) (ids : IdSet.t) (at : region)
    (typ : Il.Ast.typ) : Id.t * Il.Ast.typ * Il.Ast.iter list =
  let rec fresh_var_from_typ' (typ : Il.Ast.typ) :
      Id.t * Il.Ast.typ * Il.Ast.iter list =
    match typ.it with
    | IterT (typ, iter) ->
        let id, typ, iters = fresh_var_from_typ' typ in
        (id, typ, iters @ [ iter ])
    | _ ->
        let id = Il.Print.string_of_typ typ $ at in
        (id, typ, [])
  in
  let id, typ, iters = fresh_var_from_typ' typ in
  let id = if wildcard then "_" ^ id.it $ id.at else id in
  let id = fresh_id ids id in
  (id, typ, iters)

let fresh_var_from_exp ?(wildcard = false) (ids : IdSet.t) (exp : Il.Ast.exp) :
    Id.t * Il.Ast.typ * Il.Ast.iter list =
  fresh_var_from_typ ~wildcard ids exp.at (exp.note $ exp.at)

let fresh_exp_from_typ (ids : IdSet.t) (typ : Il.Ast.typ) : Il.Ast.exp * IdSet.t
    =
  let id_base, typ_base, iters = fresh_var_from_typ ids typ.at typ in
  let ids = IdSet.add id_base ids in
  let exp_base = Il.Ast.VarE id_base $$ (typ_base.at, typ_base.it) in
  let exp_match, _ =
    List.fold_left
      (fun (exp_match, iters) iter ->
        let typ = Il.Ast.IterT (exp_match.note $ exp_match.at, iter) in
        let var = (id_base, typ_base, iters) in
        let iterexp = (iter, [ var ]) in
        let exp_match =
          Il.Ast.IterE (exp_match, iterexp) $$ (exp_match.at, typ)
        in
        (exp_match, iters @ [ iter ]))
      (exp_base, []) iters
  in
  (exp_match, ids)
