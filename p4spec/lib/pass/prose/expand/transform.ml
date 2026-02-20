module Var = Free.Var
module Vars = Free.Vars
module VarSet = Free.VarSet
open Domain.Lib
open Lang
open Il
open Util.Source

let ( let* ) = Option.bind
let ( ++ ) = VarSet.union

let rec choose_sequential = function
  | [] -> None
  | f :: fs -> (
      match f () with Some a -> Some a | None -> choose_sequential fs)

type iter_state = {
  vars_inner : VarSet.t;
  var_new : VarSet.elt;
  vars_outer : VarSet.t;
  iterexps : iterexp list;
  exp_orig : exp;
  exp_new : exp;
  ids_used : IdSet.t;
}

let transform_list (f_transform_opt : 'a -> ('a * iter_state) option)
    (f_free : 'a -> VarSet.t) (nodes : 'a list) : ('a list * iter_state) option
    =
  let res, vars_outer, nodes_changed =
    List.fold_left
      (fun (res_prev, vars_outer, nodes_changed) node ->
        match res_prev with
        | Some _ ->
            (* If transformation already succeeded, just update free *)
            let vars_outer = vars_outer ++ f_free node in
            (* Pass node as-is *)
            let nodes_changed = nodes_changed @ [ node ] in
            (res_prev, vars_outer, nodes_changed)
        | None -> (
            let res = f_transform_opt node in
            match res with
            | Some (node_changed, _) ->
                let nodes_changed = nodes_changed @ [ node_changed ] in
                (res, vars_outer, nodes_changed)
            | None ->
                let vars_outer = vars_outer ++ f_free node in
                let nodes_changed = nodes_changed @ [ node ] in
                (res, vars_outer, nodes_changed)))
      (None, VarSet.empty, []) nodes
  in
  let* _, iter_state = res in
  let iter_state =
    { iter_state with vars_outer = iter_state.vars_outer ++ vars_outer }
  in
  Some (nodes_changed, iter_state)

let transform_first_with_iters
    (f_transform_opt : 'acc -> exp -> (exp * iter_state) option)
    (f_update_acc : 'acc -> exp -> 'acc) (acc : 'acc) (e : exp) :
    (exp * iter_state) option =
  let rec transform_exp acc (e : exp) : (exp * iter_state) option =
    let acc = f_update_acc acc e in
    let try_root () = f_transform_opt acc e in
    let try_children () =
      let { it; at; note } = e in
      match it with
      | BoolE _ | NumE _ | TextE _ | VarE _ -> None
      | UnE (unop, optyp, exp_inner) ->
          let* exp_inner', iter_state = transform_exp acc exp_inner in
          Some (UnE (unop, optyp, exp_inner') $$ (at, note), iter_state)
      | BinE (binop, optyp, exp_l, exp_r) ->
          let try_left () =
            let* exp_l', iter_state = transform_exp acc exp_l in
            let vars_r = Vars.free_exp exp_r in
            let iter_state =
              { iter_state with vars_outer = iter_state.vars_outer ++ vars_r }
            in
            Some (BinE (binop, optyp, exp_l', exp_r) $$ (at, note), iter_state)
          in
          let try_right () =
            let* exp_r', iter_state = transform_exp acc exp_r in
            let vars_l = Vars.free_exp exp_l in
            let iter_state =
              { iter_state with vars_outer = iter_state.vars_outer ++ vars_l }
            in
            Some (BinE (binop, optyp, exp_l, exp_r') $$ (at, note), iter_state)
          in
          choose_sequential [ try_left; try_right ]
      | CmpE (cmpop, optyp, exp_l, exp_r) ->
          let try_left () =
            let* exp_l', iter_state = transform_exp acc exp_l in
            let vars_r = Vars.free_exp exp_r in
            let iter_state =
              { iter_state with vars_outer = iter_state.vars_outer ++ vars_r }
            in
            Some (CmpE (cmpop, optyp, exp_l', exp_r) $$ (at, note), iter_state)
          in
          let try_right () =
            let* exp_r', iter_state = transform_exp acc exp_r in
            let vars_l = Vars.free_exp exp_l in
            let iter_state =
              { iter_state with vars_outer = iter_state.vars_outer ++ vars_l }
            in
            Some (CmpE (cmpop, optyp, exp_l, exp_r') $$ (at, note), iter_state)
          in
          choose_sequential [ try_left; try_right ]
      | UpCastE (typ, exp_inner) ->
          let* exp_inner', iter_state = transform_exp acc exp_inner in
          Some (UpCastE (typ, exp_inner') $$ (at, note), iter_state)
      | DownCastE (typ, exp_inner) ->
          let* exp_inner', iter_state = transform_exp acc exp_inner in
          Some (DownCastE (typ, exp_inner') $$ (at, note), iter_state)
      | SubE (exp_inner, typ) ->
          let* exp_inner', iter_state = transform_exp acc exp_inner in
          Some (SubE (exp_inner', typ) $$ (at, note), iter_state)
      | MatchE (exp_inner, pattern) ->
          let* exp_inner', iter_state = transform_exp acc exp_inner in
          Some (MatchE (exp_inner', pattern) $$ (at, note), iter_state)
      | TupleE exps ->
          let* exps', iter_state = transform_exps acc exps in
          Some (TupleE exps' $$ (at, note), iter_state)
      | CaseE (mixop, exps) ->
          let* exps', iter_state = transform_exps acc exps in
          Some (CaseE (mixop, exps') $$ (at, note), iter_state)
      | StrE fields ->
          let atoms, values = List.split fields in
          let* values', iter_state = transform_exps acc values in
          Some (StrE (List.combine atoms values') $$ (at, note), iter_state)
      | OptE (Some exp_inner) ->
          let* exp_inner', iter_state = transform_exp acc exp_inner in
          Some (OptE (Some exp_inner') $$ (at, note), iter_state)
      | OptE None -> None
      | ListE exps ->
          let* exps', iter_state = transform_exps acc exps in
          Some (ListE exps' $$ (at, note), iter_state)
      | ConsE (exp_h, exp_t) ->
          let try_head () =
            let* exp_h', iter_state = transform_exp acc exp_h in
            let vars_t = Vars.free_exp exp_t in
            let iter_state =
              { iter_state with vars_outer = iter_state.vars_outer ++ vars_t }
            in
            Some (ConsE (exp_h', exp_t) $$ (at, note), iter_state)
          in
          let try_tail () =
            let* exp_t', iter_state = transform_exp acc exp_t in
            let vars_h = Vars.free_exp exp_h in
            let iter_state =
              { iter_state with vars_outer = iter_state.vars_outer ++ vars_h }
            in
            Some (ConsE (exp_h, exp_t') $$ (at, note), iter_state)
          in
          choose_sequential [ try_head; try_tail ]
      | CatE (exp_l, exp_r) ->
          let try_left () =
            let* exp_l', iter_state = transform_exp acc exp_l in
            let vars_r = Vars.free_exp exp_r in
            let iter_state =
              { iter_state with vars_outer = iter_state.vars_outer ++ vars_r }
            in
            Some (CatE (exp_l', exp_r) $$ (at, note), iter_state)
          in
          let try_right () =
            let* exp_r', iter_state = transform_exp acc exp_r in
            let vars_l = Vars.free_exp exp_l in
            let iter_state =
              { iter_state with vars_outer = iter_state.vars_outer ++ vars_l }
            in
            Some (CatE (exp_l, exp_r') $$ (at, note), iter_state)
          in
          choose_sequential [ try_left; try_right ]
      | MemE (exp_l, exp_r) ->
          let try_left () =
            let* exp_l', iter_state = transform_exp acc exp_l in
            let vars_r = Vars.free_exp exp_r in
            let iter_state =
              { iter_state with vars_outer = iter_state.vars_outer ++ vars_r }
            in
            Some (MemE (exp_l', exp_r) $$ (at, note), iter_state)
          in
          let try_right () =
            let* exp_r', iter_state = transform_exp acc exp_r in
            let vars_l = Vars.free_exp exp_l in
            let iter_state =
              { iter_state with vars_outer = iter_state.vars_outer ++ vars_l }
            in
            Some (MemE (exp_l, exp_r') $$ (at, note), iter_state)
          in
          choose_sequential [ try_left; try_right ]
      | LenE exp_inner ->
          let* exp_inner', iter_state = transform_exp acc exp_inner in
          Some (LenE exp_inner' $$ (at, note), iter_state)
      | DotE (exp_inner, atom) ->
          let* exp_inner', iter_state = transform_exp acc exp_inner in
          Some (DotE (exp_inner', atom) $$ (at, note), iter_state)
      | IdxE (exp_b, exp_i) ->
          let try_base () =
            let* exp_b', iter_state = transform_exp acc exp_b in
            let vars_i = Vars.free_exp exp_i in
            let iter_state =
              { iter_state with vars_outer = iter_state.vars_outer ++ vars_i }
            in
            Some (IdxE (exp_b', exp_i) $$ (at, note), iter_state)
          in
          let try_index () =
            let* exp_i', iter_state = transform_exp acc exp_i in
            let vars_b = Vars.free_exp exp_b in
            let iter_state =
              { iter_state with vars_outer = iter_state.vars_outer ++ vars_b }
            in
            Some (IdxE (exp_b, exp_i') $$ (at, note), iter_state)
          in
          choose_sequential [ try_base; try_index ]
      | SliceE (exp_b, exp_l, exp_h) ->
          let try_base () =
            let* exp_b', iter_state = transform_exp acc exp_b in
            let vars_l = Vars.free_exp exp_l in
            let vars_h = Vars.free_exp exp_h in
            let iter_state =
              {
                iter_state with
                vars_outer = iter_state.vars_outer ++ vars_l ++ vars_h;
              }
            in
            Some (SliceE (exp_b', exp_l, exp_h) $$ (at, note), iter_state)
          in
          let try_low () =
            let* exp_l', iter_state = transform_exp acc exp_l in
            let vars_b = Vars.free_exp exp_b in
            let vars_h = Vars.free_exp exp_h in
            let iter_state =
              {
                iter_state with
                vars_outer = iter_state.vars_outer ++ vars_b ++ vars_h;
              }
            in
            Some (SliceE (exp_b, exp_l', exp_h) $$ (at, note), iter_state)
          in
          let try_high () =
            let* exp_h', iter_state = transform_exp acc exp_h in
            let vars_b = Vars.free_exp exp_b in
            let vars_l = Vars.free_exp exp_l in
            let iter_state =
              {
                iter_state with
                vars_outer = iter_state.vars_outer ++ vars_b ++ vars_l;
              }
            in
            Some (SliceE (exp_b, exp_l, exp_h') $$ (at, note), iter_state)
          in
          choose_sequential [ try_base; try_low; try_high ]
      | UpdE (exp_b, path, exp_f) ->
          let try_base () =
            let* exp_b', iter_state = transform_exp acc exp_b in
            let vars_path = Vars.free_path path in
            let vars_f = Vars.free_exp exp_f in
            let iter_state =
              {
                iter_state with
                vars_outer = iter_state.vars_outer ++ vars_path ++ vars_f;
              }
            in
            Some (UpdE (exp_b', path, exp_f) $$ (at, note), iter_state)
          in
          let try_path () =
            let* path', iter_state = transform_path acc path in
            let vars_b = Vars.free_exp exp_b in
            let vars_f = Vars.free_exp exp_f in
            let iter_state =
              {
                iter_state with
                vars_outer = iter_state.vars_outer ++ vars_b ++ vars_f;
              }
            in
            Some (UpdE (exp_b, path', exp_f) $$ (at, note), iter_state)
          in
          let try_field () =
            let* exp_f', iter_state = transform_exp acc exp_f in
            let vars_b = Vars.free_exp exp_b in
            let vars_path = Vars.free_path path in
            let iter_state =
              {
                iter_state with
                vars_outer = iter_state.vars_outer ++ vars_b ++ vars_path;
              }
            in
            Some (UpdE (exp_b, path, exp_f') $$ (at, note), iter_state)
          in
          choose_sequential [ try_base; try_path; try_field ]
      | CallE (funcprose, targs, args) ->
          let* args_new, iter_state = transform_args acc args in
          Some (CallE (funcprose, targs, args_new) $$ (at, note), iter_state)
      | IterE (exp_inner, (iter, vars)) ->
          let* exp_inner', iter_state = transform_exp acc exp_inner in
          let { vars_inner; vars_outer; var_new; iterexps; _ } = iter_state in
          (* main algorithm : compare / replace / increment iterations *)
          let vars_inner, var_new, iterexps, vars =
            VarSet.fold
              (fun var_inner
                   (vars_inner_acc, var_new_acc, iterexps_acc, vars_acc) ->
                if List.mem var_inner vars then
                  let vars_upd =
                    (* Used outside CallE *)
                    if VarSet.mem var_inner vars_outer then var_new :: vars_acc
                    else
                      var_new
                      :: List.filter
                           (fun v -> not (Var.equal v var_inner))
                           vars_acc
                  in
                  let var_new_upd =
                    let id, typ, iters = var_new_acc in
                    (id, typ, iters @ [ iter ])
                  in
                  let vars_inner_upd =
                    VarSet.map
                      (fun v ->
                        if Var.equal v var_inner then
                          let id, typ, iters = var_inner in
                          (id, typ, iters @ [ iter ])
                        else v)
                      vars_inner_acc
                  in
                  let iterexp_new =
                    (iter, List.filter (fun v -> Var.equal v var_inner) vars_acc)
                  in
                  ( vars_inner_upd,
                    var_new_upd,
                    iterexps_acc @ [ iterexp_new ],
                    vars_upd )
                else (vars_inner_acc, var_new_acc, iterexps_acc, vars_acc))
              vars_inner
              (vars_inner, var_new, iterexps, vars)
          in
          let iter_state = { iter_state with vars_inner; var_new; iterexps } in
          Some (IterE (exp_inner', (iter, vars)) $$ (at, note), iter_state)
    in
    choose_sequential [ try_root; try_children ]
  and transform_exps acc (exps : exp list) : (exp list * iter_state) option =
    transform_list (transform_exp acc) Vars.free_exp exps
  and transform_arg acc (arg : arg) : (arg * iter_state) option =
    let { it; at; _ } = arg in
    match it with
    | ExpA exp_inner ->
        let* exp_inner', iter_state = transform_exp acc exp_inner in
        Some (ExpA exp_inner' $ at, iter_state)
    | DefA _ -> None
  and transform_args acc (args : arg list) : (arg list * iter_state) option =
    transform_list (transform_arg acc) Vars.free_arg args
  and transform_path acc (path : path) : (path * iter_state) option =
    let { it; at; note } = path in
    match it with
    | RootP -> None
    | IdxP (path_b, exp_i) ->
        let try_base () =
          let* path_b', iter_state = transform_path acc path_b in
          let vars_i = Vars.free_exp exp_i in
          let iter_state =
            { iter_state with vars_outer = iter_state.vars_outer ++ vars_i }
          in
          Some (IdxP (path_b', exp_i) $$ (at, note), iter_state)
        in
        let try_index () =
          let* exp_i', iter_state = transform_exp acc exp_i in
          let vars_b = Vars.free_path path_b in
          let iter_state =
            { iter_state with vars_outer = iter_state.vars_outer ++ vars_b }
          in
          Some (IdxP (path_b, exp_i') $$ (at, note), iter_state)
        in
        choose_sequential [ try_base; try_index ]
    | SliceP (path_b, exp_l, exp_h) ->
        let try_base () =
          let* path_b', iter_state = transform_path acc path_b in
          let vars_l = Vars.free_exp exp_l in
          let vars_h = Vars.free_exp exp_h in
          let iter_state =
            {
              iter_state with
              vars_outer = iter_state.vars_outer ++ vars_l ++ vars_h;
            }
          in
          Some (SliceP (path_b', exp_l, exp_h) $$ (at, note), iter_state)
        in
        let try_low () =
          let* exp_l', iter_state = transform_exp acc exp_l in
          let vars_b = Vars.free_path path_b in
          let vars_h = Vars.free_exp exp_h in
          let iter_state =
            {
              iter_state with
              vars_outer = iter_state.vars_outer ++ vars_b ++ vars_h;
            }
          in
          Some (SliceP (path_b, exp_l', exp_h) $$ (at, note), iter_state)
        in
        let try_high () =
          let* exp_h', iter_state = transform_exp acc exp_h in
          let vars_b = Vars.free_path path_b in
          let vars_l = Vars.free_exp exp_l in
          let iter_state =
            {
              iter_state with
              vars_outer = iter_state.vars_outer ++ vars_b ++ vars_l;
            }
          in
          Some (SliceP (path_b, exp_l, exp_h') $$ (at, note), iter_state)
        in
        choose_sequential [ try_base; try_low; try_high ]
    | DotP (path_b, atom) ->
        let* path_b', iter_state = transform_path acc path_b in
        Some (DotP (path_b', atom) $$ (at, note), iter_state)
  in
  transform_exp acc e

let fresh_exp_from_typ (ids : IdSet.t) (typ : typ) : exp * var * IdSet.t =
  let id_base, typ_base, iters = Fresh.fresh_var_from_typ ids typ.at typ in
  let var_new = (id_base, typ_base, iters) in
  let ids = IdSet.add id_base ids in
  let exp_base = VarE id_base $$ (typ_base.at, typ_base.it) in
  let exp_match, _ =
    List.fold_left
      (fun (exp_match, iters) iter ->
        let typ = IterT (exp_match.note $ exp_match.at, iter) in
        let var = (id_base, typ_base, iters) in
        let iterexp = (iter, [ var ]) in
        let exp_match = IterE (exp_match, iterexp) $$ (exp_match.at, typ) in
        (exp_match, iters @ [ iter ]))
      (exp_base, []) iters
  in
  (exp_match, var_new, ids)
