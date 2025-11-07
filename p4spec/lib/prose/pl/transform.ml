open Ast
open Domain.Lib
open Util.Source
module VarSet = Free.VarSet

let ( let* ) = Option.bind
let ( + ) = VarSet.union

let rec choice = function
  | [] -> None
  | f :: fs -> (
      match f () with
      | Some a -> Some a
      | None -> ( match choice fs with Some a -> Some a | None -> None))

type iter_state = {
  vars_inner : VarSet.t;
  var_new : VarSet.elt;
  vars_outer : VarSet.t;
  iterexps : iterexp list;
  exp_orig : exp;
}

let transform_first_with_iters
    (f_transform_opt : exp -> (exp * iter_state) option) (e : exp) :
    (exp * iter_state) option =
  let rec walk_exp (e : exp) : (exp * iter_state) option =
    let try_root () = f_transform_opt e in
    let try_children () =
      let { it; at; note } = e in
      match it with
      | BoolE _ | NumE _ | TextE _ | VarE _ -> None
      | UnE (unop, optyp, exp_inner) ->
          let* exp_inner', iter_state = walk_exp exp_inner in
          Some (UnE (unop, optyp, exp_inner') $$ (at, note), iter_state)
      | BinE (binop, optyp, exp_l, exp_r) ->
          let try_left () =
            let* exp_l', iter_state = walk_exp exp_l in
            let vars_r = Free.Vars.free_exp exp_r in
            let iter_state =
              { iter_state with vars_outer = iter_state.vars_outer + vars_r }
            in
            Some (BinE (binop, optyp, exp_l', exp_r) $$ (at, note), iter_state)
          in
          let try_right () =
            let* exp_r', iter_state = walk_exp exp_r in
            let vars_l = Free.Vars.free_exp exp_l in
            let iter_state =
              { iter_state with vars_outer = iter_state.vars_outer + vars_l }
            in
            Some (BinE (binop, optyp, exp_l, exp_r') $$ (at, note), iter_state)
          in
          choice [ try_left; try_right ]
      | CmpE (cmpop, optyp, exp_l, exp_r) ->
          let try_left () =
            let* exp_l', iter_state = walk_exp exp_l in
            let vars_r = Free.Vars.free_exp exp_r in
            let iter_state =
              { iter_state with vars_outer = iter_state.vars_outer + vars_r }
            in
            Some (CmpE (cmpop, optyp, exp_l', exp_r) $$ (at, note), iter_state)
          in
          let try_right () =
            let* exp_r', iter_state = walk_exp exp_r in
            let vars_l = Free.Vars.free_exp exp_l in
            let iter_state =
              { iter_state with vars_outer = iter_state.vars_outer + vars_l }
            in
            Some (CmpE (cmpop, optyp, exp_l, exp_r') $$ (at, note), iter_state)
          in
          choice [ try_left; try_right ]
      | UpCastE (typ, exp_inner) ->
          let* exp_inner', iter_state = walk_exp exp_inner in
          Some (UpCastE (typ, exp_inner') $$ (at, note), iter_state)
      | DownCastE (typ, exp_inner) ->
          let* exp_inner', iter_state = walk_exp exp_inner in
          Some (DownCastE (typ, exp_inner') $$ (at, note), iter_state)
      | SubE (exp_inner, typ) ->
          let* exp_inner', iter_state = walk_exp exp_inner in
          Some (SubE (exp_inner', typ) $$ (at, note), iter_state)
      | MatchE (exp_inner, pattern) ->
          let* exp_inner', iter_state = walk_exp exp_inner in
          Some (MatchE (exp_inner', pattern) $$ (at, note), iter_state)
      | TupleE exps ->
          let* exps', iter_state = walk_exps exps in
          Some (TupleE exps' $$ (at, note), iter_state)
      | CaseE (id, mixop, exps, hint) ->
          let* exps', iter_state = walk_exps exps in
          Some (CaseE (id, mixop, exps', hint) $$ (at, note), iter_state)
      | StrE fields ->
          let atoms, values = List.split fields in
          let* values', iter_state = walk_exps values in
          Some (StrE (List.combine atoms values') $$ (at, note), iter_state)
      | OptE (Some exp_inner) ->
          let* exp_inner', iter_state = walk_exp exp_inner in
          Some (OptE (Some exp_inner') $$ (at, note), iter_state)
      | OptE None -> None
      | ListE exps ->
          let* exps', iter_state = walk_exps exps in
          Some (ListE exps' $$ (at, note), iter_state)
      | ConsE (exp_h, exp_t) ->
          let try_head () =
            let* exp_h', iter_state = walk_exp exp_h in
            let vars_t = Free.Vars.free_exp exp_t in
            let iter_state =
              { iter_state with vars_outer = iter_state.vars_outer + vars_t }
            in
            Some (ConsE (exp_h', exp_t) $$ (at, note), iter_state)
          in
          let try_tail () =
            let* exp_t', iter_state = walk_exp exp_t in
            let vars_h = Free.Vars.free_exp exp_h in
            let iter_state =
              { iter_state with vars_outer = iter_state.vars_outer + vars_h }
            in
            Some (ConsE (exp_h, exp_t') $$ (at, note), iter_state)
          in
          choice [ try_head; try_tail ]
      | CatE (exp_l, exp_r) ->
          let try_left () =
            let* exp_l', iter_state = walk_exp exp_l in
            let vars_r = Free.Vars.free_exp exp_r in
            let iter_state =
              { iter_state with vars_outer = iter_state.vars_outer + vars_r }
            in
            Some (CatE (exp_l', exp_r) $$ (at, note), iter_state)
          in
          let try_right () =
            let* exp_r', iter_state = walk_exp exp_r in
            let vars_l = Free.Vars.free_exp exp_l in
            let iter_state =
              { iter_state with vars_outer = iter_state.vars_outer + vars_l }
            in
            Some (CatE (exp_l, exp_r') $$ (at, note), iter_state)
          in
          choice [ try_left; try_right ]
      | MemE (exp_l, exp_r) ->
          let try_left () =
            let* exp_l', iter_state = walk_exp exp_l in
            let vars_r = Free.Vars.free_exp exp_r in
            let iter_state =
              { iter_state with vars_outer = iter_state.vars_outer + vars_r }
            in
            Some (MemE (exp_l', exp_r) $$ (at, note), iter_state)
          in
          let try_right () =
            let* exp_r', iter_state = walk_exp exp_r in
            let vars_l = Free.Vars.free_exp exp_l in
            let iter_state =
              { iter_state with vars_outer = iter_state.vars_outer + vars_l }
            in
            Some (MemE (exp_l, exp_r') $$ (at, note), iter_state)
          in
          choice [ try_left; try_right ]
      | LenE exp_inner ->
          let* exp_inner', iter_state = walk_exp exp_inner in
          Some (LenE exp_inner' $$ (at, note), iter_state)
      | DotE (exp_inner, atom) ->
          let* exp_inner', iter_state = walk_exp exp_inner in
          Some (DotE (exp_inner', atom) $$ (at, note), iter_state)
      | IdxE (exp_b, exp_i) ->
          let try_base () =
            let* exp_b', iter_state = walk_exp exp_b in
            let vars_i = Free.Vars.free_exp exp_i in
            let iter_state =
              { iter_state with vars_outer = iter_state.vars_outer + vars_i }
            in
            Some (IdxE (exp_b', exp_i) $$ (at, note), iter_state)
          in
          let try_index () =
            let* exp_i', iter_state = walk_exp exp_i in
            let vars_b = Free.Vars.free_exp exp_b in
            let iter_state =
              { iter_state with vars_outer = iter_state.vars_outer + vars_b }
            in
            Some (IdxE (exp_b, exp_i') $$ (at, note), iter_state)
          in
          choice [ try_base; try_index ]
      | SliceE (exp_b, exp_l, exp_h) ->
          let try_base () =
            let* exp_b', iter_state = walk_exp exp_b in
            let vars_l = Free.Vars.free_exp exp_l in
            let vars_h = Free.Vars.free_exp exp_h in
            let iter_state =
              {
                iter_state with
                vars_outer = iter_state.vars_outer + vars_l + vars_h;
              }
            in
            Some (SliceE (exp_b', exp_l, exp_h) $$ (at, note), iter_state)
          in
          let try_low () =
            let* exp_l', iter_state = walk_exp exp_l in
            let vars_b = Free.Vars.free_exp exp_b in
            let vars_h = Free.Vars.free_exp exp_h in
            let iter_state =
              {
                iter_state with
                vars_outer = iter_state.vars_outer + vars_b + vars_h;
              }
            in
            Some (SliceE (exp_b, exp_l', exp_h) $$ (at, note), iter_state)
          in
          let try_high () =
            let* exp_h', iter_state = walk_exp exp_h in
            let vars_b = Free.Vars.free_exp exp_b in
            let vars_l = Free.Vars.free_exp exp_l in
            let iter_state =
              {
                iter_state with
                vars_outer = iter_state.vars_outer + vars_b + vars_l;
              }
            in
            Some (SliceE (exp_b, exp_l, exp_h') $$ (at, note), iter_state)
          in
          choice [ try_base; try_low; try_high ]
      | UpdE (exp_b, path, exp_f) ->
          let try_base () =
            let* exp_b', iter_state = walk_exp exp_b in
            let vars_path = Free.Vars.free_path path in
            let vars_f = Free.Vars.free_exp exp_f in
            let iter_state =
              {
                iter_state with
                vars_outer = iter_state.vars_outer + vars_path + vars_f;
              }
            in
            Some (UpdE (exp_b', path, exp_f) $$ (at, note), iter_state)
          in
          let try_path () =
            let* path', iter_state = walk_path path in
            let vars_b = Free.Vars.free_exp exp_b in
            let vars_f = Free.Vars.free_exp exp_f in
            let iter_state =
              {
                iter_state with
                vars_outer = iter_state.vars_outer + vars_b + vars_f;
              }
            in
            Some (UpdE (exp_b, path', exp_f) $$ (at, note), iter_state)
          in
          let try_field () =
            let* exp_f', iter_state = walk_exp exp_f in
            let vars_b = Free.Vars.free_exp exp_b in
            let vars_path = Free.Vars.free_path path in
            let iter_state =
              {
                iter_state with
                vars_outer = iter_state.vars_outer + vars_b + vars_path;
              }
            in
            Some (UpdE (exp_b, path, exp_f') $$ (at, note), iter_state)
          in
          choice [ try_base; try_path; try_field ]
      | CallE (funcprose, targs, args) ->
          let* args_new, iter_state = walk_args args in
          Some (CallE (funcprose, targs, args_new) $$ (at, note), iter_state)
      | IterE (exp_inner, (iter, itervars)) ->
          let* exp_inner', iter_state = walk_exp exp_inner in
          let { vars_inner; vars_outer; var_new; iterexps; _ } = iter_state in
          (* main algorithm : compare / replace / increment iterations *)
          let vars_inner, var_new, iterexps, itervars =
            VarSet.fold
              (fun var_inner
                   (vars_inner_acc, var_new_acc, iterexps_acc, itervars_acc) ->
                if List.mem var_inner itervars then
                  let itervars_upd =
                    (* Used outside CallE *)
                    if VarSet.mem var_inner vars_outer then
                      var_new :: itervars_acc
                    else
                      var_new
                      :: List.filter
                           (fun v -> not (Free.Var.equal v var_inner))
                           itervars_acc
                  in
                  let var_new_upd =
                    let id, typ, iters = var_new_acc in
                    (id, typ, iters @ [ iter ])
                  in
                  let vars_inner_upd =
                    VarSet.map
                      (fun v ->
                        if Free.Var.equal v var_inner then
                          let id, typ, iters = var_inner in
                          (id, typ, iters @ [ iter ])
                        else v)
                      vars_inner_acc
                  in
                  let iterexp_new =
                    ( iter,
                      List.filter
                        (fun v -> Free.Var.equal v var_inner)
                        itervars_acc )
                  in
                  ( vars_inner_upd,
                    var_new_upd,
                    iterexps_acc @ [ iterexp_new ],
                    itervars_upd )
                else (vars_inner_acc, var_new_acc, iterexps_acc, itervars_acc))
              vars_inner
              (vars_inner, var_new, iterexps, itervars)
          in
          let iter_state = { iter_state with vars_inner; var_new; iterexps } in
          Some (IterE (exp_inner', (iter, itervars)) $$ (at, note), iter_state)
    in
    choice [ try_root; try_children ]
  and walk_exps (exps : exp list) : (exp list * iter_state) option =
    match exps with
    | [] -> None
    | exp :: exps -> (
        match walk_exp exp with
        | Some (exp', iter_state) -> Some (exp' :: exps, iter_state)
        | None ->
            let* exps', iter_state = walk_exps exps in
            Some (exp :: exps', iter_state))
  and walk_arg (arg : arg) : (arg * iter_state) option =
    let { it; at; _ } = arg in
    match it with
    | ExpA exp_inner ->
        let* exp_inner', iter_state = walk_exp exp_inner in
        Some (ExpA exp_inner' $ at, iter_state)
    | DefA _ -> None
  and walk_args (args : arg list) : (arg list * iter_state) option =
    match args with
    | [] -> None
    | arg :: args -> (
        match walk_arg arg with
        | Some (arg', iter_state) -> Some (arg' :: args, iter_state)
        | None ->
            let* args', iter_state = walk_args args in
            Some (arg :: args', iter_state))
  and walk_path (path : path) : (path * iter_state) option =
    let { it; at; note } = path in
    match it with
    | RootP -> None
    | IdxP (path_b, exp_i) ->
        let try_base () =
          let* path_b', iter_state = walk_path path_b in
          let vars_i = Free.Vars.free_exp exp_i in
          let iter_state =
            { iter_state with vars_outer = iter_state.vars_outer + vars_i }
          in
          Some (IdxP (path_b', exp_i) $$ (at, note), iter_state)
        in
        let try_index () =
          let* exp_i', iter_state = walk_exp exp_i in
          let vars_b = Free.Vars.free_path path_b in
          let iter_state =
            { iter_state with vars_outer = iter_state.vars_outer + vars_b }
          in
          Some (IdxP (path_b, exp_i') $$ (at, note), iter_state)
        in
        choice [ try_base; try_index ]
    | SliceP (path_b, exp_l, exp_h) ->
        let try_base () =
          let* path_b', iter_state = walk_path path_b in
          let vars_l = Free.Vars.free_exp exp_l in
          let vars_h = Free.Vars.free_exp exp_h in
          let iter_state =
            {
              iter_state with
              vars_outer = iter_state.vars_outer + vars_l + vars_h;
            }
          in
          Some (SliceP (path_b', exp_l, exp_h) $$ (at, note), iter_state)
        in
        let try_low () =
          let* exp_l', iter_state = walk_exp exp_l in
          let vars_b = Free.Vars.free_path path_b in
          let vars_h = Free.Vars.free_exp exp_h in
          let iter_state =
            {
              iter_state with
              vars_outer = iter_state.vars_outer + vars_b + vars_h;
            }
          in
          Some (SliceP (path_b, exp_l', exp_h) $$ (at, note), iter_state)
        in
        let try_high () =
          let* exp_h', iter_state = walk_exp exp_h in
          let vars_b = Free.Vars.free_path path_b in
          let vars_l = Free.Vars.free_exp exp_l in
          let iter_state =
            {
              iter_state with
              vars_outer = iter_state.vars_outer + vars_b + vars_l;
            }
          in
          Some (SliceP (path_b, exp_l, exp_h') $$ (at, note), iter_state)
        in
        choice [ try_base; try_low; try_high ]
    | DotP (path_b, atom) ->
        let* path_b', iter_state = walk_path path_b in
        Some (DotP (path_b', atom) $$ (at, note), iter_state)
  in
  walk_exp e

let rec search_exp (cond : exp -> bool) (exp : exp) : bool =
  if cond exp then true
  else
    match exp.it with
    | BoolE _ | NumE _ | TextE _ | VarE _ -> false
    | UnE (_, _, e) -> search_exp cond e
    | BinE (_, _, e1, e2) | CmpE (_, _, e1, e2) ->
        search_exp cond e1 || search_exp cond e2
    | UpCastE (_, e1) | DownCastE (_, e1) | SubE (e1, _) | MatchE (e1, _) ->
        search_exp cond e1
    | TupleE es | CaseE (_, _, es, _) -> List.exists (search_exp cond) es
    | StrE fields -> List.exists (fun (_, e) -> search_exp cond e) fields
    | OptE (Some e) -> search_exp cond e
    | OptE None -> false
    | ListE es -> List.exists (search_exp cond) es
    | ConsE (e1, e2) | CatE (e1, e2) | MemE (e1, e2) ->
        search_exp cond e1 || search_exp cond e2
    | LenE e | DotE (e, _) -> search_exp cond e
    | IdxE (e1, e2) -> search_exp cond e1 || search_exp cond e2
    | SliceE (e1, e2, e3) ->
        search_exp cond e1 || search_exp cond e2 || search_exp cond e3
    | UpdE (e1, _, e2) -> search_exp cond e1 || search_exp cond e2
    | CallE (_, _, args) -> List.exists (search_arg cond) args
    | IterE (e, _) -> search_exp cond e

and search_arg (cond : exp -> bool) (arg : arg) : bool =
  match arg.it with ExpA e -> search_exp cond e | DefA _ -> false

let fresh_exp_from_typ (ids : IdSet.t) (typ : Il.Ast.typ) : Ast.exp * Id.t =
  let id_base, typ_base, iters =
    Elaborate.Fresh.fresh_var_from_typ ids typ.at typ
  in
  (* let ids = IdSet.add id_base ids in *)
  let exp_base = Ast.VarE id_base $$ (typ_base.at, typ_base.it) in
  let exp_match, _ =
    List.fold_left
      (fun (exp_match, iters) iter ->
        let typ = Il.Ast.IterT (exp_match.note $ exp_match.at, iter) in
        let var = (id_base, typ_base, iters) in
        let iterexp = (iter, [ var ]) in
        let exp_match = Ast.IterE (exp_match, iterexp) $$ (exp_match.at, typ) in
        (exp_match, iters @ [ iter ]))
      (exp_base, []) iters
  in
  (exp_match, id_base)
