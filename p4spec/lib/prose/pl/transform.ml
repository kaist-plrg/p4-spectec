open Ast
open Domain.Lib
open Util.Source
open Fold

let ( let* ) = Option.bind

let rec choice = function
  | [] -> None
  | f :: fs -> (
      match f () with
      | Some a -> Some a
      | None -> ( match choice fs with Some a -> Some a | None -> None))

let transform_first_with_acc
    (f_transform_opt : exp -> 'acc -> (exp * 'acc) option)
    (f_fold_down : 'acc -> exp -> 'acc) (f_fold_up : (exp, arg, path) folder)
    (acc : 'acc) (e : exp) : (exp * 'acc) option =
  let rec walk_exp (acc : 'acc) (e : exp) : (exp * 'acc) option =
    let try_root () = f_transform_opt e acc in
    let try_children () =
      let acc = f_fold_down acc e in
      let { it; at; note } = e in
      match it with
      | BoolE _ | NumE _ | TextE _ | VarE _ -> None
      | UnE (unop, optyp, e_1) ->
          let* e_1', acc' = walk_exp acc e_1 in
          Some (f_fold_up.f_UnE note at unop optyp e_1', acc')
      | BinE (binop, optyp, e_1, e_2) ->
          let try_left () =
            let* e_1', acc' = walk_exp acc e_1 in
            Some (f_fold_up.f_BinE note at binop optyp e_1' e_2, acc')
          in
          let try_right () =
            let* e_2', acc' = walk_exp acc e_2 in
            Some (f_fold_up.f_BinE note at binop optyp e_1 e_2', acc')
          in
          choice [ try_left; try_right ]
      | CmpE (cmpop, optyp, e_1, e_2) ->
          let try_left () =
            let* e_1', acc' = walk_exp acc e_1 in
            Some (f_fold_up.f_CmpE note at cmpop optyp e_1' e_2, acc')
          in
          let try_right () =
            let* e_2', acc' = walk_exp acc e_2 in
            Some (f_fold_up.f_CmpE note at cmpop optyp e_1 e_2', acc')
          in
          choice [ try_left; try_right ]
      | UpCastE (typ, e_1) ->
          let* e_1', acc' = walk_exp acc e_1 in
          Some (f_fold_up.f_UpCastE note at typ e_1', acc')
      | DownCastE (typ, e_1) ->
          let* e_1', acc' = walk_exp acc e_1 in
          Some (f_fold_up.f_DownCastE note at typ e_1', acc')
      | SubE (e_1, typ) ->
          let* e_1', acc' = walk_exp acc e_1 in
          Some (f_fold_up.f_SubE note at e_1' typ, acc')
      | MatchE (e_1, pattern) ->
          let* e_1', acc' = walk_exp acc e_1 in
          Some (f_fold_up.f_MatchE note at e_1' pattern, acc')
      | TupleE exps ->
          let* exps_new, acc' = walk_exps acc exps in
          Some (f_fold_up.f_TupleE note at exps_new, acc')
      | CaseE (id, mixop, exps, hint) ->
          let* exps_new, acc' = walk_exps acc exps in
          Some (f_fold_up.f_CaseE note at id mixop exps_new hint, acc')
      | StrE fields ->
          let atoms, values = List.split fields in
          let* values', acc' = walk_exps acc values in
          Some (f_fold_up.f_StrE note at (List.combine atoms values'), acc')
      | OptE (Some exp) ->
          let* exp_new, acc' = walk_exp acc exp in
          Some (f_fold_up.f_OptE note at (Some exp_new), acc')
      | OptE None -> None
      | ListE exps ->
          let* exps_new, acc' = walk_exps acc exps in
          Some (f_fold_up.f_ListE note at exps_new, acc')
      | ConsE (exp_h, exp_t) ->
          let try_head () =
            let* exp_h', acc' = walk_exp acc exp_h in
            Some (f_fold_up.f_ConsE note at exp_h' exp_t, acc')
          in
          let try_tail () =
            let* exp_t', acc' = walk_exp acc exp_t in
            Some (f_fold_up.f_ConsE note at exp_h exp_t', acc')
          in
          choice [ try_head; try_tail ]
      | CatE (exp_l, exp_r) ->
          let try_left () =
            let* exp_l', acc' = walk_exp acc exp_l in
            Some (f_fold_up.f_CatE note at exp_l' exp_r, acc')
          in
          let try_right () =
            let* exp_r', acc' = walk_exp acc exp_r in
            Some (f_fold_up.f_CatE note at exp_l exp_r', acc')
          in
          choice [ try_left; try_right ]
      | MemE (exp_l, exp_r) ->
          let try_left () =
            let* exp_l', acc' = walk_exp acc exp_l in
            Some (f_fold_up.f_MemE note at exp_l' exp_r, acc')
          in
          let try_right () =
            let* exp_r', acc' = walk_exp acc exp_r in
            Some (f_fold_up.f_MemE note at exp_l exp_r', acc')
          in
          choice [ try_left; try_right ]
      | LenE exp ->
          let* exp', acc' = walk_exp acc exp in
          Some (f_fold_up.f_LenE note at exp', acc')
      | DotE (exp, atom) ->
          let* exp', acc' = walk_exp acc exp in
          Some (f_fold_up.f_DotE note at exp' atom, acc')
      | IdxE (exp_b, exp_i) ->
          let try_base () =
            let* exp_b', acc' = walk_exp acc exp_b in
            Some (f_fold_up.f_IdxE note at exp_b' exp_i, acc')
          in
          let try_index () =
            let* exp_i', acc' = walk_exp acc exp_i in
            Some (f_fold_up.f_IdxE note at exp_b exp_i', acc')
          in
          choice [ try_base; try_index ]
      | SliceE (exp_b, exp_l, exp_h) ->
          let try_base () =
            let* exp_b', acc' = walk_exp acc exp_b in
            Some (f_fold_up.f_SliceE note at exp_b' exp_l exp_h, acc')
          in
          let try_low () =
            let* exp_l', acc' = walk_exp acc exp_l in
            Some (f_fold_up.f_SliceE note at exp_b exp_l' exp_h, acc')
          in
          let try_high () =
            let* exp_h', acc' = walk_exp acc exp_h in
            Some (f_fold_up.f_SliceE note at exp_b exp_l exp_h', acc')
          in
          choice [ try_base; try_low; try_high ]
      | UpdE (exp_b, path, exp_f) ->
          let try_base () =
            let* exp_b', acc' = walk_exp acc exp_b in
            Some (f_fold_up.f_UpdE note at exp_b' path exp_f, acc')
          in
          let try_path () =
            let* path', acc' = walk_path acc path in
            Some (f_fold_up.f_UpdE note at exp_b path' exp_f, acc')
          in
          let try_field () =
            let* exp_f', acc' = walk_exp acc exp_f in
            Some (f_fold_up.f_UpdE note at exp_b path exp_f', acc')
          in
          choice [ try_base; try_path; try_field ]
      | CallE (funcprose, targs, args) ->
          let* args_new, acc' = walk_args acc args in
          Some (f_fold_up.f_CallE note at funcprose targs args_new, acc')
      | IterE (exp, iterexp) ->
          let* exp_new, acc' = walk_exp acc exp in
          Some (f_fold_up.f_IterE note at exp_new iterexp, acc')
    in
    choice [ try_root; try_children ]
  and walk_exps (acc : 'acc) (exps : exp list) : (exp list * 'acc) option =
    match exps with
    | [] -> None
    | exp :: exps -> (
        match walk_exp acc exp with
        | Some (exp_new, acc) -> Some (exp_new :: exps, acc)
        | None ->
            let* exps_new, acc' = walk_exps acc exps in
            Some (exp :: exps_new, acc'))
  and walk_arg (acc : 'acc) (arg : arg) : (arg * 'acc) option =
    let { it; at; _ } = arg in
    match it with
    | ExpA e ->
        let* e_new, acc' = walk_exp acc e in
        Some (f_fold_up.f_ExpA at e_new, acc')
    | DefA _ -> None
  and walk_args (acc : 'acc) (args : arg list) : (arg list * 'acc) option =
    match args with
    | [] -> None
    | arg :: args -> (
        match walk_arg acc arg with
        | Some (arg_new, acc) -> Some (arg_new :: args, acc)
        | None ->
            let* args_new, acc' = walk_args acc args in
            Some (arg :: args_new, acc'))
  and walk_path (acc : 'acc) (path : path) : (path * 'acc) option =
    let { it; at; note } = path in
    match it with
    | RootP -> None
    | IdxP (path_b, exp_i) ->
        let try_base () =
          let* path_b', acc' = walk_path acc path_b in
          Some (f_fold_up.f_IdxP note at path_b' exp_i, acc')
        in
        let try_index () =
          let* exp_i', acc' = walk_exp acc exp_i in
          Some (f_fold_up.f_IdxP note at path_b exp_i', acc')
        in
        choice [ try_base; try_index ]
    | SliceP (path_b, exp_l, exp_h) ->
        let try_base () =
          let* path_b', acc' = walk_path acc path_b in
          Some (f_fold_up.f_SliceP note at path_b' exp_l exp_h, acc')
        in
        let try_low () =
          let* exp_l', acc' = walk_exp acc exp_l in
          Some (f_fold_up.f_SliceP note at path_b exp_l' exp_h, acc')
        in
        let try_high () =
          let* exp_h', acc' = walk_exp acc exp_h in
          Some (f_fold_up.f_SliceP note at path_b exp_l exp_h', acc')
        in
        choice [ try_base; try_low; try_high ]
    | DotP (path_b, atom) ->
        let* path_b', acc' = walk_path acc path_b in
        Some (f_fold_up.f_DotP note at path_b' atom, acc')
  in
  walk_exp acc e

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
