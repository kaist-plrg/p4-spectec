open Domain.Lib
open Lang
module Var = Runtime.Dynamic.Var
open Util.Source

(* Helpers *)

let rec is_iter_var_exp (exp : Sl.exp) : Var.t option =
  match exp.it with
  | VarE id_exp -> Some (id_exp, [])
  | IterE (exp_inner, iterexp) -> (
      match is_iter_var_exp exp_inner with
      | Some (id_var, iters_var) -> (
          match iterexp with
          | iter, [ var ] ->
              let id_iter, _, iters_iter = var in
              if Id.eq id_var id_iter && iters_var = iters_iter then
                Some (id_var, iters_var @ [ iter ])
              else None
          | _ -> None)
      | None -> None)
  | _ -> None

(* Raise an Unmatch exception *)

let raise_unmatch (msg : string) : Ml.expr =
  Ml.AppE (Ml.VarE "raise", [ Ml.AppE (Ml.VarE "Unmatch", [ Ml.StrE msg ]) ])

(* Fuse [splitM (Option.map f (combineN o0 .. o(N-1)))] into a single match
   [Option.fold_N_M f o0 .. o(N-1)]: when all N inputs are [Some], apply [f]
   (returning an M-tuple) and re-wrap each output in [Some]; when all [None],
   return M [None]s; mixed optionality fails. *)
let make_opt_fold (ctx : Ctx.t) (ids_in_ml : Ml.id list)
    (ids_elem_ml : Ml.id list) (expr_inner_ml : Ml.expr) (n_in : int)
    (n_out : int) : Ctx.t * Ml.expr =
  let ctx = Ctx.add_opt_fold ctx (n_in, n_out) in
  let pats_elem_ml =
    List.map (fun id_elem_ml -> Ml.VarP id_elem_ml) ids_elem_ml
  in
  let expr_lambda_ml = Ml.FunE (pats_elem_ml, expr_inner_ml) in
  let id_fold_ml = Printf.sprintf "Option.fold_%d_%d" n_in n_out in
  let exprs_arg_ml =
    expr_lambda_ml :: List.map (fun id_in_ml -> Ml.VarE id_in_ml) ids_in_ml
  in
  let expr_ml = Ml.AppE (Ml.VarE id_fold_ml, exprs_arg_ml) in
  (ctx, expr_ml)

(* Fuse [match combineN o0 .. o(N-1) with None -> true | Some (..) -> f ..] into
   [Option.for_all_N f o0 .. o(N-1)]: all [Some] -> apply [f]; all [None] -> true;
   mixed optionality fails. *)
let make_opt_forall (ctx : Ctx.t) (ids_in_ml : Ml.id list)
    (ids_elem_ml : Ml.id list) (expr_body_ml : Ml.expr) (n_in : int) :
    Ctx.t * Ml.expr =
  let ctx = Ctx.add_opt_forall ctx n_in in
  let pats_elem_ml =
    List.map (fun id_elem_ml -> Ml.VarP id_elem_ml) ids_elem_ml
  in
  let expr_lambda_ml = Ml.FunE (pats_elem_ml, expr_body_ml) in
  let id_forall_ml = Printf.sprintf "Option.for_all_%d" n_in in
  let exprs_arg_ml =
    expr_lambda_ml :: List.map (fun id_in_ml -> Ml.VarE id_in_ml) ids_in_ml
  in
  (ctx, Ml.AppE (Ml.VarE id_forall_ml, exprs_arg_ml))

(* Fuse [splitM (List.map f (combineN l0 .. l(N-1)))] into a single
   tail-recursive pass [fold_left_N_M f l0 .. l(N-1)].

   [ids_in_ml]   : the N guiding input list vars.
   [ids_elem_ml] : the N per-element stub vars (lambda parameters).
   [expr_inner_ml]: the body, returning the M-tuple of bound vars (or a single
                    value when M = 1).

   Registers the (N, M) arity so the prelude emits the matching combinator. *)
let make_list_fold (ctx : Ctx.t) (ids_in_ml : Ml.id list)
    (ids_elem_ml : Ml.id list) (expr_inner_ml : Ml.expr) (n_in : int)
    (n_out : int) : Ctx.t * Ml.expr =
  let ctx = Ctx.add_list_fold ctx (n_in, n_out) in
  let pats_elem_ml =
    List.map (fun id_elem_ml -> Ml.VarP id_elem_ml) ids_elem_ml
  in
  let expr_lambda_ml = Ml.FunE (pats_elem_ml, expr_inner_ml) in
  let id_fold_ml = Printf.sprintf "List.fold_left_%d_%d" n_in n_out in
  let exprs_arg_ml =
    expr_lambda_ml :: List.map (fun id_in_ml -> Ml.VarE id_in_ml) ids_in_ml
  in
  let expr_ml = Ml.AppE (Ml.VarE id_fold_ml, exprs_arg_ml) in
  (ctx, expr_ml)

(* Fuse [List.for_all f (combineN l0 .. l(N-1))] into [List.for_all_N f l0 .. l(N-1)],
   a single lockstep walk over the N lists, short-circuiting on the first false. *)
let make_list_forall (ctx : Ctx.t) (ids_in_ml : Ml.id list)
    (ids_elem_ml : Ml.id list) (expr_body_ml : Ml.expr) (n_in : int) :
    Ctx.t * Ml.expr =
  let ctx = Ctx.add_list_forall ctx n_in in
  let pats_elem_ml =
    List.map (fun id_elem_ml -> Ml.VarP id_elem_ml) ids_elem_ml
  in
  let expr_lambda_ml = Ml.FunE (pats_elem_ml, expr_body_ml) in
  let id_forall_ml = Printf.sprintf "List.for_all_%d" n_in in
  let exprs_arg_ml =
    expr_lambda_ml :: List.map (fun id_in_ml -> Ml.VarE id_in_ml) ids_in_ml
  in
  (ctx, Ml.AppE (Ml.VarE id_forall_ml, exprs_arg_ml))

(* Cache helpers *)

let make_cache_key (ids_ml : Ml.id list) : Ml.expr =
  match ids_ml with
  | [] -> Ml.UnitE
  | [ id ] -> Ml.VarE id
  | ids -> Ml.TupleE (List.map (fun id -> Ml.VarE id) ids)

let make_cache_dispatcher (cache_id_ml : Ml.id) (key_ml : Ml.expr)
    (dispatch_ml : Ml.expr) : Ml.expr =
  Ml.IfE
    ( Ml.UnopE ("!", Ml.VarE "cache_enabled__"),
      Ml.LetE
        ( Ml.VarP "key__",
          key_ml,
          Ml.MatchE
            ( Ml.AppE
                ( Ml.LitE "H__.find_opt",
                  [ Ml.VarE cache_id_ml; Ml.VarE "key__" ] ),
              [
                ( Ml.VariantP (`Mono ("Some", [ Ml.VarP "result__" ])),
                  Ml.VarE "result__" );
                ( Ml.WildP,
                  Ml.LetE
                    ( Ml.VarP "cp_iface__",
                      Ml.AppE (Ml.LitE "Interface.checkpoint", [ Ml.UnitE ]),
                      Ml.LetE
                        ( Ml.VarP "cp_ext__",
                          Ml.AppE (Ml.LitE "Extern.checkpoint", [ Ml.UnitE ]),
                          Ml.LetE
                            ( Ml.VarP "result__",
                              dispatch_ml,
                              Ml.SeqE
                                [
                                  Ml.IfE
                                    ( Ml.BinopE
                                        ( "&&",
                                          Ml.UnopE
                                            ( "not",
                                              Ml.AppE
                                                ( Ml.LitE "Interface.seff",
                                                  [
                                                    Ml.VarE "cp_iface__";
                                                    Ml.AppE
                                                      ( Ml.LitE
                                                          "Interface.checkpoint",
                                                        [ Ml.UnitE ] );
                                                  ] ) ),
                                          Ml.UnopE
                                            ( "not",
                                              Ml.AppE
                                                ( Ml.LitE "Extern.seff",
                                                  [
                                                    Ml.VarE "cp_ext__";
                                                    Ml.AppE
                                                      ( Ml.LitE
                                                          "Extern.checkpoint",
                                                        [ Ml.UnitE ] );
                                                  ] ) ) ),
                                      Ml.AppE
                                        ( Ml.LitE "H__.replace",
                                          [
                                            Ml.VarE cache_id_ml;
                                            Ml.VarE "key__";
                                            Ml.VarE "result__";
                                          ] ),
                                      Some Ml.UnitE );
                                  Ml.VarE "result__";
                                ] ) ) ) );
              ] ) ),
      Some dispatch_ml )
