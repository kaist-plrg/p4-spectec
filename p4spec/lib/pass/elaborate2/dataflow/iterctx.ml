open Domain.Lib
open Lang
open Il2
open Runtime.Static
open Envs
open Util.Source

type t = (iter * var list * var list) list

(* Constructor *)

let empty : t = []

let iters_of (iterctx : t) : iter list =
  List.map (fun (iter, _, _) -> iter) iterctx

(* Adders *)

let add_vars_bound (venv : VEnv.t) (iterctx : t) : t =
  let _, iterctx =
    List.fold_left_map
      (fun venv (iter, vars_bound, vars_bind) ->
        let vars_bound =
          vars_bound
          @ (venv |> VEnv.bindings
            |> List.map (fun (id, (typ, iters)) -> (id, typ, iters)))
        in
        let venv = VEnv.map (Typdim.add_iter iter) venv in
        (venv, (iter, vars_bound, vars_bind)))
      venv iterctx
  in
  iterctx

let add_var_bound (id : Id.t) (typ : typ) (iters : iter list) (iterctx : t) : t
    =
  let venv = VEnv.singleton id (typ, iters) in
  add_vars_bound venv iterctx

let add_vars_bind (venv : VEnv.t) (iterctx : t) : t =
  let _, iterctx =
    List.fold_left_map
      (fun venv (iter, vars_bound, vars_bind) ->
        let vars_bind =
          vars_bind
          @ (venv |> VEnv.bindings
            |> List.map (fun (id, (typ, iters)) -> (id, typ, iters)))
        in
        let venv = VEnv.map (Typdim.add_iter iter) venv in
        (venv, (iter, vars_bound, vars_bind)))
      venv iterctx
  in
  iterctx

let add_var_bind (id : Id.t) (typ : typ) (iters : iter list) (iterctx : t) : t =
  let venv = VEnv.singleton id (typ, iters) in
  add_vars_bind venv iterctx

(* Filtering variables *)

let filter_bound (f : Id.t -> typ -> iter list -> bool) (iterctx : t) : t =
  List.map
    (fun (iter, vars_bound, vars_bind) ->
      let vars_bound =
        List.filter (fun (id, typ, iters) -> f id typ iters) vars_bound
      in
      (iter, vars_bound, vars_bind))
    iterctx

(* Construction of iterated expressions and premises *)

let iterate_exp (iterctx : t) (exp : exp) : exp =
  List.fold_left
    (fun exp (iter, vars_bound, _) ->
      let iterexp = (iter, vars_bound) in
      Il.IterE (exp, iterexp) $$ (exp.at, exp.note))
    exp iterctx

let iterate_prem (iterctx : t) (prem : prem) : prem =
  List.fold_left
    (fun prem (iter, vars_bound, vars_bind) ->
      Il.IterPr (prem, (iter, vars_bound, vars_bind)) $ prem.at)
    prem iterctx
