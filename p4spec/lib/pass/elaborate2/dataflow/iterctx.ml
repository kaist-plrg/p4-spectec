open Domain.Lib
open Lang
open Il2
open Runtime.Static
open Envs
open Util.Source

type t = (iter * var list * var list) list

(* Constructor *)

let empty : t = []

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

(* Filtering variables *)

let filter (f : Id.t -> bool) (iterctx : t) : t =
  List.map
    (fun (iter, vars_bound, vars_bind) ->
      let vars_bound = List.filter (fun (id, _, _) -> f id) vars_bound in
      let vars_bind = List.filter (fun (id, _, _) -> f id) vars_bind in
      (iter, vars_bound, vars_bind))
    iterctx

(* Construction of iterated premises *)

let iterate_prem (prem : prem) (iterctx : t) : prem =
  List.fold_left
    (fun prem (iter, vars_bound, vars_bind) ->
      Il.IterPr (prem, (iter, vars_bound, vars_bind)) $ prem.at)
    prem iterctx
