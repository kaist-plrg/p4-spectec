open Domain.Lib
open Lang
open Il
open Runtime.Static
open Envs
open Error
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

let filter_bound (f : var -> bool) (iterctx : t) : t =
  List.map
    (fun (iter, vars_bound, vars_bind) ->
      let vars_bound = List.filter f vars_bound in
      (iter, vars_bound, vars_bind))
    iterctx

(* Validation *)

let validate (at : region) (iterctx : t) : unit =
  List.iter
    (fun (_, vars_bound, vars_bind) ->
      if List.is_empty vars_bound then
        if List.is_empty vars_bind then error at "empty iteration"
        else
          error at
            ("cannot determine dimension of binding identifier(s) only: "
            ^ String.concat ", " (List.map Print.string_of_var vars_bind))
      else ())
    iterctx

(* Construction of iterated premises *)

let iterate_prem (iterctx : t) (prem : prem) : prem =
  List.fold_left
    (fun prem (iter, vars_bound, vars_bind) ->
      IterPr (prem, (iter, vars_bound, vars_bind)) $ prem.at)
    prem iterctx
