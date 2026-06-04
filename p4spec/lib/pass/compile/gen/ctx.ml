open Domain.Lib
open Runtime.Dynamic_OCaml
open Envs
open Util.Source

(* Preamble *)

type preamble = { opts : int list; lists : int list }

(* Block *)

module Blk = struct
  type t = IdSet.t list

  (* Push and pop block *)

  let push (blk : t) : t = IdSet.empty :: blk
  let pop (blk : t) : t = List.tl blk

  (* Fresh ocaml variable *)

  let fresh (blk : t) (id_ml : Ml.id) : t * Ml.id =
    let ids_ml = List.fold_left IdSet.union IdSet.empty blk in
    let rec fresh (id_ml : Ml.id) : Ml.id =
      if IdSet.mem (id_ml $ no_region) ids_ml then id_ml ^ "_" |> fresh
      else id_ml
    in
    let id_ml = fresh id_ml in
    let blk =
      match blk with
      | [] -> assert false
      | ids_ml_h :: blk_t -> IdSet.add (id_ml $ no_region) ids_ml_h :: blk_t
    in
    (blk, id_ml)
end

(* Context *)

type t = { preamble : preamble; blk : Blk.t; bindings : NEnv.t }

let init () : t =
  {
    preamble = { opts = []; lists = [] };
    blk = [ IdSet.empty ];
    bindings = NEnv.empty;
  }

(* Fresh *)

let fresh (ctx : t) (id_ml : Ml.id) : t * Ml.id =
  let blk, id_ml = Blk.fresh ctx.blk id_ml in
  let ctx = { ctx with blk } in
  (ctx, id_ml)

(* Preamble setters *)

let add_opt_arity (ctx : t) (n : int) : t =
  let preamble = ctx.preamble in
  if List.mem n preamble.opts then ctx
  else
    let preamble = { preamble with opts = n :: preamble.opts } in
    { ctx with preamble }

let add_list_arity (ctx : t) (n : int) : t =
  let preamble = ctx.preamble in
  if List.mem n preamble.lists then ctx
  else
    let preamble = { preamble with lists = n :: preamble.lists } in
    { ctx with preamble }

(* Block setters *)

let push (ctx : t) : t = { ctx with blk = Blk.push ctx.blk }
let pop (ctx : t) : t = { ctx with blk = Blk.pop ctx.blk }

(* Adders *)

let add_binding (ctx : t) (var : Var.t) (id_ml : Ml.id) : t =
  let bindings = NEnv.add var id_ml ctx.bindings in
  { ctx with bindings }

let add_bindings (ctx : t) (vars : Var.t list) (ids_ml : Ml.id list) : t =
  List.fold_left2 add_binding ctx vars ids_ml
