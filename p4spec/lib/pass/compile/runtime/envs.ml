open Domain.Lib
open Util.Source

(* Environments *)

module Typdefs = Runtime.Type.Envs.TDEnv

(* Constructor environment *)

module Ctors = struct
  module CaseMap = Map.Make (Case)
  include CaseMap

  type t = Ml.ctor CaseMap.t
end

(* Binding environment mapping SpecTec vars to OCaml vars *)

module Bindings = Runtime.Dynamic.Envs.MakeVarEnv (struct
  type t = Ml.id

  let to_string = Fun.id
end)

(* Bound OCaml vars *)

module Bounds = struct
  type t = IdSet.t list

  (* Push and pop block *)

  let push (bounds : t) : t = IdSet.empty :: bounds
  let pop (bounds : t) : t = List.tl bounds

  (* Fresh ocaml variable *)

  let fresh (bounds : t) (id_ml : Ml.id) : t * Ml.id =
    let ids_ml = List.fold_left IdSet.union IdSet.empty bounds in
    let rec fresh (id_ml : Ml.id) : Ml.id =
      if IdSet.mem (id_ml $ no_region) ids_ml then id_ml ^ "_" |> fresh
      else id_ml
    in
    let id_ml = fresh id_ml in
    let bounds =
      match bounds with
      | [] -> assert false
      | ids_ml_h :: bounds_t ->
          IdSet.add (id_ml $ no_region) ids_ml_h :: bounds_t
    in
    (bounds, id_ml)
end
