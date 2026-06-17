module Mixfix = Domain.Mixfix
open Lang
open Il
module Typ = Runtime.Type.Typ
module V = Valrep.V_value
open Error
open Util.Source

(* Value set, ordered by the value representation's own [compare]. *)

module VSet = Set.Make (struct
  type t = V.t

  let compare = V.compare
end)

type set = VSet.t

(* Conversion between meta-sets and OCaml lists, via the generic case DSL
   ([( <<| )] / [Get.case]) — under V_typed those route to the typed bridge's
   set arm (API.md B2c). The set mixop is [`{ k }]; inspection needs only the
   type head, so a targ-less [set] suffices ([case_of_typed] ignores targs, and
   V_value ignores the type entirely). *)

let mixop_set = "`{ k }"
let typ_set = Typ.Make.var ("set" $ no_region) []

let set_of_value (value : V.t) : set =
  match Mixfix.args (V.Get.case value typ_set) with
  | [ value_elements ] -> value_elements |> V.Get.list |> VSet.of_list
  | _ ->
      error no_region
        (Format.asprintf "expected a set, but got %s" (V.to_string value))

let value_of_set (add : V.t -> unit) (typ_key : typ) (set : set) : V.t =
  let values_element = VSet.elements set in
  let typ_list = Typ.Make.list typ_key in
  let value_elements = V.Make.list typ_list values_element in
  add value_elements;
  let value =
    let typ = Typ.Make.var ("set" $ no_region) [ typ_key ] in
    V.Make.(mixop_set <| [ value_elements ] <<| typ)
  in
  add value;
  value

(* dec $intersect_set<K>(set<K>, set<K>) : set<K> *)

let intersect_set (add : value -> unit) (at : region) (targs : targ list)
    (values_input : value list) : value =
  let typ_key = Extract.one at targs in
  let value_set_a, value_set_b = Extract.two at values_input in
  let set_a = set_of_value value_set_a in
  let set_b = set_of_value value_set_b in
  VSet.inter set_a set_b |> value_of_set add typ_key

(* dec $union_set<K>(set<K>, set<K>) : set<K> *)

let union_set (add : value -> unit) (at : region) (targs : targ list)
    (values_input : value list) : value =
  let typ_key = Extract.one at targs in
  let value_set_a, value_set_b = Extract.two at values_input in
  let set_a = set_of_value value_set_a in
  let set_b = set_of_value value_set_b in
  VSet.union set_a set_b |> value_of_set add typ_key

(* dec $unions_set<K>(set<K>* ) : set<K> *)

let unions_set (add : value -> unit) (at : region) (targs : targ list)
    (values_input : value list) : value =
  let typ_key = Extract.one at targs in
  let value_sets = Extract.one at values_input in
  let sets = value_sets |> V.Get.list |> List.map set_of_value in
  sets |> List.fold_left VSet.union VSet.empty |> value_of_set add typ_key

(* dec $diff_set<K>(set<K>, set<K>) : set<K> *)

let diff_set (add : value -> unit) (at : region) (targs : targ list)
    (values_input : value list) : value =
  let typ_key = Extract.one at targs in
  let value_set_a, value_set_b = Extract.two at values_input in
  let set_a = set_of_value value_set_a in
  let set_b = set_of_value value_set_b in
  VSet.diff set_a set_b |> value_of_set add typ_key

(* dec $sub_set<K>(set<K>, set<K>) : bool *)

let sub_set (add : value -> unit) (at : region) (targs : targ list)
    (values_input : value list) : value =
  let _typ_key = Extract.one at targs in
  let value_set_a, value_set_b = Extract.two at values_input in
  let set_a = set_of_value value_set_a in
  let set_b = set_of_value value_set_b in
  let value = V.Make.bool (VSet.subset set_a set_b) in
  add value;
  value

(* dec $eq_set<K>(set<K>, set<K>) : bool *)

let eq_set (add : value -> unit) (at : region) (targs : targ list)
    (values_input : value list) : value =
  let _typ_key = Extract.one at targs in
  let value_set_a, value_set_b = Extract.two at values_input in
  let set_a = set_of_value value_set_a in
  let set_b = set_of_value value_set_b in
  let value = V.Make.bool (VSet.equal set_a set_b) in
  add value;
  value
