module Mixfix = Domain.Mixfix
open Lang
open Il
module Typ = Runtime.Type.Typ
module Value = Runtime.Value

module Make (V : Valrep.SAFE) = struct
  open Error
  open Util.Source

  (* Total order on values of spec type [typ]: convert both to a real [Value.t]
     (identity under [V_value]; the type-directed [marshal_<typ>] under [V_native],
     since a typed [Obj.t] is erased) then [Value.compare]. This keeps the element
     order identical across representations. *)
  let vcompare (typ : typ) (a : V.t) (b : V.t) : int =
    Value.compare (V.marshal typ a) (V.marshal typ b)

  (* A set is a [V.t list] kept sorted-and-deduped under a runtime comparator
     [cmp = vcompare typ_key]. The comparator cannot be a module-level [Set.Make]
     argument because the element type is only known per call, so the ops are
     list-based; sets are small (ids in typing), so this is cheap and the element
     order is canonical (= [V_value]'s [Value.compare] order). *)

  let norm cmp xs = List.sort_uniq cmp xs
  let mem cmp x ys = List.exists (fun y -> cmp x y = 0) ys
  let inter cmp a b = List.filter (fun x -> mem cmp x b) a
  let union cmp a b = norm cmp (a @ b)
  let diff cmp a b = List.filter (fun x -> not (mem cmp x b)) a
  let subset cmp a b = List.for_all (fun x -> mem cmp x b) a

  let equal cmp a b =
    List.compare_lengths a b = 0 && List.for_all2 (fun x y -> cmp x y = 0) a b

  (* Conversion between meta-sets and OCaml lists, via the generic case ops
     ([( <<| )] / [Get.case]) — under [V_native] those route to the typed case
     bridge's set arm. The set mixop is [`{ k }]; inspection needs only the type
     head, so a targ-less [set] suffices ([case_of_typed] ignores targs, and
     [V_value] ignores the type entirely). *)

  let mixop_set = "`{ k }"
  let typ_set = Typ.Make.var ("set" $ no_region) []

  let set_of_value cmp (value : V.t) : V.t list =
    match Mixfix.args (V.Get.case value typ_set) with
    | [ value_elements ] -> value_elements |> V.Get.list |> norm cmp
    | _ ->
        error no_region
          (Format.asprintf "expected a set, but got %s" (V.to_string value))

  let value_of_set (add : V.t -> unit) (typ_key : typ) (set : V.t list) : V.t =
    let typ_list = Typ.Make.list typ_key in
    let value_elements = V.Make.list typ_list set in
    add value_elements;
    let value =
      let typ = Typ.Make.var ("set" $ no_region) [ typ_key ] in
      V.Make.(mixop_set <| [ value_elements ] <<| typ)
    in
    add value;
    value

  (* dec $intersect_set<K>(set<K>, set<K>) : set<K> *)

  let intersect_set (add : V.t -> unit) (at : region) (targs : targ list)
      (values_input : V.t list) : V.t =
    let typ_key = Extract.one at targs in
    let cmp = vcompare typ_key in
    let value_set_a, value_set_b = Extract.two at values_input in
    let set_a = set_of_value cmp value_set_a in
    let set_b = set_of_value cmp value_set_b in
    inter cmp set_a set_b |> value_of_set add typ_key

  (* dec $union_set<K>(set<K>, set<K>) : set<K> *)

  let union_set (add : V.t -> unit) (at : region) (targs : targ list)
      (values_input : V.t list) : V.t =
    let typ_key = Extract.one at targs in
    let cmp = vcompare typ_key in
    let value_set_a, value_set_b = Extract.two at values_input in
    let set_a = set_of_value cmp value_set_a in
    let set_b = set_of_value cmp value_set_b in
    union cmp set_a set_b |> value_of_set add typ_key

  (* dec $unions_set<K>(set<K>* ) : set<K> *)

  let unions_set (add : V.t -> unit) (at : region) (targs : targ list)
      (values_input : V.t list) : V.t =
    let typ_key = Extract.one at targs in
    let cmp = vcompare typ_key in
    let value_sets = Extract.one at values_input in
    let sets = value_sets |> V.Get.list |> List.map (set_of_value cmp) in
    sets |> List.fold_left (union cmp) [] |> value_of_set add typ_key

  (* dec $diff_set<K>(set<K>, set<K>) : set<K> *)

  let diff_set (add : V.t -> unit) (at : region) (targs : targ list)
      (values_input : V.t list) : V.t =
    let typ_key = Extract.one at targs in
    let cmp = vcompare typ_key in
    let value_set_a, value_set_b = Extract.two at values_input in
    let set_a = set_of_value cmp value_set_a in
    let set_b = set_of_value cmp value_set_b in
    diff cmp set_a set_b |> value_of_set add typ_key

  (* dec $sub_set<K>(set<K>, set<K>) : bool *)

  let sub_set (add : V.t -> unit) (at : region) (targs : targ list)
      (values_input : V.t list) : V.t =
    let typ_key = Extract.one at targs in
    let cmp = vcompare typ_key in
    let value_set_a, value_set_b = Extract.two at values_input in
    let set_a = set_of_value cmp value_set_a in
    let set_b = set_of_value cmp value_set_b in
    let value = V.Make.bool (subset cmp set_a set_b) in
    add value;
    value

  (* dec $eq_set<K>(set<K>, set<K>) : bool *)

  let eq_set (add : V.t -> unit) (at : region) (targs : targ list)
      (values_input : V.t list) : V.t =
    let typ_key = Extract.one at targs in
    let cmp = vcompare typ_key in
    let value_set_a, value_set_b = Extract.two at values_input in
    let set_a = set_of_value cmp value_set_a in
    let set_b = set_of_value cmp value_set_b in
    let value = V.Make.bool (equal cmp set_a set_b) in
    add value;
    value
end
