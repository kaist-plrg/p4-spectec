open Lang
open Xl
open Il
module Typ = Runtime.Type.Typ
module Value = Runtime.Value
open Util.Source

(* dec $rev_<X>(X* ) : X* *)

let rev_ (add : value -> unit) (at : region) (targs : targ list)
    (values_input : value list) : value =
  let typ = Extract.one at targs in
  let typ_list = Typ.Make.list typ in
  let values = Extract.one at values_input |> Value.Get.list in
  let value = Value.Make.list typ_list (List.rev values) in
  add value;
  value

(* dec $concat_<X>((X* )* ) : X* *)

let concat_ (add : value -> unit) (at : region) (targs : targ list)
    (values_input : value list) : value =
  let typ = Extract.one at targs in
  let typ_list = Typ.Make.list typ in
  let values =
    Extract.one at values_input
    |> Value.Get.list
    |> List.concat_map Value.Get.list
  in
  let value = Value.Make.list typ_list values in
  add value;
  value

(* dec $distinct_<K>(K* ) : bool *)

let distinct_ (add : value -> unit) (at : region) (targs : targ list)
    (values_input : value list) : value =
  let _typ = Extract.one at targs in
  let values = Extract.one at values_input |> Value.Get.list in
  let set = Sets.VSet.of_list values in
  let value = Value.Make.bool (Sets.VSet.cardinal set = List.length values) in
  add value;
  value

(* dec $partition_<X>(X*, nat) : (X*, X* ) *)

let partition_ (add : value -> unit) (at : region) (targs : targ list)
    (values_input : value list) : value =
  let typ = Extract.one at targs in
  let typ_list = Typ.Make.list typ in
  let value_list, value_len = Extract.two at values_input in
  let values = Value.Get.list value_list in
  let len = value_len |> Value.Get.num |> Num.to_int |> Bigint.to_int_exn in
  let values_left, values_right =
    values
    |> List.mapi (fun idx value -> (idx, value))
    |> List.partition (fun (idx, _) -> idx < len)
  in
  let value_left = Value.Make.list typ_list (List.map snd values_left) in
  add value_left;
  let value_right = Value.Make.list typ_list (List.map snd values_right) in
  add value_right;
  let typ_tuple = Typ.Make.tuple [ typ; typ ] in
  let value = Value.Make.tuple typ_tuple [ value_left; value_right ] in
  add value;
  value

(* dec $assoc_<X, Y>(X, (X, Y)* ) : Y? *)

let assoc_ (add : value -> unit) (at : region) (targs : targ list)
    (values_input : value list) : value =
  let _typ_key, typ_value = Extract.two at targs in
  let value, value_list = Extract.two at values_input in
  let values =
    value_list |> Value.Get.list
    |> List.map (fun value ->
           match value.it with
           | TupleV [ value_key; value_value ] -> (value_key, value_value)
           | _ -> assert false)
  in
  let typ_opt = Typ.Make.opt typ_value in
  let value_opt =
    List.fold_left
      (fun value_found (value_key, value_value) ->
        match value_found with
        | Some _ -> value_found
        | None when Value.compare value value_key = 0 -> Some value_value
        | None -> None)
      None values
  in
  let value = Value.Make.opt typ_opt value_opt in
  add value;
  value

(* dec $sort_<X>((nat, X)* ) : (nat, X)* *)

let sort_ (add : value -> unit) (at : region) (targs : targ list)
    (values_input : value list) : value =
  let typ_value = Extract.one at targs in
  let typ = Typ.Make.tuple [ Typ.Make.nat; typ_value ] |> Typ.Make.list in
  let value_list = Extract.one at values_input in
  let values =
    value_list |> Value.Get.list
    |> List.map (fun value ->
           match value.it with
           | TupleV [ value_key; value_value ] ->
               let n_key = value_key |> Value.Get.num |> Num.to_int in
               (n_key, (value_key, value_value, value.note))
           | _ -> assert false)
  in
  let values =
    List.sort (fun (n_a, _) (n_b, _) -> Bigint.compare n_a n_b) values
  in
  let values =
    List.map
      (fun (_, (value_key, value_value, note)) ->
        TupleV [ value_key; value_value ] $$$ note)
      values
  in
  let value = Value.Make.list typ values in
  add value;
  value
