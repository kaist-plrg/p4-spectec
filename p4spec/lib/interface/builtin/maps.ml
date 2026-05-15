open Domain
open Lang
open Il
module Typ = Runtime.Type.Typ
module Value = Runtime.Value
open Error
open Util.Source

(* Value map *)

type map = value list

let mixop_pair = Value.Mixops.of_string "k `: v"
let mixop_map = Value.Mixops.of_string "`{ k }"

let rec map_find_opt key = function
  | [] -> None
  | pair :: pairs -> (
      match pair.it with
      | CaseV (mixop, [ value_key; value_value ])
        when Mixop.eq mixop mixop_pair && Value.eq value_key key ->
          Some value_value
      | _ -> map_find_opt key pairs)

let make_pair (add : value -> unit) (typ_key : typ) (typ_value : typ)
    (value_key : value) (value_value : value) : value =
  let typ = Typ.Make.var ("pair" $ no_region) [ typ_key; typ_value ] in
  let valuecase = (mixop_pair, [ value_key; value_value ]) in
  let value_pair = Value.Make.case typ valuecase in
  add value_pair;
  value_pair

let rec map_update make_pair key value = function
  | [] -> [ make_pair key value ]
  | pair :: pairs -> (
      match pair.it with
      | CaseV (mixop, [ value_key; _ ])
        when Mixop.eq mixop mixop_pair && Value.eq value_key key ->
          make_pair key value :: pairs
      | _ -> pair :: map_update make_pair key value pairs)

(* Conversion between meta-maps and OCaml lists *)

let map_of_value (value : value) : map =
  match value.it with
  | CaseV (mixop, [ value_pairs ]) when Mixop.eq mixop mixop_map ->
      value_pairs |> Value.Get.list
  | _ ->
      error no_region
        (Format.asprintf "expected a map, but got %s" (Value.to_string value))

let value_of_map (add : value -> unit) (typ_key : typ) (typ_value : typ)
    (map : map) : value =
  let value_pairs =
    let typ =
      Typ.Make.var ("pair" $ no_region) [ typ_key; typ_value ] |> Typ.Make.list
    in
    Value.Make.list typ map
  in
  add value_pairs;
  let value =
    let typ = Typ.Make.var ("map" $ no_region) [ typ_key; typ_value ] in
    let valuecase = (mixop_map, [ value_pairs ]) in
    Value.Make.case typ valuecase
  in
  add value;
  value

(* Built-in implementations *)

(* dec $find_map<K, V>(map<K, V>, K) : V? *)

let find_map (add : value -> unit) (at : region) (targs : targ list)
    (values_input : value list) : value =
  let _typ_key, typ_value = Extract.two at targs in
  let value_map, value_key = Extract.two at values_input in
  let map = map_of_value value_map in
  let typ_opt = Typ.Make.opt typ_value in
  let value_opt = map_find_opt value_key map in
  let value = Value.Make.opt typ_opt value_opt in
  add value;
  value

(* dec $find_maps<K, V>(map<K, V>*, K) : V? *)

let find_maps (add : value -> unit) (at : region) (targs : targ list)
    (values_input : value list) : value =
  let _typ_key, typ_value = Extract.two at targs in
  let value_maps, value_key = Extract.two at values_input in
  let maps = value_maps |> Value.Get.list |> List.map map_of_value in
  let typ_opt = Typ.Make.opt typ_value in
  let value_opt =
    List.fold_left
      (fun value_opt map ->
        match value_opt with
        | Some _ -> value_opt
        | None -> map_find_opt value_key map)
      None maps
  in
  let value = Value.Make.opt typ_opt value_opt in
  add value;
  value

(* dec $add_map<K, V>(map<K, V>, K, V) : map<K, V> *)

let add_map (add : value -> unit) (at : region) (targs : targ list)
    (values_input : value list) : value =
  let typ_key, typ_value = Extract.two at targs in
  let value_map, value_key, value_value = Extract.three at values_input in
  let mk = make_pair add typ_key typ_value in
  map_of_value value_map
  |> map_update mk value_key value_value
  |> value_of_map add typ_key typ_value

(* dec $adds_map<K, V>(map<K, V>, K*, V* ) : map<K, V> *)

let adds_map (add : value -> unit) (at : region) (targs : targ list)
    (values_input : value list) : value =
  let typ_key, typ_value = Extract.two at targs in
  let value_map, value_keys, value_values = Extract.three at values_input in
  let map = map_of_value value_map in
  let values_key = value_keys |> Value.Get.list in
  let values_value = value_values |> Value.Get.list in
  let mk = make_pair add typ_key typ_value in
  List.fold_left2
    (fun map value_key value_value -> map_update mk value_key value_value map)
    map values_key values_value
  |> value_of_map add typ_key typ_value

(* dec $update_map<K, V>(map<K, V>, K, V) : map<K, V> *)

let update_map (add : value -> unit) (at : region) (targs : targ list)
    (values_input : value list) : value =
  let typ_key, typ_value = Extract.two at targs in
  let value_map, value_key, value_value = Extract.three at values_input in
  let mk = make_pair add typ_key typ_value in
  map_of_value value_map
  |> map_update mk value_key value_value
  |> value_of_map add typ_key typ_value
