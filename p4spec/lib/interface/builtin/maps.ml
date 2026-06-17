module Mixfix = Domain.Mixfix
open Lang
open Il
module Typ = Runtime.Type.Typ

module Make (V : Valrep.VAL) = struct
  open Error
  open Util.Source

  (* Value map. Built/inspected via the generic case DSL ([( <<| )] / [Get.case]),
     which under V_typed routes to the typed bridge's pair/map arms (API.md B2c).
     A map value is a [`{ k }] of [k `: v] pairs ([map = pair set]). Inspection
     needs only the type head, so targ-less [pair]/[map] suffice ([case_of_typed]
     ignores targs; V_value ignores the type). *)

  type map = V.t list

  let mixop_pair = "k `: v"
  let mixop_map = "`{ k }"
  let typ_pair = Typ.Make.var ("pair" $ no_region) []
  let typ_map = Typ.Make.var ("map" $ no_region) []

  let rec map_find_opt key = function
    | [] -> None
    | pair :: pairs -> (
        match Mixfix.args (V.Get.case pair typ_pair) with
        | [ value_key; value_value ] when V.equal value_key key ->
            Some value_value
        | _ -> map_find_opt key pairs)

  let make_pair (add : V.t -> unit) (typ_key : typ) (typ_value : typ)
      (value_key : V.t) (value_value : V.t) : V.t =
    let typ = Typ.Make.var ("pair" $ no_region) [ typ_key; typ_value ] in
    let value_pair =
      V.Make.(mixop_pair <| [ value_key; value_value ] <<| typ)
    in
    add value_pair;
    value_pair

  let rec map_update make_pair key value = function
    | [] -> [ make_pair key value ]
    | pair :: pairs -> (
        match Mixfix.args (V.Get.case pair typ_pair) with
        | [ value_key; _ ] when V.equal value_key key ->
            make_pair key value :: pairs
        | _ -> pair :: map_update make_pair key value pairs)

  (* Conversion between meta-maps and OCaml lists *)

  let map_of_value (value : V.t) : map =
    match Mixfix.args (V.Get.case value typ_map) with
    | [ value_pairs ] -> value_pairs |> V.Get.list
    | _ ->
        error no_region
          (Format.asprintf "expected a map, but got %s" (V.to_string value))

  let value_of_map (add : V.t -> unit) (typ_key : typ) (typ_value : typ)
      (map : map) : V.t =
    let value_pairs =
      let typ =
        Typ.Make.var ("pair" $ no_region) [ typ_key; typ_value ]
        |> Typ.Make.list
      in
      V.Make.list typ map
    in
    add value_pairs;
    let value =
      let typ = Typ.Make.var ("map" $ no_region) [ typ_key; typ_value ] in
      V.Make.(mixop_map <| [ value_pairs ] <<| typ)
    in
    add value;
    value

  (* Built-in implementations *)

  (* dec $find_map<K, V>(map<K, V>, K) : V? *)

  let find_map (add : V.t -> unit) (at : region) (targs : targ list)
      (values_input : V.t list) : V.t =
    let _typ_key, typ_value = Extract.two at targs in
    let value_map, value_key = Extract.two at values_input in
    let map = map_of_value value_map in
    let typ_opt = Typ.Make.opt typ_value in
    let value_opt = map_find_opt value_key map in
    let value = V.Make.opt typ_opt value_opt in
    add value;
    value

  (* dec $find_maps<K, V>(map<K, V>*, K) : V? *)

  let find_maps (add : V.t -> unit) (at : region) (targs : targ list)
      (values_input : V.t list) : V.t =
    let _typ_key, typ_value = Extract.two at targs in
    let value_maps, value_key = Extract.two at values_input in
    let maps = value_maps |> V.Get.list |> List.map map_of_value in
    let typ_opt = Typ.Make.opt typ_value in
    let value_opt =
      List.fold_left
        (fun value_opt map ->
          match value_opt with
          | Some _ -> value_opt
          | None -> map_find_opt value_key map)
        None maps
    in
    let value = V.Make.opt typ_opt value_opt in
    add value;
    value

  (* dec $add_map<K, V>(map<K, V>, K, V) : map<K, V> *)

  let add_map (add : V.t -> unit) (at : region) (targs : targ list)
      (values_input : V.t list) : V.t =
    let typ_key, typ_value = Extract.two at targs in
    let value_map, value_key, value_value = Extract.three at values_input in
    let mk = make_pair add typ_key typ_value in
    map_of_value value_map
    |> map_update mk value_key value_value
    |> value_of_map add typ_key typ_value

  (* dec $adds_map<K, V>(map<K, V>, K*, V* ) : map<K, V> *)

  let adds_map (add : V.t -> unit) (at : region) (targs : targ list)
      (values_input : V.t list) : V.t =
    let typ_key, typ_value = Extract.two at targs in
    let value_map, value_keys, value_values = Extract.three at values_input in
    let map = map_of_value value_map in
    let values_key = value_keys |> V.Get.list in
    let values_value = value_values |> V.Get.list in
    let mk = make_pair add typ_key typ_value in
    List.fold_left2
      (fun map value_key value_value -> map_update mk value_key value_value map)
      map values_key values_value
    |> value_of_map add typ_key typ_value

  (* dec $update_map<K, V>(map<K, V>, K, V) : map<K, V> *)

  let update_map (add : V.t -> unit) (at : region) (targs : targ list)
      (values_input : V.t list) : V.t =
    let typ_key, typ_value = Extract.two at targs in
    let value_map, value_key, value_value = Extract.three at values_input in
    let mk = make_pair add typ_key typ_value in
    map_of_value value_map
    |> map_update mk value_key value_value
    |> value_of_map add typ_key typ_value
end
