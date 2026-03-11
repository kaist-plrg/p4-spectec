open Domain
open Lang
open Il
module Value = Runtime.Dynamic_Il.Value
open Error
open Util.Source

(* Value map *)

type map = (Value.t * Value.t) list

let rec map_find_opt key = function
  | [] -> None
  | (k, v) :: _ when Value.eq k key -> Some v
  | _ :: t -> map_find_opt key t

let rec map_update key value = function
  | [] -> [ (key, value) ]
  | (k, _) :: t when Value.eq k key -> (key, value) :: t
  | h :: t -> h :: map_update key value t

(* Conversion between meta-maps and OCaml assoc lists *)

let map_of_value (value : value) : map =
  let tuple_of_value (value : value) : value * value =
    match value.it with
    | CaseV ([ []; [ { it = Atom.Colon; _ } ]; [] ], [ value_key; value_value ])
      ->
        (value_key, value_value)
    | _ ->
        error no_region
          (Format.asprintf "expected a pair, but got %s" (Value.to_string value))
  in
  match value.it with
  | CaseV
      ( [ [ { it = Atom.LBrace; _ } ]; [ { it = Atom.RBrace; _ } ] ],
        [ value_pairs ] ) ->
      Value.get_list value_pairs |> List.map tuple_of_value
  | _ ->
      error no_region
        (Format.asprintf "expected a map, but got %s" (Value.to_string value))

let value_of_map (add : value -> unit) (typ_key : typ) (typ_value : typ)
    (map : map) : value =
  let value_of_tuple ((value_key, value_value) : value * value) : value =
    let value =
      let typ = Il.VarT ("pair" $ no_region, [ typ_key; typ_value ]) in
      Value.make typ
        (CaseV
           ([ []; [ Atom.Colon $ no_region ]; [] ], [ value_key; value_value ]))
    in
    add value;
    value
  in
  let value_pairs =
    let typ =
      Il.IterT
        ( Il.VarT ("pair" $ no_region, [ typ_key; typ_value ]) $ no_region,
          Il.List )
    in
    Value.make typ (ListV (map |> List.map value_of_tuple))
  in
  add value_pairs;
  let value =
    let typ = Il.VarT ("map" $ no_region, [ typ_key; typ_value ]) in
    Value.make typ
      (CaseV
         ( [ [ Atom.LBrace $ no_region ]; [ Atom.RBrace $ no_region ] ],
           [ value_pairs ] ))
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
  let value_opt = map_find_opt value_key map in
  let value =
    let typ = Il.IterT (typ_value, Il.Opt) in
    Value.make typ (OptV value_opt)
  in
  add value;
  value

(* dec $find_maps<K, V>(map<K, V>*, K) : V? *)

let find_maps (add : value -> unit) (at : region) (targs : targ list)
    (values_input : value list) : value =
  let _typ_key, typ_value = Extract.two at targs in
  let value_maps, value_key = Extract.two at values_input in
  let maps = value_maps |> Value.get_list |> List.map map_of_value in
  let value_opt =
    List.fold_left
      (fun value_opt map ->
        match value_opt with
        | Some _ -> value_opt
        | None -> map_find_opt value_key map)
      None maps
  in
  let value =
    let typ = Il.IterT (typ_value, Il.Opt) in
    Value.make typ (OptV value_opt)
  in
  add value;
  value

(* dec $add_map<K, V>(map<K, V>, K, V) : map<K, V> *)

let add_map (add : value -> unit) (at : region) (targs : targ list)
    (values_input : value list) : value =
  let typ_key, typ_value = Extract.two at targs in
  let value_map, value_key, value_value = Extract.three at values_input in
  map_of_value value_map
  |> map_update value_key value_value
  |> value_of_map add typ_key typ_value

(* dec $adds_map<K, V>(map<K, V>, K*, V* ) : map<K, V> *)

let adds_map (add : value -> unit) (at : region) (targs : targ list)
    (values_input : value list) : value =
  let typ_key, typ_value = Extract.two at targs in
  let value_map, value_keys, value_values = Extract.three at values_input in
  let map = map_of_value value_map in
  let values_key = value_keys |> Value.get_list in
  let values_value = value_values |> Value.get_list in
  List.fold_left2
    (fun map value_key value_value -> map_update value_key value_value map)
    map values_key values_value
  |> value_of_map add typ_key typ_value

(* dec $update_map<K, V>(map<K, V>, K, V) : map<K, V> *)

let update_map (add : value -> unit) (at : region) (targs : targ list)
    (values_input : value list) : value =
  let typ_key, typ_value = Extract.two at targs in
  let value_map, value_key, value_value = Extract.three at values_input in
  map_of_value value_map
  |> map_update value_key value_value
  |> value_of_map add typ_key typ_value
