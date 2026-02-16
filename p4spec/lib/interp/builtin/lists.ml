open Lang
open Xl
open Il
module Value = Runtime.Dynamic_Il.Value
open Util.Source

(* dec $rev_<X>(X* ) : X* *)

let rev_ (add : value -> unit) (at : region) (targs : targ list)
    (values_input : value list) : value =
  let typ = Extract.one at targs in
  let values = Extract.one at values_input |> Value.get_list in
  let value =
    let typ = Il.IterT (typ, Il.List) in
    Value.make typ (ListV (List.rev values))
  in
  add value;
  value

(* dec $concat_<X>((X* )* ) : X* *)

let concat_ (add : value -> unit) (at : region) (targs : targ list)
    (values_input : value list) : value =
  let typ = Extract.one at targs in
  let values =
    Extract.one at values_input
    |> Value.get_list
    |> List.concat_map Value.get_list
  in
  let value =
    let typ = Il.IterT (typ, Il.List) in
    Value.make typ (ListV values)
  in
  add value;
  value

(* dec $distinct_<K>(K* ) : bool *)

let distinct_ (add : value -> unit) (at : region) (targs : targ list)
    (values_input : value list) : value =
  let _typ = Extract.one at targs in
  let values = Extract.one at values_input |> Value.get_list in
  let set = Sets.VSet.of_list values in
  let value =
    Value.make Il.BoolT (BoolV (Sets.VSet.cardinal set = List.length values))
  in
  add value;
  value

(* dec $partition_<X>(X*, nat) : (X*, X* ) *)

let partition_ (add : value -> unit) (at : region) (targs : targ list)
    (values_input : value list) : value =
  let typ = Extract.one at targs in
  let value_list, value_len = Extract.two at values_input in
  let values = Value.get_list value_list in
  let len = value_len |> Value.get_num |> Num.to_int |> Bigint.to_int_exn in
  let values_left, values_right =
    values
    |> List.mapi (fun idx value -> (idx, value))
    |> List.partition (fun (idx, _) -> idx < len)
  in
  let value_left =
    let typ = Il.IterT (typ, Il.List) in
    Value.make typ (ListV (List.map snd values_left))
  in
  add value_left;
  let value_right =
    let typ = Il.IterT (typ, Il.List) in
    Value.make typ (ListV (List.map snd values_right))
  in
  add value_right;
  let value =
    let typ =
      Il.TupleT
        [ value_left.note.typ $ no_region; value_right.note.typ $ no_region ]
    in
    Value.make typ (TupleV [ value_left; value_right ])
  in
  add value;
  value

(* dec $assoc_<X, Y>(X, (X, Y)* ) : Y? *)

let assoc_ (add : value -> unit) (at : region) (targs : targ list)
    (values_input : value list) : value =
  let _typ_key, typ_value = Extract.two at targs in
  let value, value_list = Extract.two at values_input in
  let values =
    Value.get_list value_list
    |> List.map (fun value ->
           match value.it with
           | TupleV [ value_key; value_value ] -> (value_key, value_value)
           | _ -> assert false)
  in
  let value_opt =
    List.fold_left
      (fun value_found (value_key, value_value) ->
        match value_found with
        | Some _ -> value_found
        | None when Value.compare value value_key = 0 -> Some value_value
        | None -> None)
      None values
  in
  let value =
    let typ = Il.IterT (typ_value, Il.Opt) in
    Value.make typ (OptV value_opt)
  in
  add value;
  value

(* dec $sort_<X>((nat, X)* ) : (nat, X)* *)

let sort_ (add : value -> unit) (at : region) (targs : targ list)
    (values_input : value list) : value =
  let _typ_value = Extract.one at targs in
  let value_list = Extract.one at values_input in
  let values =
    Value.get_list value_list
    |> List.map (fun value ->
           match value.it with
           | TupleV [ value_key; value_value ] ->
               let n_key = Value.get_num value_key |> Num.to_int in
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
  let value = Value.make value_list.note.typ (ListV values) in
  add value;
  value
