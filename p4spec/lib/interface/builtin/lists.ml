open Lang
open Xl
open Il
module Typ = Runtime.Type.Typ
module Value = Runtime.Value
open Error
open Util.Source

module Make (V : Valrep.SAFE) = struct
  let compare_v (typ : typ) (v_a : V.t) (v_b : V.t) : int =
    Value.compare (V.marshal typ v_a) (V.marshal typ v_b)

  (* dec $rev_<X>(X* ) : X* *)

  let rev_ (add : V.t -> unit) (at : region) (targs : targ list)
      (values_input : V.t list) : V.t =
    let typ = Extract.one at targs in
    let typ_list = Typ.Make.list typ in
    let values = Extract.one at values_input |> V.Get.list in
    let value = V.Make.list typ_list (List.rev values) in
    add value;
    value

  (* dec $concat_<X>((X* )* ) : X* *)

  let concat_ (add : V.t -> unit) (at : region) (targs : targ list)
      (values_input : V.t list) : V.t =
    let typ = Extract.one at targs in
    let typ_list = Typ.Make.list typ in
    let values =
      Extract.one at values_input |> V.Get.list |> List.concat_map V.Get.list
    in
    let value = V.Make.list typ_list values in
    add value;
    value

  (* dec $distinct_<K>(K* ) : bool *)

  let distinct_ (add : V.t -> unit) (at : region) (targs : targ list)
      (values_input : V.t list) : V.t =
    let typ = Extract.one at targs in
    let cmp = compare_v typ in
    let values = Extract.one at values_input |> V.Get.list in
    let value =
      V.Make.bool (List.length (List.sort_uniq cmp values) = List.length values)
    in
    add value;
    value

  (* dec $partition_<X>(X*, nat) : (X*, X* ) *)

  let partition_ (add : V.t -> unit) (at : region) (targs : targ list)
      (values_input : V.t list) : V.t =
    let typ = Extract.one at targs in
    let typ_list = Typ.Make.list typ in
    let value_list, value_len = Extract.two at values_input in
    let values = V.Get.list value_list in
    let len = value_len |> V.Get.num |> Num.to_int |> Bigint.to_int_exn in
    let values_left, values_right =
      values
      |> List.mapi (fun idx value -> (idx, value))
      |> List.partition (fun (idx, _) -> idx < len)
    in
    let value_left = V.Make.list typ_list (List.map snd values_left) in
    add value_left;
    let value_right = V.Make.list typ_list (List.map snd values_right) in
    add value_right;
    let typ_tuple = Typ.Make.tuple [ typ; typ ] in
    let value = V.Make.tuple typ_tuple [ value_left; value_right ] in
    add value;
    value

  (* dec $assoc_<X, Y>(X, (X, Y)* ) : Y? *)

  let assoc_ (add : V.t -> unit) (at : region) (targs : targ list)
      (values_input : V.t list) : V.t =
    let typ_key, typ_value = Extract.two at targs in
    let cmp = compare_v typ_key in
    let value, value_list = Extract.two at values_input in
    let values =
      value_list |> V.Get.list
      |> List.map (fun value ->
             match V.Get.tuple value with
             | [ value_key; value_value ] -> (value_key, value_value)
             | _ -> assert false)
    in
    let typ_opt = Typ.Make.opt typ_value in
    let value_opt =
      List.fold_left
        (fun value_found (value_key, value_value) ->
          match value_found with
          | Some _ -> value_found
          | None when cmp value value_key = 0 -> Some value_value
          | None -> None)
        None values
    in
    let value = V.Make.opt typ_opt value_opt in
    add value;
    value

  (* dec $sort_<X>((nat, X)* ) : (nat, X)* *)

  let sort_ (add : V.t -> unit) (at : region) (targs : targ list)
      (values_input : V.t list) : V.t =
    let typ_value = Extract.one at targs in
    let typ = Typ.Make.tuple [ Typ.Make.nat; typ_value ] |> Typ.Make.list in
    let value_list = Extract.one at values_input in
    let values =
      value_list |> V.Get.list
      |> List.map (fun value ->
             match V.Get.tuple value with
             | [ value_key; _ ] ->
                 let n_key = value_key |> V.Get.num |> Num.to_int in
                 (n_key, value)
             | _ -> assert false)
    in
    let values =
      List.sort (fun (n_a, _) (n_b, _) -> Bigint.compare n_a n_b) values
    in
    (* Keep the original tuple values, just reordered (preserves their ids). *)
    let values = List.map snd values in
    let value = V.Make.list typ values in
    add value;
    value

  (* builtin dec $transpose_<X>(X** ) : X** *)

  let transpose_ (add : V.t -> unit) (at : region) (targs : targ list)
      (values_input : V.t list) : V.t =
    let typ = Extract.one at targs in
    let typ_list = Typ.Make.list typ in
    let typ_matrix = Typ.Make.list typ_list in
    let value = Extract.one at values_input in
    let value_matrix =
      value |> V.Get.list |> List.map (fun value -> value |> V.Get.list)
    in
    let value_matrix =
      match value_matrix with
      | [] -> []
      | value_row_h :: _ -> (
          let width = List.length value_row_h in
          let value_cols = Array.make width [] in
          try
            List.iter
              (fun value_row ->
                if List.length value_row <> width then
                  raise (Invalid_argument "cannot transpose a matrix of values");
                List.iteri
                  (fun j value -> value_cols.(j) <- value :: value_cols.(j))
                  value_row)
              (List.rev value_matrix);
            Array.to_list value_cols
          with Invalid_argument msg -> error no_region msg)
    in
    let value =
      value_matrix
      |> List.map (fun values_row ->
             let value_row = V.Make.list typ_list values_row in
             add value_row;
             value_row)
      |> V.Make.list typ_matrix
    in
    add value;
    value
end
