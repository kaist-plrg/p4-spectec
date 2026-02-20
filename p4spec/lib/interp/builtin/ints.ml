open Lang
open Xl
open Il
module Value = Runtime.Dynamic_Il.Value
open Util.Source

(* Conversion between meta-numerics and OCaml numerics *)

let bigint_of_value (value : value) : Bigint.t =
  value |> Value.get_num |> Num.to_int

let value_of_bigint (add : value -> unit) (i : Bigint.t) : value =
  let value = Value.make (Il.NumT `IntT) (NumV (`Int i)) in
  add value;
  value

(* dec $sum_int(nat* ) : nat *)

let sum_int (add : value -> unit) (at : region) (targs : targ list)
    (values_input : value list) : value =
  Extract.zero at targs;
  let values =
    Extract.one at values_input |> Value.get_list |> List.map bigint_of_value
  in
  let sum = List.fold_left Bigint.( + ) Bigint.zero values in
  value_of_bigint add sum

(* dec $max_int(int* ) : int *)

let max_int (add : value -> unit) (at : region) (targs : targ list)
    (values_input : value list) : value =
  Extract.zero at targs;
  let values =
    Extract.one at values_input |> Value.get_list |> List.map bigint_of_value
  in
  let max =
    match values with
    | [] -> Bigint.zero
    | value_h :: values_t -> List.fold_left Bigint.max value_h values_t
  in
  value_of_bigint add max

(* dec $min_int(int* ) : int *)

let min_int (add : value -> unit) (at : region) (targs : targ list)
    (values_input : value list) : value =
  Extract.zero at targs;
  let values =
    Extract.one at values_input |> Value.get_list |> List.map bigint_of_value
  in
  let min =
    match values with
    | [] -> Bigint.zero
    | value_h :: values_t -> List.fold_left Bigint.min value_h values_t
  in
  value_of_bigint add min
