open Lang
open Xl
open Il
module Value = Runtime.Dynamic_Il.Value
open Util.Source
open Error

(* Conversion between meta-numerics and OCaml numerics *)

let bigint_of_value (value : value) : Bigint.t =
  value |> Value.get_num |> Num.to_int

let value_of_bigint (add : value -> unit) (i : Bigint.t) : value =
  let value = Value.make (Il.NumT `NatT) (NumV (`Nat i)) in
  add value;
  value

(* dec $sum_nat(nat* ) : nat *)

let sum_nat (add : value -> unit) (at : region) (targs : targ list)
    (values_input : value list) : value =
  Extract.zero at targs;
  let values =
    Extract.one at values_input |> Value.get_list |> List.map bigint_of_value
  in
  let sum = List.fold_left Bigint.( + ) Bigint.zero values in
  value_of_bigint add sum

(* dec $max_nat(nat* ) : nat *)

let max_nat (add : value -> unit) (at : region) (targs : targ list)
    (values_input : value list) : value =
  Extract.zero at targs;
  let values =
    Extract.one at values_input |> Value.get_list |> List.map bigint_of_value
  in
  let max =
    match values with
    | [] -> error at "max of empty list"
    | hd :: tl -> List.fold_left Bigint.max hd tl
  in
  value_of_bigint add max

(* dec $min_nat(nat* ) : nat *)

let min_nat (add : value -> unit) (at : region) (targs : targ list)
    (values_input : value list) : value =
  Extract.zero at targs;
  let values =
    Extract.one at values_input |> Value.get_list |> List.map bigint_of_value
  in
  let min =
    match values with
    | [] -> error at "min of empty list"
    | hd :: tl -> List.fold_left Bigint.min hd tl
  in
  value_of_bigint add min
