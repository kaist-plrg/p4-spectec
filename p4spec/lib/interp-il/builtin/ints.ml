open Xl
open Il.Ast
module Value = Runtime_dynamic.Value
open Util.Source

(* Conversion between meta-numerics and OCaml numerics *)

let bigint_of_value (value : value) : Bigint.t =
  value |> Value.get_num |> Num.to_int

let value_of_bigint (i : Bigint.t) : value =
  let value =
    let vid = Value.fresh () in
    let typ = Il.Ast.NumT `IntT in
    NumV (`Int i) $$$ { vid; typ }
  in
  value

(* dec $sum_int(int* ) : int *)

let sum_int (at : region) (targs : targ list) (values_input : value list) :
    value =
  Extract.zero at targs;
  let values =
    Extract.one at values_input |> Value.get_list |> List.map bigint_of_value
  in
  let sum = List.fold_left Bigint.( + ) Bigint.zero values in
  value_of_bigint sum

(* dec $max_int(int* ) : int *)

let max_int (at : region) (targs : targ list) (values_input : value list) :
    value =
  Extract.zero at targs;
  let values =
    Extract.one at values_input |> Value.get_list |> List.map bigint_of_value
  in
  let max = List.fold_left Bigint.max Bigint.zero values in
  value_of_bigint max

(* dec $min_int(int* ) : int *)

let min_int (at : region) (targs : targ list) (values_input : value list) :
    value =
  Extract.zero at targs;
  let values =
    Extract.one at values_input |> Value.get_list |> List.map bigint_of_value
  in
  let min = List.fold_left Bigint.min Bigint.zero values in
  value_of_bigint min
