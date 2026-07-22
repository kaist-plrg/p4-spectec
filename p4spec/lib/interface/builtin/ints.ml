open Lang
open Xl
open Il
open Util.Source

module Make (V : Valrep.SAFE) = struct
  (* Conversion between meta-numerics and OCaml numerics *)

  let bigint_of_value (value : V.t) : Bigint.t =
    value |> V.Get.num |> Num.to_int

  let value_of_bigint (add : V.t -> unit) (i : Bigint.t) : V.t =
    let value = V.Make.int i in
    add value;
    value

  (* dec $sum_int(nat* ) : nat *)

  let sum_int (add : V.t -> unit) (at : region) (targs : targ list)
      (values_input : V.t list) : V.t =
    Extract.zero at targs;
    let values =
      Extract.one at values_input |> V.Get.list |> List.map bigint_of_value
    in
    let sum = List.fold_left Bigint.( + ) Bigint.zero values in
    value_of_bigint add sum

  (* dec $max_int(int* ) : int *)

  let max_int (add : V.t -> unit) (at : region) (targs : targ list)
      (values_input : V.t list) : V.t =
    Extract.zero at targs;
    let values =
      Extract.one at values_input |> V.Get.list |> List.map bigint_of_value
    in
    let max =
      match values with
      | [] -> Bigint.zero
      | value_h :: values_t -> List.fold_left Bigint.max value_h values_t
    in
    value_of_bigint add max

  (* dec $min_int(int* ) : int *)

  let min_int (add : V.t -> unit) (at : region) (targs : targ list)
      (values_input : V.t list) : V.t =
    Extract.zero at targs;
    let values =
      Extract.one at values_input |> V.Get.list |> List.map bigint_of_value
    in
    let min =
      match values with
      | [] -> Bigint.zero
      | value_h :: values_t -> List.fold_left Bigint.min value_h values_t
    in
    value_of_bigint add min
end
