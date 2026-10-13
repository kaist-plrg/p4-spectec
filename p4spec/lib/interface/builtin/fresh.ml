open Lang
open Il
module Value = Runtime.Value
open Util.Source

(* dec $fresh_typeId() : typeId *)

let fresh_typeId (ctr : int ref) (add : value -> unit) (at : region)
    (targs : targ list) (values_input : value list) : value =
  Extract.zero at targs;
  Extract.zero at values_input;
  let tid = "FRESH__" ^ string_of_int !ctr in
  ctr := !ctr + 1;
  let value = Value.Make.text tid in
  add value;
  value

(* dec $fresh_nat() : nat -- 0, 1, 2, ..., from the counter of fresh_typeId,
   so that a call is a side effect the call caches see *)

let fresh_nat (ctr : int ref) (add : value -> unit) (at : region)
    (targs : targ list) (values_input : value list) : value =
  Extract.zero at targs;
  Extract.zero at values_input;
  let value = Value.Make.nat (Bigint.of_int !ctr) in
  ctr := !ctr + 1;
  add value;
  value
