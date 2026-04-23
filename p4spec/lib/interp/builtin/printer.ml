open Lang
open Il
module Value = Runtime.Value
open Util.Source

(* dec $print_<X>(X) : text *)

let print (printer : (value -> string) ref) (add : value -> unit) (at : region)
    (targs : targ list) (values_input : value list) : value =
  let _typ = Extract.one at targs in
  let value = Extract.one at values_input in
  let text = !printer value in
  let value = Value.Make.text text in
  add value;
  value
