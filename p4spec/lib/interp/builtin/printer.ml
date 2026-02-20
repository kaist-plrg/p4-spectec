open Lang
open Il
module Value = Runtime.Dynamic_Il.Value
open Util.Source

let printer = ref (fun _ -> "")

(* dec $print_<X>(X) : text *)

let print (add : value -> unit) (at : region) (targs : targ list)
    (values_input : value list) : value =
  let _typ = Extract.one at targs in
  let value = Extract.one at values_input in
  let text = !printer value in
  let value = Value.make Il.TextT (TextV text) in
  add value;
  value
