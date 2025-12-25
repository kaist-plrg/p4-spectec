open Lang
open Il
module Value = Runtime.Dynamic_Il.Value
open Util.Source

let ctr = ref 0

(* dec $fresh_typeId() : typeId *)

let fresh_typeId (add : value -> unit) (at : region) (targs : targ list)
    (values_input : value list) : value =
  Extract.zero at targs;
  Extract.zero at values_input;
  let tid = "FRESH__" ^ string_of_int !ctr in
  ctr := !ctr + 1;
  let value =
    let vid = Value.fresh () in
    let typ = Il.VarT ("typeId" $ no_region, []) in
    TextV tid $$$ { vid; typ }
  in
  add value;
  value
