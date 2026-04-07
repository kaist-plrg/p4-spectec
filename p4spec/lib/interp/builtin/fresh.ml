open Lang
open Il
module Value = Runtime.Dynamic_Il.Value
open Util.Source

(* Domain-local counter to avoid contention *)
let ctr_table : (int, int ref) Hashtbl.t = Hashtbl.create 16
let ctr_lock = Mutex.create ()

let get_ctr () =
  let id : int = (Stdlib.Domain.self () :> int) in
  Mutex.lock ctr_lock;
  let counter =
    try Hashtbl.find ctr_table id
    with Not_found ->
      let new_ctr = ref 0 in
      Hashtbl.add ctr_table id new_ctr;
      new_ctr
  in
  Mutex.unlock ctr_lock;
  counter

let reset () =
  let ctr = get_ctr () in
  ctr := 0

let get_count () =
  let ctr = get_ctr () in
  !ctr

(* dec $fresh_typeId() : typeId *)

let fresh_typeId (add : value -> unit) (at : region) (targs : targ list)
    (values_input : value list) : value =
  Extract.zero at targs;
  Extract.zero at values_input;
  let ctr = get_ctr () in
  let tid = "FRESH__" ^ string_of_int !ctr in
  ctr := !ctr + 1;
  let value = Value.make (Il.VarT ("typeId" $ no_region, [])) (TextV tid) in
  add value;
  value
