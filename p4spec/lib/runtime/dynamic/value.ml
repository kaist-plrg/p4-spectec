open Domain
open Lang
open Xl
open Il
open Il.Print
open Util.Source

(* Ticker for node identifier tracking *)

let tick = ref 0
let refresh () = tick := 0

let fresh () =
  let id = !tick in
  tick := id + 1;
  id

(* Value *)

type t = value [@@deriving yojson]
type id = vid

(* Stringifier *)

let to_string t = string_of_value t

(* Comparison *)

let rec compare (value_l : t) (value_r : t) =
  let tag (value : t) =
    match value.it with
    | BoolV _ -> 0
    | NumV _ -> 1
    | TextV _ -> 2
    | StructV _ -> 3
    | CaseV _ -> 4
    | TupleV _ -> 5
    | OptV None -> 6
    | OptV _ -> 7
    | ListV _ -> 8
    | FuncV _ -> 9
    | ExternV _ -> 10
  in
  match (value_l.it, value_r.it) with
  | BoolV b_l, BoolV b_r -> Stdlib.compare b_l b_r
  | NumV n_l, NumV n_r -> Num.compare n_l n_r
  | TextV s_l, TextV s_r -> String.compare s_l s_r
  | StructV fields_l, StructV fields_r -> compare_fields fields_l fields_r
  | CaseV (mixop_l, values_l), CaseV (mixop_r, values_r) ->
      let cmp_mixop = Mixop.compare mixop_l mixop_r in
      if cmp_mixop <> 0 then cmp_mixop else compares values_l values_r
  | TupleV values_l, TupleV values_r -> compares values_l values_r
  | OptV value_opt_l, OptV value_opt_r -> (
      match (value_opt_l, value_opt_r) with
      | Some value_l, Some value_r -> compare value_l value_r
      | Some _, None -> 1
      | None, Some _ -> -1
      | None, None -> 0)
  | ListV values_l, ListV values_r -> compares values_l values_r
  | ExternV json_l, ExternV json_r -> Stdlib.compare json_l json_r
  | _ -> Int.compare (tag value_l) (tag value_r)

and compare_fields fields_l fields_r =
  match (fields_l, fields_r) with
  | [], [] -> 0
  | [], _ :: _ -> -1
  | _ :: _, [] -> 1
  | (atom_l, value_l) :: fields_l, (atom_r, value_r) :: fields_r ->
      let c = Atom.compare atom_l.it atom_r.it in
      if c <> 0 then c
      else
        let c = compare value_l value_r in
        if c <> 0 then c else compare_fields fields_l fields_r

and compares (values_l : t list) (values_r : t list) : int =
  match (values_l, values_r) with
  | [], [] -> 0
  | [], _ :: _ -> -1
  | _ :: _, [] -> 1
  | value_l :: values_l, value_r :: values_r ->
      let cmp = compare value_l value_r in
      if cmp <> 0 then cmp else compares values_l values_r

(* Equality *)

let eq (value_l : t) (value_r : t) : bool = compare value_l value_r = 0

(* Boolean *)

let get_bool (value : t) =
  match value.it with BoolV b -> b | _ -> failwith "get_bool"

(* Number *)

let get_num (value : t) =
  match value.it with NumV n -> n | _ -> failwith "get_num"

(* Text *)

let get_text (value : t) =
  match value.it with TextV s -> s | _ -> failwith "get_text"

(* List *)

let to_list (values : t list) = ListV values

let get_list (value : t) =
  match value.it with ListV values -> values | _ -> failwith "unseq"

(* Option *)

let to_opt (value : t option) = OptV value

let get_opt (value : t) =
  match value.it with OptV value -> value | _ -> failwith "get_opt"

(* Struct *)

let get_struct (value : t) =
  match value.it with StructV fields -> fields | _ -> failwith "get_struct"

(* Hash computation *)

let hash_of (v : value') : int =
  let h = ref 0 in
  let go (v : value') =
    match v with
    | BoolV b -> h := (!h * 31) + if b then 1231 else 1237
    | NumV (`Nat n) -> h := (!h * 31) + (1 + Bigint.hash n)
    | NumV (`Int i) -> h := (!h * 31) + (2 + Bigint.hash i)
    | TextV s -> h := (!h * 31) + Hashtbl.hash s
    | StructV valuefields ->
        List.iter
          (fun (atom, value_field) ->
            h := (!h * 31) + Hashtbl.hash atom.it;
            h := (!h * 31) + value_field.note.vhash)
          valuefields
    | CaseV (mixop, values) ->
        List.iter
          (fun atoms ->
            List.iter (fun atom -> h := (!h * 31) + Hashtbl.hash atom.it) atoms)
          mixop;
        List.iter (fun value -> h := (!h * 31) + value.note.vhash) values
    | TupleV values ->
        h := (!h * 31) + 1001;
        List.iter (fun value -> h := (!h * 31) + value.note.vhash) values
    | ListV values ->
        h := (!h * 31) + 1003;
        List.iter (fun value -> h := (!h * 31) + value.note.vhash) values
    | OptV None -> h := (!h * 31) + 997
    | OptV (Some value) ->
        h := (!h * 31) + 1009;
        h := (!h * 31) + value.note.vhash
    | FuncV id -> h := (!h * 31) + Hashtbl.hash id.it
    | ExternV json -> h := (!h * 31) + Hashtbl.hash json
  in
  go v;
  !h land 0x7FFFFFFF

(* Value constructor with precomputed hash *)

let make (typ : Il.typ') (v : value') : value =
  let vid = fresh () in
  let vhash = hash_of v in
  Util.Source.( $$$ ) v { vid; typ; vhash }
