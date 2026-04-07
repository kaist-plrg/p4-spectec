module Fresh_ = Fresh
module Match = Match
open Domain
open Lang
open Xl
open Il
open Il.Print
module Typ = Type.Typ
open Error
open Util.Source

(* Value *)

type t = value [@@deriving yojson]

(* Stringifier *)

let to_string t = string_of_value t

(* Comparison *)

let rec compare (value_l : t) (value_r : t) =
  if value_l == value_r then 0
  else
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
        mixop |> Mixop.atoms
        |> List.iter (fun atom -> h := (!h * 31) + Hashtbl.hash atom.it);
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

(* Constructors *)

module Make = struct
  (* Mixop generator *)

  let mixop_cache : (string, Mixop.t) Hashtbl.t = Hashtbl.create 64

  let mixop_of (s : string) : Mixop.t =
    match Hashtbl.find_opt mixop_cache s with
    | Some m -> m
    | None ->
        let m = Frontend.Parse.parse_mixop s in
        Hashtbl.add mixop_cache s m;
        m

  (* Constructors *)

  let mk (at : region) (typ : typ') (value : value') : value =
    let vid = Fresh_.fresh () in
    let vhash = hash_of value in
    value $$ (at, { vid; typ; vhash })

  let with_at_typ (at : region) (typ : typ) (value : value') : value =
    mk at typ.it value

  let bool ?(at = no_region) (b : bool) : value =
    BoolV b |> with_at_typ at Typ.Make.bool

  let nat ?(at = no_region) (n : Bigint.t) : value =
    NumV (`Nat n) |> with_at_typ at Typ.Make.nat

  let int ?(at = no_region) (i : Bigint.t) : value =
    NumV (`Int i) |> with_at_typ at Typ.Make.int

  let num ?(at = no_region) (n : Num.t) : value =
    match n with `Nat n -> nat ~at n | `Int i -> int ~at i

  let text ?(at = no_region) (s : string) : value =
    TextV s |> with_at_typ at Typ.Make.text

  let str ?(at = no_region) (typ : typ) (valuefields : valuefield list) : value
      =
    StructV valuefields |> with_at_typ at typ

  let case ?(at = no_region) (typ : typ) (valuecase : valuecase) : value =
    CaseV valuecase |> with_at_typ at typ

  let tuple ?(at = no_region) (typ : typ) (values : value list) : value =
    TupleV values |> with_at_typ at typ

  let opt ?(at = no_region) (typ : typ) (value_opt : value option) : value =
    OptV value_opt |> with_at_typ at typ

  let list ?(at = no_region) (typ : typ) (values : value list) : value =
    ListV values |> with_at_typ at typ

  let func ?(at = no_region) (id : id) (tparams : tparam list)
      (typs_params : typ list) (typ : typ) : value =
    FuncV id |> with_at_typ at (Typ.Make.func tparams typs_params typ)

  let extern ?(at = no_region) (typ : typ) (json : Yojson.Safe.t) : value =
    ExternV json |> with_at_typ at typ

  (* Operators *)

  let ( <| ) (s_mixop : string) (values : value list) : string * value list =
    (s_mixop, values)

  let ( <<| ) ((s_mixop, values) : string * value list) (s : string) : value =
    let typ = Typ.Make.var (s $ no_region) [] in
    let valuecase = (mixop_of s_mixop, values) in
    let at =
      values |> List.map at
      |> List.filter (fun region -> region <> no_region)
      |> over_region
    in
    case ~at typ valuecase

  let ( <<<| ) (value : value) (at : region) : value = { value with at }

  let ( #@@ ) (value : value) (s : string) : value =
    { value with note = { value.note with typ = VarT (s $ no_region, []) } }
end

(* Getters *)

module Get = struct
  let bool (value : t) =
    match value.it with BoolV b -> b | _ -> error no_region "not a bool"

  let num (value : t) =
    match value.it with NumV n -> n | _ -> error no_region "not a num"

  let text (value : t) =
    match value.it with TextV s -> s | _ -> error no_region "not a text"

  let str (value : t) =
    match value.it with
    | StructV valuefields -> valuefields
    | _ -> error no_region "not a struct"

  let case (value : t) =
    match value.it with
    | CaseV valuecase -> valuecase
    | _ -> error no_region "not a case"

  let tuple (value : t) =
    match value.it with
    | TupleV values -> values
    | _ -> error no_region "not a tuple"

  let opt (value : t) =
    match value.it with
    | OptV value -> value
    | _ -> error no_region "not an option"

  let list (value : t) =
    match value.it with
    | ListV values -> values
    | _ -> error no_region "not a list"

  let func (value : t) =
    match value.it with FuncV id -> id | _ -> error no_region "not a function"

  let extern (value : t) =
    match value.it with
    | ExternV json -> json
    | _ -> error no_region "not an extern"
end
