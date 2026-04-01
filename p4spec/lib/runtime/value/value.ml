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

  let mk (typ : typ') (value : value') : value =
    let vid = Fresh_.fresh () in
    let vhash = hash_of value in
    Util.Source.( $$$ ) value { vid; typ; vhash }

  let with_typ (typ : typ) (value : value') : value = mk typ.it value
  let bool (b : bool) : value = BoolV b |> with_typ Typ.Make.bool
  let nat (n : Bigint.t) : value = NumV (`Nat n) |> with_typ Typ.Make.nat
  let int (i : Bigint.t) : value = NumV (`Int i) |> with_typ Typ.Make.int
  let num (n : Num.t) : value = match n with `Nat n -> nat n | `Int i -> int i
  let text (s : string) : value = TextV s |> with_typ Typ.Make.text

  let str (typ : typ) (valuefields : valuefield list) : value =
    StructV valuefields |> with_typ typ

  let case (typ : typ) (valuecase : valuecase) : value =
    CaseV valuecase |> with_typ typ

  let tuple (typ : typ) (values : value list) : value =
    TupleV values |> with_typ typ

  let opt (typ : typ) (value_opt : value option) : value =
    OptV value_opt |> with_typ typ

  let list (typ : typ) (values : value list) : value =
    ListV values |> with_typ typ

  let func (id : id) : value = FuncV id |> with_typ Typ.Make.func

  let extern (typ : typ) (json : Yojson.Safe.t) : value =
    ExternV json |> with_typ typ

  (* Operators *)

  let ( <| ) (s_mixop : string) (values : value list) : string * value list =
    (s_mixop, values)

  let ( <<| ) ((s_mixop, values) : string * value list) (s : string) : value =
    let typ = Typ.Make.var (s $ no_region) [] in
    let valuecase = (mixop_of s_mixop, values) in
    case typ valuecase

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
