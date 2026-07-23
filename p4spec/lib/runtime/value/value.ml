module Fresh_ = Fresh
module Match = Match
open Domain
module Mixfix = Domain.Mixfix
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
  else if value_l.note.vid = value_r.note.vid then 0
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
    | CaseV valuecase_l, CaseV valuecase_r ->
        Mixfix.compare ~compare_arg:compare valuecase_l valuecase_r
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

let eq (value_l : t) (value_r : t) : bool =
  if value_l == value_r then true
  else if value_l.note.vid = value_r.note.vid then true
  else if value_l.note.vhash <> value_r.note.vhash then false
  else compare value_l value_r = 0

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
    | CaseV valuecase ->
        let rec go = function
          | Mixfix.Arg value -> h := (!h * 31) + value.note.vhash
          | Mixfix.Atom atom -> h := (!h * 31) + Hashtbl.hash atom.it
          | Mixfix.Brack (atom_l, mixfix, atom_r) ->
              h := (!h * 31) + Hashtbl.hash atom_l.it;
              go mixfix;
              h := (!h * 31) + Hashtbl.hash atom_r.it
          | Mixfix.Infix (mixfix_l, atom, mixfix_r) ->
              go mixfix_l;
              h := (!h * 31) + Hashtbl.hash atom.it;
              go mixfix_r
          | Mixfix.Seq mixfixes -> List.iter go mixfixes
        in
        go valuecase
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

(* Mixops *)

module Mixops = struct
  let cache : (string, Mixop.t) Hashtbl.t = Hashtbl.create 64

  let of_string (s : string) : Mixop.t =
    match Hashtbl.find_opt cache s with
    | Some mixop -> mixop
    | None ->
        let mixop = Frontend.Parse.parse_mixop s in
        Hashtbl.replace cache s mixop;
        mixop

  let of_atoms_matrix (atoms_matrix : Atom.t list list) : Mixop.t =
    atoms_matrix
    |> List.map (fun atoms ->
           atoms |> List.map Atom.string_of_atom |> String.concat " ")
    |> String.concat " x " |> of_string
end

(* Constructors *)

module Make = struct
  (* Constructors *)

  let mk (at : region) (typ : typ') (value : value') : value =
    let vid = Fresh_.fresh () in
    let vhash = hash_of value in
    value $$ (at, { vid; typ; vhash })

  let with_typ (typ : typ) ((at, value) : region * value') : value =
    mk at typ.it value

  let with_region (at : region) (value : value') : region * value' = (at, value)

  let bool ?(at = no_region) (b : bool) : value =
    BoolV b |> with_region at |> with_typ Typ.Make.bool

  let nat ?(at = no_region) (n : Bigint.t) : value =
    NumV (`Nat n) |> with_region at |> with_typ Typ.Make.nat

  let int ?(at = no_region) (i : Bigint.t) : value =
    NumV (`Int i) |> with_region at |> with_typ Typ.Make.int

  let num ?(at = no_region) (n : Num.t) : value =
    match n with `Nat n -> nat ~at n | `Int i -> int ~at i

  let text ?(at = no_region) (s : string) : value =
    TextV s |> with_region at |> with_typ Typ.Make.text

  let str ?(at = no_region) (typ : typ) (valuefields : valuefield list) : value
      =
    StructV valuefields |> with_region at |> with_typ typ

  let case ?(at = no_region) (typ : typ) (valuecase : valuecase) : value =
    CaseV valuecase |> with_region at |> with_typ typ

  let tuple ?(at = no_region) (typ : typ) (values : value list) : value =
    TupleV values |> with_region at |> with_typ typ

  let opt ?(at = no_region) (typ : typ) (value_opt : value option) : value =
    OptV value_opt |> with_region at |> with_typ typ

  let list ?(at = no_region) (typ : typ) (values : value list) : value =
    ListV values |> with_region at |> with_typ typ

  let func ?(at = no_region) (id : id) (tparams : tparam list)
      (typs_params : typ list) (typ : typ) : value =
    FuncV id |> with_region at
    |> with_typ (Typ.Make.func tparams typs_params typ)

  let extern ?(at = no_region) (typ : typ) (json : Yojson.Safe.t) : value =
    ExternV json |> with_region at |> with_typ typ

  (* Operators *)

  let ( <| ) (s_mixop : string) (values : value list) : string * value list =
    (s_mixop, values)

  let ( <<| ) ((s_mixop, values) : string * value list) (s : string) : value =
    let typ = Typ.Make.var (s $ no_region) [] in
    let valuecase = Mixfix.fill (Mixops.of_string s_mixop) values in
    let at =
      values |> List.map at
      |> List.filter (fun region -> region <> no_region)
      |> over_region
    in
    case ~at typ valuecase

  let ( <<<| ) (value : value) (at : region) : value = { value with at }

  let ( #@@ ) (value : value) (s : string) : value =
    { value with note = { value.note with typ = VarT (s $ no_region, []) } }

  let ( <|! ) (mixop : Mixop.t) (values : value list) : Mixop.t * value list =
    (mixop, values)

  let ( <<|! ) ((mixop, values) : Mixop.t * value list) (typ : typ) : value =
    let valuecase = Mixfix.fill mixop values in
    case ~at:no_region typ valuecase
end

(* Getters *)

module Get = struct
  let bool (value : t) : bool =
    match value.it with BoolV b -> b | _ -> error no_region "not a bool"

  let num (value : t) : Num.t =
    match value.it with NumV n -> n | _ -> error no_region "not a num"

  let text (value : t) : string =
    match value.it with TextV s -> s | _ -> error no_region "not a text"

  let str (value : t) : valuefield list =
    match value.it with
    | StructV valuefields -> valuefields
    | _ -> error no_region "not a struct"

  let case (value : t) : valuecase =
    match value.it with
    | CaseV valuecase -> valuecase
    | _ -> error no_region "not a case"

  let tuple (value : t) : value list =
    match value.it with
    | TupleV values -> values
    | _ -> error no_region "not a tuple"

  let opt (value : t) : value option =
    match value.it with
    | OptV value -> value
    | _ -> error no_region "not an option"

  let list (value : t) : value list =
    match value.it with
    | ListV values -> values
    | _ -> error no_region "not a list"

  let func (value : t) : id =
    match value.it with FuncV id -> id | _ -> error no_region "not a function"

  let extern (value : t) : Yojson.Safe.t =
    match value.it with
    | ExternV json -> json
    | _ -> error no_region "not an extern"

  (* Extractors *)

  let nth (n : int) (values : value list) : value = List.nth values n

  let one (values : value list) : value =
    match values with
    | [ value ] -> value
    | _ -> error no_region "expected exactly one value"

  let two (values : value list) : value * value =
    match values with
    | [ value_a; value_b ] -> (value_a, value_b)
    | _ -> error no_region "expected exactly two values"

  let three (values : value list) : value * value * value =
    match values with
    | [ value_a; value_b; value_c ] -> (value_a, value_b, value_c)
    | _ -> error no_region "expected exactly three values"

  (* Match *)

  let mtch (value : t) (cases : (string * (value list -> 'a)) list)
      (case_default : value list -> 'a) : 'a =
    match value.it with
    | CaseV valuecase -> (
        let values = Mixfix.args valuecase in
        let f_opt =
          List.find_opt
            (fun (s_mixop, _) ->
              Mixfix.eq_mixop valuecase (Mixops.of_string s_mixop))
            cases
          |> Option.map snd
        in
        match f_opt with Some f -> f values | None -> case_default values)
    | _ -> case_default []

  module MixopHashed = struct
    type t = Mixop.t

    let equal = Mixop.eq

    let hash (m : Mixop.t) : int =
      Hashtbl.hash (Mixop.string_of_mixop m) land 0x7FFFFFFF
  end

  module MtchTbl = Hashtbl.Make (MixopHashed)

  type 'a mtch = region -> value list -> 'a
  type 'a mtchtbl = 'a mtch MtchTbl.t

  let build_mtchtbl (cases : (Mixop.t * (region -> value list -> 'a)) list) :
      'a mtchtbl =
    let tbl = MtchTbl.create (List.length cases) in
    List.iter (fun (mixop, f) -> MtchTbl.add tbl mixop f) cases;
    tbl

  let mtch_dispatch (value : t) (tbl : 'a mtchtbl) (case_default : 'a mtch) : 'a
      =
    let at = value.at in
    match value.it with
    | CaseV valuecase -> (
        let mixop, values = Mixfix.split valuecase in
        match MtchTbl.find_opt tbl mixop with
        | Some f -> f at values
        | None -> case_default at values)
    | _ -> case_default at []

  let build_dispatch = build_mtchtbl

  (* Operators *)

  let ( |>> ) (value : t) (s_mixop : string) : value list =
    match value.it with
    | CaseV valuecase ->
        let mixop_expect = Mixops.of_string s_mixop in
        if Mixfix.eq_mixop valuecase mixop_expect then Mixfix.args valuecase
        else
          error no_region
            (Format.asprintf "expected case with %s, but got %s"
               (Mixop.string_of_mixop mixop_expect)
               (Mixop.string_of_mixop (Mixfix.to_mixop valuecase)))
    | _ -> error no_region "not a case"

  let ( |>>! ) (value : t) (mixop_expect : Mixop.t) : value list =
    match value.it with
    | CaseV valuecase ->
        if Mixfix.eq_mixop valuecase mixop_expect then Mixfix.args valuecase
        else
          error no_region
            (Format.asprintf "expected case with %s, but got %s"
               (Mixop.string_of_mixop mixop_expect)
               (Mixop.string_of_mixop (Mixfix.to_mixop valuecase)))
    | _ -> error no_region "not a case"

  let ( |>>? ) (value : t) (s_mixop : string) : value list option =
    match value.it with
    | CaseV valuecase ->
        let mixop_expect = Mixops.of_string s_mixop in
        if Mixfix.eq_mixop valuecase mixop_expect then
          Some (Mixfix.args valuecase)
        else None
    | _ -> None
end
