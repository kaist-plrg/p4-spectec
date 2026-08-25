module Atom = Domain.Atom
module Mixop = Domain.Mixop
module Mixfix = Domain.Mixfix
module Il = Lang.Il
module Typ = Runtime.Type.Typ
module Value = Runtime.Value
open Util.Source

(* The external interface for builtin and extern calls, for the K
   specification.

   val   ::= ["boolV", <bool>]
           | ["natN", "<decimal>"] | ["intN", "<decimal>"]
           | ["textV", <string>]
           | ["strV", [[<atom>, val], ...]]
           | ["injV", mixop, [val, ...]]
           | ["tupV", [val, ...]]
           | ["optV", null] | ["optV", val]
           | ["listV", [val, ...]]
           | ["funcV", <id>]
           | ["extV", <json>]

   mixop ::= [[<atom>, ...], ...]

   typ   ::= ["natT"] | ["intT"] | ["boolT"] | ["textT"]
           | ["varT", <id>, [typ, ...]] | ["tupT", [typ, ...]]
           | ["iterT", typ, "?"|"*"] | ["funcT"]

   request  ::= {"builtin":     <id>, "targs": [typ, ...], "args": [val, ...]}
              | {"extern-func": <id>, "targs": [typ, ...], "args": [val, ...]}
              | {"extern-rel":  <id>, "args": [val, ...]}

   response ::= {"ok": val}
              | {"ok": [val, ...]}
              | {"fail": null} *)

exception Error of string

let error fmt = Format.kasprintf (fun msg -> raise (Error msg)) fmt

(* Atoms *)

let json_of_atom (atom : Atom.t) : Yojson.Safe.t =
  `String (Atom.string_of_atom atom)

let atom_of_json (json : Yojson.Safe.t) : Atom.t =
  match json with
  | `String s -> Atom.atom_of_string s
  | _ ->
      error "expected an atom string, but got %s" (Yojson.Safe.to_string json)

(* Mixops *)

let json_of_mixop (mixop : Mixop.t) : Yojson.Safe.t =
  `List
    (Mixop.atoms_matrix mixop
    |> List.map (fun atoms ->
           `List (atoms |> List.map it |> List.map json_of_atom)))

let mixop_of_json (json : Yojson.Safe.t) : Mixop.t =
  match json with
  | `List jsons_atoms ->
      jsons_atoms
      |> List.map (fun json_atoms ->
             match json_atoms with
             | `List jsons_atom -> List.map atom_of_json jsons_atom
             | _ ->
                 error "expected a row of atoms, but got %s"
                   (Yojson.Safe.to_string json_atoms))
      |> Value.Mixops.of_atoms_matrix
  | _ ->
      error "expected an atoms matrix, but got %s" (Yojson.Safe.to_string json)

(* Types *)

let rec json_of_typ (typ : Typ.t) : Yojson.Safe.t =
  match typ.it with
  | BoolT -> `List [ `String "boolT" ]
  | NumT `NatT -> `List [ `String "natT" ]
  | NumT `IntT -> `List [ `String "intT" ]
  | TextT -> `List [ `String "textT" ]
  | VarT (id, targs) ->
      `List
        [ `String "varT"; `String id.it; `List (List.map json_of_typ targs) ]
  | TupleT typs -> `List [ `String "tupT"; `List (List.map json_of_typ typs) ]
  | IterT (typ, iter) ->
      let s_iter = match iter with Opt -> "?" | List -> "*" in
      `List [ `String "iterT"; json_of_typ typ; `String s_iter ]
  | FuncT _ -> `List [ `String "funcT" ]

let rec typ_of_json (json : Yojson.Safe.t) : Typ.t =
  match json with
  | `List [ `String "boolT" ] -> Typ.Make.bool
  | `List [ `String "natT" ] -> Typ.Make.nat
  | `List [ `String "intT" ] -> Typ.Make.int
  | `List [ `String "textT" ] -> Typ.Make.text
  | `List [ `String "varT"; `String id; `List jsons_targ ] ->
      Typ.Make.var (id $ no_region) (List.map typ_of_json jsons_targ)
  | `List [ `String "tupT"; `List jsons_typ ] ->
      Typ.Make.tuple (List.map typ_of_json jsons_typ)
  | `List [ `String "iterT"; json_typ; `String "?" ] ->
      Typ.Make.opt (typ_of_json json_typ)
  | `List [ `String "iterT"; json_typ; `String "*" ] ->
      Typ.Make.list (typ_of_json json_typ)
  | `List [ `String "funcT" ] ->
      error "a function type cannot cross the external interface"
  | _ -> error "expected a type, but got %s" (Yojson.Safe.to_string json)

let typs_of_json (json : Yojson.Safe.t) : Typ.t list =
  match json with
  | `List jsons_typ -> List.map typ_of_json jsons_typ
  | _ ->
      error "expected a list of types, but got %s" (Yojson.Safe.to_string json)

(* Values *)

let rec json_of_val (value : Value.t) : Yojson.Safe.t =
  match value.it with
  | BoolV b -> `List [ `String "boolV"; `Bool b ]
  | NumV (`Nat n) -> `List [ `String "natN"; `String (Bigint.to_string n) ]
  | NumV (`Int i) -> `List [ `String "intN"; `String (Bigint.to_string i) ]
  | TextV s -> `List [ `String "textV"; `String s ]
  | StructV valuefields ->
      `List
        [
          `String "strV";
          `List
            (List.map
               (fun (atom, value_field) ->
                 `List [ json_of_atom atom.it; json_of_val value_field ])
               valuefields);
        ]
  | CaseV valuecase ->
      `List
        [
          `String "injV";
          json_of_mixop (Mixfix.to_mixop valuecase);
          `List (Mixfix.args valuecase |> List.map json_of_val);
        ]
  | TupleV values ->
      `List [ `String "tupV"; `List (List.map json_of_val values) ]
  | OptV None -> `List [ `String "optV"; `Null ]
  | OptV (Some value) -> `List [ `String "optV"; json_of_val value ]
  | ListV values ->
      `List [ `String "listV"; `List (List.map json_of_val values) ]
  | FuncV id -> `List [ `String "funcV"; `String id.it ]
  | ExternV json -> `List [ `String "extV"; json ]

let typ_placeholder : Typ.t = Typ.Make.var ("_" $ no_region) []
let typ_of_val (value : Value.t) : Typ.t = value.note.Il.typ $ no_region

let rec val_of_json (json : Yojson.Safe.t) : Value.t =
  match json with
  | `List [ `String "boolV"; `Bool b ] -> Value.Make.bool b
  | `List [ `String "natN"; `String s ] -> Value.Make.nat (Bigint.of_string s)
  | `List [ `String "intN"; `String s ] -> Value.Make.int (Bigint.of_string s)
  | `List [ `String "textV"; `String s ] -> Value.Make.text s
  | `List [ `String "strV"; `List jsons_field ] ->
      let valuefields =
        List.map
          (fun json_field ->
            match json_field with
            | `List [ json_atom; json_value ] ->
                let atom = atom_of_json json_atom $ no_region in
                (atom, val_of_json json_value)
            | _ ->
                error "expected a struct field, but got %s"
                  (Yojson.Safe.to_string json_field))
          jsons_field
      in
      Value.Make.str typ_placeholder valuefields
  | `List [ `String "injV"; json_mixop; `List jsons_value ] ->
      let mixop = mixop_of_json json_mixop in
      let values = List.map val_of_json jsons_value in
      let valuecase = Mixfix.fill mixop values in
      Value.Make.case typ_placeholder valuecase
  | `List [ `String "tupV"; `List jsons_value ] ->
      let values = List.map val_of_json jsons_value in
      let typ = Typ.Make.tuple (List.map typ_of_val values) in
      Value.Make.tuple typ values
  | `List [ `String "optV"; `Null ] ->
      Value.Make.opt (Typ.Make.opt typ_placeholder) None
  | `List [ `String "optV"; json_value ] ->
      let value = val_of_json json_value in
      Value.Make.opt (Typ.Make.opt (typ_of_val value)) (Some value)
  | `List [ `String "listV"; `List jsons_value ] ->
      let values = List.map val_of_json jsons_value in
      let typ_elem =
        match values with
        | [] -> typ_placeholder
        | value :: _ -> typ_of_val value
      in
      Value.Make.list (Typ.Make.list typ_elem) values
  | `List [ `String "funcV"; `String id ] ->
      Value.Make.(
        FuncV (id $ no_region)
        |> with_region no_region |> with_typ typ_placeholder)
  | `List [ `String "extV"; json_ext ] ->
      Value.Make.extern typ_placeholder json_ext
  | _ -> error "expected a value, but got %s" (Yojson.Safe.to_string json)

let vals_of_json (json : Yojson.Safe.t) : Value.t list =
  match json with
  | `List jsons_value -> List.map val_of_json jsons_value
  | _ ->
      error "expected a list of values, but got %s" (Yojson.Safe.to_string json)

(* Requests and responses *)

type request =
  | Builtin of string * Typ.t list * Value.t list
  | ExternFunc of string * Typ.t list * Value.t list
  | ExternRel of string * Value.t list

let request_of_json (json : Yojson.Safe.t) : request =
  match json with
  | `Assoc fields ->
      let find (name : string) : Yojson.Safe.t =
        match List.assoc_opt name fields with
        | Some json -> json
        | None -> error "request is missing the field %s" name
      in
      let name_of (key : string) : string =
        match find key with
        | `String name -> name
        | json ->
            error "expected a name for %s, but got %s" key
              (Yojson.Safe.to_string json)
      in
      let kinds =
        [ "builtin"; "extern-func"; "extern-rel" ]
        |> List.filter (fun key -> List.mem_assoc key fields)
      in
      let kind =
        match kinds with
        | [ kind ] -> kind
        | [] ->
            error
              "request has none of the fields builtin, extern-func, extern-rel"
        | _ ->
            error
              "request has more than one of builtin, extern-func, extern-rel"
      in
      if kind = "extern-rel" then
        ExternRel (name_of kind, vals_of_json (find "args"))
      else
        let name = name_of kind in
        let targs = typs_of_json (find "targs") in
        let args = vals_of_json (find "args") in
        if kind = "builtin" then Builtin (name, targs, args)
        else ExternFunc (name, targs, args)
  | _ ->
      error "expected a request object, but got %s" (Yojson.Safe.to_string json)

let json_of_response (value : Value.t) : Yojson.Safe.t =
  `Assoc [ ("ok", json_of_val value) ]

let json_of_response_multi (values : Value.t list) : Yojson.Safe.t =
  `Assoc [ ("ok", `List (List.map json_of_val values)) ]

let json_of_response_fail () : Yojson.Safe.t = `Assoc [ ("fail", `Null) ]
