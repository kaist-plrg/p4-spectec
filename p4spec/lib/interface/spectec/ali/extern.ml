module Atom = Domain.Atom
module Mixop = Domain.Mixop
module Mixfix = Domain.Mixfix
module Il = Lang.Il
module Typ = Runtime.Type.Typ
module Value = Runtime.Value
open Util.Source

(* The external interface for builtin calls, for the K specification in
   `spec-meta-k/`.

   This is the mirror image of `kast.ml`: where that emits a booted program
   *into* K once, this bridges values back and forth on every builtin call.
   K shells out to `spectec-boot builtin`, which decodes a request here,
   invokes the OCaml builtin, and encodes the result back.  The OCaml
   registry in `interface/builtin/` is thereby the single authority for what
   a builtin means; the K side holds no implementation of its own.

   The wire format is a custom, self-describing JSON, deliberately neither
   KAST JSON nor `Value.t`'s derived yojson:

   - KAST JSON cannot express it.  `kast.ml`'s `sort_of_typ` accepts only a
     bare `VarT (id, [])`, but a builtin result is noted with types like
     `IterT (VarT ("pair", _), List)` and `VarT ("map", [k; v])`.  `kast.ml`
     is a script emitter; teaching it runtime types would reintroduce exactly
     the coupling this interface removes.

   - The derived yojson carries `vid`.  Deserializing vids minted by another
     process into this one imports identifiers that collide with locally
     allocated ones, and `Value.compare` short-circuits on vid equality
     (`lang/il/ast.ml:66-69`), so two structurally distinct values would
     compare equal.  Everything decoded here is therefore rebuilt through
     `Value.Make.*`, which mints fresh vids.

   Numbers travel as decimal *strings*, not JSON ints: `natN`/`intN` carry
   `Bigint.t` and P4 routinely exceeds 64 bits (`bitstr_to_int`, `pow2`).

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

   mixop ::= [[<atom>, ...], ...]        (* atoms matrix; matches K's MixOp *)

   typ   ::= ["natT"] | ["intT"] | ["boolT"] | ["textT"]
           | ["varT", <id>, [typ, ...]] | ["tupT", [typ, ...]]
           | ["iterT", typ, "?"|"*"] | ["funcT"]

   request  ::= {"builtin": <id>, "targs": [typ, ...], "args": [val, ...]}
   response ::= {"ok": val}

   INVARIANT, and the reason decoding is possible at all: builtins read their
   arguments *structurally* and never inspect an argument's `note.typ`.
   `Value.compare`, `Value.eq`, `Value.Get.*` and `Mixfix.eq_mixop` are all
   purely structural, and the maps builtins dispatch on the mixop
   (`maps.ml:20,39,50`), never on the note.  A builtin computes its *result*
   type from `targs` (`lists.ml:14-16`, `maps.ml:29,67,72`), which is why
   `targs` are decoded faithfully while argument notes are only structural
   placeholders.  If a future builtin breaks this invariant, it breaks here. *)

exception Error of string

let error (fmt : ('a, Format.formatter, unit, 'b) format4) : 'a =
  Format.kasprintf (fun msg -> raise (Error msg)) fmt

(* Atoms.

   `Atom.string_of_atom` is documented parse-faithful (`atom.ml:42`), so it
   round-trips through `Atom.atom_of_string`. *)

let json_of_atom (atom : Atom.t) : Yojson.Safe.t =
  `String (Atom.string_of_atom atom)

let atom_of_json (json : Yojson.Safe.t) : Atom.t =
  match json with
  | `String s -> Atom.atom_of_string s
  | _ -> error "expected an atom string, but got %s" (Yojson.Safe.to_string json)

(* Mixops.

   The atoms matrix is the same representation K's `MixOp` uses, and the same
   round trip `unboot_mixop` (`common/unboot.ml:64-77`) already performs on
   every booted program, so it is exercised well beyond this file. *)

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

(* Types.

   Unlike values, types are carried faithfully: `targs` are what a builtin
   derives its result type from. *)

let rec json_of_typ (typ : Il.typ) : Yojson.Safe.t =
  match typ.it with
  | Il.BoolT -> `List [ `String "boolT" ]
  | Il.NumT `NatT -> `List [ `String "natT" ]
  | Il.NumT `IntT -> `List [ `String "intT" ]
  | Il.TextT -> `List [ `String "textT" ]
  | Il.VarT (id, targs) ->
      `List [ `String "varT"; `String id.it; `List (List.map json_of_typ targs) ]
  | Il.TupleT typs -> `List [ `String "tupT"; `List (List.map json_of_typ typs) ]
  | Il.IterT (typ, iter) ->
      let s_iter = match iter with Il.Opt -> "?" | Il.List -> "*" in
      `List [ `String "iterT"; json_of_typ typ; `String s_iter ]
  | Il.FuncT _ -> `List [ `String "funcT" ]

let rec typ_of_json (json : Yojson.Safe.t) : Il.typ =
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
  (* K's `funcT()` is nullary while `Typ.Make.func` is not.  No builtin in the
     registry takes a function-typed `targ`, so a faithful decoding is not
     needed; reject rather than invent one. *)
  | `List [ `String "funcT" ] ->
      error "a function type cannot cross the external interface"
  | _ -> error "expected a type, but got %s" (Yojson.Safe.to_string json)

let typs_of_json (json : Yojson.Safe.t) : Il.typ list =
  match json with
  | `List jsons_typ -> List.map typ_of_json jsons_typ
  | _ -> error "expected a list of types, but got %s" (Yojson.Safe.to_string json)

(* Values, outbound.

   `note.typ` is deliberately dropped: it is not on the wire, and K's `Val`
   has no place to put it. *)

let rec json_of_val (value : Value.t) : Yojson.Safe.t =
  match value.it with
  | Il.BoolV b -> `List [ `String "boolV"; `Bool b ]
  | Il.NumV (`Nat n) -> `List [ `String "natN"; `String (Bigint.to_string n) ]
  | Il.NumV (`Int i) -> `List [ `String "intN"; `String (Bigint.to_string i) ]
  | Il.TextV s -> `List [ `String "textV"; `String s ]
  | Il.StructV valuefields ->
      `List
        [
          `String "strV";
          `List
            (List.map
               (fun (atom, value_field) ->
                 `List [ json_of_atom atom.it; json_of_val value_field ])
               valuefields);
        ]
  | Il.CaseV valuecase ->
      `List
        [
          `String "injV";
          json_of_mixop (Mixfix.to_mixop valuecase);
          `List (Mixfix.args valuecase |> List.map json_of_val);
        ]
  | Il.TupleV values -> `List [ `String "tupV"; `List (List.map json_of_val values) ]
  | Il.OptV None -> `List [ `String "optV"; `Null ]
  | Il.OptV (Some value) -> `List [ `String "optV"; json_of_val value ]
  | Il.ListV values ->
      `List [ `String "listV"; `List (List.map json_of_val values) ]
  | Il.FuncV id -> `List [ `String "funcV"; `String id.it ]
  | Il.ExternV json -> `List [ `String "extV"; json ]

(* Values, inbound.

   `Value.Make.{str,case,tuple,opt,list}` all require a `typ`, but the wire
   carries none.  By the invariant at the head of this file no builtin reads
   an argument's `note.typ`, so a structural placeholder built bottom-up
   suffices: as faithful as the wire allows for the shapes that matter, and a
   plain `_` where the type is unrecoverable and unread. *)

let typ_placeholder : Il.typ = Typ.Make.var ("_" $ no_region) []

(* The (placeholder) type a decoded value ended up noted with, so that
   enclosing lists, options and tuples can be noted structurally too. *)
let typ_of_val (value : Value.t) : Il.typ = value.note.Il.typ $ no_region

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
      (* A function value is opaque to every builtin: it is only ever passed
         through.  `Typ.Make.func` needs a signature the wire does not carry,
         so note it with the placeholder. *)
      Value.Make.(FuncV (id $ no_region) |> with_region no_region |> with_typ typ_placeholder)
  | `List [ `String "extV"; json_ext ] -> Value.Make.extern typ_placeholder json_ext
  | _ -> error "expected a value, but got %s" (Yojson.Safe.to_string json)

let vals_of_json (json : Yojson.Safe.t) : Value.t list =
  match json with
  | `List jsons_value -> List.map val_of_json jsons_value
  | _ ->
      error "expected a list of values, but got %s" (Yojson.Safe.to_string json)

(* Requests and responses *)

let request_of_json (json : Yojson.Safe.t) : string * Il.typ list * Value.t list
    =
  match json with
  | `Assoc fields ->
      let find (name : string) : Yojson.Safe.t =
        match List.assoc_opt name fields with
        | Some json -> json
        | None -> error "request is missing the field %s" name
      in
      let name =
        match find "builtin" with
        | `String name -> name
        | json -> error "expected a builtin name, but got %s" (Yojson.Safe.to_string json)
      in
      let targs = typs_of_json (find "targs") in
      let args = vals_of_json (find "args") in
      (name, targs, args)
  | _ -> error "expected a request object, but got %s" (Yojson.Safe.to_string json)

let json_of_response (value : Value.t) : Yojson.Safe.t =
  `Assoc [ ("ok", json_of_val value) ]
