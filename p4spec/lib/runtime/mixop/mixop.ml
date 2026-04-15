open Util.Source
module Atom = Domain.Atom

(* Elaboration-time mixop: a tree representation used while building up
   mixfix operators from the EL surface syntax. Converted to the flat IL
   [Il.Mixop.t] form via [to_il] at the end of elaboration. *)

type atom = Atom.t phrase

type t =
  | Arg
  | Atom of atom
  | Brack of atom * t * atom
  | Infix of t * atom * t
  | Seq of t list

(* Normalization: flatten to list form. *)
let rec flatten_to_list (mixop : t) : t list =
  match mixop with
  | Arg | Atom _ -> [ mixop ]
  | Brack (al, inner, ar) -> (Atom al :: flatten_to_list inner) @ [ Atom ar ]
  | Infix (ml, atom, mr) ->
      flatten_to_list ml @ [ Atom atom ] @ flatten_to_list mr
  | Seq parts -> List.concat_map flatten_to_list parts

let normalize (mixop : t) : t list = flatten_to_list mixop

(* Comparison: compares on the normalized form. *)
let compare_atom (atom_a : atom) (atom_b : atom) =
  Atom.compare atom_a.it atom_b.it

let compare_primitive (a : t) (b : t) =
  match (a, b) with
  | Arg, Arg -> 0
  | Arg, _ -> -1
  | _, Arg -> 1
  | Atom atom_a, Atom atom_b -> compare_atom atom_a atom_b
  | _ -> assert false

let compare (mixop_a : t) (mixop_b : t) =
  if mixop_a == mixop_b then 0
  else List.compare compare_primitive (normalize mixop_a) (normalize mixop_b)

let eq (mixop_a : t) (mixop_b : t) = compare mixop_a mixop_b = 0

(* Arity *)
let rec arity = function
  | Arg -> 1
  | Atom _ -> 0
  | Brack (_, mixop, _) -> arity mixop
  | Infix (mixop_l, _, mixop_r) -> arity mixop_l + arity mixop_r
  | Seq mixops -> List.fold_left (fun acc mixop -> acc + arity mixop) 0 mixops

(* Extract atoms *)
let rec atoms = function
  | Arg -> []
  | Atom atom -> [ atom ]
  | Brack (atom_l, mixop, atom_r) -> (atom_l :: atoms mixop) @ [ atom_r ]
  | Infix (mixop_l, atom, mixop_r) -> atoms mixop_l @ [ atom ] @ atoms mixop_r
  | Seq mixops -> List.concat_map atoms mixops

(* --- Constructors --- *)

let arg : t = Arg
let mk_atom (s : string) : t = Atom (Atom.Atom s $ no_region)
let silent_atom (s : string) : t = Atom (Atom.SilentAtom s $ no_region)

let brack (l : string) (inner : t) (r : string) : t =
  Brack (Atom.Atom l $ no_region, inner, Atom.Atom r $ no_region)

let seq (ts : t list) : t = Seq ts

(* Assembler: interleave rendered atoms and argument strings *)

let assemble ~(string_of_atom : atom -> string) (mixop : t) (args : string list)
    : string =
  let rec assemble (mixop : t) (args : string list) : string * string list =
    match mixop with
    | Arg -> (
        match args with
        | [] -> failwith "not enough arguments"
        | arg :: args -> (arg, args))
    | Atom atom ->
        let smixop = string_of_atom atom in
        (smixop, args)
    | Brack (atom_l, mixop, atom_r) ->
        let smixop, args = assemble mixop args in
        let smixop =
          [ string_of_atom atom_l; smixop; string_of_atom atom_r ]
          |> List.filter (fun s -> s <> "")
          |> String.concat " "
        in
        (smixop, args)
    | Infix (mixop_l, atom, mixop_r) ->
        let smixop_l, args = assemble mixop_l args in
        let smixop_r, args = assemble mixop_r args in
        let smixop =
          [ smixop_l; string_of_atom atom; smixop_r ]
          |> List.filter (fun s -> s <> "")
          |> String.concat " "
        in
        (smixop, args)
    | Seq mixops ->
        let smixops, args =
          List.fold_left
            (fun (smixops, args) mixop ->
              let smixop, args = assemble mixop args in
              (smixops @ [ smixop ], args))
            ([], args) mixops
        in
        let smixop =
          smixops |> List.filter (fun s -> s <> "") |> String.concat " "
        in
        (smixop, args)
  in
  let smixop, args = assemble mixop args in
  match args with [] -> smixop | _ -> failwith "too many arguments"

(* Stringifier *)

let string_of_mixop (mixop : t) =
  let rec to_string = function
    | Arg -> "%"
    | Atom atom -> Atom.string_of_atom atom.it
    | Brack (atom_l, mixop, atom_r) ->
        Atom.string_of_atom atom_l.it
        ^ to_string mixop
        ^ Atom.string_of_atom atom_r.it
    | Infix (mixop_l, atom, mixop_r) ->
        to_string mixop_l ^ Atom.string_of_atom atom.it ^ to_string mixop_r
    | Seq mixops -> String.concat " " (List.map to_string mixops)
  in
  "`" ^ to_string mixop ^ "`"

(* Conversion to the flat representation *)

let to_flat (mixop : t) : Domain.Mixop.t =
  let rec flatten (mixop : t) (mixop_rev : Domain.Mixop.t) : Domain.Mixop.t =
    match mixop with
    | Arg -> Domain.Mixop.Arg :: mixop_rev
    | Atom atom -> Domain.Mixop.Atom atom :: mixop_rev
    | Brack (atom_left, inner, atom_right) ->
        let mixop_rev = Domain.Mixop.Atom atom_left :: mixop_rev in
        let mixop_rev = flatten inner mixop_rev in
        Domain.Mixop.Atom atom_right :: mixop_rev
    | Infix (mixop_left, atom, mixop_right) ->
        let mixop_rev = flatten mixop_left mixop_rev in
        let mixop_rev = Domain.Mixop.Atom atom :: mixop_rev in
        flatten mixop_right mixop_rev
    | Seq parts ->
        List.fold_left (fun mixop_rev p -> flatten p mixop_rev) mixop_rev parts
  in
  List.rev (flatten mixop [])
