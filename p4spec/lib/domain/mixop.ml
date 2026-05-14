open Util.Source

(* Mixop is a generalized case constructor *)

type atom = Atom.t phrase [@@deriving yojson]

type t =
  | Arg
  | Atom of atom
  | Brack of atom * t * atom
  | Infix of t * atom * t
  | Seq of t list
[@@deriving yojson]

let compare_atom (atom_a : atom) (atom_b : atom) =
  Atom.compare atom_a.it atom_b.it

let rec compare (mixop_a : t) (mixop_b : t) =
  if mixop_a == mixop_b then 0
  else
    let tag (mixop : t) =
      match mixop with
      | Arg -> 0
      | Atom _ -> 1
      | Brack _ -> 2
      | Infix _ -> 3
      | Seq _ -> 4
    in
    match (mixop_a, mixop_b) with
    | Arg, Arg -> 0
    | Atom atom_a, Atom atom_b -> compare_atom atom_a atom_b
    | Brack (atom_a_l, mixop_a, atom_a_r), Brack (atom_b_l, mixop_b, atom_b_r)
      ->
        let c = compare_atom atom_a_l atom_b_l in
        if c <> 0 then c
        else
          let c = compare mixop_a mixop_b in
          if c <> 0 then c else compare_atom atom_a_r atom_b_r
    | Infix (mixop_a_l, atom_a, mixop_a_r), Infix (mixop_b_l, atom_b, mixop_b_r)
      ->
        let c = compare mixop_a_l mixop_b_l in
        if c <> 0 then c
        else
          let c = compare_atom atom_a atom_b in
          if c <> 0 then c else compare mixop_a_r mixop_b_r
    | Seq mixops_a, Seq mixops_b -> compare_mixops mixops_a mixops_b
    | mixop_a, mixop_b -> Int.compare (tag mixop_a) (tag mixop_b)

and compare_mixops (mixops_a : t list) (mixops_b : t list) =
  match (mixops_a, mixops_b) with
  | [], [] -> 0
  | mixop_a :: mixops_a, mixop_b :: mixops_b ->
      let c = compare mixop_a mixop_b in
      if c <> 0 then c else compare_mixops mixops_a mixops_b
  | [], _ -> -1
  | _, [] -> 1

let eq (mixop_a : t) (mixop_b : t) = compare mixop_a mixop_b = 0

(* Arity *)

let arity (mixop : t) : int =
  let rec arity (mixop : t) : int =
    match mixop with
    | Arg -> 1
    | Atom _ -> 0
    | Brack (_, mixop, _) -> arity mixop
    | Infix (mixop_l, _, mixop_r) -> arity mixop_l + arity mixop_r
    | Seq mixops ->
        List.fold_left (fun arity_acc mixop -> arity_acc + arity mixop) 0 mixops
  in
  arity mixop

(* Atoms *)

let atoms (mixop : t) : atom list =
  let rec atoms (atoms_acc : atom list) (mixop : t) =
    match mixop with
    | Arg -> atoms_acc
    | Atom atom -> atom :: atoms_acc
    | Brack (atom_l, mixop, atom_r) ->
        atom_r :: atoms (atom_l :: atoms_acc) mixop
    | Infix (mixop_l, atom, mixop_r) ->
        atoms (atom :: atoms atoms_acc mixop_l) mixop_r
    | Seq mixops -> List.fold_left atoms atoms_acc mixops
  in
  List.rev (atoms [] mixop)

type atom_internal = Atom_internal of atom | Arg_internal

let atoms_matrix (mixop : t) : atom list list =
  let rec atoms (atoms_acc : atom_internal list) (mixop : t) =
    match mixop with
    | Arg -> Arg_internal :: atoms_acc
    | Atom atom -> Atom_internal atom :: atoms_acc
    | Brack (atom_l, mixop, atom_r) ->
        Atom_internal atom_r :: atoms (Atom_internal atom_l :: atoms_acc) mixop
    | Infix (mixop_l, atom, mixop_r) ->
        atoms (Atom_internal atom :: atoms atoms_acc mixop_l) mixop_r
    | Seq mixops -> List.fold_left atoms atoms_acc mixops
  in
  let atoms_internal = List.rev (atoms [] mixop) in
  let rec split atoms_acc atoms_curr = function
    | [] -> List.rev (List.rev atoms_curr :: atoms_acc)
    | Arg_internal :: rest -> split (List.rev atoms_curr :: atoms_acc) [] rest
    | Atom_internal atom :: rest -> split atoms_acc (atom :: atoms_curr) rest
  in
  split [] [] atoms_internal

(* Stringifier *)

let string_of_mixop (mixop : t) =
  let rec string_of_mixop (mixop : t) =
    match mixop with
    | Arg -> "%"
    | Atom atom -> Atom.render_atom atom.it
    | Brack (atom_l, mixop, atom_r) ->
        Atom.render_atom atom_l.it ^ string_of_mixop mixop
        ^ Atom.render_atom atom_r.it
    | Infix (mixop_l, atom, mixop_r) ->
        string_of_mixop mixop_l ^ Atom.render_atom atom.it
        ^ string_of_mixop mixop_r
    | Seq mixops -> String.concat " " (List.map string_of_mixop mixops)
  in
  let s = "`" ^ string_of_mixop mixop ^ "`" in
  s

(* Assembler *)

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
