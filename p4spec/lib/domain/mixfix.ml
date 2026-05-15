open Util.Source

type atom = Atom.t phrase [@@deriving yojson]

type 'a t =
  | Arg of 'a
  | Atom of atom
  | Brack of atom * 'a t * atom
  | Infix of 'a t * atom * 'a t
  | Seq of 'a t list
[@@deriving yojson]

type mixop = unit t

exception Arity_mismatch of string

(* Json serialization and deserialization *)

let rec mixop_to_yojson = function
  | Arg () -> `List [ `String "Arg"; `Null ]
  | Atom atom -> `List [ `String "Atom"; atom_to_yojson atom ]
  | Brack (atom_l, mixop, atom_r) ->
      `List
        [
          `String "Brack";
          atom_to_yojson atom_l;
          mixop_to_yojson mixop;
          atom_to_yojson atom_r;
        ]
  | Infix (mixop_l, atom, mixop_r) ->
      `List
        [
          `String "Infix";
          mixop_to_yojson mixop_l;
          atom_to_yojson atom;
          mixop_to_yojson mixop_r;
        ]
  | Seq mixops ->
      `List [ `String "Seq"; `List (List.map mixop_to_yojson mixops) ]

let rec mixop_of_yojson = function
  | `List [ `String "Arg"; `Null ] -> Ok (Arg ())
  | `List [ `String "Atom"; json_atom ] -> (
      match atom_of_yojson json_atom with
      | Ok atom -> Ok (Atom atom)
      | Error _ as err -> err)
  | `List [ `String "Brack"; json_atom_l; json_mixop; json_atom_r ] -> (
      match atom_of_yojson json_atom_l with
      | Error _ as err -> err
      | Ok atom_l -> (
          match mixop_of_yojson json_mixop with
          | Error _ as err -> err
          | Ok mixop -> (
              match atom_of_yojson json_atom_r with
              | Ok atom_r -> Ok (Brack (atom_l, mixop, atom_r))
              | Error _ as err -> err)))
  | `List [ `String "Infix"; json_mixop_l; json_atom; json_mixop_r ] -> (
      match mixop_of_yojson json_mixop_l with
      | Error _ as err -> err
      | Ok mixop_l -> (
          match atom_of_yojson json_atom with
          | Error _ as err -> err
          | Ok atom -> (
              match mixop_of_yojson json_mixop_r with
              | Ok mixop_r -> Ok (Infix (mixop_l, atom, mixop_r))
              | Error _ as err -> err)))
  | `List [ `String "Seq"; `List json_mixops ] -> (
      let rec loop acc = function
        | [] -> Ok (List.rev acc)
        | json_mixop :: json_mixops -> (
            match mixop_of_yojson json_mixop with
            | Ok mixop -> loop (mixop :: acc) json_mixops
            | Error _ as err -> err)
      in
      match loop [] json_mixops with
      | Ok mixops -> Ok (Seq mixops)
      | Error _ as err -> err)
  | _ -> Error "Mixfix.mixop_of_yojson"

(* Equality and comparison *)

let compare_atom (atom_a : atom) (atom_b : atom) =
  Atom.compare atom_a.it atom_b.it

let rec compare : type a b. compare_arg:(a -> b -> int) -> a t -> b t -> int =
 fun ~compare_arg mixfix_a mixfix_b ->
  if Obj.repr mixfix_a == Obj.repr mixfix_b then 0
  else
    let tag = function
      | Arg _ -> 0
      | Atom _ -> 1
      | Brack _ -> 2
      | Infix _ -> 3
      | Seq _ -> 4
    in
    match (mixfix_a, mixfix_b) with
    | Arg arg_a, Arg arg_b -> compare_arg arg_a arg_b
    | Atom atom_a, Atom atom_b -> compare_atom atom_a atom_b
    | Brack (atom_a_l, mixfix_a, atom_a_r), Brack (atom_b_l, mixfix_b, atom_b_r)
      ->
        let c = compare_atom atom_a_l atom_b_l in
        if c <> 0 then c
        else
          let c = compare ~compare_arg mixfix_a mixfix_b in
          if c <> 0 then c else compare_atom atom_a_r atom_b_r
    | ( Infix (mixfix_a_l, atom_a, mixfix_a_r),
        Infix (mixfix_b_l, atom_b, mixfix_b_r) ) ->
        let c = compare ~compare_arg mixfix_a_l mixfix_b_l in
        if c <> 0 then c
        else
          let c = compare_atom atom_a atom_b in
          if c <> 0 then c else compare ~compare_arg mixfix_a_r mixfix_b_r
    | Seq mixfixes_a, Seq mixfixes_b ->
        let rec compare_mixfixes mixfixes_a mixfixes_b =
          match (mixfixes_a, mixfixes_b) with
          | [], [] -> 0
          | [], _ :: _ -> -1
          | _ :: _, [] -> 1
          | mixfix_a :: mixfixes_a, mixfix_b :: mixfixes_b ->
              let c = compare ~compare_arg mixfix_a mixfix_b in
              if c <> 0 then c else compare_mixfixes mixfixes_a mixfixes_b
        in
        compare_mixfixes mixfixes_a mixfixes_b
    | _ -> Int.compare (tag mixfix_a) (tag mixfix_b)

let eq ~(eq_arg : 'a -> 'b -> bool) (mixfix_a : 'a t) (mixfix_b : 'b t) =
  compare
    ~compare_arg:(fun arg_a arg_b -> if eq_arg arg_a arg_b then 0 else -1)
    mixfix_a mixfix_b
  = 0

let compare_mixop (mixfix_a : 'a t) (mixfix_b : 'b t) =
  compare ~compare_arg:(fun _ _ -> 0) mixfix_a mixfix_b

let eq_mixop (mixfix_a : 'a t) (mixfix_b : 'b t) =
  compare_mixop mixfix_a mixfix_b = 0

(* Fold, map, and iter *)

let rec fold (f : 'acc -> 'a -> 'acc) (acc : 'acc) (mixfix : 'a t) =
  match mixfix with
  | Arg arg -> f acc arg
  | Atom _ -> acc
  | Brack (_, mixfix, _) -> fold f acc mixfix
  | Infix (mixfix_l, _, mixfix_r) -> fold f (fold f acc mixfix_l) mixfix_r
  | Seq mixfixes -> List.fold_left (fold f) acc mixfixes

let rec map (f : 'a -> 'b) = function
  | Arg arg -> Arg (f arg)
  | Atom atom -> Atom atom
  | Brack (atom_l, mixfix, atom_r) -> Brack (atom_l, map f mixfix, atom_r)
  | Infix (mixfix_l, atom, mixfix_r) ->
      Infix (map f mixfix_l, atom, map f mixfix_r)
  | Seq mixfixes -> Seq (List.map (map f) mixfixes)

let rec map_atoms (f : atom -> atom) = function
  | Arg arg -> Arg arg
  | Atom atom -> Atom (f atom)
  | Brack (atom_l, mixfix, atom_r) ->
      Brack (f atom_l, map_atoms f mixfix, f atom_r)
  | Infix (mixfix_l, atom, mixfix_r) ->
      Infix (map_atoms f mixfix_l, f atom, map_atoms f mixfix_r)
  | Seq mixfixes -> Seq (List.map (map_atoms f) mixfixes)

let iter (f : 'a -> unit) (mixfix : 'a t) =
  fold
    (fun () arg ->
      f arg;
      ())
    () mixfix

let iter_atoms (f : atom -> unit) (mixfix : 'a t) =
  ignore
    (map_atoms
       (fun atom ->
         f atom;
         atom)
       mixfix)

(* Conversion *)

let to_string (mixfix : 'a t) =
  let rec to_string' = function
    | Arg _ -> "%"
    | Atom atom -> Atom.render_atom atom.it
    | Brack (atom_l, mixfix, atom_r) ->
        Atom.render_atom atom_l.it ^ to_string' mixfix
        ^ Atom.render_atom atom_r.it
    | Infix (mixfix_l, atom, mixfix_r) ->
        to_string' mixfix_l ^ Atom.render_atom atom.it ^ to_string' mixfix_r
    | Seq mixfixes -> String.concat " " (List.map to_string' mixfixes)
  in
  "`" ^ to_string' mixfix ^ "`"

let to_mixop (mixfix : 'a t) : mixop = map (fun _ -> ()) mixfix

(* Arity *)

let arity (mixfix : 'a t) = fold (fun arity _ -> arity + 1) 0 mixfix

(* Atoms and args *)

let rec atoms (mixfix : 'a t) =
  match mixfix with
  | Arg _ -> []
  | Atom atom -> [ atom ]
  | Brack (atom_l, mixfix, atom_r) -> (atom_l :: atoms mixfix) @ [ atom_r ]
  | Infix (mixfix_l, atom, mixfix_r) ->
      atoms mixfix_l @ [ atom ] @ atoms mixfix_r
  | Seq mixfixes ->
      List.fold_left
        (fun atoms_acc mixfix -> atoms_acc @ atoms mixfix)
        [] mixfixes

type atom_internal = Atom_internal of atom | Arg_internal

let atoms_matrix (mixfix : 'a t) =
  let rec atoms (atoms_acc : atom_internal list) (mixfix : 'a t) =
    match mixfix with
    | Arg _ -> Arg_internal :: atoms_acc
    | Atom atom -> Atom_internal atom :: atoms_acc
    | Brack (atom_l, mixfix, atom_r) ->
        Atom_internal atom_r :: atoms (Atom_internal atom_l :: atoms_acc) mixfix
    | Infix (mixfix_l, atom, mixfix_r) ->
        atoms (Atom_internal atom :: atoms atoms_acc mixfix_l) mixfix_r
    | Seq mixfixes -> List.fold_left atoms atoms_acc mixfixes
  in
  let atoms_internal = List.rev (atoms [] mixfix) in
  let rec split atoms_acc atoms_curr = function
    | [] -> List.rev (List.rev atoms_curr :: atoms_acc)
    | Arg_internal :: rest -> split (List.rev atoms_curr :: atoms_acc) [] rest
    | Atom_internal atom :: rest -> split atoms_acc (atom :: atoms_curr) rest
  in
  split [] [] atoms_internal

let args (mixfix : 'a t) =
  let rec args' args_rev = function
    | Arg arg -> arg :: args_rev
    | Atom _ -> args_rev
    | Brack (_, mixfix, _) -> args' args_rev mixfix
    | Infix (mixfix_l, _, mixfix_r) ->
        let args_rev = args' args_rev mixfix_r in
        args' args_rev mixfix_l
    | Seq mixfixes ->
        List.fold_right
          (fun mixfix args_rev -> args' args_rev mixfix)
          mixfixes args_rev
  in
  args' [] mixfix

(* Filling and splitting *)

let fill (mixop : mixop) (args : 'a list) : 'a t =
  let rec fill' mixop args =
    match mixop with
    | Arg () -> (
        match args with
        | arg :: args -> (Arg arg, args)
        | [] -> raise (Arity_mismatch "Mixfix.fill: too few arguments"))
    | Atom atom -> (Atom atom, args)
    | Brack (atom_l, mixop, atom_r) ->
        let mixfix, args = fill' mixop args in
        (Brack (atom_l, mixfix, atom_r), args)
    | Infix (mixop_l, atom, mixop_r) ->
        let mixfix_l, args = fill' mixop_l args in
        let mixfix_r, args = fill' mixop_r args in
        (Infix (mixfix_l, atom, mixfix_r), args)
    | Seq mixops ->
        let mixfixes, args =
          List.fold_left
            (fun (mixfixes_rev, args) mixop ->
              let mixfix, args = fill' mixop args in
              (mixfix :: mixfixes_rev, args))
            ([], args) mixops
        in
        (Seq (List.rev mixfixes), args)
  in
  match fill' mixop args with
  | mixfix, [] -> mixfix
  | _, _ :: _ -> raise (Arity_mismatch "Mixfix.fill: too many arguments")

let split (mixfix : 'a t) = (to_mixop mixfix, args mixfix)

(* Rendering *)

let render ~(string_of_atom : atom -> string) ~(string_of_arg : 'a -> string)
    (mixfix : 'a t) =
  let rec render' = function
    | Arg arg -> string_of_arg arg
    | Atom atom -> string_of_atom atom
    | Brack (atom_l, mixfix, atom_r) ->
        [ string_of_atom atom_l; render' mixfix; string_of_atom atom_r ]
        |> List.filter (fun s -> s <> "")
        |> String.concat " "
    | Infix (mixfix_l, atom, mixfix_r) ->
        [ render' mixfix_l; string_of_atom atom; render' mixfix_r ]
        |> List.filter (fun s -> s <> "")
        |> String.concat " "
    | Seq mixfixes ->
        mixfixes |> List.map render'
        |> List.filter (fun s -> s <> "")
        |> String.concat " "
  in
  render' mixfix
