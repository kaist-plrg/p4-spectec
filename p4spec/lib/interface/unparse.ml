open Domain
open Lang
open Il.Eq
open Flatten
open Hint
module Value = Runtime.Dynamic_Il.Value
open Util.Source
module F = Format

(* Numbers *)

let pp_num fmt (num : Il.num) : unit =
  match num with
  | `Nat n -> F.fprintf fmt "%s" (Bigint.to_string n)
  | `Int i ->
      F.fprintf fmt "%s"
        ((if i >= Bigint.zero then "" else "-")
        ^ Bigint.to_string (Bigint.abs i))

(* Atoms *)

let pp_atom fmt (atom : Il.atom) : unit =
  match atom.it with
  | Atom.SilentAtom _ -> F.fprintf fmt ""
  | _ ->
      F.fprintf fmt "%s" (Atom.string_of_atom atom.it |> String.lowercase_ascii)

let pp_atoms fmt (atoms : Il.atom list) : unit =
  match atoms with
  | [] -> F.fprintf fmt ""
  | _ ->
      let atoms =
        atoms
        |> List.map (fun atom -> F.asprintf "%a" pp_atom atom)
        |> List.filter (fun str -> str <> String.empty)
      in
      F.fprintf fmt "%s" (String.concat " " atoms)

(* Values *)

let rec pp_value (hmap : hmap) fmt (value : Value.t) : unit =
  match value.it with
  | BoolV b -> F.fprintf fmt "%b" b
  | NumV n -> F.fprintf fmt "%a" pp_num n
  | TextV _ -> pp_text_v fmt value
  | StructV _ -> failwith "@pp_value: StructV not implemented"
  | CaseV _ -> pp_case_v hmap fmt value
  | TupleV values ->
      F.fprintf fmt "(%s)"
        (String.concat ", "
           (List.map (fun v -> F.asprintf "%a" (pp_value hmap) v) values))
  | OptV _ -> pp_opt_v hmap fmt value
  | ListV _ -> pp_list_v hmap fmt value
  | _ -> failwith "@pp_value: TODO"

(* TextV *)

and pp_text_v fmt (value : Value.t) : unit =
  match value.it with
  | TextV text -> F.fprintf fmt "%s" (String.escaped text)
  | _ -> failwith "@pp_text_v: expected TextV value"

(* CaseV *)

and pp_case_v (hmap : hmap) fmt (value : Value.t) : unit =
  match flatten_case_v_opt value with
  | Some (id, _, values) -> (
      let matches_hint nottyp value =
        match value.it with
        | Il.CaseV (mixop, _) -> eq_mixop (fst nottyp.it) mixop
        | _ -> false
      in
      let find_hint id value =
        match SMap.find_opt id hmap with
        | None -> None
        | Some typs ->
            List.find_opt (fun (nottyp, _) -> matches_hint nottyp value) typs
            |> Option.map snd
      in
      match find_hint id value with
      | Some hintexp -> pp_hint_case_v hmap hintexp fmt values
      | None -> pp_default_case_v hmap fmt value)
  | _ -> assert false

and pp_hint_case_v (hmap : hmap) (hintexp : El.exp) fmt (values : Value.t list)
    : unit =
  let str =
    Hints.Alter.alternate
      ~base_atom:(fun atom -> F.asprintf "%a" pp_atom atom)
      hintexp
      (fun value -> F.asprintf "%a" (pp_value hmap) value)
      values
  in
  F.fprintf fmt "%s" str

and pp_default_case_v (hmap : hmap) fmt (value : Value.t) : unit =
  match value.it with
  | CaseV (mixop, values) ->
      let len = List.length mixop + List.length values in
      List.init len (fun idx ->
          if idx mod 2 = 0 then
            idx / 2 |> List.nth mixop |> F.asprintf "%a" pp_atoms
          else idx / 2 |> List.nth values |> F.asprintf "%a" (pp_value hmap))
      |> List.filter (fun str -> str <> "")
      |> String.concat " " |> F.fprintf fmt "%s"
  | _ -> failwith "@pp_default_case_v: Expected CaseV value"

(* OptV *)

and pp_opt_v (hmap : hmap) fmt (value : Value.t) : unit =
  match value.it with
  | OptV (Some v) -> F.fprintf fmt "%a" (pp_value hmap) v
  | OptV None -> ()
  | _ -> failwith "@pp_opt_v: expected OptV value"

(* ListV *)

and pp_list_v (hmap : hmap) fmt (value : Value.t) : unit =
  let values =
    match value.it with
    | ListV values -> values
    | _ ->
        failwith
          (F.asprintf "@pp_list_v: expected ListV, got %a" (pp_value hmap) value)
  in
  let ss = List.map (F.asprintf "%a" (pp_value hmap)) values in
  F.fprintf fmt "%s" (String.concat " " ss)

(* P4 program *)

let pp_program_il (spec_il : Il.spec) fmt (value : Value.t) : unit =
  let hmap = hints_of_spec_il spec_il in
  pp_value hmap fmt value

let pp_program_sl (spec_sl : Sl.spec) fmt (value : Value.t) : unit =
  let hmap = hints_of_spec_sl spec_sl in
  pp_value hmap fmt value
