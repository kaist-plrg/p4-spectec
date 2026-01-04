open Domain
open Lang
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

let rec pp_value (henv : HEnv.t) fmt (value : Value.t) : unit =
  let note = value.note in
  match value.it with
  | BoolV b -> F.fprintf fmt "%b" b
  | NumV n -> F.fprintf fmt "%a" pp_num n
  | TextV _ -> pp_text_v fmt value
  | StructV _ -> failwith "@pp_value: StructV not implemented"
  | CaseV valuecase -> pp_case_v note henv fmt valuecase
  | TupleV values ->
      F.fprintf fmt "(%s)"
        (String.concat ", "
           (List.map (fun v -> F.asprintf "%a" (pp_value henv) v) values))
  | OptV _ -> pp_opt_v henv fmt value
  | ListV _ -> pp_list_v henv fmt value
  | _ -> failwith "@pp_value: TODO"

(* TextV *)

and pp_text_v fmt (value : Value.t) : unit =
  match value.it with
  | TextV text -> F.fprintf fmt "%s" (String.escaped text)
  | _ -> failwith "@pp_text_v: expected TextV value"

(* CaseV *)

and pp_case_v (note : Il.vnote) (henv : HEnv.t) fmt (valuecase : Il.valuecase) :
    unit =
  let mixop, values = valuecase in
  let cid_opt =
    match note.typ with VarT (tid, _) -> Some (tid, mixop) | _ -> None
  in
  let hint_alter_opt =
    Option.bind cid_opt (fun cid -> HEnv.find_opt cid henv)
  in
  match hint_alter_opt with
  | Some hint_alter -> pp_hint_case_v henv hint_alter fmt values
  | None -> pp_default_case_v henv fmt valuecase

and pp_hint_case_v (henv : HEnv.t) (hint : Hints.Alter.t) fmt
    (values : Value.t list) : unit =
  let str =
    Hints.Alter.alternate
      ~base_atom:(fun atom -> F.asprintf "%a" pp_atom atom)
      hint
      (fun value -> F.asprintf "%a" (pp_value henv) value)
      values
  in
  F.fprintf fmt "%s" str

and pp_default_case_v (henv : HEnv.t) fmt (valuecase : Il.valuecase) : unit =
  let mixop, values = valuecase in
  let len = List.length mixop + List.length values in
  List.init len (fun idx ->
      if idx mod 2 = 0 then
        idx / 2 |> List.nth mixop |> F.asprintf "%a" pp_atoms
      else idx / 2 |> List.nth values |> F.asprintf "%a" (pp_value henv))
  |> List.filter (fun str -> str <> "")
  |> String.concat " " |> F.fprintf fmt "%s"

(* OptV *)

and pp_opt_v (henv : HEnv.t) fmt (value : Value.t) : unit =
  match value.it with
  | OptV (Some v) -> F.fprintf fmt "%a" (pp_value henv) v
  | OptV None -> ()
  | _ -> failwith "@pp_opt_v: expected OptV value"

(* ListV *)

and pp_list_v (henv : HEnv.t) fmt (value : Value.t) : unit =
  let values =
    match value.it with
    | ListV values -> values
    | _ ->
        failwith
          (F.asprintf "@pp_list_v: expected ListV, got %a" (pp_value henv) value)
  in
  let ss = List.map (F.asprintf "%a" (pp_value henv)) values in
  F.fprintf fmt "%s" (String.concat " " ss)

(* P4 program *)

let pp_program_il (spec_il : Il.spec) fmt (value : Value.t) : unit =
  let henv = hints_of_spec_il spec_il in
  pp_value henv fmt value

let pp_program_sl (spec_sl : Sl.spec) fmt (value : Value.t) : unit =
  let henv = hints_of_spec_sl spec_sl in
  pp_value henv fmt value
