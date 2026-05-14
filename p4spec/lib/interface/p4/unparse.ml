open Domain.Lib
module Atom = Domain.Atom
module Mixfix = Domain.Mixfix
module Mixop = Domain.Mixop
open Lang
module Value = Runtime.Value
open Util.Source
module F = Format

(* Hint environment *)

module HEnv = MakeCaseIdEnv (Hints.Alter)

let hintid = "print"

(* Types *)

let hints_of_typcase (henv : HEnv.t) (tid : TId.t) (typcase : Il.typcase) :
    HEnv.t =
  let nottyp, _, hints = typcase in
  let hint_opt =
    List.find_opt (fun hint -> El.(hint.hintid.it = hintid)) hints
    |> Option.map (fun hint -> El.(hint.hintexp))
  in
  let hint_alter_opt = Option.bind hint_opt Hints.Alter.init in
  match hint_alter_opt with
  | Some hint_alter ->
      let mixop, _ = Mixfix.split nottyp.it in
      let cid = (tid, mixop) in
      HEnv.add cid hint_alter henv
  | None -> henv

let hints_of_typcases (henv : HEnv.t) (tid : TId.t) (typcases : Il.typcase list)
    : HEnv.t =
  List.fold_left
    (fun henv typcase -> hints_of_typcase henv tid typcase)
    henv typcases

let hints_of_deftyp (henv : HEnv.t) (tid : TId.t) (deftyp : Il.deftyp) : HEnv.t
    =
  match deftyp.it with
  | VariantT typcases -> hints_of_typcases henv tid typcases
  | _ -> henv

(* Definitions *)

let hints_of_def_il (henv : HEnv.t) (def_il : Il.def) : HEnv.t =
  match def_il.it with
  | TypD (id, _, deftyp, _) -> hints_of_deftyp henv id deftyp
  | _ -> henv

let hints_of_def_sl (henv : HEnv.t) (def_sl : Sl.def) : HEnv.t =
  match def_sl.it with
  | TypD (id, _, deftyp, _) -> hints_of_deftyp henv id deftyp
  | _ -> henv

(* Spec *)

let hints_of_spec_il (spec_il : Il.spec) : HEnv.t =
  List.fold_left hints_of_def_il HEnv.empty spec_il

let hints_of_spec_sl (spec_sl : Sl.spec) : HEnv.t =
  List.fold_left hints_of_def_sl HEnv.empty spec_sl

(* Unparsing *)

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
  | Atom.SilentAtom _ -> ()
  | _ ->
      atom.it |> Atom.string_of_atom |> String.lowercase_ascii
      |> String.split_on_char '`' |> String.concat "" |> F.fprintf fmt "%s"

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
  let mixop, values = Mixfix.split valuecase in
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
  F.fprintf fmt "%s"
    (Mixfix.render
       ~string_of_atom:(fun atom -> F.asprintf "%a" pp_atom atom)
       ~string_of_arg:(F.asprintf "%a" (pp_value henv))
       valuecase)

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
