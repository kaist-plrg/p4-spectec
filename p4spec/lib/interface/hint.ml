open Domain.Lib
module Mixfix = Domain.Mixfix
open Lang
open Util.Source

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
      let mixop = Mixfix.to_mixop nottyp.it in
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

let hints_of_def_al (henv : HEnv.t) (def_al : Al.def) : HEnv.t =
  match def_al.it with
  | TypD (id, _, deftyp, _) -> hints_of_deftyp henv id deftyp
  | _ -> henv

let hints_of_def_sl (henv : HEnv.t) (def_sl : Sl.def) : HEnv.t =
  match def_sl.it with
  | TypD (id, _, deftyp, _) -> hints_of_deftyp henv id deftyp
  | _ -> henv

(* Spec *)

let hints_of_spec_il (spec_il : Il.spec) : HEnv.t =
  List.fold_left hints_of_def_il HEnv.empty spec_il

let hints_of_spec_al (spec_al : Al.spec) : HEnv.t =
  List.fold_left hints_of_def_al HEnv.empty spec_al

let hints_of_spec_sl (spec_sl : Sl.spec) : HEnv.t =
  List.fold_left hints_of_def_sl HEnv.empty spec_sl

let hints_of_def_pl (henv : HEnv.t) (def_pl : Pl.def) : HEnv.t =
  let open Pl in
  match def_pl.Annot.node.it with
  | TypD (id, _, deftyp) -> hints_of_deftyp henv id deftyp
  | _ -> henv

let hints_of_spec_pl (spec_pl : Pl.spec) : HEnv.t =
  List.fold_left hints_of_def_pl HEnv.empty spec_pl
