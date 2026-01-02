open Util.Source
open Lang
module SMap = Map.Make (String)

type hmap = (Il.nottyp * El.exp) list SMap.t

let hintid = "print"

(* Types *)

let hints_of_typcase hmap (id : Il.id) (typcase : Il.typcase) : hmap =
  let nottyp, hints = typcase in
  match List.find_opt (fun hint -> El.(hint.hintid.it = hintid)) hints with
  | Some hint ->
      let hint = (nottyp, hint.hintexp) in
      let add = function
        | None -> Some [ hint ]
        | Some hints -> Some (hints @ [ hint ])
      in
      SMap.update id.it add hmap
  | None -> hmap

let hints_of_typcases hmap (id : Il.id) (typcases : Il.typcase list) : hmap =
  List.fold_left
    (fun hmap typcase -> hints_of_typcase hmap id typcase)
    hmap typcases

let hints_of_deftyp hmap (id : Il.id) (deftyp : Il.deftyp) : hmap =
  match deftyp.it with
  | VariantT typcases -> hints_of_typcases hmap id typcases
  | _ -> hmap

(* Definitions *)

let hints_of_def_il hmap (def_il : Il.def) : hmap =
  match def_il.it with
  | TypD (id, _, deftyp, _) -> hints_of_deftyp hmap id deftyp
  | _ -> hmap

let hints_of_def_sl hmap (def_sl : Sl.def) : hmap =
  match def_sl.it with
  | TypD (id, _, deftyp, _) -> hints_of_deftyp hmap id deftyp
  | _ -> hmap

(* Spec *)

let hints_of_spec_il (spec_il : Il.spec) : hmap =
  List.fold_left hints_of_def_il SMap.empty spec_il

let hints_of_spec_sl (spec_sl : Sl.spec) : hmap =
  List.fold_left hints_of_def_sl SMap.empty spec_sl
