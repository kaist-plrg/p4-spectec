open Util.Source
module SMap = Map.Make (String)

type hmap = (Il.Ast.nottyp * El.Ast.exp) list SMap.t

let hintid = "print"

(* Types *)

let hints_of_typcase hmap (id : Il.Ast.id) (typcase : Il.Ast.typcase) : hmap =
  let nottyp, hints = typcase in
  match List.find_opt (fun hint -> El.Ast.(hint.hintid.it = hintid)) hints with
  | Some hint ->
      let hint = (nottyp, hint.hintexp) in
      let add = function
        | None -> Some [ hint ]
        | Some hints -> Some (hints @ [ hint ])
      in
      SMap.update id.it add hmap
  | None -> hmap

let hints_of_typcases hmap (id : Il.Ast.id) (typcases : Il.Ast.typcase list) :
    hmap =
  List.fold_left
    (fun hmap typcase -> hints_of_typcase hmap id typcase)
    hmap typcases

let hints_of_deftyp hmap (id : Il.Ast.id) (deftyp : Il.Ast.deftyp) : hmap =
  match deftyp.it with
  | VariantT typcases -> hints_of_typcases hmap id typcases
  | _ -> hmap

(* Definitions *)

let hints_of_def_il hmap (def_il : Il.Ast.def) : hmap =
  match def_il.it with
  | TypD (id, _, deftyp, _) -> hints_of_deftyp hmap id deftyp
  | _ -> hmap

let hints_of_def_sl hmap (def_sl : Sl.Ast.def) : hmap =
  match def_sl.it with
  | TypD (id, _, deftyp, _) -> hints_of_deftyp hmap id deftyp
  | _ -> hmap

(* Spec *)

let hints_of_spec_il (spec_il : Il.Ast.spec) : hmap =
  List.fold_left hints_of_def_il SMap.empty spec_il

let hints_of_spec_sl (spec_sl : Sl.Ast.spec) : hmap =
  List.fold_left hints_of_def_sl SMap.empty spec_sl
