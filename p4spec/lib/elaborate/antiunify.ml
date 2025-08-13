open Domain.Lib
open Il.Ast
open Runtime_static
open Attempt
open Util.Source

(* Compute the anti-unified template by overlapping expressions *)

let rec overlap_exp (tdenv : Envs.TDEnv.t) (frees : IdSet.t)
    (unifiers : IdSet.t) (exp_template : exp) (exp : exp) :
    (IdSet.t * IdSet.t * exp) attempt =
  let at, note = (exp_template.at, exp_template.note) in
  let overlap_exp_unequal' () : (IdSet.t * IdSet.t * exp) attempt =
    match (exp_template.it, exp.it) with
    | VarE id_template, _ when IdSet.mem id_template unifiers ->
        Ok (frees, unifiers, exp_template)
    | UpCastE (typ_template, exp_template), UpCastE (typ, exp)
      when Il.Eq.eq_typ typ_template typ ->
        let* frees, unifiers, exp_template =
          overlap_exp tdenv frees unifiers exp_template exp
        in
        let exp_template = UpCastE (typ_template, exp_template) $$ (at, note) in
        Ok (frees, unifiers, exp_template)
    | TupleE exps_template, TupleE exps ->
        let* frees, unifiers, exps_template =
          overlap_exps tdenv frees unifiers exps_template exps
        in
        let exp_template = TupleE exps_template $$ (at, note) in
        Ok (frees, unifiers, exp_template)
    | CaseE (mixop_template, exps_template), CaseE (mixop, exps)
      when Il.Eq.eq_mixop mixop_template mixop ->
        let* frees, unifiers, exps_template =
          overlap_exps tdenv frees unifiers exps_template exps
        in
        let exp_template =
          CaseE (mixop_template, exps_template) $$ (at, note)
        in
        Ok (frees, unifiers, exp_template)
    | _ ->
        fail exp.at
          (Format.asprintf "cannot anti-unify expressions %s and %s"
             (Il.Print.string_of_exp exp_template)
             (Il.Print.string_of_exp exp))
  in
  let overlap_exp_unequal () : (IdSet.t * IdSet.t * exp) attempt =
    match overlap_exp_unequal' () with
    | Ok (frees, unifiers, exp_template) -> Ok (frees, unifiers, exp_template)
    | Fail _ ->
        let typ_template = exp_template.note $ exp_template.at in
        let plaintyp_template = typ_template |> Plaintyp.of_internal_typ in
        let plaintyp = exp.note $ exp.at |> Plaintyp.of_internal_typ in
        if not (Equiv.equiv_plaintyp tdenv plaintyp_template plaintyp) then
          fail exp.at
            (Format.asprintf "cannot anti-unify expressions %s and %s"
               (Il.Print.string_of_exp exp_template)
               (Il.Print.string_of_exp exp))
        else
          let id_fresh, typ_fresh, iter_fresh =
            Fresh.fresh_from_typ frees exp_template.at typ_template
          in
          let frees = IdSet.add id_fresh frees in
          let unifiers = IdSet.add id_fresh unifiers in
          let exp_template = Fresh.as_exp id_fresh typ_fresh iter_fresh in
          Ok (frees, unifiers, exp_template)
  in
  if Il.Eq.eq_exp exp_template exp then Ok (frees, unifiers, exp_template)
  else overlap_exp_unequal ()

and overlap_exps (tdenv : Envs.TDEnv.t) (frees : IdSet.t) (unifiers : IdSet.t)
    (exps_template : exp list) (exps : exp list) :
    (IdSet.t * IdSet.t * exp list) attempt =
  match (exps_template, exps) with
  | [], [] -> Ok (frees, unifiers, [])
  | exp_template :: exps_template, exp :: exps ->
      let* frees, unifiers, exp_template =
        overlap_exp tdenv frees unifiers exp_template exp
      in
      let* frees, unifiers, exps_template =
        overlap_exps tdenv frees unifiers exps_template exps
      in
      Ok (frees, unifiers, exp_template :: exps_template)
  | _ ->
      fail no_region "cannot anti-unify expression lists of different lengths"

let overlap_exp_group (tdenv : Envs.TDEnv.t) (frees : IdSet.t) (exps : exp list)
    : IdSet.t * IdSet.t * exp =
  let exp_template, exps = (List.hd exps, List.tl exps) in
  List.fold_left
    (fun (frees, unifiers, exp_template) exp ->
      let+ frees, unifiers, exp_template =
        overlap_exp tdenv frees unifiers exp_template exp
      in
      (frees, unifiers, exp_template))
    (frees, IdSet.empty, exp_template)
    exps

let overlap_exps_group (tdenv : Envs.TDEnv.t) (frees : IdSet.t)
    (exps_group : exp list list) : IdSet.t * exp list =
  match exps_group with
  | [] -> (IdSet.empty, [])
  | [ exps ] -> (IdSet.empty, exps)
  | _ ->
      let exps_batch =
        let width = exps_group |> List.hd |> List.length in
        let height = List.length exps_group in
        List.init width (fun j ->
            List.init height (fun i -> List.nth (List.nth exps_group i) j))
      in
      let _, unifiers, exps_template =
        List.fold_left
          (fun (frees, unifiers, exps_template) exp_batch ->
            let frees, unifiers_batch, exp_batch_template =
              overlap_exp_group tdenv frees exp_batch
            in
            let unifiers = IdSet.union unifiers unifiers_batch in
            (frees, unifiers, exps_template @ [ exp_batch_template ]))
          (frees, IdSet.empty, []) exps_batch
      in
      (unifiers, exps_template)

(* Populate the template, creating side-conditions for unifiers *)

let rec populate_exp (unifiers : IdSet.t) (exp_template : exp) (exp : exp) :
    prem list =
  let populate_exp_unequal () =
    match (exp_template.it, exp.it) with
    | VarE id_template, _ when IdSet.mem id_template unifiers ->
        let exp_match =
          CmpE (`EqOp, `BoolT, exp, exp_template) $$ (exp.at, BoolT)
        in
        let prem_match = IfPr exp_match $ exp_match.at in
        [ prem_match ]
    | UpCastE (typ_template, exp_template), UpCastE (typ_exp, exp)
      when Il.Eq.eq_typ typ_template typ_exp ->
        populate_exp unifiers exp_template exp
    | TupleE exps_template, TupleE exps ->
        populate_exps unifiers exps_template exps
    | CaseE (mixop_template, exps_template), CaseE (mixop, exps)
      when Il.Eq.eq_mixop mixop_template mixop ->
        populate_exps unifiers exps_template exps
    | _ ->
        let exp_match =
          CmpE (`EqOp, `BoolT, exp, exp_template) $$ (exp.at, BoolT)
        in
        let prem_match = IfPr exp_match $ exp_match.at in
        [ prem_match ]
  in
  if Il.Eq.eq_exp exp_template exp then [] else populate_exp_unequal ()

and populate_exps (unifiers : IdSet.t) (exps_template : exp list)
    (exps : exp list) : prem list =
  List.fold_left2
    (fun prems_match exp_template exp ->
      prems_match @ populate_exp unifiers exp_template exp)
    [] exps_template exps

let populate_exp_group (unifiers : IdSet.t) (exps_template : exp list)
    (exps : exp list) : prem list =
  List.fold_left2
    (fun prems_match exp_template exp ->
      prems_match @ populate_exp unifiers exp_template exp)
    [] exps_template exps

let populate_exps_group (unifiers : IdSet.t) (exps_template : exp list)
    (exps_group : exp list list) : prem list list =
  List.map (populate_exp_group unifiers exps_template) exps_group

(* Entry point *)

let antiunify (ctx : Ctx.t) (exps_group : exp list list) :
    Ctx.t * exp list * prem list list =
  let tdenv = ctx.tdenv in
  let frees = ctx.frees in
  let unifiers, exps_template = overlap_exps_group tdenv frees exps_group in
  let prems_match_group =
    populate_exps_group unifiers exps_template exps_group
  in
  let ctx = { ctx with frees = IdSet.union ctx.frees unifiers } in
  (ctx, exps_template, prems_match_group)
