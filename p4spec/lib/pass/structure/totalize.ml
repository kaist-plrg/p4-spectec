open Domain
open Ol.Ast
module Typ = Runtime.Dynamic_Sl.Typ
open Runtime.Dynamic_Sl.Envs
open Util.Source

(* Totalize case analysis of variant matches *)

let find_variant_case_analysis (tdenv : TDEnv.t) (cases : case list) :
    mixop list option =
  List.fold_left
    (fun mixops_opt (guard, _) ->
      match mixops_opt with
      | Some mixops -> (
          match guard with
          | SubG typ ->
              Some
                (mixops
                @ (typ |> Opt.Overlap.typ_as_variant tdenv |> Option.get))
          | MatchG (CaseP mixop) -> Some (mixops @ [ mixop ])
          | _ -> None)
      | None -> None)
    (Some []) cases

let rec totalize_case_analysis (tdenv : TDEnv.t) (block : block) : block =
  List.map (totalize_case_analysis' tdenv) block

and totalize_case_analysis' (tdenv : TDEnv.t) (instr : instr) : instr =
  let at = instr.at in
  match instr.it with
  | IfI (exp_cond, iterexps, block_then) ->
      let block_then = totalize_case_analysis tdenv block_then in
      IfI (exp_cond, iterexps, block_then) $ at
  | HoldI (id, notexp, iterexps, block_hold, block_nothold) ->
      let block_hold = totalize_case_analysis tdenv block_hold in
      let block_nothold = totalize_case_analysis tdenv block_nothold in
      HoldI (id, notexp, iterexps, block_hold, block_nothold) $ at
  | CaseI (exp, cases, total) -> (
      let cases =
        let guards, blocks = List.split cases in
        let blocks = List.map (totalize_case_analysis tdenv) blocks in
        List.combine guards blocks
      in
      match find_variant_case_analysis tdenv cases with
      | Some mixops_case ->
          let module Set = Set.Make (Mixop) in
          let mixops_total =
            let typ = exp.note $ exp.at in
            typ |> Opt.Overlap.typ_as_variant tdenv |> Option.get
          in
          let mixops_total = Set.of_list mixops_total in
          let mixops_case = Set.of_list mixops_case in
          let total = Set.equal mixops_case mixops_total in
          CaseI (exp, cases, total) $ at
      | None -> CaseI (exp, cases, total) $ at)
  | GroupI (id_group, rel_signature, exps_group, block) ->
      let block = totalize_case_analysis tdenv block in
      GroupI (id_group, rel_signature, exps_group, block) $ at
  | LetI (exp_l, exp_r, iterinstrs, block) ->
      let block = totalize_case_analysis tdenv block in
      LetI (exp_l, exp_r, iterinstrs, block) $ at
  | RuleI (id, notexp, inputs, iterinstrs, block) ->
      let block = totalize_case_analysis tdenv block in
      RuleI (id, notexp, inputs, iterinstrs, block) $ at
  | _ -> instr

let totalize (tdenv : TDEnv.t) (block : block)
    (elseblock_opt : elseblock option) : block * elseblock option =
  let block = totalize_case_analysis tdenv block in
  let elseblock_opt = Option.map (totalize_case_analysis tdenv) elseblock_opt in
  (block, elseblock_opt)

let totalize_without_else (tdenv : TDEnv.t) (block : block) : block =
  totalize_case_analysis tdenv block
