open Ol.Ast
open Util.Source

(* Parallelize if conditions in logical or *)

let rec parallelize_exp_disjunction (iterexps : iterexp list) (exp : exp) :
    exp list option =
  match iterexps with [] -> parallelize_exp_disjunction' exp | _ -> None

and parallelize_exp_disjunction' (exp : exp) : exp list option =
  match exp.it with
  | BinE (`OrOp, _, exp_l, exp_r) -> (
      let exps_l = parallelize_exp_disjunction' exp_l in
      let exps_r = parallelize_exp_disjunction' exp_r in
      match (exps_l, exps_r) with
      | Some exps_l, Some exps_r -> Some (exps_l @ exps_r)
      | Some exps_l, None -> Some (exps_l @ [ exp_r ])
      | None, Some exps_r -> Some (exps_r @ [ exp_l ])
      | None, None -> Some [ exp_l; exp_r ])
  | _ -> None

let rec parallelize_if_disjunction (instr : instr) : instr list =
  let at = instr.at in
  match instr.it with
  | IfI (exp_cond, iterexps, instrs_then) -> (
      let instrs_then = parallelize_if_disjunctions instrs_then in
      match parallelize_exp_disjunction iterexps exp_cond with
      | Some exps_cond ->
          List.map
            (fun exp_cond -> IfI (exp_cond, iterexps, instrs_then) $ at)
            exps_cond
      | None -> [ IfI (exp_cond, iterexps, instrs_then) $ at ])
  | HoldI (id, notexp, iterexps, instrs_hold, instrs_nothold) ->
      let instrs_hold = parallelize_if_disjunctions instrs_hold in
      let instrs_nothold = parallelize_if_disjunctions instrs_nothold in
      [ HoldI (id, notexp, iterexps, instrs_hold, instrs_nothold) $ at ]
  | CaseI (exp, cases, total) ->
      let cases =
        let guards, blocks = List.split cases in
        let blocks = List.map parallelize_if_disjunctions blocks in
        List.combine guards blocks
      in
      [ CaseI (exp, cases, total) $ at ]
  | GroupI (id_group, rel_signature, exps_group, instrs_group) ->
      let instrs_group = parallelize_if_disjunctions instrs_group in
      [ GroupI (id_group, rel_signature, exps_group, instrs_group) $ at ]
  | _ -> [ instr ]

and parallelize_if_disjunctions (instrs : instr list) : instr list =
  List.concat_map parallelize_if_disjunction instrs

let apply (instrs : instr list) : instr list =
  parallelize_if_disjunctions instrs
