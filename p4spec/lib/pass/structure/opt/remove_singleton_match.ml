open Ol.Ast
module Typ = Runtime.Dynamic_Sl.Typ
open Runtime.Dynamic_Sl.Envs
open Util.Source

(* Remove redundant match on singleton case

   with type foo = AAA,

   if foo matches pattern AAA then ...

   will be removed *)

let is_singleton_case (tdenv : TDEnv.t) (typ : typ) : bool =
  let typ_unrolled = TDEnv.unroll tdenv typ in
  match typ_unrolled.it with
  | VarT (tid, _) -> (
      let td = TDEnv.find tid tdenv in
      match td with
      | Param | Extern -> false
      | Defined (_, deftyp) -> (
          match deftyp.it with
          | VariantT typcases -> List.length typcases = 1
          | _ -> false))
  | _ -> false

let is_singleton_match (tdenv : TDEnv.t) (exp : exp) : bool =
  match exp.it with
  | MatchE (exp, _) -> is_singleton_case tdenv (exp.note $ exp.at)
  | _ -> false

let rec remove (tdenv : TDEnv.t) (instrs : instr list) : instr list =
  match instrs with
  | [] -> []
  | instr_h :: instrs_t -> (
      match instr_h.it with
      | IfI (exp_cond, _iterexps, instrs) when is_singleton_match tdenv exp_cond
        ->
          instrs @ instrs_t |> remove tdenv
      | IfI (exp_cond, iterexps, instrs) ->
          let instrs = remove tdenv instrs in
          let instr_h = IfI (exp_cond, iterexps, instrs) $ instr_h.at in
          let instrs_t = remove tdenv instrs_t in
          instr_h :: instrs_t
      | HoldI (id, notexp, iterexps, instrs_hold, instrs_nothold) ->
          let instrs_hold = remove tdenv instrs_hold in
          let instrs_nothold = remove tdenv instrs_nothold in
          let instr_h =
            HoldI (id, notexp, iterexps, instrs_hold, instrs_nothold)
            $ instr_h.at
          in
          let instrs_t = remove tdenv instrs_t in
          instr_h :: instrs_t
      | CaseI (exp, cases, total) ->
          let cases =
            let guards, instrss = List.split cases in
            let instrss = List.map (remove tdenv) instrss in
            List.combine guards instrss
          in
          let instr_h = CaseI (exp, cases, total) $ instr_h.at in
          let instrs_t = remove tdenv instrs_t in
          instr_h :: instrs_t
      | GroupI (id_group, rel_signature, exps_group, instrs_group) ->
          let instrs_group = remove tdenv instrs_group in
          let instr_h =
            GroupI (id_group, rel_signature, exps_group, instrs_group)
            $ instr_h.at
          in
          let instrs_t = remove tdenv instrs_t in
          instr_h :: instrs_t
      | _ -> instr_h :: remove tdenv instrs_t)

let apply (tdenv : TDEnv.t) (instrs : instr list) : instr list =
  remove tdenv instrs
