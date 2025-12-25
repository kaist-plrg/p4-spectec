open Lang
open Ol.Ast
module TDEnv = Runtime.Dynamic_Sl.Envs.TDEnv
open Util.Source

(* Insert phantom instructions at dangling else branches,
   with the path condition necessary to reach the else branch

   Note that this does not take fall-through into account,
   so the path condition is not precise

   Fall-through may happen due to the heuristic-driven syntactic optimization of SL,

   (i) Good case

   -- if i >= 0   and   -- if i < 0
   -- if j >= 0         -- if j >= 0

   are nicely merged into

   if i >= 0 then
     if j >= 0 then ...
     else Phantom: i >= 0 && j < 0
   else
     if j >= 0 then ...
     else Phantom: i < 0 && j < 0

   (ii) Bad case

   -- if j >= 0   and  -- if i < 0
   -- if i >= 0        -- if j >= 0

   are merged into

   if j >= 0 then
     if i >= 0 then ...
     else Phantom: j >= 0 && i < 0
   else Phantom: j < 0

   ... if i = -1, j = 3 is given as input, it falls through

   if i < 0 then
      if j >= 0 then ...
      else Phantom: i < 0 && j < 0
   else Phantom: i >= 0 *)

(* Phantom id generator *)

let tick = ref 0

let pid () =
  let pid = !tick in
  tick := !tick + 1;
  pid

(* Path condition *)

let negate_exp (exp : exp) : exp =
  Il.UnE (`NotOp, `BoolT, exp) $$ (exp.at, exp.note)

let rec negate_pathcond (pathcond : pathcond) : pathcond =
  match pathcond with
  | ForallC (pathcond, iterexps) -> ExistsC (negate_pathcond pathcond, iterexps)
  | ExistsC (pathcond, iterexps) -> ForallC (negate_pathcond pathcond, iterexps)
  | PlainC exp_cond -> PlainC (negate_exp exp_cond)
  | HoldC (id, notexp) -> NotHoldC (id, notexp)
  | NotHoldC (id, notexp) -> HoldC (id, notexp)

(* Phantom insertion *)

let rec insert_phantom (tdenv : TDEnv.t) (pathconds : pathcond list)
    (instrs : instr list) : Sl.instr list =
  List.map (insert_phantom' tdenv pathconds) instrs

and insert_phantom' (tdenv : TDEnv.t) (pathconds : pathcond list)
    (instr : instr) : Sl.instr =
  let at = instr.at in
  match instr.it with
  | IfI (exp_cond, iterexps, instrs_then) ->
      let pathcond =
        if iterexps = [] then PlainC exp_cond
        else ForallC (PlainC exp_cond, iterexps)
      in
      let instrs_then =
        let pathconds = pathconds @ [ pathcond ] in
        insert_phantom tdenv pathconds instrs_then
      in
      let phantom =
        let pid = pid () in
        let pathconds = pathconds @ [ negate_pathcond pathcond ] in
        (pid, pathconds)
      in
      Sl.IfI (exp_cond, iterexps, instrs_then, Some phantom) $ at
  | HoldI (id, notexp, iterexps, instrs_hold, instrs_nothold) ->
      let pathcond_hold =
        if iterexps = [] then HoldC (id, notexp)
        else ForallC (HoldC (id, notexp), iterexps)
      in
      let instrs_hold =
        let pathconds = pathconds @ [ pathcond_hold ] in
        insert_phantom tdenv pathconds instrs_hold
      in
      let pathcond_nothold =
        if iterexps = [] then NotHoldC (id, notexp)
        else ForallC (NotHoldC (id, notexp), iterexps)
      in
      let instrs_nothold =
        let pathconds = pathconds @ [ pathcond_nothold ] in
        insert_phantom tdenv pathconds instrs_nothold
      in
      let holdcase =
        match (instrs_hold, instrs_nothold) with
        | [], [] -> assert false
        | instrs_hold, [] ->
            let phantom =
              let pid = pid () in
              let pathconds = pathconds @ [ negate_pathcond pathcond_hold ] in
              (pid, pathconds)
            in
            Sl.HoldH (instrs_hold, Some phantom)
        | [], instrs_nothold ->
            let phantom =
              let pid = pid () in
              let pathconds =
                pathconds @ [ negate_pathcond pathcond_nothold ]
              in
              (pid, pathconds)
            in
            Sl.NotHoldH (instrs_nothold, Some phantom)
        | instrs_hold, instrs_nothold -> Sl.BothH (instrs_hold, instrs_nothold)
      in
      Sl.HoldI (id, notexp, iterexps, holdcase) $ at
  | CaseI (exp, cases, total) ->
      let pathconds_cases =
        List.map
          (fun (guard, _) ->
            let exp_cond = Optimize.guard_as_exp exp guard in
            PlainC exp_cond)
          cases
      in
      let cases =
        let guards, blocks = List.split cases in
        let guards =
          List.map
            (function
              | BoolG b -> Sl.BoolG b
              | CmpG (cmpop, optyp, exp) -> Sl.CmpG (cmpop, optyp, exp)
              | SubG typ -> Sl.SubG typ
              | MatchG pattern -> Sl.MatchG pattern
              | MemG exp -> Sl.MemG exp)
            guards
        in
        let blocks =
          List.map2
            (fun pathcond_case instrs ->
              let pathconds = pathconds @ [ pathcond_case ] in
              insert_phantom tdenv pathconds instrs)
            pathconds_cases blocks
        in
        List.combine guards blocks
      in
      let phantom_opt =
        if total then None
        else
          let pid = pid () in
          let pathcond = pathconds @ List.map negate_pathcond pathconds_cases in
          Some (pid, pathcond)
      in
      Sl.CaseI (exp, cases, phantom_opt) $ at
  | OtherwiseI instr ->
      let instr = insert_phantom' tdenv pathconds instr in
      Sl.OtherwiseI instr $ at
  | GroupI (id_group, exps_group, instrs_group) ->
      let instrs_group = insert_phantom tdenv pathconds instrs_group in
      Sl.GroupI (id_group, exps_group, instrs_group) $ at
  | LetI (exp_l, exp_r, iterexps) -> Sl.LetI (exp_l, exp_r, iterexps) $ at
  | RuleI (id, notexp, iterexps) -> Sl.RuleI (id, notexp, iterexps) $ at
  | ResultI exps -> Sl.ResultI exps $ at
  | ReturnI exp -> Sl.ReturnI exp $ at
  | DebugI exp -> Sl.DebugI exp $ at

(* Nop pass *)

let rec insert_nothing (instrs : instr list) : Sl.instr list =
  List.map insert_nothing' instrs

and insert_nothing' (instr : instr) : Sl.instr =
  let at = instr.at in
  match instr.it with
  | IfI (exp_cond, iterexps, instrs_then) ->
      let instrs_then = insert_nothing instrs_then in
      Sl.IfI (exp_cond, iterexps, instrs_then, None) $ at
  | HoldI (id, notexp, iterexps, instrs_hold, instrs_nothold) ->
      let instrs_hold = insert_nothing instrs_hold in
      let instrs_nothold = insert_nothing instrs_nothold in
      let holdcase =
        match (instrs_hold, instrs_nothold) with
        | [], [] -> assert false
        | instrs_hold, [] -> Sl.HoldH (instrs_hold, None)
        | [], instrs_nothold -> Sl.NotHoldH (instrs_nothold, None)
        | instrs_hold, instrs_nothold -> Sl.BothH (instrs_hold, instrs_nothold)
      in
      Sl.HoldI (id, notexp, iterexps, holdcase) $ at
  | CaseI (exp, cases, _total) ->
      let cases =
        let guards, blocks = List.split cases in
        let guards =
          List.map
            (function
              | BoolG b -> Sl.BoolG b
              | CmpG (cmpop, optyp, exp) -> Sl.CmpG (cmpop, optyp, exp)
              | SubG typ -> Sl.SubG typ
              | MatchG pattern -> Sl.MatchG pattern
              | MemG exp -> Sl.MemG exp)
            guards
        in
        let blocks = List.map insert_nothing blocks in
        List.combine guards blocks
      in
      Sl.CaseI (exp, cases, None) $ at
  | OtherwiseI instr ->
      let instr = insert_nothing' instr in
      Sl.OtherwiseI instr $ at
  | GroupI (id_group, exps_group, instrs_group) ->
      let instrs_group = insert_nothing instrs_group in
      Sl.GroupI (id_group, exps_group, instrs_group) $ at
  | LetI (exp_l, exp_r, iterexps) -> Sl.LetI (exp_l, exp_r, iterexps) $ at
  | RuleI (id, notexp, iterexps) -> Sl.RuleI (id, notexp, iterexps) $ at
  | ResultI exps -> Sl.ResultI exps $ at
  | ReturnI exp -> Sl.ReturnI exp $ at
  | DebugI exp -> Sl.DebugI exp $ at

(* Instrumentation *)

let instrument (tdenv : TDEnv.t) (instrs : instr list) : Sl.instr list =
  if
    List.exists
      (fun instr -> match instr.it with OtherwiseI _ -> true | _ -> false)
      instrs
  then insert_nothing instrs
  else insert_phantom tdenv [] instrs
