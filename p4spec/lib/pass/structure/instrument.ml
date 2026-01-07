open Lang
open Ol.Ast
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

(* Instruction id generator *)

let tick_iid = ref 0

let iid () : Sl.iid =
  let iid = !tick_iid in
  tick_iid := !tick_iid + 1;
  iid

(* Phantom id generator *)

let tick_pid = ref 0

let pid () : Sl.pid =
  let pid = !tick_pid in
  tick_pid := !tick_pid + 1;
  pid

(* Phantom insertion *)

let rec insert_phantom (instrs : instr list) : Sl.instr list =
  List.map insert_phantom' instrs

and insert_phantom' (instr : instr) : Sl.instr =
  let iid = iid () in
  insert_phantom'' instr $$ (instr.at, { iid })

and insert_phantom'' (instr : instr) : Sl.instr' =
  match instr.it with
  | IfI (exp_cond, iterexps, instrs_then) ->
      let instrs_then = insert_phantom instrs_then in
      let phantom_opt = Some (pid ()) in
      Sl.IfI (exp_cond, iterexps, instrs_then, phantom_opt)
  | HoldI (id, notexp, iterexps, instrs_hold, instrs_nothold) ->
      let instrs_hold = insert_phantom instrs_hold in
      let instrs_nothold = insert_phantom instrs_nothold in
      let holdcase =
        match (instrs_hold, instrs_nothold) with
        | [], [] -> assert false
        | instrs_hold, [] ->
            let phantom_opt = Some (pid ()) in
            Sl.HoldH (instrs_hold, phantom_opt)
        | [], instrs_nothold ->
            let phantom_opt = Some (pid ()) in
            Sl.NotHoldH (instrs_nothold, phantom_opt)
        | instrs_hold, instrs_nothold -> Sl.BothH (instrs_hold, instrs_nothold)
      in
      Sl.HoldI (id, notexp, iterexps, holdcase)
  | CaseI (exp, cases, total) ->
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
        let blocks = List.map insert_phantom blocks in
        List.combine guards blocks
      in
      let phantom_opt = if total then None else Some (pid ()) in
      Sl.CaseI (exp, cases, phantom_opt)
  | OtherwiseI instr ->
      let instr = insert_phantom' instr in
      Sl.OtherwiseI instr
  | GroupI (id_group, rel_signature, exps_group, instrs_group) ->
      let instrs_group = insert_phantom instrs_group in
      Sl.GroupI (id_group, rel_signature, exps_group, instrs_group)
  | LetI (exp_l, exp_r, iterexps) -> Sl.LetI (exp_l, exp_r, iterexps)
  | RuleI (id, notexp, iterexps) -> Sl.RuleI (id, notexp, iterexps)
  | ResultI (rel_signature, exps) -> Sl.ResultI (rel_signature, exps)
  | ReturnI exp -> Sl.ReturnI exp
  | DebugI exp -> Sl.DebugI exp

(* Nop pass *)

let rec insert_nothing (instrs : instr list) : Sl.instr list =
  List.map insert_nothing' instrs

and insert_nothing' (instr : instr) : Sl.instr =
  let iid = iid () in
  insert_nothing'' instr $$ (instr.at, { iid })

and insert_nothing'' (instr : instr) : Sl.instr' =
  match instr.it with
  | IfI (exp_cond, iterexps, instrs_then) ->
      let instrs_then = insert_nothing instrs_then in
      Sl.IfI (exp_cond, iterexps, instrs_then, None)
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
      Sl.HoldI (id, notexp, iterexps, holdcase)
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
      Sl.CaseI (exp, cases, None)
  | OtherwiseI instr ->
      let instr = insert_nothing' instr in
      Sl.OtherwiseI instr
  | GroupI (id_group, rel_signature, exps_group, instrs_group) ->
      let instrs_group = insert_nothing instrs_group in
      Sl.GroupI (id_group, rel_signature, exps_group, instrs_group)
  | LetI (exp_l, exp_r, iterexps) -> Sl.LetI (exp_l, exp_r, iterexps)
  | RuleI (id, notexp, iterexps) -> Sl.RuleI (id, notexp, iterexps)
  | ResultI (rel_signature, exps) -> Sl.ResultI (rel_signature, exps)
  | ReturnI exp -> Sl.ReturnI exp
  | DebugI exp -> Sl.DebugI exp

(* Instrumentation *)

let instrument (instrs : instr list) : Sl.instr list =
  if
    List.exists
      (fun instr -> match instr.it with OtherwiseI _ -> true | _ -> false)
      instrs
  then insert_nothing instrs
  else insert_phantom instrs
