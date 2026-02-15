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

let rec insert_phantom (block : block) : Sl.block =
  List.map insert_phantom' block

and insert_phantom' (instr : instr) : Sl.instr =
  let iid = iid () in
  insert_phantom'' instr $$ (instr.at, { iid })

and insert_phantom'' (instr : instr) : Sl.instr' =
  match instr.it with
  | IfI (exp_cond, iterexps, block_then) ->
      let block_then = insert_phantom block_then in
      let phantom_opt = Some (pid ()) in
      Sl.IfI (exp_cond, iterexps, block_then, phantom_opt)
  | HoldI (id, notexp, iterexps, block_hold, block_nothold) ->
      let block_hold = insert_phantom block_hold in
      let block_nothold = insert_phantom block_nothold in
      let holdcase =
        match (block_hold, block_nothold) with
        | [], [] -> assert false
        | block_hold, [] ->
            let phantom_opt = Some (pid ()) in
            Sl.HoldH (block_hold, phantom_opt)
        | [], block_nothold ->
            let phantom_opt = Some (pid ()) in
            Sl.NotHoldH (block_nothold, phantom_opt)
        | block_hold, block_nothold -> Sl.BothH (block_hold, block_nothold)
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
  | GroupI (id_group, rel_signature, exps_group, block) ->
      let block = insert_phantom block in
      Sl.GroupI (id_group, rel_signature, exps_group, block)
  | LetI (exp_l, exp_r, iterinstrs, block) ->
      let block = insert_phantom block in
      Sl.LetI (exp_l, exp_r, iterinstrs, block)
  | RuleI (id, notexp, inputs, iterinstrs, block) ->
      let block = insert_phantom block in
      Sl.RuleI (id, notexp, inputs, iterinstrs, block)
  | ResultI (rel_signature, exps) -> Sl.ResultI (rel_signature, exps)
  | ReturnI exp -> Sl.ReturnI exp
  | DebugI exp -> Sl.DebugI exp

(* Nop pass *)

let rec insert_nothing (block : block) : Sl.block =
  List.map insert_nothing' block

and insert_nothing' (instr : instr) : Sl.instr =
  let iid = iid () in
  insert_nothing'' instr $$ (instr.at, { iid })

and insert_nothing'' (instr : instr) : Sl.instr' =
  match instr.it with
  | IfI (exp_cond, iterexps, block_then) ->
      let block_then = insert_nothing block_then in
      Sl.IfI (exp_cond, iterexps, block_then, None)
  | HoldI (id, notexp, iterexps, block_hold, block_nothold) ->
      let block_hold = insert_nothing block_hold in
      let block_nothold = insert_nothing block_nothold in
      let holdcase =
        match (block_hold, block_nothold) with
        | [], [] -> assert false
        | block_hold, [] -> Sl.HoldH (block_hold, None)
        | [], block_nothold -> Sl.NotHoldH (block_nothold, None)
        | block_hold, block_nothold -> Sl.BothH (block_hold, block_nothold)
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
  | GroupI (id_group, rel_signature, exps_group, block) ->
      let block = insert_nothing block in
      Sl.GroupI (id_group, rel_signature, exps_group, block)
  | LetI (exp_l, exp_r, iterinstrs, block) ->
      let block = insert_nothing block in
      Sl.LetI (exp_l, exp_r, iterinstrs, block)
  | RuleI (id, notexp, inputs, iterinstrs, block) ->
      let block = insert_nothing block in
      Sl.RuleI (id, notexp, inputs, iterinstrs, block)
  | ResultI (rel_signature, exps) -> Sl.ResultI (rel_signature, exps)
  | ReturnI exp -> Sl.ReturnI exp
  | DebugI exp -> Sl.DebugI exp

(* Instrumentation *)

let instrument (block : block) (elseblock_opt : elseblock option) :
    Sl.block * Sl.elseblock option =
  match elseblock_opt with
  | Some elseblock ->
      let block = insert_nothing block in
      let elseblock = insert_nothing elseblock in
      (block, Some elseblock)
  | None ->
      let block = insert_phantom block in
      (block, None)

let instrument_without_else (block : block) : Sl.block = insert_nothing block
