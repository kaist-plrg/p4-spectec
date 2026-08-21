open Lang
open Ol.Ast
open Util.Source

(* Insert dangle flag at dangling else branches

   Fall-through may happen due to the heuristic-driven syntactic optimization of SL,

   (i) Good case

   -- if i >= 0   and   -- if i < 0
   -- if j >= 0         -- if j >= 0

   are nicely merged into

   if i >= 0 then
     if j >= 0 then ...
     else Dangle
   else
     if j >= 0 then ...
     else Dangle

   (ii) Bad case

   -- if j >= 0   and  -- if i < 0
   -- if i >= 0        -- if j >= 0

   are merged into

   if j >= 0 then
     if i >= 0 then ...
     else Dangle
   else Dangle

   ... if i = -1, j = 3 is given as input, it falls through

   if i < 0 then
      if j >= 0 then ...
      else Dangle
   else Dangle *)

(* Instruction id generator *)

let tick_iid = ref 0

let iid () : Sl.iid =
  let iid = !tick_iid in
  tick_iid := !tick_iid + 1;
  iid

(* Dalgne insertion *)

let rec insert_dangle (block : block) : Sl.block = List.map insert_dangle' block

and insert_dangle' (instr : instr) : Sl.instr =
  let iid = iid () in
  insert_dangle'' instr $$ (instr.at, { iid })

and insert_dangle'' (instr : instr) : Sl.instr' =
  match instr.it with
  | IfI (exp_cond, iterexps, block_then) ->
      let block_then = insert_dangle block_then in
      Sl.IfI (exp_cond, iterexps, block_then, true)
  | HoldI (id, notexp, iterexps, block_hold, block_nothold) ->
      let block_hold = insert_dangle block_hold in
      let block_nothold = insert_dangle block_nothold in
      let holdcase =
        match (block_hold, block_nothold) with
        | [], [] -> assert false
        | block_hold, [] -> Sl.HoldH (block_hold, true)
        | [], block_nothold -> Sl.NotHoldH (block_nothold, true)
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
              | SubG (typ, subcheck) -> Sl.SubG (typ, subcheck)
              | MatchG pattern -> Sl.MatchG pattern
              | MemG exp -> Sl.MemG exp)
            guards
        in
        let blocks = List.map insert_dangle blocks in
        List.combine guards blocks
      in
      Sl.CaseI (exp, cases, not total)
  | GroupI (id_group, rel_signature, exps_group, block) ->
      let block = insert_dangle block in
      Sl.GroupI (id_group, rel_signature, exps_group, block)
  | LetI (exp_l, exp_r, iterinstrs, block) ->
      let block = insert_dangle block in
      Sl.LetI (exp_l, exp_r, iterinstrs, block)
  | RuleI (id, notexp, inputs, iterinstrs, block) ->
      let block = insert_dangle block in
      Sl.RuleI (id, notexp, inputs, iterinstrs, block)
  | ResultI (rel_signature, exps) -> Sl.ResultI (rel_signature, exps)
  | ReturnI exp -> Sl.ReturnI exp
  | DebugI (exp, instr) -> Sl.DebugI (exp, insert_dangle' instr)

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
      Sl.IfI (exp_cond, iterexps, block_then, false)
  | HoldI (id, notexp, iterexps, block_hold, block_nothold) ->
      let block_hold = insert_nothing block_hold in
      let block_nothold = insert_nothing block_nothold in
      let holdcase =
        match (block_hold, block_nothold) with
        | [], [] -> assert false
        | block_hold, [] -> Sl.HoldH (block_hold, false)
        | [], block_nothold -> Sl.NotHoldH (block_nothold, false)
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
              | SubG (typ, subcheck) -> Sl.SubG (typ, subcheck)
              | MatchG pattern -> Sl.MatchG pattern
              | MemG exp -> Sl.MemG exp)
            guards
        in
        let blocks = List.map insert_nothing blocks in
        List.combine guards blocks
      in
      Sl.CaseI (exp, cases, false)
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
  | DebugI (exp, instr) -> Sl.DebugI (exp, insert_nothing' instr)

(* Instrumentation *)

let instrument (block : block) (elseblock_opt : elseblock option) :
    Sl.block * Sl.elseblock option =
  match elseblock_opt with
  | Some elseblock ->
      let block = insert_nothing block in
      let elseblock = insert_nothing elseblock in
      (block, Some elseblock)
  | None ->
      let block = insert_dangle block in
      (block, None)

let instrument_without_else (block : block) : Sl.block = insert_nothing block
