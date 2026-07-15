open Lang
module Mixfix = Domain.Mixfix
module Collect = Il.Walk.Collect
module StringSet = Set.Make (String)

(* ===== Poly-call collector ===== *)

(* A collector that records (func_id, targs) for every CallE to a poly function.
   Accumulator type: (string * Il.typ list) list *)
let make_poly_call_collector (poly_funcs : StringSet.t) :
    (string * Il.typ list) list Collect.collector =
  let collect_exp c exp =
    let sub = Collect.default_collect_exp c exp in
    match exp.it with
    | Il.CallE (id, targs, _) when targs <> [] && StringSet.mem id.it poly_funcs
      ->
        c.compose sub [ (id.it, targs) ]
    | _ -> sub
  in
  { (Collect.make_base ~default:[] ~compose:( @ )) with collect_exp }

(* ===== All-call collector ===== *)

(* Every (callee, targs) pair in a block, regardless of polymorphism —
   used to detect a generic function's own boundary crossings. *)
let make_all_call_collector () : (string * Il.typ list) list Collect.collector
    =
  let collect_exp c exp =
    let sub = Collect.default_collect_exp c exp in
    match exp.it with
    | Il.CallE (id, targs, _) -> c.compose sub [ (id.it, targs) ]
    | _ -> sub
  in
  { (Collect.make_base ~default:[] ~compose:( @ )) with collect_exp }

(* ===== Collecting from SL blocks ===== *)

let collect_from_exp (c : (string * Il.typ list) list Collect.collector)
    (exps : Il.exp list) : (string * Il.typ list) list =
  List.fold_left
    (fun acc exp -> c.compose acc (Collect.collect_exp c exp))
    c.default exps

let rec collect_in_block (c : (string * Il.typ list) list Collect.collector)
    (block : Sl.block) : (string * Il.typ list) list =
  List.fold_left
    (fun acc instr -> c.compose acc (collect_in_instr c instr))
    c.default block

and collect_in_instr (c : (string * Il.typ list) list Collect.collector)
    (instr : Sl.instr) : (string * Il.typ list) list =
  let ( $@ ) = c.compose in
  match instr.it with
  | Sl.IfI (exp, _iterexps, block, _) ->
      collect_from_exp c [ exp ] $@ collect_in_block c block
  | Sl.HoldI (_, notexp, _iterexps, holdcase) ->
      collect_from_exp c (Mixfix.args notexp) $@ collect_in_holdcase c holdcase
  | Sl.CaseI (exp, cases, _) ->
      let cases_acc =
        List.fold_left
          (fun acc (_, block) -> acc $@ collect_in_block c block)
          c.default cases
      in
      collect_from_exp c [ exp ] $@ cases_acc
  | Sl.GroupI (_, _rel_sig, exps, block) ->
      collect_from_exp c exps $@ collect_in_block c block
  | Sl.LetI (lhs_exp, rhs_exp, _iterinstrs, block) ->
      collect_from_exp c [ lhs_exp; rhs_exp ] $@ collect_in_block c block
  | Sl.RuleI (_, notexp, _hints, _iterinstrs, block) ->
      collect_from_exp c (Mixfix.args notexp) $@ collect_in_block c block
  | Sl.ResultI (_, exps) -> collect_from_exp c exps
  | Sl.ReturnI exp -> collect_from_exp c [ exp ]
  | Sl.DebugI exp -> collect_from_exp c [ exp ]

and collect_in_holdcase (c : (string * Il.typ list) list Collect.collector)
    (holdcase : Sl.holdcase) : (string * Il.typ list) list =
  match holdcase with
  | Sl.BothH (block_hold, block_nhold) ->
      c.compose (collect_in_block c block_hold) (collect_in_block c block_nhold)
  | Sl.HoldH (block, _) -> collect_in_block c block
  | Sl.NotHoldH (block, _) -> collect_in_block c block

(* ===== Collecting from full SL spec ===== *)

let collect_from_def (c : (string * Il.typ list) list Collect.collector)
    (def : Sl.def) : (string * Il.typ list) list =
  match def.it with
  | Sl.FuncDecD (_, tparams, _, _, block, elseblock, _) ->
      (* Skip polymorphic function bodies — their call sites are discovered
         transitively when the function is specialized during worklist processing.
         Scanning them here would seed the worklist with type-variable targs. *)
      if tparams <> [] then c.default
      else
        let acc_main = collect_in_block c block in
        let acc_else =
          match elseblock with
          | None -> c.default
          | Some elsebl -> collect_in_block c elsebl
        in
        c.compose acc_main acc_else
  | Sl.RelD (_, _, exps, block, elseblock, _) ->
      let acc_exps = collect_from_exp c exps in
      let acc_main = collect_in_block c block in
      let acc_else =
        match elseblock with
        | None -> c.default
        | Some elsebl -> collect_in_block c elsebl
      in
      c.compose acc_exps (c.compose acc_main acc_else)
  | _ -> c.default

(* ===== Entry point ===== *)

(* Collect all (func_id, targs) call sites in the spec where func_id ∈ poly_funcs.
   Returns a deduplicated list of (func_id, concrete_targs) pairs. *)
let collect_call_sites ~(poly_funcs : StringSet.t) (spec : Sl.spec) :
    (string * Il.typ list) list =
  let collector = make_poly_call_collector poly_funcs in
  let raw =
    List.fold_left
      (fun acc def -> collector.compose acc (collect_from_def collector def))
      collector.default spec
  in
  (* Deduplicate by (func_id, targs) structural equality *)
  List.sort_uniq
    (fun (id_a, targs_a) (id_b, targs_b) ->
      let cmp_id = String.compare id_a id_b in
      if cmp_id <> 0 then cmp_id else compare targs_a targs_b)
    raw

(* All (callee, targs) call sites in a block; feeds [Gen.Func]'s boundary
   check. *)
let collect_all_calls_in_block (block : Sl.block) : (string * Il.typ list) list
    =
  let c = make_all_call_collector () in
  collect_in_block c block
