open Domain
open Lib
open Lang
open Pl
open Util.Source

(* Helper for making a new instruction *)

let mk_instr (instr_src : 'instr_tier instr) (instr_it : 'instr_tier instr') :
    'instr_tier instr =
  {
    node = instr_it $$ (instr_src.node.at, instr_src.node.note);
    hints = instr_src.hints;
  }

(* Single-instruction shorthands *)

let rec eq_exp_var (exp_a : exp) (exp_b : exp) : bool =
  match (exp_a.node.it, exp_b.node.it) with
  | VarE id_a, VarE id_b -> Id.eq id_a id_b
  | IterE (exp_a, _), IterE (exp_b, _) -> eq_exp_var exp_a exp_b
  | _ -> false

let strip_leading_rename (exp_scrut : exp) (block : 'instr_tier block) :
    (exp * 'instr_tier block) option =
  let is_scrut_alias (exp_r : exp) : bool =
    match exp_r.node.it with
    | DownCastE (_, exp_scrut_inner) -> eq_exp_var exp_scrut exp_scrut_inner
    | _ -> eq_exp_var exp_scrut exp_r
  in
  match block with
  | { node = { it = LetI (exp_target, exp_r, []); _ }; _ } :: block_rest
    when is_scrut_alias exp_r ->
      Some (exp_target, block_rest)
  | _ -> None

(* Shorthand for an arm of a multi-arm CaseI *)

let shorten_case_let_guard (exp_scrut : exp) ((guard, block) : 'instr_tier case)
    : 'instr_tier case =
  match (strip_leading_rename exp_scrut block, guard) with
  | Some (exp_target, block_rest), SubG (typ, subcheck) ->
      (CheckLetSubG (typ, subcheck, exp_target), block_rest)
  | Some (exp_target, block_rest), MatchG pattern ->
      (CheckLetMatchG (pattern, exp_target), block_rest)
  | _ -> (guard, block)

let shorten_check_let_guard (instr : 'instr_tier instr) :
    'instr_tier instr option =
  match instr.node.it with
  | CaseI (exp, cases, dangle) ->
      let cases = List.map (shorten_case_let_guard exp) cases in
      Some (mk_instr instr (CaseI (exp, cases, dangle)))
  | _ -> None

(* Shortens the following:

   - If exp_scrut matches pattern:
     - Let exp_target = exp_scrut
     - block_rest

   Or,

   - If exp_scrut <: typ:
     - Let exp_target = exp_scrut [as typ]
     - block_rest

   Into:

   - Check let exp_target be exp_scrut:
     - block_rest *)

let shorten_check_let (instr : 'instr_tier instr) : 'instr_tier instr option =
  let try_lift (mk : exp -> exp -> 'instr_tier block -> 'instr_tier instr')
      (exp_scrut : exp) (block_rest : 'instr_tier block) :
      'instr_tier instr option =
    strip_leading_rename exp_scrut block_rest
    |> Option.map (fun (exp_target, block_rest) ->
           mk_instr instr (mk exp_target exp_scrut block_rest))
  in
  let mk_sub typ subcheck exp_target exp_scrut rest =
    CheckLetSubI (typ, subcheck, exp_target, exp_scrut, rest)
  in
  let mk_match patt exp_target exp_scrut rest =
    CheckLetMatchI (patt, exp_target, exp_scrut, rest)
  in
  match instr.node.it with
  | IfI (exp_cond, [], block, _) -> (
      match exp_cond.node.it with
      | SubE (exp_scrut, typ, subcheck) ->
          try_lift (mk_sub typ subcheck) exp_scrut block
      | MatchE (exp_scrut, pattern) ->
          try_lift (mk_match pattern) exp_scrut block
      | _ -> None)
  | CaseI (exp_scrut, [ (SubG (typ, subcheck), block) ], _dangle) ->
      try_lift (mk_sub typ subcheck) exp_scrut block
  | CaseI (exp_scrut, [ (MatchG pattern, block) ], _dangle) ->
      try_lift (mk_match pattern) exp_scrut block
  | _ -> None

(* Shortens the following, when prose_fields is set on the LetI's hintbag:

   - Let (CaseE pattern) = exp_r

   Into:

   - Destruct exp_r into the named fields of the CaseE pattern *)

let shorten_destruct (instr : 'instr_tier instr) : 'instr_tier instr option =
  let rec is_visible (exp : exp) : bool =
    match exp.node.it with
    | VarE id when Id.is_underscored id -> false
    | IterE (exp, _) -> is_visible exp
    | _ -> true
  in
  match instr.node.it with
  | LetI (exp_l, exp_r, []) -> (
      match (exp_l.node.it, instr.hints.prose_fields) with
      | CaseE notexp_l, Some fields
        when List.length (Mixfix.args notexp_l) = List.length fields ->
          let exps_l = Mixfix.args notexp_l in
          let destruct_fields =
            List.combine exps_l fields
            |> List.map (fun ((exp, name) : exp * string) ->
                   if is_visible exp then (Some name, exp) else (None, exp))
          in
          if List.for_all (fun (name, _) -> name = None) destruct_fields then
            None
          else Some (mk_instr instr (DestructI (destruct_fields, exp_r)))
      | _ -> None)
  | _ -> None

let shorten_instr (instr : 'instr_tier instr) : 'instr_tier instr list =
  let shorteners =
    [ shorten_check_let_guard; shorten_check_let; shorten_destruct ]
  in
  let instr =
    List.fold_left
      (fun instr shortener ->
        match shortener instr with
        | Some instr_short -> instr_short
        | None -> instr)
      instr shorteners
  in
  [ instr ]

(* Multi-instruction shorthands (shared control-flow, both tiers) *)

(* Shortens the following sequence of instructions:

   - Let exp_tmp = exp
   - If exp_tmp matches Some:
     - Let ?(exp_target_inner) = tmp (instr_then_h)
     - (block_then_rest) ...

   Into:

   - Let exp_target_inner = ! exp
   - block_then_rest ... *)

let shorten_option_get (instrs : 'instr_tier instr list) :
    ('instr_tier instr list * 'instr_tier instr list) option =
  match instrs with
  | instr_h :: instr_t :: instrs_rest -> (
      match (instr_h.node.it, instr_t.node.it) with
      | LetI (exp_tmp, exp, []), IfI (exp_cond, [], block_then, _) -> (
          match (exp_cond.node.it, block_then) with
          | MatchE (exp_scrut, Il.OptP `Some), instr_then_h :: block_then_rest
            when eq_exp_var exp_tmp exp_scrut -> (
              match instr_then_h.node.it with
              | LetI (exp_target, exp_r, []) when eq_exp_var exp_tmp exp_r -> (
                  match exp_target.node.it with
                  | OptE (Some exp_target_inner) ->
                      Some
                        ( [
                            mk_instr instr_h
                              (OptionGetI
                                 (exp_target_inner, exp, block_then_rest));
                          ],
                          instrs_rest )
                  | _ -> None)
              | _ -> None)
          | _ -> None)
      | _ -> None)
  | _ -> None

(* Recursive traversal (shared control-flow, both tiers) *)

let rec shorten_single (instr : 'instr_tier instr) : 'instr_tier instr list =
  shorten_instr instr

and shorten_multi (instrs : 'instr_tier instr list) : 'instr_tier instr list =
  match instrs with
  | [] -> []
  | instr_h :: instrs_t -> (
      match shorten_option_get instrs with
      | Some (instr_short, instrs_rest) ->
          instr_short @ shorten_multi instrs_rest
      | None -> instr_h :: shorten_multi instrs_t)

(* Shared walker *)

let shorten_recurse_shared
    (shorten_block : 'instr_tier block -> 'instr_tier block)
    (shorten_instr_tier : 'instr_tier -> 'instr_tier)
    (instr : 'instr_tier instr) : 'instr_tier instr =
  let at, note = (instr.node.at, instr.node.note) in
  match instr.node.it with
  | IfI (cond, iterexps, block_then, dangle) ->
      let block_then = shorten_block block_then in
      let node = IfI (cond, iterexps, block_then, dangle) $$ (at, note) in
      { instr with node }
  | HoldI (id, notexp, iterexps, holdcase) ->
      let holdcase =
        match holdcase with
        | BothH (block_hold, block_nothold) ->
            let block_hold = shorten_block block_hold in
            let block_nothold = shorten_block block_nothold in
            BothH (block_hold, block_nothold)
        | HoldH (block_hold, dangle) ->
            let block_hold = shorten_block block_hold in
            HoldH (block_hold, dangle)
        | NotHoldH (block_nothold, dangle) ->
            let block_nothold = shorten_block block_nothold in
            NotHoldH (block_nothold, dangle)
      in
      let node = HoldI (id, notexp, iterexps, holdcase) $$ (at, note) in
      { instr with node }
  | CaseI (exp, cases, dangle) ->
      let cases =
        cases
        |> List.map (fun (guard, block) ->
               let block = shorten_block block in
               (guard, block))
      in
      let node = CaseI (exp, cases, dangle) $$ (at, note) in
      { instr with node }
  | TierI instr_tier ->
      let node = TierI (shorten_instr_tier instr_tier) $$ (at, note) in
      { instr with node }
  | LetI _ | DebugI _ | DestructI _ -> instr
  | CheckLetSubI (typ, subcheck, exp_l, exp_r, block_then) ->
      let block_then = shorten_block block_then in
      let node =
        CheckLetSubI (typ, subcheck, exp_l, exp_r, block_then) $$ (at, note)
      in
      { instr with node }
  | CheckLetMatchI (pattern, exp_l, exp_r, block_then) ->
      let block_then = shorten_block block_then in
      let node =
        CheckLetMatchI (pattern, exp_l, exp_r, block_then) $$ (at, note)
      in
      { instr with node }
  | OptionGetI (exp_l, exp_r, block_then) ->
      let block_then = shorten_block block_then in
      let node = OptionGetI (exp_l, exp_r, block_then) $$ (at, note) in
      { instr with node }

(* Tiered traversal *)

let rec shorten_instr_dispatch (instr : instr_dispatch instr) :
    instr_dispatch instr =
  shorten_recurse_shared shorten_block_dispatch instr_dispatch_of instr

and shorten_instr_group (instr : instr_group instr) : instr_group instr =
  shorten_recurse_shared shorten_block_group instr_group_of instr

and shorten_block_dispatch (block : block_dispatch) : block_dispatch =
  let block = shorten_multi block in
  let block = List.concat_map shorten_single block in
  List.map shorten_instr_dispatch block

and shorten_block_group (block : block_group) : block_group =
  let block = shorten_multi block in
  let block = List.concat_map shorten_single block in
  List.map shorten_instr_group block

and instr_dispatch_of (instr_dispatch : instr_dispatch) : instr_dispatch =
  match instr_dispatch with
  | GroupI (id_rulegroup, id_rel, rel_signature, exps, block) ->
      GroupI
        (id_rulegroup, id_rel, rel_signature, exps, shorten_block_group block)
  | RouteI arms -> RouteI (List.map shorten_block_dispatch arms)

and instr_group_of (instr_group : instr_group) : instr_group =
  match instr_group with
  | ResultI _ | ReturnI _ | RuleI _ -> instr_group
  | BacktrackI arms -> BacktrackI (List.map shorten_block_group arms)

(* Entry point *)

let shorten_def (def : def) : def =
  let at, note = (def.node.at, def.node.note) in
  match def.node.it with
  | RelD (id, rel_signature, exps, block, elseblock_opt) ->
      let block = shorten_block_dispatch block in
      let elseblock_opt = Option.map shorten_block_dispatch elseblock_opt in
      let node =
        RelD (id, rel_signature, exps, block, elseblock_opt) $$ (at, note)
      in
      { def with node }
  | TableDecD (id, params, typ, tablerows) ->
      let tablerows =
        List.map
          (fun (exps, exp, block) ->
            let block = shorten_block_group block in
            (exps, exp, block))
          tablerows
      in
      let node = TableDecD (id, params, typ, tablerows) $$ (at, note) in
      { def with node }
  | FuncDecD (id, tparams, params, typ, block, elseblock_opt) ->
      let block = shorten_block_group block in
      let elseblock_opt = Option.map shorten_block_group elseblock_opt in
      let node =
        FuncDecD (id, tparams, params, typ, block, elseblock_opt) $$ (at, note)
      in
      { def with node }
  | _ -> def

let shorten_defs (defs : def list) : def list = List.map shorten_def defs
