open Domain
open Lib
open Lang
open Pl
open Util.Source

(* True iff [a] and [b] denote the same variable, peeling [IterE] wrappers. *)
let rec eq_exp_var (a : exp) (b : exp) : bool =
  match (a.node.it, b.node.it) with
  | VarE id_a, VarE id_b -> id_a.it = id_b.it
  | IterE (a_inner, _), IterE (b_inner, _) -> eq_exp_var a_inner b_inner
  | _ -> false

let mk_instr (src : instr) (it : instr') : instr =
  { node = it $$ (src.node.at, src.node.note); hints = src.hints }

(* Single-instruction shorthands *)

(* Shortens either of the following:

   - If exp_scrut matches pat:
     - Let exp_target = exp_scrut
     - rest...

   Or,

   - If exp_scrut <: typ:
     - Let exp_target = exp_scrut [as typ]
     - rest...

   (Plus the corresponding single-arm CaseI forms.)

   Into:

   - Check let exp_target be exp_scrut:
     - rest... *)

(* True if evaluating [exp_rhs] yields the same value as [exp_scrut], possibly
   through a DownCastE (which only narrows the static type). *)
let is_scrut_alias (exp_scrut : exp) (exp_rhs : exp) : bool =
  match exp_rhs.node.it with
  | DownCastE (_, exp_scrut') -> eq_exp_var exp_scrut exp_scrut'
  | _ -> eq_exp_var exp_scrut exp_rhs

(* If [block] begins with "let target = scrut" or "let target = (T) scrut",
   returns (target, rest); the leading LetI is a no-op rename. *)
let strip_leading_rename (exp_scrut : exp) (block : block) :
    (exp * block) option =
  match block with
  | { node = { it = LetI (exp_target, exp_rhs, []); _ }; _ } :: rest
    when is_scrut_alias exp_scrut exp_rhs ->
      Some (exp_target, rest)
  | _ -> None

let shorten_check_let (instr : instr) : instr option =
  let try_lift (exp_scrut : exp) (block : block) : instr option =
    strip_leading_rename exp_scrut block
    |> Option.map (fun (exp_target, rest) ->
           mk_instr instr (CheckLetI (exp_target, exp_scrut, rest)))
  in
  match instr.node.it with
  | IfI (exp_cond, [], block, _dangle) -> (
      match exp_cond.node.it with
      | SubE (exp_scrut, _) | MatchE (exp_scrut, _) -> try_lift exp_scrut block
      | _ -> None)
  | CaseI (exp_scrut, [ ((SubG _ | MatchG _), block) ], _dangle) ->
      try_lift exp_scrut block
  | _ -> None

(* Shorthand for an arm of a multi-arm CaseI:

   (SubG typ | MatchG p, [LetI(target, scrut [or (T) scrut], []); rest])
     becomes
   (CheckLetSubG (typ, target) | CheckLetMatchG (p, target), rest)

   The single-arm form is handled by shorten_check_let above; this catches
   the same pattern per-arm in multi-arm CaseI, where the shorthand cannot
   collapse the whole instruction. *)
let shorten_case_let_guard (exp_scrut : exp) ((guard, block) : case) : case =
  match (strip_leading_rename exp_scrut block, guard) with
  | Some (exp_target, rest), SubG typ -> (CheckLetSubG (typ, exp_target), rest)
  | Some (exp_target, rest), MatchG patt ->
      (CheckLetMatchG (patt, exp_target), rest)
  | _ -> (guard, block)

(* Shortens the following, when prose_fields is set on the LetI's hintbag:

   - Let (CaseE pattern) = exp_r

   Into:

   - Destruct exp_r into the named fields of the CaseE pattern *)

let shorten_destruct (instr : instr) : instr option =
  match instr.node.it with
  | LetI (exp_l, exp_r, []) -> (
      match (exp_l.node.it, instr.hints.prose_fields) with
      | CaseE notexp_l, Some fields
        when List.length (Mixfix.args notexp_l) = List.length fields ->
          let exps_l = Mixfix.args notexp_l in
          let destruct_fields =
            List.combine exps_l fields
            |> List.map (fun ((e : exp), name) ->
                   let visible =
                     match e.node.it with
                     | VarE id when Id.is_underscored id -> false
                     | IterE ({ node = { it = VarE id; _ }; _ }, _)
                       when Id.is_underscored id ->
                         false
                     | _ -> true
                   in
                   ((if visible then Some name else None), e))
          in
          if List.for_all (fun (n, _) -> n = None) destruct_fields then None
          else Some (mk_instr instr (DestructI (destruct_fields, exp_r)))
      | _ -> None)
  | _ -> None

let shorten_instr (instr : instr) : instr list =
  match shorten_check_let instr with
  | Some instr' -> [ instr' ]
  | None -> (
      match shorten_destruct instr with
      | Some instr' -> [ instr' ]
      | None -> [ instr ])

(* Multi-instruction shorthands *)

(* Shortens the following sequence of instructions:

   - Let tmp = call
   - If tmp matches Some:
     - Let ?(target) = tmp
     - body_rest...

   Into:

   - Let target = ! call
   - body_rest... *)

let shorten_option_get (instrs : instr list) : (instr list * instr list) option
    =
  match instrs with
  | i1 :: i2 :: rest -> (
      match (i1.node.it, i2.node.it) with
      | LetI (exp_tmp, exp_call, []), IfI (exp_cond, [], inner_block, _dangle)
        -> (
          match (exp_cond.node.it, inner_block) with
          | MatchE (exp_scrut, Il.OptP `Some), inner :: body_rest
            when eq_exp_var exp_tmp exp_scrut -> (
              match inner.node.it with
              | LetI (exp_target, exp_tmp', []) when eq_exp_var exp_tmp exp_tmp'
                -> (
                  match exp_target.node.it with
                  | OptE (Some exp_inner) ->
                      Some
                        ( mk_instr i1 (OptionGetI (exp_inner, exp_call))
                          :: body_rest,
                          rest )
                  | _ -> None)
              | _ -> None)
          | _ -> None)
      | _ -> None)
  | _ -> None

(* Recursive traversal *)

let rec shorten_block (block : block) : block =
  let block = shorten_block_seq block in
  let block = List.concat_map shorten_instr_shallow block in
  List.map recurse_into_nested block

and shorten_instr_shallow (instr : instr) : instr list = shorten_instr instr

and recurse_into_nested (instr : instr) : instr =
  let it' =
    match instr.node.it with
    | IfI (cond, iterexps, block_then, dangle) ->
        IfI (cond, iterexps, shorten_block block_then, dangle)
    | HoldI (id, notexp, iterexps, holdcase) ->
        let holdcase' =
          match holdcase with
          | BothH (b1, b2) -> BothH (shorten_block b1, shorten_block b2)
          | HoldH (b, d) -> HoldH (shorten_block b, d)
          | NotHoldH (b, d) -> NotHoldH (shorten_block b, d)
        in
        HoldI (id, notexp, iterexps, holdcase')
    | CaseI (exp, cases, dangle) ->
        let cases' =
          cases
          |> List.map (shorten_case_let_guard exp)
          |> List.map (fun (guard, block) -> (guard, shorten_block block))
        in
        CaseI (exp, cases', dangle)
    | TryI arms -> TryI (List.map shorten_block arms)
    | GroupI (id_rg, id_rel, rsig, exps, block) ->
        GroupI (id_rg, id_rel, rsig, exps, shorten_block block)
    | CheckLetI (e_l, e_r, block_inner) ->
        CheckLetI (e_l, e_r, shorten_block block_inner)
    | LetI _ | RuleI _ | ResultI _ | ReturnI _ | DebugI _ | DestructI _
    | OptionGetI _ ->
        instr.node.it
  in
  { instr with node = it' $$ (instr.node.at, instr.node.note) }

and shorten_block_seq (instrs : instr list) : instr list =
  match instrs with
  | [] -> []
  | _ -> (
      match shorten_option_get instrs with
      | Some (shortened, rest) -> shortened @ shorten_block_seq rest
      | None -> List.hd instrs :: shorten_block_seq (List.tl instrs))

(* Entry points *)

let shorten_def (def : def) : def =
  let it' =
    match def.node.it with
    | RelD (id, rsig, exps, block, elseblock_opt) ->
        RelD
          ( id,
            rsig,
            exps,
            shorten_block block,
            Option.map shorten_block elseblock_opt )
    | FuncDecD (id, tparams, params, typ, block, elseblock_opt) ->
        FuncDecD
          ( id,
            tparams,
            params,
            typ,
            shorten_block block,
            Option.map shorten_block elseblock_opt )
    | TableDecD (id, params, typ, tablerows) ->
        let tablerows' =
          List.map
            (fun (exps, exp, block) -> (exps, exp, shorten_block block))
            tablerows
        in
        TableDecD (id, params, typ, tablerows')
    | ExternRelD _ | ExternDecD _ | BuiltinDecD _ | ExternTypD _ | TypD _
    | VarD _ ->
        def.node.it
  in
  { def with node = it' $$ (def.node.at, def.node.note) }

let shorten_defs (defs : def list) : def list = List.map shorten_def defs
