open Lang
open Util.Source
module Mixfix = Domain.Mixfix

(* Type substitution delegates to Runtime.Type.Subst *)

type theta = Runtime.Type.Subst.theta

let subst_typ = Runtime.Type.Subst.subst_typ
let subst_nottyp = Runtime.Type.Subst.subst_nottyp

(* ===== Expressions ===== *)

let rec subst_exp (theta : theta) (exp : Il.exp) : Il.exp =
  let note' =
    (subst_typ theta { it = exp.note; at = no_region; note = () }).it
  in
  { exp with it = subst_exp' theta exp.it; note = note' }

and subst_exp' (theta : theta) (exp' : Il.exp') : Il.exp' =
  match exp' with
  | Il.BoolE _ -> exp'
  | Il.NumE _ -> exp'
  | Il.TextE _ -> exp'
  | Il.VarE _ -> exp'
  | Il.UnE (op, ot, exp) -> Il.UnE (op, ot, subst_exp theta exp)
  | Il.BinE (op, ot, exp_l, exp_r) ->
      Il.BinE (op, ot, subst_exp theta exp_l, subst_exp theta exp_r)
  | Il.CmpE (op, ot, exp_l, exp_r) ->
      Il.CmpE (op, ot, subst_exp theta exp_l, subst_exp theta exp_r)
  | Il.UpCastE (typ, exp) ->
      Il.UpCastE (subst_typ theta typ, subst_exp theta exp)
  | Il.DownCastE (typ, exp) ->
      Il.DownCastE (subst_typ theta typ, subst_exp theta exp)
  | Il.SubE (exp, typ) -> Il.SubE (subst_exp theta exp, subst_typ theta typ)
  | Il.MatchE (exp, pat) -> Il.MatchE (subst_exp theta exp, pat)
  | Il.TupleE exps -> Il.TupleE (List.map (subst_exp theta) exps)
  | Il.CaseE notexp -> Il.CaseE (Mixfix.map (subst_exp theta) notexp)
  | Il.StrE expfields ->
      Il.StrE
        (List.map (fun (atom, exp) -> (atom, subst_exp theta exp)) expfields)
  | Il.OptE (Some exp) -> Il.OptE (Some (subst_exp theta exp))
  | Il.OptE None -> Il.OptE None
  | Il.ListE exps -> Il.ListE (List.map (subst_exp theta) exps)
  | Il.ConsE (exp_h, exp_t) ->
      Il.ConsE (subst_exp theta exp_h, subst_exp theta exp_t)
  | Il.CatE (exp_l, exp_r) ->
      Il.CatE (subst_exp theta exp_l, subst_exp theta exp_r)
  | Il.MemE (exp_e, exp_s) ->
      Il.MemE (subst_exp theta exp_e, subst_exp theta exp_s)
  | Il.LenE exp -> Il.LenE (subst_exp theta exp)
  | Il.DotE (exp, atom) -> Il.DotE (subst_exp theta exp, atom)
  | Il.IdxE (exp_l, exp_r) ->
      Il.IdxE (subst_exp theta exp_l, subst_exp theta exp_r)
  | Il.SliceE (exp_l, exp_m, exp_r) ->
      Il.SliceE
        (subst_exp theta exp_l, subst_exp theta exp_m, subst_exp theta exp_r)
  | Il.UpdE (exp_b, path, exp_u) ->
      Il.UpdE
        (subst_exp theta exp_b, subst_path theta path, subst_exp theta exp_u)
  | Il.CallE (id, targs, args) ->
      Il.CallE
        (id, List.map (subst_typ theta) targs, List.map (subst_arg theta) args)
  | Il.IterE (exp, iterexp) -> Il.IterE (subst_exp theta exp, iterexp)

and subst_path (theta : theta) (path : Il.path) : Il.path =
  let note' =
    (subst_typ theta { it = path.note; at = no_region; note = () }).it
  in
  { path with it = subst_path' theta path.it; note = note' }

and subst_path' (theta : theta) (path' : Il.path') : Il.path' =
  match path' with
  | Il.RootP -> Il.RootP
  | Il.IdxP (path, exp) -> Il.IdxP (subst_path theta path, subst_exp theta exp)
  | Il.SliceP (path, exp_l, exp_r) ->
      Il.SliceP
        (subst_path theta path, subst_exp theta exp_l, subst_exp theta exp_r)
  | Il.DotP (path, atom) -> Il.DotP (subst_path theta path, atom)

and subst_arg (theta : theta) (arg : Il.arg) : Il.arg =
  match arg.it with
  | Il.ExpA exp -> Il.ExpA (subst_exp theta exp) $ arg.at
  | Il.DefA _ -> arg

(* ===== Notexp ===== *)

(* notexp = exp Mixfix.t (NOT a phrase) *)
let subst_notexp (theta : theta) (notexp : Sl.notexp) : Sl.notexp =
  Mixfix.map (subst_exp theta) notexp

(* ===== Guards ===== *)

let subst_guard (theta : theta) (guard : Sl.guard) : Sl.guard =
  match guard with
  | Sl.BoolG _ -> guard
  | Sl.CmpG (op, ot, exp) -> Sl.CmpG (op, ot, subst_exp theta exp)
  | Sl.SubG typ -> Sl.SubG (subst_typ theta typ)
  | Sl.MatchG _ -> guard
  | Sl.MemG exp -> Sl.MemG (subst_exp theta exp)

(* ===== Instructions and Blocks ===== *)

let rec subst_instr (theta : theta) (instr : Sl.instr) : Sl.instr =
  { instr with it = subst_instr' theta instr.it }

and subst_instr' (theta : theta) (instr' : Sl.instr') : Sl.instr' =
  match instr' with
  | Sl.IfI (exp, iterexps, block, dangle) ->
      Sl.IfI (subst_exp theta exp, iterexps, subst_block theta block, dangle)
  | Sl.HoldI (id, notexp, iterexps, holdcase) ->
      Sl.HoldI
        (id, subst_notexp theta notexp, iterexps, subst_holdcase theta holdcase)
  | Sl.CaseI (exp, cases, dangle) ->
      Sl.CaseI (subst_exp theta exp, List.map (subst_case theta) cases, dangle)
  | Sl.GroupI (id, (nottyp, hints_input), exps, block) ->
      Sl.GroupI
        ( id,
          (subst_nottyp theta nottyp, hints_input),
          List.map (subst_exp theta) exps,
          subst_block theta block )
  | Sl.LetI (lhs_exp, rhs_exp, iterinstrs, block) ->
      Sl.LetI
        ( subst_exp theta lhs_exp,
          subst_exp theta rhs_exp,
          iterinstrs,
          subst_block theta block )
  | Sl.RuleI (id, notexp, hints, iterinstrs, block) ->
      Sl.RuleI
        ( id,
          subst_notexp theta notexp,
          hints,
          iterinstrs,
          subst_block theta block )
  | Sl.ResultI (rel_sig, exps) ->
      Sl.ResultI (rel_sig, List.map (subst_exp theta) exps)
  | Sl.ReturnI exp -> Sl.ReturnI (subst_exp theta exp)
  | Sl.DebugI exp -> Sl.DebugI (subst_exp theta exp)

and subst_block (theta : theta) (block : Sl.block) : Sl.block =
  List.map (subst_instr theta) block

and subst_holdcase (theta : theta) (holdcase : Sl.holdcase) : Sl.holdcase =
  match holdcase with
  | Sl.BothH (block_hold, block_nhold) ->
      Sl.BothH (subst_block theta block_hold, subst_block theta block_nhold)
  | Sl.HoldH (block, dangle) -> Sl.HoldH (subst_block theta block, dangle)
  | Sl.NotHoldH (block, dangle) -> Sl.NotHoldH (subst_block theta block, dangle)

and subst_case (theta : theta) ((guard, block) : Sl.case) : Sl.case =
  (subst_guard theta guard, subst_block theta block)

(* ===== SL Parameters ===== *)

let rec subst_sl_param (theta : theta) (param : Sl.param) : Sl.param =
  match param.it with
  | Sl.ExpP (typ, exp) ->
      Sl.ExpP (subst_typ theta typ, subst_exp theta exp) $ param.at
  | Sl.DefP (id, tparams, params, typ) ->
      Sl.DefP
        ( id,
          tparams,
          List.map (subst_sl_param theta) params,
          subst_typ theta typ )
      $ param.at
