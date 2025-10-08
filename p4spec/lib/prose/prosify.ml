(* Converts SL AST to PL AST *)

open Sl.Ast
open Util.Source
open Ctx
module HEnv = Hintenv
module IEnv = Runtime_static.Envs.IEnv
module InputHint = Runtime_static.Rel.InputHint

let ( let* ) = Option.bind

let split_iters (exps_out : exp list) (iterexps : iterexp list) :
    var list * var list =
  let out_ids = Il.Free.free_exps exps_out in
  List.fold_left
    (fun (out_vars_acc, in_vars_acc) (iter, vars) ->
      assert (iter = Il.Ast.List);
      let out_vars, in_vars =
        List.partition (fun (id, _, _) -> Domain.Lib.IdSet.mem id out_ids) vars
      in
      (out_vars_acc @ out_vars, in_vars_acc @ in_vars))
    ([], []) iterexps

(* Re-index hint holes based on input hints *)

let rec align_hint (inputs : InputHint.t) (hintexp : El.Ast.exp) : El.Ast.exp =
  match hintexp.it with
  | El.Ast.HoleE (`Num i) ->
      let offset = List.filter (fun inp -> inp <= i) inputs |> List.length in
      El.Ast.HoleE (`Num (i - offset)) $ hintexp.at
  | El.Ast.SeqE exps ->
      let exps = List.map (align_hint inputs) exps in
      El.Ast.SeqE exps $ hintexp.at
  | El.Ast.FuseE (exp_l, exp_r) ->
      let exp_l = align_hint inputs exp_l in
      let exp_r = align_hint inputs exp_r in
      El.Ast.FuseE (exp_l, exp_r) $ hintexp.at
  | _ -> hintexp

let prosify_iterated_let exps_out iterexps (instr : Pl.Ast.instr) =
  if List.is_empty iterexps then instr
  else
    let out_vars, in_vars = split_iters exps_out iterexps in
    Pl.Ast.ForEach (out_vars, instr, in_vars) $ no_region

let prosify_iterated_cond ?(neg = false) iterexps (cond : Pl.Ast.cond) =
  if List.is_empty iterexps then cond
  else
    let out_vars, in_vars = split_iters [] iterexps in
    assert (List.is_empty out_vars);
    if neg then Pl.Ast.ForAnyCond (cond, in_vars)
    else Pl.Ast.ForAllCond (cond, in_vars)

let prosify_guard ctx exp_case guard : Pl.Ast.cond =
  let exp_of_guard : Pl.Ast.exp =
    match guard with
    | BoolG b ->
        if b then exp_case
        else
          Il.Ast.UnE (`NotOp, `BoolT, exp_case) $$ (exp_case.at, exp_case.note)
    | CmpG (cmpop, optyp, exp_r) ->
        Il.Ast.CmpE (cmpop, optyp, exp_case, exp_r)
        $$ (exp_case.at, exp_case.note)
    | SubG typ -> Il.Ast.SubE (exp_case, typ) $$ (exp_case.at, exp_case.note)
    | MatchG pattern ->
        Il.Ast.MatchE (exp_case, pattern) $$ (exp_case.at, exp_case.note)
    | MemG exp -> Il.Ast.MemE (exp_case, exp) $$ (exp_case.at, exp_case.note)
  in
  Pl.Ast.ExpCond exp_of_guard

let rec prosify_case ctx exp (guard, instrs) : Pl.Ast.instr list =
  let instrs_pl = prosify_instrs ctx instrs in
  let cond = prosify_guard ctx exp guard in
  match ctx.cond_style with
  | Some Check -> [ Pl.Ast.Check cond $ no_region ] @ instrs_pl
  | Some If -> [ Pl.Ast.Branch (Pl.Ast.If, cond, instrs_pl) $ no_region ]
  | Some ElseIf ->
      [ Pl.Ast.Branch (Pl.Ast.ElseIf, cond, instrs_pl) $ no_region ]
  | Some Else -> [ Pl.Ast.Branch (Pl.Ast.Else, cond, instrs_pl) $ no_region ]
  | None -> assert false

and prosify_cases ctx ~closed exp cases : Pl.Ast.instr list =
  let num_cases = List.length cases in
  if num_cases = 0 then failwith "no cases"
  else if num_cases = 1 then
    prosify_case (ctx |> as_cond Check) exp (List.hd cases)
  else
    List.mapi
      (fun i case ->
        if i = 0 then prosify_case (ctx |> as_cond If) exp case
        else if closed && i = num_cases - 1 then
          prosify_case (ctx |> as_cond Else) exp case
        else prosify_case (ctx |> as_cond ElseIf) exp case)
      cases
    |> List.concat

and prosify_instr ctx instr : Pl.Ast.instr list =
  match instr.it with
  | IfI (exp, iterexps, instrs, phantom) -> (
      match ctx.cond_style with
      | Some Check ->
          let instrs_pl = prosify_instrs ctx instrs in
          let cond = Pl.Ast.ExpCond exp |> prosify_iterated_cond iterexps in
          let instr_check = Pl.Ast.Check cond $ instr.at in
          [ instr_check ] @ instrs_pl
      | Some cond_style ->
          let branchtype =
            match cond_style with
            | If -> Pl.Ast.If
            | ElseIf -> Pl.Ast.ElseIf
            | Else -> Pl.Ast.Else
            | Check -> assert false
          in
          let instrs_pl = prosify_instrs ctx instrs in
          let cond = Pl.Ast.ExpCond exp |> prosify_iterated_cond iterexps in
          [ Pl.Ast.Branch (branchtype, cond, instrs_pl) $ instr.at ]
      | _ -> assert false)
  | HoldI (id, (mixop, exps), iterexps, holdcase) -> (
      match holdcase with
      | BothH (instrs_hold, instrs_nothold) ->
          (* create if-branch for hold *)
          let instrs_hold_sl = prosify_instrs ctx instrs_hold in
          let relation_true =
            match HEnv.get_rel id ctx.penv.prose_true with
            | Some hintexp -> Pl.Ast.Prose (hintexp, [], exps)
            | None -> Pl.Ast.Mixop (mixop, exps)
          in
          let cond_if =
            Pl.Ast.RelCond (relation_true, id)
            |> prosify_iterated_cond ~neg:false iterexps
          in
          let instr_if =
            Pl.Ast.Branch (Pl.Ast.If, cond_if, instrs_hold_sl) $ instr.at
          in
          (* create else-branch for not-hold *)
          let instrs_nothold_sl = prosify_instrs ctx instrs_nothold in
          let relation_false =
            match HEnv.get_rel id ctx.penv.prose_false with
            | Some hintexp -> Pl.Ast.Prose (hintexp, [], exps)
            | None -> Pl.Ast.Mixop (mixop, exps)
          in
          let cond_else =
            Pl.Ast.RelCond (relation_false, id)
            |> prosify_iterated_cond ~neg:true iterexps
          in
          let instr_else =
            Pl.Ast.Branch (Pl.Ast.Else, cond_else, instrs_nothold_sl) $ instr.at
          in
          [ instr_if; instr_else ]
      | HoldH (instrs_hold, _) ->
          let instrs_hold_sl = prosify_instrs ctx instrs_hold in
          let relation_true =
            match HEnv.get_rel id ctx.penv.prose_true with
            | Some hintexp -> Pl.Ast.Prose (hintexp, [], exps)
            | None -> Pl.Ast.Mixop (mixop, exps)
          in
          let cond =
            Pl.Ast.RelCond (relation_true, id) |> prosify_iterated_cond iterexps
          in
          let instr =
            Pl.Ast.Branch (Pl.Ast.If, cond, instrs_hold_sl) $ instr.at
          in
          [ instr ]
      | NotHoldH (instrs_nothold, _) ->
          let instrs_nothold_sl = prosify_instrs ctx instrs_nothold in
          let relation_false =
            match HEnv.get_rel id ctx.penv.prose_false with
            | Some hintexp -> Pl.Ast.Prose (hintexp, [], exps)
            | None -> Pl.Ast.Mixop (mixop, exps)
          in
          let cond =
            Pl.Ast.RelCond (relation_false, id)
            |> prosify_iterated_cond iterexps
          in
          let instr =
            Pl.Ast.Branch (Pl.Ast.If, cond, instrs_nothold_sl) $ instr.at
          in
          [ instr ])
  | CaseI (exp, cases, Some _) -> prosify_cases ctx ~closed:false exp cases
  | CaseI (exp, cases, None) -> prosify_cases ctx ~closed:true exp cases
  | OtherwiseI instr ->
      let instrs = prosify_instr ctx instr in
      List.map (fun instr -> Pl.Ast.Otherwise instr $ instr.at) instrs
  | GroupI (id, exps, instrs) ->
      (* TODO *)
      let instrs = prosify_instrs ctx instrs in
      [ Pl.Ast.Group (id, exps, instrs) $ instr.at ]
  | LetI (exp_l, exp_r, iterexps) ->
      [
        Pl.Ast.Let (exp_l, exp_r)
        $ instr.at
        |> prosify_iterated_let [ exp_l ] iterexps;
      ]
  | RuleI (id, (mixop, exps), iterexps) ->
      let hint_opt = HEnv.get_rel id ctx.penv.prose_in in
      let inputs = IEnv.find_opt id ctx.ienv |> Option.value ~default:[] in
      let exps_in, exps_out = InputHint.split_exps_without_idx inputs exps in
      let relation =
        match hint_opt with
        | Some hintexp -> Pl.Ast.Prose (hintexp, exps_out, exps_in)
        | None -> Pl.Ast.Mixop (mixop, exps)
      in
      [
        Pl.Ast.Rel (relation, id)
        $ instr.at
        |> prosify_iterated_let exps_out iterexps;
      ]
  | ResultI exps ->
      let rid = get_rel_id ctx in
      let hint_opt = HEnv.get_rel rid ctx.penv.prose_out in
      let inputs = IEnv.find_opt rid ctx.ienv |> Option.value ~default:[] in
      let hint_opt = Option.map (align_hint inputs) hint_opt in
      [ Pl.Ast.Result (hint_opt, exps) $ instr.at ]
  | ReturnI exp -> [ Pl.Ast.Return exp $ instr.at ]
  | DebugI exp -> []

and prosify_instrs ctx (instrs : instr list) : Pl.Ast.instr list =
  let num_if_instrs =
    List.filter
      (fun instr ->
        match instr.it with IfI _ | OtherwiseI _ -> true | _ -> false)
      instrs
    |> List.length
  in
  if num_if_instrs = 1 then
    instrs |> List.concat_map (prosify_instr (ctx |> as_cond Check))
  else
    List.mapi
      (fun i instr ->
        if i = 0 then prosify_instr (ctx |> as_cond If) instr
        else prosify_instr (ctx |> as_cond ElseIf) instr)
      instrs
    |> List.concat

let prosify_def (ctx : Ctx.t) (def : def) : Pl.Ast.def option =
  match def.it with
  | TypD _ -> None
  | RelD (id, _, exps, instrs, _) ->
      let ctx = ctx |> in_rel id in
      let instrs = prosify_instrs ctx instrs in
      Some (Pl.Ast.RelD (id, exps, instrs) $ def.at)
  | DecD _ -> None
(* let instrs = prosify_instrs ctx instrs in *)
(* Some (Pl.Ast.DecD (id, tparams, args, instrs) $ def.at) *)

let prosify_spec (spec : spec) : Pl.Ast.spec =
  let ctx = Ctx.init spec in
  List.filter_map (prosify_def ctx) spec
