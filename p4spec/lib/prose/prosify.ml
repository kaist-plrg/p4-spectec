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

let prosify_iterated_let (exps_out : exp list) iterexps (instr : Pl.Ast.instr) =
  if List.is_empty iterexps then instr
  else
    let out_vars, in_vars = split_iters exps_out iterexps in
    Pl.Ast.ForEachI (out_vars, instr, in_vars) $ no_region

let prosify_iterated_cond ?(neg = false) iterexps (cond : Pl.Ast.cond) =
  if List.is_empty iterexps then cond
  else
    let out_vars, in_vars = split_iters [] iterexps in
    assert (List.is_empty out_vars);
    if neg then Pl.Ast.ForAnyCond (cond, in_vars)
    else Pl.Ast.ForAllCond (cond, in_vars)

let rec prosify_exp (ctx : Ctx.t) (exp : exp) : Pl.Ast.exp =
  let exp' =
    match exp.it with
    | BoolE b -> Pl.Ast.BoolE b
    | NumE n -> Pl.Ast.NumE n
    | TextE s -> Pl.Ast.TextE s
    | VarE id -> Pl.Ast.VarE id
    | UnE (unop, optyp, exp) ->
        let exp = prosify_exp ctx exp in
        Pl.Ast.UnE (unop, optyp, exp)
    | BinE (binop, optyp, exp_l, exp_r) ->
        let exp_l = prosify_exp ctx exp_l in
        let exp_r = prosify_exp ctx exp_r in
        Pl.Ast.BinE (binop, optyp, exp_l, exp_r)
    | CmpE (cmpop, optyp, exp_l, exp_r) ->
        let exp_l = prosify_exp ctx exp_l in
        let exp_r = prosify_exp ctx exp_r in
        Pl.Ast.CmpE (cmpop, optyp, exp_l, exp_r)
    | UpCastE (typ, exp) ->
        let exp = prosify_exp ctx exp in
        Pl.Ast.UpCastE (typ, exp)
    | DownCastE (typ, exp) ->
        let exp = prosify_exp ctx exp in
        Pl.Ast.DownCastE (typ, exp)
    | SubE (exp, typ) ->
        let exp = prosify_exp ctx exp in
        Pl.Ast.SubE (exp, typ)
    | MatchE (exp, pattern) ->
        let exp = prosify_exp ctx exp in
        Pl.Ast.MatchE (exp, pattern)
    | TupleE exps ->
        let exps = List.map (prosify_exp ctx) exps in
        Pl.Ast.TupleE exps
    | CaseE (mixop, exps) ->
        let exps = List.map (prosify_exp ctx) exps in
        Pl.Ast.CaseE (mixop, exps)
    | StrE expfields ->
        let atoms, exps = List.split expfields in
        let exps = List.map (prosify_exp ctx) exps in
        let expfields = List.combine atoms exps in
        Pl.Ast.StrE expfields
    | OptE (Some exp) ->
        let exp = prosify_exp ctx exp in
        Pl.Ast.OptE (Some exp)
    | OptE None -> Pl.Ast.OptE None
    | ListE exps ->
        let exps = List.map (prosify_exp ctx) exps in
        Pl.Ast.ListE exps
    | ConsE (exp_h, exp_t) ->
        let exp_h = prosify_exp ctx exp_h in
        let exp_t = prosify_exp ctx exp_t in
        Pl.Ast.ConsE (exp_h, exp_t)
    | CatE (exp_l, exp_r) ->
        let exp_l = prosify_exp ctx exp_l in
        let exp_r = prosify_exp ctx exp_r in
        Pl.Ast.CatE (exp_l, exp_r)
    | MemE (exp_e, exp_s) ->
        let exp_e = prosify_exp ctx exp_e in
        let exp_s = prosify_exp ctx exp_s in
        Pl.Ast.MemE (exp_e, exp_s)
    | LenE exp ->
        let exp = prosify_exp ctx exp in
        Pl.Ast.LenE exp
    | DotE (exp, atom) ->
        let exp = prosify_exp ctx exp in
        Pl.Ast.DotE (exp, atom)
    | IdxE (exp_b, exp_i) ->
        let exp_b = prosify_exp ctx exp_b in
        let exp_i = prosify_exp ctx exp_i in
        Pl.Ast.IdxE (exp_b, exp_i)
    | SliceE (exp_b, exp_l, exp_h) ->
        let exp_b = prosify_exp ctx exp_b in
        let exp_l = prosify_exp ctx exp_l in
        let exp_h = prosify_exp ctx exp_h in
        Pl.Ast.SliceE (exp_b, exp_l, exp_h)
    | UpdE (exp_b, path, exp_f) ->
        let exp_b = prosify_exp ctx exp_b in
        let path = prosify_path ctx path in
        let exp_f = prosify_exp ctx exp_f in
        Pl.Ast.UpdE (exp_b, path, exp_f)
    | CallE (id, targs, args) ->
        let funcprose =
          match exp.note with
          (* conditional functions have prose_true and optionally false *)
          | BoolT -> (
              match HEnv.get_func id ctx.penv.prose_true with
              | Some prose_true ->
                  let prose_false_opt = HEnv.get_func id ctx.penv.prose_false in
                  Pl.Ast.BoolProse (id, prose_true, prose_false_opt)
              | None -> Pl.Ast.Def id)
          (* Non-boolean functions have prose_in *)
          | _ -> (
              match HEnv.get_func id ctx.penv.prose_in with
              | Some prose_in -> Pl.Ast.InputProse (id, prose_in)
              | None -> Pl.Ast.Def id)
        in
        let args = prosify_args ctx args in
        Pl.Ast.CallE (funcprose, targs, args)
    | IterE (exp, iterexp) ->
        let exp = prosify_exp ctx exp in
        Pl.Ast.IterE (exp, iterexp)
  in
  exp' $$ (exp.at, exp.note)

and prosify_exps (ctx : Ctx.t) (exps : exp list) : Pl.Ast.exp list =
  List.map (prosify_exp ctx) exps

and prosify_path (ctx : Ctx.t) (path : path) : Pl.Ast.path =
  let path' =
    match path.it with
    | RootP -> Pl.Ast.RootP
    | IdxP (path, exp) ->
        let path = prosify_path ctx path in
        let exp = prosify_exp ctx exp in
        Pl.Ast.IdxP (path, exp)
    | SliceP (path, exp_l, exp_h) ->
        let path = prosify_path ctx path in
        let exp_l = prosify_exp ctx exp_l in
        let exp_h = prosify_exp ctx exp_h in
        Pl.Ast.SliceP (path, exp_l, exp_h)
    | DotP (path, atom) ->
        let path = prosify_path ctx path in
        Pl.Ast.DotP (path, atom)
  in
  path' $$ (path.at, path.note)

and prosify_arg (ctx : Ctx.t) (arg : arg) : Pl.Ast.arg =
  match arg.it with
  | ExpA exp ->
      let exp = prosify_exp ctx exp in
      Pl.Ast.ExpA exp $ arg.at
  | DefA id -> Pl.Ast.DefA id $ arg.at

and prosify_args ctx args = List.map (prosify_arg ctx) args

let prosify_guard ctx (exp_case : exp) guard : Pl.Ast.cond =
  let exp_case = prosify_exp ctx exp_case in
  let exp_of_guard : Pl.Ast.exp' =
    match guard with
    | BoolG b -> if b then exp_case.it else Pl.Ast.UnE (`NotOp, `BoolT, exp_case)
    | CmpG (cmpop, optyp, exp_r) ->
        let exp_r = prosify_exp ctx exp_r in
        Pl.Ast.CmpE (cmpop, optyp, exp_case, exp_r)
    | SubG typ -> Pl.Ast.SubE (exp_case, typ)
    | MatchG pattern -> Pl.Ast.MatchE (exp_case, pattern)
    | MemG exp ->
        let exp = prosify_exp ctx exp in
        Pl.Ast.MemE (exp_case, exp)
  in
  Pl.Ast.ExpCond (exp_of_guard $$ (exp_case.at, exp_case.note))

let rec prosify_case ctx exp (guard, instrs) : Pl.Ast.instr list =
  let instrs_pl = prosify_instrs ctx instrs in
  let cond = prosify_guard ctx exp guard in
  match ctx.cond_style with
  | Some Check -> [ Pl.Ast.CheckI cond $ no_region ] @ instrs_pl
  | Some If -> [ Pl.Ast.BranchI (Pl.Ast.If, cond, instrs_pl) $ no_region ]
  | Some ElseIf ->
      [ Pl.Ast.BranchI (Pl.Ast.ElseIf, cond, instrs_pl) $ no_region ]
  | Some Else -> [ Pl.Ast.BranchI (Pl.Ast.Else, cond, instrs_pl) $ no_region ]
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
          let exp = prosify_exp ctx exp in
          let cond = Pl.Ast.ExpCond exp |> prosify_iterated_cond iterexps in
          let instr_check = Pl.Ast.CheckI cond $ instr.at in
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
          let exp = prosify_exp ctx exp in
          let cond = Pl.Ast.ExpCond exp |> prosify_iterated_cond iterexps in
          [ Pl.Ast.BranchI (branchtype, cond, instrs_pl) $ instr.at ]
      | _ -> assert false)
  | HoldI (id, (mixop, exps), iterexps, holdcase) -> (
      let exps = prosify_exps ctx exps in
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
            Pl.Ast.BranchI (Pl.Ast.If, cond_if, instrs_hold_sl) $ instr.at
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
            Pl.Ast.BranchI (Pl.Ast.Else, cond_else, instrs_nothold_sl)
            $ instr.at
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
            Pl.Ast.BranchI (Pl.Ast.If, cond, instrs_hold_sl) $ instr.at
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
            Pl.Ast.BranchI (Pl.Ast.If, cond, instrs_nothold_sl) $ instr.at
          in
          [ instr ])
  | CaseI (exp, cases, Some _) -> prosify_cases ctx ~closed:false exp cases
  | CaseI (exp, cases, None) -> prosify_cases ctx ~closed:true exp cases
  | OtherwiseI instr ->
      let instrs = prosify_instr ctx instr in
      List.map (fun instr -> Pl.Ast.OtherwiseI instr $ instr.at) instrs
  | GroupI (id, exps, instrs) ->
      (* TODO *)
      let instrs = prosify_instrs ctx instrs in
      let exps = prosify_exps ctx exps in
      [ Pl.Ast.GroupI (id, exps, instrs) $ instr.at ]
  | LetI (exp_l, exp_r, iterexps) ->
      let exp_l_pl = prosify_exp ctx exp_l in
      let exp_r = prosify_exp ctx exp_r in
      [
        Pl.Ast.LetI (exp_l_pl, exp_r)
        $ instr.at
        |> prosify_iterated_let [ exp_l ] iterexps;
      ]
  | RuleI (id, (mixop, exps), iterexps) ->
      let hint_opt = HEnv.get_rel id ctx.penv.prose_in in
      let inputs = IEnv.find_opt id ctx.ienv |> Option.value ~default:[] in
      let exps_in, exps_out = InputHint.split_exps_without_idx inputs exps in
      let exps_in = prosify_exps ctx exps_in in
      let exps_out_pl = prosify_exps ctx exps_out in
      let exps = prosify_exps ctx exps in
      let relation =
        match hint_opt with
        | Some hintexp -> Pl.Ast.Prose (hintexp, exps_out_pl, exps_in)
        | None -> Pl.Ast.Mixop (mixop, exps)
      in
      [
        Pl.Ast.RelI (relation, id)
        $ instr.at
        |> prosify_iterated_let exps_out iterexps;
      ]
  | ResultI exps ->
      let rid = get_rel_id ctx in
      let hint_opt = HEnv.get_rel rid ctx.penv.prose_out in
      let inputs = IEnv.find_opt rid ctx.ienv |> Option.value ~default:[] in
      let hint_opt = Option.map (align_hint inputs) hint_opt in
      let exps = prosify_exps ctx exps in
      [ Pl.Ast.ResultI (hint_opt, exps) $ instr.at ]
  | ReturnI exp ->
      let exp = prosify_exp ctx exp in
      [ Pl.Ast.ReturnI exp $ instr.at ]
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
      let exps = prosify_exps ctx exps in
      Some (Pl.Ast.RelD (id, exps, instrs) $ def.at)
  | DecD _ -> None
(* let instrs = prosify_instrs ctx instrs in *)
(* Some (Pl.Ast.DecD (id, tparams, args, instrs) $ def.at) *)

let prosify_spec (spec : spec) : Pl.Ast.spec =
  let ctx = Ctx.init spec in
  List.filter_map (prosify_def ctx) spec

(* Splicer entrypoints *)

let prosify_rulegroup (ctx : Ctx.t) (id_rel : id) (mixop : mixop)
    (inputs : int list) (exps_in : exp list) (instrs : instr list) =
  assert (List.length inputs = List.length exps_in);
  let ctx = ctx |> in_rel id_rel in
  let relcall =
    let prose_in_opt = HEnv.get_rel id_rel ctx.penv.prose_in in
    match prose_in_opt with
    | Some hintexp ->
        let exps_in = prosify_exps ctx exps_in in
        Pl.Ast.Prose (hintexp, [], exps_in)
    | None -> Pl.Ast.Mixop (mixop, prosify_exps ctx exps_in)
  in
  let instrs = prosify_instrs ctx instrs in
  (relcall, id_rel, instrs)

let prosify_func (ctx : Ctx.t) (id_def : id) (tparams : tparam list)
    (args_input : arg list) (typ : typ) (instrs : instr list) =
  let funcprose =
    match typ.it with
    | BoolT -> (
        match HEnv.get_func id_def ctx.penv.prose_true with
        | Some prose_true ->
            let prose_false_opt = HEnv.get_func id_def ctx.penv.prose_false in
            Pl.Ast.BoolProse (id_def, prose_true, prose_false_opt)
        | None -> Pl.Ast.Def id_def)
    | _ -> (
        match HEnv.get_func id_def ctx.penv.prose_in with
        | Some prose_in -> Pl.Ast.InputProse (id_def, prose_in)
        | None -> Pl.Ast.Def id_def)
  in
  let args_input = prosify_args ctx args_input in
  let instrs = prosify_instrs ctx instrs in
  (funcprose, tparams, args_input, instrs)
