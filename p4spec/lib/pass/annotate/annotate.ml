open Domain.Lib
module Mixfix = Domain.Mixfix
open Lang
open Ll.Ast
module Annot = Pl.Annot
open Util.Source

(* Hint lookup *)

let hints_of_case_exp (ctx : Ctx.t) (note : Sl.typ') (mixop : mixop) :
    Annot.hints =
  match note with
  | Il.VarT (tid, _) ->
      let cid = (tid, mixop) in
      let prose = Ctx.find_hint_prose ctx (`Typ cid) in
      let prose_fields = Ctx.find_hint_prose_fields ctx (`Typ cid) in
      { Annot.empty with prose; prose_fields }
  | _ -> Annot.empty

let hints_of_call_exp (ctx : Ctx.t) (id : id) : Annot.hints =
  let prose_in = Ctx.find_hint_prose_in ctx (`Func id) in
  let prose_true = Ctx.find_hint_prose_true ctx (`Func id) in
  let prose_false = Ctx.find_hint_prose_false ctx (`Func id) in
  { Annot.empty with prose_in; prose_true; prose_false }

let hints_of_hold_instr (ctx : Ctx.t) (id_rel : id) : Annot.hints =
  let prose_true = Ctx.find_hint_prose_true ctx (`Rel id_rel) in
  let prose_false = Ctx.find_hint_prose_false ctx (`Rel id_rel) in
  { Annot.empty with prose_true; prose_false }

let hints_of_rule_instr (ctx : Ctx.t) (id_rel : id) (inputs : Hints.Input.t) :
    Annot.hints =
  let prose_in = Ctx.find_hint_prose_in ctx (`Rel id_rel) in
  let prose_out =
    Ctx.find_hint_prose_out ctx (`Rel id_rel)
    |> Option.map (fun a -> Hints.Alter.realign a inputs)
  in
  { Annot.empty with prose_in; prose_out }

let hints_of_result_instr (ctx : Ctx.t) (inputs : Hints.Input.t) : Annot.hints =
  let id_rel = Ctx.get_namespace ctx in
  let prose_out =
    Ctx.find_hint_prose_out ctx (`Rel id_rel)
    |> Option.map (fun a -> Hints.Alter.realign a inputs)
  in
  { Annot.empty with prose_out }

let hints_of_group_instr (ctx : Ctx.t) : Annot.hints =
  let id_rel = Ctx.get_namespace ctx in
  let prose_in = Ctx.find_hint_prose_in ctx (`Rel id_rel) in
  let prose_true = Ctx.find_hint_prose_true ctx (`Rel id_rel) in
  { Annot.empty with prose_in; prose_true }

let hints_of_rel_def (ctx : Ctx.t) (id_rel : id) (rel_signature : rel_signature)
    : Annot.hints =
  let nottyp, inputs = rel_signature in
  let prose = Ctx.find_hint_prose ctx (`Rel id_rel) in
  let prose_in = Ctx.find_hint_prose_in ctx (`Rel id_rel) in
  let prose_out =
    Ctx.find_hint_prose_out ctx (`Rel id_rel)
    |> Option.map (fun a -> Hints.Alter.realign a inputs)
  in
  let prose_true = Ctx.find_hint_prose_true ctx (`Rel id_rel) in
  let prose_false = Ctx.find_hint_prose_false ctx (`Rel id_rel) in
  let fresh_exps_from_typs typs =
    let _, exps =
      List.fold_left_map
        (fun frees typ -> Il.Fresh.exp_from_typ ~dim:true ctx.menv frees typ)
        IdSet.empty typs
    in
    exps
  in
  let prose_input_exps, prose_output_exps =
    match (prose_in, prose_out) with
    | Some _, Some _ ->
        let typs = Mixfix.args nottyp.it in
        let typs_input, typs_output = Hints.Input.split inputs typs in
        ( Some (fresh_exps_from_typs typs_input),
          Some (fresh_exps_from_typs typs_output) )
    | Some _, None ->
        let typs = Mixfix.args nottyp.it in
        let typs_input, _ = Hints.Input.split inputs typs in
        (Some (fresh_exps_from_typs typs_input), None)
    | _ -> (None, None)
  in
  {
    Annot.empty with
    prose;
    prose_in;
    prose_out;
    prose_true;
    prose_false;
    prose_input_exps;
    prose_output_exps;
  }

let hints_of_func_def (ctx : Ctx.t) (id_func : id) : Annot.hints =
  let prose_in = Ctx.find_hint_prose_in ctx (`Func id_func) in
  let prose_true = Ctx.find_hint_prose_true ctx (`Func id_func) in
  let prose_false = Ctx.find_hint_prose_false ctx (`Func id_func) in
  { Annot.empty with prose_in; prose_true; prose_false }

(* Hint validation *)

let validate_hint_at (at : region) (n : int) : Hints.Alter.t option -> unit =
  let slots = List.init n (fun _ -> ()) in
  function None -> () | Some h -> Ctx.validate_hint_alter at h slots

let validate_annot_alter (at : region) (annot : Annot.hints) (n : int) : unit =
  let validate = validate_hint_at at n in
  validate annot.prose;
  validate annot.prose_in;
  validate annot.prose_out;
  validate annot.prose_true;
  validate annot.prose_false

let validate_annot_split (at : region) (annot : Annot.hints) ~(n_in : int)
    ~(n_out : int) : unit =
  validate_hint_at at n_in annot.prose_in;
  validate_hint_at at n_out annot.prose_out

let validate_annot_fields (at : region) (annot : Annot.hints) (arity : int) :
    unit =
  match annot.prose_fields with
  | None -> ()
  | Some h -> Ctx.validate_hint_fields at h arity

(* Expressions *)

let rec annotate_exp (ctx : Ctx.t) (exp : exp) : Pl.exp =
  let at, note = (exp.at, exp.note) in
  match exp.it with
  | BoolE b ->
      let node = Pl.BoolE b $$ (at, note) in
      let hints = Annot.empty in
      { node; hints }
  | NumE n ->
      let node = Pl.NumE n $$ (at, note) in
      let hints = Annot.empty in
      { node; hints }
  | TextE s ->
      let node = Pl.TextE s $$ (at, note) in
      let hints = Annot.empty in
      { node; hints }
  | VarE id ->
      let node = Pl.VarE id $$ (at, note) in
      let hints = Annot.empty in
      { node; hints }
  | UnE (unop, optyp, exp) ->
      let exp_pl = annotate_exp ctx exp in
      let node = Pl.UnE (unop, optyp, exp_pl) $$ (at, note) in
      let hints = Annot.empty in
      { node; hints }
  | BinE (binop, optyp, exp_l, exp_r) ->
      let exp_l_pl = annotate_exp ctx exp_l in
      let exp_r_pl = annotate_exp ctx exp_r in
      let node = Pl.BinE (binop, optyp, exp_l_pl, exp_r_pl) $$ (at, note) in
      let hints = Annot.empty in
      { node; hints }
  | CmpE (cmpop, optyp, exp_l, exp_r) ->
      let exp_l_pl = annotate_exp ctx exp_l in
      let exp_r_pl = annotate_exp ctx exp_r in
      let node = Pl.CmpE (cmpop, optyp, exp_l_pl, exp_r_pl) $$ (at, note) in
      let hints = Annot.empty in
      { node; hints }
  | UpCastE (typ, exp) ->
      let exp_pl = annotate_exp ctx exp in
      let node = Pl.UpCastE (typ, exp_pl) $$ (at, note) in
      let hints = Annot.empty in
      { node; hints }
  | DownCastE (typ, exp) ->
      let exp_pl = annotate_exp ctx exp in
      let node = Pl.DownCastE (typ, exp_pl) $$ (at, note) in
      let hints = Annot.empty in
      { node; hints }
  | SubE (exp, typ, subcheck) ->
      let exp_pl = annotate_exp ctx exp in
      let node = Pl.SubE (exp_pl, typ, subcheck) $$ (at, note) in
      let hints = Annot.empty in
      { node; hints }
  | MatchE (exp, pattern) ->
      let exp_pl = annotate_exp ctx exp in
      let node = Pl.MatchE (exp_pl, pattern) $$ (at, note) in
      let hints = Annot.empty in
      { node; hints }
  | TupleE exps ->
      let exps_pl = annotate_exps ctx exps in
      let node = Pl.TupleE exps_pl $$ (at, note) in
      let hints = Annot.empty in
      { node; hints }
  | CaseE notexp ->
      let mixop, exps = Mixfix.split notexp in
      let notexp_pl = annotate_notexp ctx notexp in
      let node = Pl.CaseE notexp_pl $$ (at, note) in
      let hints = hints_of_case_exp ctx note mixop in
      validate_annot_alter at hints (List.length exps);
      validate_annot_fields at hints (List.length exps);
      { node; hints }
  | StrE expfields ->
      let expfields_pl =
        List.map
          (fun (atom, exp) ->
            let exp_pl = annotate_exp ctx exp in
            (atom, exp_pl))
          expfields
      in
      let node = Pl.StrE expfields_pl $$ (at, note) in
      let hints = Annot.empty in
      { node; hints }
  | OptE exp_opt ->
      let exp_pl_opt = Option.map (annotate_exp ctx) exp_opt in
      let node = Pl.OptE exp_pl_opt $$ (at, note) in
      let hints = Annot.empty in
      { node; hints }
  | ListE exps ->
      let exps_pl = annotate_exps ctx exps in
      let node = Pl.ListE exps_pl $$ (at, note) in
      let hints = Annot.empty in
      { node; hints }
  | ConsE (exp_h, exp_t) ->
      let exp_h_pl = annotate_exp ctx exp_h in
      let exp_t_pl = annotate_exp ctx exp_t in
      let node = Pl.ConsE (exp_h_pl, exp_t_pl) $$ (at, note) in
      let hints = Annot.empty in
      { node; hints }
  | CatE (exp_l, exp_r) ->
      let exp_l_pl = annotate_exp ctx exp_l in
      let exp_r_pl = annotate_exp ctx exp_r in
      let node = Pl.CatE (exp_l_pl, exp_r_pl) $$ (at, note) in
      let hints = Annot.empty in
      { node; hints }
  | MemE (exp_e, exp_s) ->
      let exp_e_pl = annotate_exp ctx exp_e in
      let exp_s_pl = annotate_exp ctx exp_s in
      let node = Pl.MemE (exp_e_pl, exp_s_pl) $$ (at, note) in
      let hints = Annot.empty in
      { node; hints }
  | LenE exp ->
      let exp_pl = annotate_exp ctx exp in
      let node = Pl.LenE exp_pl $$ (at, note) in
      let hints = Annot.empty in
      { node; hints }
  | DotE (exp, atom) ->
      let exp_pl = annotate_exp ctx exp in
      let node = Pl.DotE (exp_pl, atom) $$ (at, note) in
      let hints = Annot.empty in
      { node; hints }
  | IdxE (exp_b, exp_i) ->
      let exp_b_pl = annotate_exp ctx exp_b in
      let exp_i_pl = annotate_exp ctx exp_i in
      let node = Pl.IdxE (exp_b_pl, exp_i_pl) $$ (at, note) in
      let hints = Annot.empty in
      { node; hints }
  | SliceE (exp_b, exp_i, exp_n) ->
      let exp_b_pl = annotate_exp ctx exp_b in
      let exp_i_pl = annotate_exp ctx exp_i in
      let exp_n_pl = annotate_exp ctx exp_n in
      let node = Pl.SliceE (exp_b_pl, exp_i_pl, exp_n_pl) $$ (at, note) in
      let hints = Annot.empty in
      { node; hints }
  | UpdE (exp_b, path, exp_f) ->
      let exp_b_pl = annotate_exp ctx exp_b in
      let path_pl = annotate_path ctx path in
      let exp_f_pl = annotate_exp ctx exp_f in
      let node = Pl.UpdE (exp_b_pl, path_pl, exp_f_pl) $$ (at, note) in
      let hints = Annot.empty in
      { node; hints }
  | CallE (id, targs, args) ->
      let args_pl = annotate_args ctx args in
      let node = Pl.CallE (id, targs, args_pl) $$ (at, note) in
      let hints = hints_of_call_exp ctx id in
      validate_annot_alter at hints (List.length args_pl);
      { node; hints }
  | IterE (exp, iterexp) ->
      let exp_pl = annotate_exp ctx exp in
      let node = Pl.IterE (exp_pl, iterexp) $$ (at, note) in
      let hints = Annot.empty in
      { node; hints }

and annotate_exps (ctx : Ctx.t) (exps : exp list) : Pl.exp list =
  List.map (annotate_exp ctx) exps

and annotate_notexp (ctx : Ctx.t) (notexp : notexp) : Pl.notexp =
  Mixfix.map (annotate_exp ctx) notexp

(* Paths *)

and annotate_path (ctx : Ctx.t) (path : path) : Pl.path =
  let at, note = (path.at, path.note) in
  match path.it with
  | Il.RootP -> Pl.RootP $$ (at, note)
  | Il.IdxP (path, exp) ->
      let path_pl = annotate_path ctx path in
      let exp_pl = annotate_exp ctx exp in
      Pl.IdxP (path_pl, exp_pl) $$ (at, note)
  | Il.SliceP (path, exp_l, exp_h) ->
      let path_pl = annotate_path ctx path in
      let exp_pl_l = annotate_exp ctx exp_l in
      let exp_pl_h = annotate_exp ctx exp_h in
      Pl.SliceP (path_pl, exp_pl_l, exp_pl_h) $$ (at, note)
  | Il.DotP (path, atom) ->
      let path_pl = annotate_path ctx path in
      Pl.DotP (path_pl, atom) $$ (at, note)

(* Arguments *)

and annotate_arg (ctx : Ctx.t) (arg : arg) : Pl.arg =
  let at = arg.at in
  match arg.it with
  | ExpA exp ->
      let exp_pl = annotate_exp ctx exp in
      Pl.ExpA exp_pl $ at
  | DefA id -> Pl.DefA id $ at

and annotate_args (ctx : Ctx.t) (args : arg list) : Pl.arg list =
  List.map (annotate_arg ctx) args

(* Parameters *)

let rec annotate_param (ctx : Ctx.t) (param : param) : Pl.param =
  let at = param.at in
  match param.it with
  | ExpP (typ, exp) ->
      let exp_pl = annotate_exp ctx exp in
      Pl.ExpP (typ, exp_pl) $ at
  | DefP (id, tparams, params, typ) ->
      let params_pl = annotate_params ctx params in
      Pl.DefP (id, tparams, params_pl, typ) $ at

and annotate_params (ctx : Ctx.t) (params : param list) : Pl.param list =
  List.map (annotate_param ctx) params

(* Holding conditions *)

let annotate_holdcase_shared (ctx : Ctx.t)
    (annotate_block : Ctx.t -> block -> 'instr_tier Pl.block)
    (holdcase : holdcase) : 'instr_tier Pl.holdcase =
  match holdcase with
  | BothH (block_hold, block_nothold) ->
      let block_hold_pl = annotate_block ctx block_hold in
      let block_nothold_pl = annotate_block ctx block_nothold in
      Pl.BothH (block_hold_pl, block_nothold_pl)
  | HoldH (block_hold, dangle) ->
      let block_hold_pl = annotate_block ctx block_hold in
      Pl.HoldH (block_hold_pl, dangle)
  | NotHoldH (block_nothold, dangle) ->
      let block_nothold_pl = annotate_block ctx block_nothold in
      Pl.NotHoldH (block_nothold_pl, dangle)

(* Case analysis *)

and annotate_guard (ctx : Ctx.t) (guard : guard) : Pl.guard =
  match guard with
  | BoolG b -> Pl.BoolG b
  | CmpG (cmpop, optyp, exp) ->
      let exp_pl = annotate_exp ctx exp in
      Pl.CmpG (cmpop, optyp, exp_pl)
  | SubG (typ, subcheck) -> Pl.SubG (typ, subcheck)
  | MatchG pattern -> Pl.MatchG pattern
  | MemG exp ->
      let exp_pl = annotate_exp ctx exp in
      Pl.MemG exp_pl

let annotate_case_shared (ctx : Ctx.t)
    (annotate_block : Ctx.t -> block -> 'instr_tier Pl.block)
    ((guard, block) : case) : 'instr_tier Pl.case =
  let guard_pl = annotate_guard ctx guard in
  let block_pl = annotate_block ctx block in
  (guard_pl, block_pl)

let annotate_cases_shared (ctx : Ctx.t)
    (annotate_block : Ctx.t -> block -> 'instr_tier Pl.block)
    (cases : case list) : 'instr_tier Pl.case list =
  List.map (annotate_case_shared ctx annotate_block) cases

(* Instructions *)

let annotate_instr_shared (ctx : Ctx.t)
    (annotate_block : Ctx.t -> block -> 'instr_tier Pl.block)
    (instr_tier_of : Ctx.t -> instr -> 'instr_tier Pl.instr) (instr : instr) :
    'instr_tier Pl.instr =
  let at, note = (instr.at, { Pl.iid = instr.note.iid; fallthrough = None }) in
  match instr.it with
  | IfI (exp_cond, iterexps, block_then, dangle) ->
      let exp_cond_pl = annotate_exp ctx exp_cond in
      let block_then_pl = annotate_block ctx block_then in
      let node =
        Pl.IfI (exp_cond_pl, iterexps, block_then_pl, dangle) $$ (at, note)
      in
      let hints = Annot.empty in
      { node; hints }
  | HoldI (id_rel, notexp, iterexps, holdcase) ->
      let notexp_pl = annotate_notexp ctx notexp in
      let holdcase_pl = annotate_holdcase_shared ctx annotate_block holdcase in
      let node =
        Pl.HoldI (id_rel, notexp_pl, iterexps, holdcase_pl) $$ (at, note)
      in
      let hints = hints_of_hold_instr ctx id_rel in
      let exps = Mixfix.args notexp in
      validate_annot_alter at hints (List.length exps);
      { node; hints }
  | CaseI (exp, cases, dangle) ->
      let exp_pl = annotate_exp ctx exp in
      let cases_pl = annotate_cases_shared ctx annotate_block cases in
      let node = Pl.CaseI (exp_pl, cases_pl, dangle) $$ (at, note) in
      let hints = Annot.empty in
      { node; hints }
  | LetI (exp_l, exp_r, iterinstrs) ->
      let exp_l_pl = annotate_exp ctx exp_l in
      let exp_r_pl = annotate_exp ctx exp_r in
      let node = Pl.LetI (exp_l_pl, exp_r_pl, iterinstrs) $$ (at, note) in
      let hints =
        match exp_l_pl.node.it with
        | Pl.CaseE _ ->
            { Annot.empty with prose_fields = exp_l_pl.hints.prose_fields }
        | _ -> Annot.empty
      in
      { node; hints }
  | DebugI exp ->
      let exp_pl = annotate_exp ctx exp in
      let node = Pl.DebugI exp_pl $$ (at, note) in
      let hints = Annot.empty in
      { node; hints }
  | GroupI _ | RuleI _ | ResultI _ | ReturnI _ | BlockI _ ->
      instr_tier_of ctx instr

(* Tiered instructions and blocks *)

let rec annotate_instr_dispatch (ctx : Ctx.t) (instr_ll : instr) :
    Pl.instr_dispatch Pl.instr =
  annotate_instr_shared ctx annotate_block_dispatch instr_dispatch_of_ll
    instr_ll

and annotate_instr_group (ctx : Ctx.t) (instr_ll : instr) :
    Pl.instr_group Pl.instr =
  annotate_instr_shared ctx annotate_block_group instr_group_of_ll instr_ll

and annotate_block_dispatch (ctx : Ctx.t) (block_ll : block) : Pl.block_dispatch
    =
  List.map (annotate_instr_dispatch ctx) block_ll

and annotate_block_group (ctx : Ctx.t) (block_ll : block) : Pl.block_group =
  List.map (annotate_instr_group ctx) block_ll

(* Enforcement: dispatch bodies hold only rule groups *)

and instr_dispatch_of_ll (ctx : Ctx.t) (instr_ll : instr) :
    Pl.instr_dispatch Pl.instr =
  let at, note =
    (instr_ll.at, { Pl.iid = instr_ll.note.iid; fallthrough = None })
  in
  match instr_ll.it with
  | GroupI (id_rulegroup, rel_signature, exps, block) ->
      let id_rel = Ctx.get_namespace ctx in
      let exps_pl = annotate_exps ctx exps in
      let block_pl = annotate_block_group ctx block in
      let node =
        Pl.TierI
          (Pl.GroupI (id_rulegroup, id_rel, rel_signature, exps_pl, block_pl))
        $$ (at, note)
      in
      let hints = hints_of_group_instr ctx in
      let _, inputs = rel_signature in
      let exps_in, _ = Hints.Input.split inputs exps in
      validate_annot_alter at hints (List.length exps_in);
      { node; hints }
  | BlockI arms ->
      let arms_pl = List.map (annotate_block_dispatch ctx) arms in
      let node = Pl.TierI (Pl.RouteI arms_pl) $$ (at, note) in
      { node; hints = Annot.empty }
  | _ ->
      Error.error instr_ll.at
        "a result, return, or rule application cannot appear at dispatch level"

(* Enforcement: rule-group bodies never nest another rule group *)

and instr_group_of_ll (ctx : Ctx.t) (instr_ll : instr) : Pl.instr_group Pl.instr
    =
  let at, note =
    (instr_ll.at, { Pl.iid = instr_ll.note.iid; fallthrough = None })
  in
  match instr_ll.it with
  | RuleI (id_rel, notexp, inputs, iterinstrs) ->
      let notexp_pl = annotate_notexp ctx notexp in
      let node =
        Pl.TierI (Pl.RuleI (id_rel, notexp_pl, inputs, iterinstrs)) $$ (at, note)
      in
      let hints = hints_of_rule_instr ctx id_rel inputs in
      let exps = Mixfix.args notexp in
      let exps_in, exps_out = Hints.Input.split inputs exps in
      validate_annot_split at hints ~n_in:(List.length exps_in)
        ~n_out:(List.length exps_out);
      { node; hints }
  | ResultI (rel_signature, exps) ->
      let exps_pl = annotate_exps ctx exps in
      let node = Pl.TierI (Pl.ResultI (rel_signature, exps_pl)) $$ (at, note) in
      let _, inputs = rel_signature in
      let hints = hints_of_result_instr ctx inputs in
      validate_annot_alter at hints (List.length exps_pl);
      { node; hints }
  | ReturnI exp ->
      let exp_pl = annotate_exp ctx exp in
      let node = Pl.TierI (Pl.ReturnI exp_pl) $$ (at, note) in
      let hints = Annot.empty in
      { node; hints }
  | BlockI arms ->
      let arms_pl = List.map (annotate_block_group ctx) arms in
      let node = Pl.TierI (Pl.BacktrackI arms_pl) $$ (at, note) in
      { node; hints = Annot.empty }
  | _ -> Error.error instr_ll.at "a rule group cannot appear in a group body"

(* Definitions *)

let annotate_def (ctx : Ctx.t) (def : def) : Pl.def =
  let at = def.at in
  match def.it with
  | ExternTypD (id, _) ->
      let node = Pl.ExternTypD id $ at in
      let hints = Annot.empty in
      { node; hints }
  | TypD (id, tparams, deftyp, _) ->
      let node = Pl.TypD (id, tparams, deftyp) $ at in
      let hints = Annot.empty in
      { node; hints }
  | VarD (id, typ, _) ->
      let node = Pl.VarD (id, typ) $ at in
      let hints = Annot.empty in
      { node; hints }
  | ExternRelD (id, rel_signature, exps, _) ->
      let ctx_rel = Ctx.enter_rel ctx id in
      let exps_pl = annotate_exps ctx_rel exps in
      let node = Pl.ExternRelD (id, rel_signature, exps_pl) $ at in
      let hints = hints_of_rel_def ctx_rel id rel_signature in
      { node; hints }
  | RelD (id, rel_signature, exps, block, elseblock_opt, _) ->
      let ctx_rel = Ctx.enter_rel ctx id in
      let exps_pl = annotate_exps ctx_rel exps in
      let block_pl =
        block |> Linearize.linearize_block |> annotate_block_dispatch ctx_rel
      in
      let elseblock_pl_opt =
        elseblock_opt
        |> Option.map Linearize.linearize_block
        |> Option.map (annotate_block_dispatch ctx_rel)
      in
      let node =
        Pl.RelD (id, rel_signature, exps_pl, block_pl, elseblock_pl_opt) $ at
      in
      let hints = hints_of_rel_def ctx_rel id rel_signature in
      { node; hints }
  | ExternDecD (id, tparams, params, typ, _) ->
      let ctx_local = Ctx.add_tparams ctx tparams in
      let params_pl = annotate_params ctx_local params in
      let node = Pl.ExternDecD (id, tparams, params_pl, typ) $ at in
      let hints = hints_of_func_def ctx id in
      { node; hints }
  | BuiltinDecD (id, tparams, params, typ, _) ->
      let ctx_local = Ctx.add_tparams ctx tparams in
      let params_pl = annotate_params ctx_local params in
      let node = Pl.BuiltinDecD (id, tparams, params_pl, typ) $ at in
      let hints = hints_of_func_def ctx id in
      { node; hints }
  | TableDecD (id, params, typ, tablerows, _) ->
      let params_pl = annotate_params ctx params in
      let tablerows_pl =
        List.map
          (fun (exps_in, exp_out, block) ->
            let exps_pl_in = annotate_exps ctx exps_in in
            let exp_pl_out = annotate_exp ctx exp_out in
            let block_pl =
              block |> Linearize.linearize_block |> annotate_block_group ctx
            in
            (exps_pl_in, exp_pl_out, block_pl))
          tablerows
      in
      let node = Pl.TableDecD (id, params_pl, typ, tablerows_pl) $ at in
      let hints = hints_of_func_def ctx id in
      { node; hints }
  | FuncDecD (id, tparams, params, typ, block, elseblock_opt, _) ->
      let ctx_local = Ctx.add_tparams ctx tparams in
      let params_pl = annotate_params ctx_local params in
      let block_pl =
        block |> Linearize.linearize_block |> annotate_block_group ctx_local
      in
      let elseblock_pl_opt =
        elseblock_opt
        |> Option.map Linearize.linearize_block
        |> Option.map (annotate_block_group ctx_local)
      in
      let node =
        Pl.FuncDecD (id, tparams, params_pl, typ, block_pl, elseblock_pl_opt)
        $ at
      in
      let hints = hints_of_func_def ctx id in
      { node; hints }

let annotate_defs (ctx : Ctx.t) (spec : spec) : Pl.spec =
  List.map (annotate_def ctx) spec

(* Errors *)

type error = { at : region; msg : string }

let to_region_msg { at; msg } = (at, msg)

(* Entry point *)

let annotate_spec (spec : spec) : (Pl.spec, error) result =
  try
    let ctx = Ctx.init () in
    let ctx = Ctx.load_spec ctx spec in
    Ok
      (spec |> Expand.expand_spec |> annotate_defs ctx |> Shorthand.shorten_defs
     |> Stamp.stamp_defs)
  with Error.ProseError (at, msg) -> Error { at; msg }
