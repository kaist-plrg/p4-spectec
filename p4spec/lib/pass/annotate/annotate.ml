open Lang
open Util.Source
open Ll.Ast
module Mixfix = Domain.Mixfix
module Annot = Pl.Annot
module Ctx = Ctx
module Expand = Expand
module IdSet = Domain.Lib.IdSet

(* Hints lookup *)

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
  let node', hints = annotate_exp' ctx exp in
  { node = node' $$ (exp.at, exp.note); hints }

and annotate_exp' (ctx : Ctx.t) (exp : exp) : Pl.exp' * Annot.hints =
  let { it; at; note } = exp in
  match it with
  | Il.BoolE b -> (Pl.BoolE b, Annot.empty)
  | Il.NumE n -> (Pl.NumE n, Annot.empty)
  | Il.TextE s -> (Pl.TextE s, Annot.empty)
  | Il.VarE id -> (Pl.VarE id, Annot.empty)
  | Il.UnE (unop, optyp, exp) ->
      (Pl.UnE (unop, optyp, annotate_exp ctx exp), Annot.empty)
  | Il.BinE (binop, optyp, exp_l, exp_r) ->
      ( Pl.BinE (binop, optyp, annotate_exp ctx exp_l, annotate_exp ctx exp_r),
        Annot.empty )
  | Il.CmpE (cmpop, optyp, exp_l, exp_r) ->
      ( Pl.CmpE (cmpop, optyp, annotate_exp ctx exp_l, annotate_exp ctx exp_r),
        Annot.empty )
  | Il.UpCastE (typ, exp) ->
      (Pl.UpCastE (typ, annotate_exp ctx exp), Annot.empty)
  | Il.DownCastE (typ, exp) ->
      (Pl.DownCastE (typ, annotate_exp ctx exp), Annot.empty)
  | Il.SubE (exp, typ) -> (Pl.SubE (annotate_exp ctx exp, typ), Annot.empty)
  | Il.MatchE (exp, pattern) ->
      (Pl.MatchE (annotate_exp ctx exp, pattern), Annot.empty)
  | Il.TupleE exps -> (Pl.TupleE (List.map (annotate_exp ctx) exps), Annot.empty)
  | Il.CaseE notexp ->
      let mixop, exps = Mixfix.split notexp in
      let notexp_pl = annotate_notexp ctx notexp in
      let hints = hints_of_case_exp ctx note mixop in
      validate_annot_alter at hints (List.length exps);
      validate_annot_fields at hints (List.length exps);
      (Pl.CaseE notexp_pl, hints)
  | Il.StrE expfields ->
      let expfields_pl =
        List.map (fun (atom, exp) -> (atom, annotate_exp ctx exp)) expfields
      in
      (Pl.StrE expfields_pl, Annot.empty)
  | Il.OptE exp_opt ->
      (Pl.OptE (Option.map (annotate_exp ctx) exp_opt), Annot.empty)
  | Il.ListE exps -> (Pl.ListE (List.map (annotate_exp ctx) exps), Annot.empty)
  | Il.ConsE (exp_h, exp_t) ->
      (Pl.ConsE (annotate_exp ctx exp_h, annotate_exp ctx exp_t), Annot.empty)
  | Il.CatE (exp_l, exp_r) ->
      (Pl.CatE (annotate_exp ctx exp_l, annotate_exp ctx exp_r), Annot.empty)
  | Il.MemE (exp_e, exp_s) ->
      (Pl.MemE (annotate_exp ctx exp_e, annotate_exp ctx exp_s), Annot.empty)
  | Il.LenE exp -> (Pl.LenE (annotate_exp ctx exp), Annot.empty)
  | Il.DotE (exp, atom) -> (Pl.DotE (annotate_exp ctx exp, atom), Annot.empty)
  | Il.IdxE (exp_b, exp_i) ->
      (Pl.IdxE (annotate_exp ctx exp_b, annotate_exp ctx exp_i), Annot.empty)
  | Il.SliceE (exp_b, exp_l, exp_h) ->
      ( Pl.SliceE
          ( annotate_exp ctx exp_b,
            annotate_exp ctx exp_l,
            annotate_exp ctx exp_h ),
        Annot.empty )
  | Il.UpdE (exp_b, path, exp_f) ->
      ( Pl.UpdE
          ( annotate_exp ctx exp_b,
            annotate_path ctx path,
            annotate_exp ctx exp_f ),
        Annot.empty )
  | Il.CallE (id, targs, args) ->
      let args_pl = List.map (annotate_arg ctx) args in
      let hints = hints_of_call_exp ctx id in
      validate_annot_alter at hints (List.length args_pl);
      (Pl.CallE (id, targs, args_pl), hints)
  | Il.IterE (exp, iterexp) ->
      (Pl.IterE (annotate_exp ctx exp, iterexp), Annot.empty)

and annotate_path (ctx : Ctx.t) (path : path) : Pl.path =
  let { it; at; note } = path in
  let it =
    match it with
    | Il.RootP -> Pl.RootP
    | Il.IdxP (path, exp) ->
        Pl.IdxP (annotate_path ctx path, annotate_exp ctx exp)
    | Il.SliceP (path, exp_l, exp_h) ->
        Pl.SliceP
          ( annotate_path ctx path,
            annotate_exp ctx exp_l,
            annotate_exp ctx exp_h )
    | Il.DotP (path, atom) -> Pl.DotP (annotate_path ctx path, atom)
  in
  it $$ (at, note)

and annotate_arg (ctx : Ctx.t) (arg : arg) : Pl.arg =
  let it =
    match arg.it with
    | Il.ExpA exp -> Pl.ExpA (annotate_exp ctx exp)
    | Il.DefA id -> Pl.DefA id
  in
  it $ arg.at

and annotate_notexp (ctx : Ctx.t) (notexp : notexp) : Pl.notexp =
  Mixfix.map (annotate_exp ctx) notexp

(* Guards / cases / holdcases *)

and annotate_guard (ctx : Ctx.t) (guard : guard) : Pl.guard =
  match guard with
  | BoolG b -> Pl.BoolG b
  | CmpG (cmpop, optyp, exp) -> Pl.CmpG (cmpop, optyp, annotate_exp ctx exp)
  | SubG typ -> Pl.SubG typ
  | MatchG pattern -> Pl.MatchG pattern
  | MemG exp -> Pl.MemG (annotate_exp ctx exp)

and annotate_case (ctx : Ctx.t) ((guard, block) : case) : Pl.case =
  (annotate_guard ctx guard, annotate_block ctx block)

and annotate_holdcase (ctx : Ctx.t) (holdcase : holdcase) : Pl.holdcase =
  match holdcase with
  | BothH (block_hold, block_nothold) ->
      Pl.BothH (annotate_block ctx block_hold, annotate_block ctx block_nothold)
  | HoldH (block_hold, dangle) ->
      Pl.HoldH (annotate_block ctx block_hold, dangle)
  | NotHoldH (block_nothold, dangle) ->
      Pl.NotHoldH (annotate_block ctx block_nothold, dangle)

(* Instructions *)

and annotate_instr (ctx : Ctx.t) (instr : instr) : Pl.instr =
  let node', hints = annotate_instr' ctx instr in
  { node = node' $$ (instr.at, instr.note); hints }

and annotate_instr' (ctx : Ctx.t) (instr : instr) : Pl.instr' * Annot.hints =
  let { it; at; _ } = instr in
  match it with
  | IfI (exp_cond, iterexps, block_then, dangle) ->
      ( Pl.IfI
          ( annotate_exp ctx exp_cond,
            iterexps,
            annotate_block ctx block_then,
            dangle ),
        Annot.empty )
  | HoldI (id_rel, notexp, iterexps, holdcase) ->
      let notexp_pl = annotate_notexp ctx notexp in
      let holdcase_pl = annotate_holdcase ctx holdcase in
      let hints = hints_of_hold_instr ctx id_rel in
      let exps = Mixfix.args notexp in
      validate_annot_alter at hints (List.length exps);
      (Pl.HoldI (id_rel, notexp_pl, iterexps, holdcase_pl), hints)
  | CaseI (exp, cases, dangle) ->
      ( Pl.CaseI
          (annotate_exp ctx exp, List.map (annotate_case ctx) cases, dangle),
        Annot.empty )
  | OtherwiseI _block -> assert false
  | GroupI (id_rulegroup, rel_signature, exps, block) ->
      let id_rel = Ctx.get_namespace ctx in
      let exps_pl = List.map (annotate_exp ctx) exps in
      let block_pl = annotate_block ctx block in
      let hints = hints_of_group_instr ctx in
      let _, inputs = rel_signature in
      let exps_in, _ = Hints.Input.split inputs exps in
      validate_annot_alter at hints (List.length exps_in);
      (Pl.GroupI (id_rulegroup, id_rel, rel_signature, exps_pl, block_pl), hints)
  | TryI arms -> (Pl.TryI (List.map (annotate_block ctx) arms), Annot.empty)
  | DebugI exp -> (Pl.DebugI (annotate_exp ctx exp), Annot.empty)
  | LetI (exp_l, exp_r, iterinstrs) ->
      let exp_l_pl = annotate_exp ctx exp_l in
      let exp_r_pl = annotate_exp ctx exp_r in
      let hints =
        match exp_l_pl.node.it with
        | Pl.CaseE _ ->
            { Annot.empty with prose_fields = exp_l_pl.hints.prose_fields }
        | _ -> Annot.empty
      in
      (Pl.LetI (exp_l_pl, exp_r_pl, iterinstrs), hints)
  | RuleI (id_rel, notexp, inputs, iterinstrs) ->
      let notexp_pl = annotate_notexp ctx notexp in
      let hints = hints_of_rule_instr ctx id_rel inputs in
      let exps = Mixfix.args notexp in
      let exps_in, exps_out = Hints.Input.split inputs exps in
      validate_annot_split at hints ~n_in:(List.length exps_in)
        ~n_out:(List.length exps_out);
      (Pl.RuleI (id_rel, notexp_pl, inputs, iterinstrs), hints)
  | ResultI (rel_signature, exps) ->
      let exps_pl = List.map (annotate_exp ctx) exps in
      let _, inputs = rel_signature in
      let hints = hints_of_result_instr ctx inputs in
      validate_annot_alter at hints (List.length exps_pl);
      (Pl.ResultI (rel_signature, exps_pl), hints)
  | ReturnI exp -> (Pl.ReturnI (annotate_exp ctx exp), Annot.empty)

and annotate_block (ctx : Ctx.t) (block : block) : Pl.block =
  List.map (annotate_instr ctx) block

(* Definitions *)

let rec annotate_param (ctx : Ctx.t) (param : param) : Pl.param =
  let it' =
    match param.it with
    | ExpP (typ, exp) -> Pl.ExpP (typ, annotate_exp ctx exp)
    | DefP (id, tparams, params, typ) ->
        Pl.DefP (id, tparams, annotate_params ctx params, typ)
  in
  it' $ param.at

and annotate_params (ctx : Ctx.t) (params : param list) : Pl.param list =
  List.map (annotate_param ctx) params

let annotate_def (ctx : Ctx.t) (def : def) : Pl.def =
  let { it; at; _ } = def in
  let node', hints =
    match it with
    | ExternTypD (id, _) -> (Pl.ExternTypD id, Annot.empty)
    | TypD (id, tparams, deftyp, _) ->
        (Pl.TypD (id, tparams, deftyp), Annot.empty)
    | VarD (id, typ, _) -> (Pl.VarD (id, typ), Annot.empty)
    | ExternRelD (id, rel_signature, exps, _) ->
        let ctx_rel = Ctx.enter_rel ctx id in
        let exps_pl = List.map (annotate_exp ctx_rel) exps in
        let hints = hints_of_rel_def ctx_rel id rel_signature in
        (Pl.ExternRelD (id, rel_signature, exps_pl), hints)
    | RelD (id, rel_signature, exps, block, elseblock_opt, _) ->
        let ctx_rel = Ctx.enter_rel ctx id in
        let exps_pl = List.map (annotate_exp ctx_rel) exps in
        let block_ll = Linearize.linearize_block block in
        let block_pl = annotate_block ctx_rel block_ll in
        let elseblock_ll_opt =
          Option.map Linearize.linearize_block elseblock_opt
        in
        let elseblock_pl_opt =
          Option.map (annotate_block ctx_rel) elseblock_ll_opt
        in
        let hints = hints_of_rel_def ctx_rel id rel_signature in
        (Pl.RelD (id, rel_signature, exps_pl, block_pl, elseblock_pl_opt), hints)
    | ExternDecD (id, tparams, params, typ, _) ->
        let ctx_local = Ctx.add_tparams ctx tparams in
        let params_pl = annotate_params ctx_local params in
        let hints = hints_of_func_def ctx id in
        (Pl.ExternDecD (id, tparams, params_pl, typ), hints)
    | BuiltinDecD (id, tparams, params, typ, _) ->
        let ctx_local = Ctx.add_tparams ctx tparams in
        let params_pl = annotate_params ctx_local params in
        let hints = hints_of_func_def ctx id in
        (Pl.BuiltinDecD (id, tparams, params_pl, typ), hints)
    | TableDecD (id, params, typ, tablerows, _) ->
        let params_pl = annotate_params ctx params in
        let tablerows_pl =
          List.map
            (fun (exps_in, exp_out, block) ->
              let block_ll = Linearize.linearize_block block in
              ( List.map (annotate_exp ctx) exps_in,
                annotate_exp ctx exp_out,
                annotate_block ctx block_ll ))
            tablerows
        in
        let hints = hints_of_func_def ctx id in
        (Pl.TableDecD (id, params_pl, typ, tablerows_pl), hints)
    | FuncDecD (id, tparams, params, typ, block, elseblock_opt, _) ->
        let ctx_local = Ctx.add_tparams ctx tparams in
        let params_pl = annotate_params ctx_local params in
        let block_ll = Linearize.linearize_block block in
        let block_pl = annotate_block ctx_local block_ll in
        let elseblock_ll_opt =
          Option.map Linearize.linearize_block elseblock_opt
        in
        let elseblock_pl_opt =
          Option.map (annotate_block ctx_local) elseblock_ll_opt
        in
        let hints = hints_of_func_def ctx id in
        ( Pl.FuncDecD (id, tparams, params_pl, typ, block_pl, elseblock_pl_opt),
          hints )
  in
  { node = node' $ at; hints }

(* Entry point *)

let annotate_defs (spec : spec) : Pl.spec =
  let ctx = Ctx.init () in
  let ctx = Ctx.load_spec ctx spec in
  List.map (annotate_def ctx) spec

let annotate_spec (spec : spec) : Pl.spec =
  spec |> Expand.expand_spec |> annotate_defs |> Shorthand.shorten_defs
