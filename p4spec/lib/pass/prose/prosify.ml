open Domain.Lib
open Lang
open Xl
open Ll.Ast
open Util.Source

(* Expression prosification *)

let prosify_hole_exp () : Pl.exp =
  Pl.VarE ("%" $ no_region) $$ (no_region, Il.TextT)

let rec prosify_exp (ctx : Ctx.t) (exp : exp) : Pl.exp =
  prosify_exp' ctx exp $$ (exp.at, exp.note)

and prosify_exp' (ctx : Ctx.t) (exp : exp) : Pl.exp' =
  let at = exp.at in
  let note = exp.note in
  match exp.it with
  | BoolE b -> prosify_bool_exp b
  | NumE n -> prosify_num_exp n
  | TextE s -> prosify_text_exp s
  | VarE id -> prosify_var_exp id
  | UnE (unop, optyp, exp) -> prosify_un_exp ctx unop optyp exp
  | BinE (binop, optyp, exp_l, exp_r) ->
      prosify_bin_exp ctx binop optyp exp_l exp_r
  | CmpE (cmpop, optyp, exp_l, exp_r) ->
      prosify_cmp_exp ctx cmpop optyp exp_l exp_r
  | UpCastE (typ, exp) -> prosify_upcast_exp ctx typ exp
  | DownCastE (typ, exp) -> prosify_downcast_exp ctx typ exp
  | SubE (exp, typ) -> prosify_sub_exp ctx exp typ
  | MatchE (exp, pattern) -> prosify_match_exp ctx exp pattern
  | TupleE exps -> prosify_tuple_exp ctx exps
  | CaseE (mixop, exps) -> prosify_case_exp note ctx mixop exps
  | StrE expfields -> prosify_str_exp ctx expfields
  | OptE exp_opt -> prosify_opt_exp ctx exp_opt
  | ListE exps -> prosify_list_exp ctx exps
  | ConsE (exp_h, exp_t) -> prosify_cons_exp ctx exp_h exp_t
  | CatE (exp_l, exp_r) -> prosify_cat_exp ctx exp_l exp_r
  | MemE (exp_e, exp_s) -> prosify_mem_exp ctx exp_e exp_s
  | LenE exp -> prosify_len_exp ctx exp
  | DotE (exp, atom) -> prosify_dot_exp ctx exp atom
  | IdxE (exp_b, exp_i) -> prosify_idx_exp ctx exp_b exp_i
  | SliceE (exp_b, exp_l, exp_h) -> prosify_slice_exp ctx exp_b exp_l exp_h
  | UpdE (exp_b, path, exp_f) -> prosify_upd_exp ctx exp_b path exp_f
  | CallE (id, targs, args) -> prosify_call_exp at note ctx id targs args
  | IterE (exp, iterexp) -> prosify_iter_exp ctx exp iterexp

and prosify_exps (ctx : Ctx.t) (exps : exp list) : Pl.exp list =
  List.map (prosify_exp ctx) exps

(* Boolean expression prosification *)

and prosify_bool_exp (b : bool) : Pl.exp' = Pl.BoolE b

(* Numeric expression prosification *)

and prosify_num_exp (n : Num.t) : Pl.exp' = Pl.NumE n

(* Text expression prosification *)

and prosify_text_exp (s : string) : Pl.exp' = Pl.TextE s

(* Variable expression prosification *)

and prosify_var_exp (id : id) : Pl.exp' = Pl.VarE id

(* Unary expression prosification *)

and prosify_un_exp (ctx : Ctx.t) (unop : unop) (optyp : optyp) (exp : exp) :
    Pl.exp' =
  let exp_pl = prosify_exp ctx exp in
  Pl.UnE (unop, optyp, exp_pl)

(* Binary expression prosification *)

and prosify_bin_exp (ctx : Ctx.t) (binop : binop) (optyp : optyp) (exp_l : exp)
    (exp_r : exp) : Pl.exp' =
  let exp_l_pl = prosify_exp ctx exp_l in
  let exp_r_pl = prosify_exp ctx exp_r in
  Pl.BinE (binop, optyp, exp_l_pl, exp_r_pl)

(* Comparison expression prosification *)

and prosify_cmp_exp (ctx : Ctx.t) (cmpop : cmpop) (optyp : optyp) (exp_l : exp)
    (exp_r : exp) : Pl.exp' =
  let exp_l_pl = prosify_exp ctx exp_l in
  let exp_r_pl = prosify_exp ctx exp_r in
  Pl.CmpE (cmpop, optyp, exp_l_pl, exp_r_pl)

(* Upcast expression prosification *)

and prosify_upcast_exp (ctx : Ctx.t) (typ : typ) (exp : exp) : Pl.exp' =
  let exp_pl = prosify_exp ctx exp in
  Pl.UpCastE (typ, exp_pl)

(* Downcast expression prosification *)

and prosify_downcast_exp (ctx : Ctx.t) (typ : typ) (exp : exp) : Pl.exp' =
  let exp_pl = prosify_exp ctx exp in
  Pl.DownCastE (typ, exp_pl)

(* Subtype check expression prosification *)

and prosify_sub_exp (ctx : Ctx.t) (exp : exp) (typ : typ) : Pl.exp' =
  let exp_pl = prosify_exp ctx exp in
  Pl.SubE (exp_pl, typ)

(* Match expression prosification *)

and prosify_match_exp (ctx : Ctx.t) (exp : exp) (pattern : pattern) : Pl.exp' =
  let exp_pl = prosify_exp ctx exp in
  Pl.MatchE (exp_pl, pattern)

(* Tuple expression prosification *)

and prosify_tuple_exp (ctx : Ctx.t) (exps : exp list) : Pl.exp' =
  let exps_pl = prosify_exps ctx exps in
  Pl.TupleE exps_pl

(* Case expression prosification *)

and prosify_case_exp (note : typ') (ctx : Ctx.t) (mixop : mixop)
    (exps : exp list) : Pl.exp' =
  let exps_pl = prosify_exps ctx exps in
  let tid = match note with VarT (tid, _) -> tid | _ -> assert false in
  let cid = (tid, mixop) in
  let hint_prose_opt = Ctx.find_hint_prose ctx (`Typ cid) in
  Pl.CaseE (tid, mixop, exps_pl, hint_prose_opt)

(* Struct expression prosification *)

and prosify_str_exp (ctx : Ctx.t) (expfields : (atom * exp) list) : Pl.exp' =
  let atoms, exps = List.split expfields in
  let exps_pl = prosify_exps ctx exps in
  let expfields_pl = List.combine atoms exps_pl in
  Pl.StrE expfields_pl

(* Option expression prosification *)

and prosify_opt_exp (ctx : Ctx.t) (exp_opt : exp option) : Pl.exp' =
  let exp_pl_opt = Option.map (prosify_exp ctx) exp_opt in
  Pl.OptE exp_pl_opt

(* List expression prosification *)

and prosify_list_exp (ctx : Ctx.t) (exps : exp list) : Pl.exp' =
  let exps_pl = prosify_exps ctx exps in
  Pl.ListE exps_pl

(* Cons expression prosification *)

and prosify_cons_exp (ctx : Ctx.t) (exp_h : exp) (exp_t : exp) : Pl.exp' =
  let exp_h_pl = prosify_exp ctx exp_h in
  let exp_t_pl = prosify_exp ctx exp_t in
  Pl.ConsE (exp_h_pl, exp_t_pl)

(* Concatenation expression prosification *)

and prosify_cat_exp (ctx : Ctx.t) (exp_l : exp) (exp_r : exp) : Pl.exp' =
  let exp_l_pl = prosify_exp ctx exp_l in
  let exp_r_pl = prosify_exp ctx exp_r in
  Pl.CatE (exp_l_pl, exp_r_pl)

(* Membership expression prosification *)

and prosify_mem_exp (ctx : Ctx.t) (exp_e : exp) (exp_s : exp) : Pl.exp' =
  let exp_e_pl = prosify_exp ctx exp_e in
  let exp_s_pl = prosify_exp ctx exp_s in
  Pl.MemE (exp_e_pl, exp_s_pl)

(* Length expression prosification *)

and prosify_len_exp (ctx : Ctx.t) (exp : exp) : Pl.exp' =
  let exp_pl = prosify_exp ctx exp in
  Pl.LenE exp_pl

(* Dot expression prosification *)

and prosify_dot_exp (ctx : Ctx.t) (exp : exp) (atom : atom) : Pl.exp' =
  let exp_pl = prosify_exp ctx exp in
  Pl.DotE (exp_pl, atom)

(* Index expression prosification *)

and prosify_idx_exp (ctx : Ctx.t) (exp_b : exp) (exp_i : exp) : Pl.exp' =
  let exp_b_pl = prosify_exp ctx exp_b in
  let exp_i_pl = prosify_exp ctx exp_i in
  Pl.IdxE (exp_b_pl, exp_i_pl)

(* Slice expression prosification *)

and prosify_slice_exp (ctx : Ctx.t) (exp_b : exp) (exp_i : exp) (exp_n : exp) :
    Pl.exp' =
  let exp_b_pl = prosify_exp ctx exp_b in
  let exp_i_pl = prosify_exp ctx exp_i in
  let exp_n_pl = prosify_exp ctx exp_n in
  Pl.SliceE (exp_b_pl, exp_i_pl, exp_n_pl)

(* Update expression prosification *)

and prosify_upd_exp (ctx : Ctx.t) (exp_b : exp) (path : path) (exp_f : exp) :
    Pl.exp' =
  let exp_b_pl = prosify_exp ctx exp_b in
  let path_pl = prosify_path ctx path in
  let exp_f_pl = prosify_exp ctx exp_f in
  Pl.UpdE (exp_b_pl, path_pl, exp_f_pl)

(* Call expression prosification *)

and prosify_call_exp (at : region) (note : typ') (ctx : Ctx.t) (id : id)
    (targs : typ list) (args : arg list) : Pl.exp' =
  let func_call =
    let typ_ret = Ctx.unroll_typ ctx (note $ no_region) in
    match typ_ret.it with
    | BoolT -> prosify_call_exp_check at ctx id targs args
    | _ -> prosify_call_exp_yield at ctx id targs args
  in
  Pl.CallE func_call

and prosify_call_exp_check (at : region) (ctx : Ctx.t) (id : id)
    (targs : typ list) (args : arg list) : Pl.func_call =
  let prose_true_opt = Ctx.find_hint_prose_true ctx (`Func id) in
  let prose_false_opt = Ctx.find_hint_prose_false ctx (`Func id) in
  match (prose_true_opt, prose_false_opt) with
  | Some prose_true, Some prose_false ->
      let args_pl = prosify_args ctx args in
      Ctx.validate_hint_alter at prose_true args_pl;
      Ctx.validate_hint_alter at prose_false args_pl;
      Pl.(ProseFuncCall (`Check (id, prose_true, prose_false, targs, args_pl)))
  | _ -> prosify_call_exp_math ctx id targs args

and prosify_call_exp_yield (at : region) (ctx : Ctx.t) (id : id)
    (targs : typ list) (args : arg list) : Pl.func_call =
  let prose_in_opt = Ctx.find_hint_prose_in ctx (`Func id) in
  match prose_in_opt with
  | Some prose_in ->
      let args_pl = prosify_args ctx args in
      Ctx.validate_hint_alter at prose_in args_pl;
      Pl.(ProseFuncCall (`Yield (id, prose_in, targs, args_pl)))
  | None -> prosify_call_exp_math ctx id targs args

and prosify_call_exp_math (ctx : Ctx.t) (id : id) (targs : targ list)
    (args : arg list) : Pl.func_call =
  let args_pl = prosify_args ctx args in
  Pl.MathFuncCall (id, targs, args_pl)

(* Iterated expression prosification *)

and prosify_iter_exp (ctx : Ctx.t) (exp : exp) (iterexp : iterexp) : Pl.exp' =
  let exp_pl = prosify_exp ctx exp in
  Pl.IterE (exp_pl, iterexp)

(* Path prosification *)

and prosify_path (ctx : Ctx.t) (path : path) : Pl.path =
  prosify_path' ctx path $$ (path.at, path.note)

and prosify_path' (ctx : Ctx.t) (path : path) : Pl.path' =
  match path.it with
  | RootP -> Pl.RootP
  | IdxP (path, exp) ->
      let path_pl = prosify_path ctx path in
      let exp_pl = prosify_exp ctx exp in
      Pl.IdxP (path_pl, exp_pl)
  | SliceP (path, exp_l, exp_n) ->
      let path_pl = prosify_path ctx path in
      let exp_l_pl = prosify_exp ctx exp_l in
      let exp_n_pl = prosify_exp ctx exp_n in
      Pl.SliceP (path_pl, exp_l_pl, exp_n_pl)
  | DotP (path, atom) ->
      let path_pl = prosify_path ctx path in
      Pl.DotP (path_pl, atom)

(* Parameter prosification *)

and prosify_param (ctx : Ctx.t) (param : param) : Pl.param =
  match param.it with
  | ExpP (typ, exp) ->
      let exp_pl = prosify_exp ctx exp in
      Pl.ExpP (typ, exp_pl) $ param.at
  | DefP id -> Pl.DefP id $ param.at

and prosify_params (ctx : Ctx.t) (params : param list) =
  List.map (prosify_param ctx) params

(* Argument prosification *)

and prosify_arg (ctx : Ctx.t) (arg : arg) : Pl.arg =
  match arg.it with
  | ExpA exp ->
      let exp = prosify_exp ctx exp in
      Pl.ExpA exp $ arg.at
  | DefA id -> Pl.DefA id $ arg.at

and prosify_args (ctx : Ctx.t) (args : arg list) =
  List.map (prosify_arg ctx) args

(* Instruction prosification *)

and iterate_cond (cond : Pl.cond) (iterexps : iterexp list) : Pl.cond =
  match iterexps with
  | [] -> cond
  | _ ->
      let vars_bound =
        iterexps |> List.map (fun (_, vars) -> vars) |> List.concat
      in
      Pl.ForAllCond (cond, vars_bound)

and iterate_bind (instr : Pl.instr) (iterinstrs : iterinstr list) =
  match iterinstrs with
  | [] -> instr
  | _ ->
      let vars_bound, vars_bind =
        iterinstrs
        |> List.map (fun (_, vars_bound, vars_bind) -> (vars_bound, vars_bind))
        |> List.split
        |> fun (vars_bound_group, vars_bind_group) ->
        (List.concat vars_bound_group, List.concat vars_bind_group)
      in
      Pl.ForEachI (vars_bind, instr, vars_bound) $ no_region

and prosify_instr (ctx : Ctx.t) (instr : instr) : Pl.block =
  let at = instr.at in
  match instr.it with
  | IfI (exp_cond, iterexps, block, _phantom_opt) ->
      prosify_if_instr at ctx exp_cond iterexps block
  | HoldI (id_rel, notexp, iterexps, holdcase) ->
      prosify_hold_instr at ctx id_rel notexp iterexps holdcase
  | CaseI (exp, cases, phantom_opt) ->
      prosify_case_instr at ctx exp cases phantom_opt
  | OtherwiseI block -> prosify_otherwise_instr at ctx block
  | GroupI _ -> assert false
  | LetI (exp_l, exp_r, iterinstrs) ->
      prosify_let_instr at ctx exp_l exp_r iterinstrs
  | RuleI (id, notexp, inputs, iterinstrs) ->
      prosify_rule_instr at ctx id notexp inputs iterinstrs
  | ResultI (rel_signature, exps) ->
      prosify_result_instr at ctx rel_signature exps
  | ReturnI exp -> prosify_return_instr at ctx exp

and prosify_block (ctx : Ctx.t) (block : block) : Pl.block =
  (* Expand nested calls *)
  let block = Expand.expand ctx.frees Expand.expand_nested_calls block in
  (* Prosify instructions *)
  let block_pl = prosify_block' ctx block in
  (* Apply shorthands *)
  Shorthand.apply_check_option_get block_pl

and prosify_block' (ctx : Ctx.t) (block : block) : Pl.block =
  let is_branch_instr instr =
    match instr.it with IfI _ | CaseI _ | OtherwiseI _ -> true | _ -> false
  in
  let num_branch_block = block |> List.filter is_branch_instr |> List.length in
  let rec trailing_block block =
    match block with
    | [] -> []
    | instr_h :: block_t ->
        if is_branch_instr instr_h then block_t else trailing_block block_t
  in
  if num_branch_block = 1 then
    let block_trailing = trailing_block block in
    let ctx =
      match block_trailing with
      | [] -> Ctx.set_branch ctx Check
      | _ -> Ctx.set_branch ctx If
    in
    block |> List.concat_map (prosify_instr ctx)
  else
    let ctx = Ctx.set_branch ctx If in
    block |> List.concat_map (prosify_instr ctx)

(* If instruction prosification *)

and prosify_if_cond (ctx : Ctx.t) (exp : exp) (iterexps : iterexp list) :
    Pl.cond =
  let exp_pl = prosify_exp ctx exp in
  let cond_pl = Pl.ExpCond exp_pl in
  iterate_cond cond_pl iterexps

and prosify_if_instr (at : region) (ctx : Ctx.t) (exp_cond : exp)
    (iterexps : iterexp list) (block_then : block) : Pl.block =
  let cond_pl = prosify_if_cond ctx exp_cond iterexps in
  match ctx.branch with
  | Check ->
      let instr_pl = Pl.CheckI cond_pl $ at in
      let block_then_pl = prosify_block ctx block_then in
      instr_pl :: block_then_pl
  | If ->
      let branch_pl = Pl.If in
      let block_then_pl = prosify_block ctx block_then in
      let instr_pl = Pl.BranchI (branch_pl, cond_pl, block_then_pl) $ at in
      [ instr_pl ]
  | ElseIf ->
      let branch_pl = Pl.ElseIf in
      let block_then_pl = prosify_block ctx block_then in
      let instr_pl = Pl.BranchI (branch_pl, cond_pl, block_then_pl) $ at in
      [ instr_pl ]
  | Else ->
      let branch_pl = Pl.Else in
      let block_then_pl = prosify_block ctx block_then in
      let instr_pl = Pl.BranchI (branch_pl, cond_pl, block_then_pl) $ at in
      [ instr_pl ]
  | Empty -> assert false

(* Hold instruction prosification *)

and prosify_hold_cond ~(hold : bool) (at : region) (ctx : Ctx.t) (id_rel : id)
    (notexp : notexp) (iterexps : iterexp list) : Pl.cond =
  let mixop, exps = notexp in
  let exps_pl = prosify_exps ctx exps in
  let hid = if hold then "prose_true" else "prose_false" in
  let rel_call_pl =
    match Ctx.find_hint_alter ctx hid (`Rel id_rel) with
    | Some prose ->
        Ctx.validate_hint_alter at prose exps;
        Pl.ProseRelCall (`Hold (id_rel, prose, exps_pl))
    | None -> Pl.MathRelCall (id_rel, mixop, exps_pl)
  in
  let cond_pl = Pl.RelCond rel_call_pl in
  iterate_cond cond_pl iterexps

and prosify_hold_instr (at : region) (ctx : Ctx.t) (id_rel : id)
    (notexp : notexp) (iterexps : iterexp list) (holdcase : holdcase) : Pl.block
    =
  match holdcase with
  | BothH (block_hold, block_not_hold) ->
      prosify_hold_both_instr at ctx id_rel notexp iterexps block_hold
        block_not_hold
  | HoldH (block_hold, _) ->
      prosify_hold_only_instr at ctx id_rel notexp iterexps block_hold
  | NotHoldH (block_not_hold, _) ->
      prosify_not_hold_only_instr at ctx id_rel notexp iterexps block_not_hold

and prosify_hold_both_instr (at : region) (ctx : Ctx.t) (id_rel : id)
    (notexp : notexp) (iterexps : iterexp list) (block_hold : block)
    (block_not_hold : block) : Pl.block =
  let cond_hold_pl =
    prosify_hold_cond ~hold:true at ctx id_rel notexp iterexps
  in
  let block_hold_pl = prosify_block ctx block_hold in
  let instr_hold_pl = Pl.BranchI (Pl.If, cond_hold_pl, block_hold_pl) $ at in
  let cond_not_hold_pl =
    prosify_hold_cond ~hold:false at ctx id_rel notexp iterexps
  in
  let block_not_hold_pl = prosify_block ctx block_not_hold in
  let instr_not_hold_pl =
    Pl.BranchI (Pl.Else, cond_not_hold_pl, block_not_hold_pl) $ at
  in
  [ instr_hold_pl; instr_not_hold_pl ]

and prosify_hold_only_instr (at : region) (ctx : Ctx.t) (id_rel : id)
    (notexp : notexp) (iterexps : iterexp list) (block_hold : block) : Pl.block
    =
  let cond_hold_pl =
    prosify_hold_cond ~hold:true at ctx id_rel notexp iterexps
  in
  let block_hold_pl = prosify_block ctx block_hold in
  let instr_check_pl = Pl.CheckI cond_hold_pl $ at in
  instr_check_pl :: block_hold_pl

and prosify_not_hold_only_instr (at : region) (ctx : Ctx.t) (id_rel : id)
    (notexp : notexp) (iterexps : iterexp list) (block_not_hold : block) :
    Pl.block =
  let cond_not_hold_pl =
    prosify_hold_cond ~hold:false at ctx id_rel notexp iterexps
  in
  let block_not_hold_pl = prosify_block ctx block_not_hold in
  let instr_check_pl = Pl.CheckI cond_not_hold_pl $ at in
  instr_check_pl :: block_not_hold_pl

(* Case instruction prosification *)

and prosify_case_cond (ctx : Ctx.t) (exp : exp) (guard : guard) : Pl.cond =
  let exp_pl = prosify_exp ctx exp in
  let exp_pl =
    match guard with
    | BoolG true -> exp_pl
    | BoolG false -> Pl.UnE (`NotOp, `BoolT, exp_pl) $$ (exp.at, exp.note)
    | CmpG (cmpop, optyp, exp_r) ->
        let exp_r_pl = prosify_exp ctx exp_r in
        Pl.CmpE (cmpop, optyp, exp_pl, exp_r_pl) $$ (exp.at, exp.note)
    | SubG typ -> Pl.SubE (exp_pl, typ) $$ (exp.at, exp.note)
    | MatchG pattern -> Pl.MatchE (exp_pl, pattern) $$ (exp.at, exp.note)
    | MemG exp_s ->
        let exp_s_pl = prosify_exp ctx exp_s in
        Pl.MemE (exp_pl, exp_s_pl) $$ (exp.at, exp.note)
  in
  Pl.ExpCond exp_pl

and prosify_case (at : region) (ctx : Ctx.t) (exp : exp) (case : case) :
    Pl.block =
  let guard, block_then = case in
  let cond_pl = prosify_case_cond ctx exp guard in
  let block_then_pl = prosify_block ctx block_then in
  match ctx.branch with
  | Check ->
      let instr_pl = Pl.CheckI cond_pl $ at in
      instr_pl :: block_then_pl
  | If ->
      let branch_pl = Pl.If in
      let instr_pl = Pl.BranchI (branch_pl, cond_pl, block_then_pl) $ at in
      [ instr_pl ]
  | ElseIf ->
      let branch_pl = Pl.ElseIf in
      let instr_pl = Pl.BranchI (branch_pl, cond_pl, block_then_pl) $ at in
      [ instr_pl ]
  | Else ->
      let branch_pl = Pl.Else in
      let instr_pl = Pl.BranchI (branch_pl, cond_pl, block_then_pl) $ at in
      [ instr_pl ]
  | Empty -> assert false

and prosify_cases ~(total : bool) (at : region) (ctx : Ctx.t) (exp : exp)
    (cases : case list) : Pl.block =
  match cases with
  | [] -> assert false
  | [ case ] ->
      let ctx = Ctx.set_branch ctx Check in
      prosify_case at ctx exp case
  | _ ->
      cases
      |> List.mapi (fun idx case ->
             let ctx =
               if idx = 0 then Ctx.set_branch ctx If
               else if idx = List.length cases - 1 && total then
                 Ctx.set_branch ctx Else
               else Ctx.set_branch ctx ElseIf
             in
             prosify_case at ctx exp case)
      |> List.concat

and prosify_case_instr (at : region) (ctx : Ctx.t) (exp : exp)
    (cases : case list) (phantom_opt : phantom option) : Pl.block =
  let total = Option.is_none phantom_opt in
  prosify_cases ~total at ctx exp cases

(* Otherwise instruction prosification *)

and prosify_otherwise_instr (at : region) (ctx : Ctx.t) (block : block) :
    Pl.block =
  let block_pl = prosify_block ctx block in
  let instr_pl = Pl.OtherwiseI block_pl $ at in
  [ instr_pl ]

(* Let instruction prosification *)

and prosify_let_instr (at : region) (ctx : Ctx.t) (exp_l : exp) (exp_r : exp)
    (iterinstrs : iterinstr list) : Pl.block =
  match prosify_let_case_instr at ctx exp_l exp_r iterinstrs with
  | Some block_pl -> block_pl
  | None -> prosify_let_non_case_instr at ctx exp_l exp_r iterinstrs

and prosify_let_case_instr (at : region) (ctx : Ctx.t) (exp_l : exp)
    (exp_r : exp) (iterinstrs : iterinstr list) : Pl.block option =
  match exp_l.it with
  | CaseE (mixop, exps_l) -> (
      let tid =
        match exp_l.note with VarT (tid, _) -> tid | _ -> assert false
      in
      let cid = (tid, mixop) in
      let prose_fields_opt = Ctx.find_hint_prose_fields ctx (`Typ cid) in
      match prose_fields_opt with
      | Some prose_fields ->
          Ctx.validate_hint_fields at prose_fields (List.length exps_l);
          let exps_bind_pl =
            List.map2
              (fun exp_l prose_field ->
                match exp_l.it with
                | Il.VarE id when Id.is_underscored id -> None
                | Il.IterE ({ it = Il.VarE id; _ }, _) when Id.is_underscored id
                  ->
                    None
                | _ ->
                    let exp_pl = prosify_exp ctx exp_l in
                    Some (exp_pl, prose_field))
              exps_l prose_fields
            |> List.filter_map Fun.id
          in
          let exp_r_pl = prosify_exp ctx exp_r in
          let instr_pl = Pl.DestructI (exps_bind_pl, exp_r_pl) $ at in
          let instr_pl = iterate_bind instr_pl iterinstrs in
          Some [ instr_pl ]
      | _ -> None)
  | _ -> None

and prosify_let_non_case_instr (at : region) (ctx : Ctx.t) (exp_l : exp)
    (exp_r : exp) (iterinstrs : iterinstr list) : Pl.block =
  let exp_l_pl = prosify_exp ctx exp_l in
  let exp_r_pl = prosify_exp ctx exp_r in
  let instr_pl = Pl.LetI (exp_l_pl, exp_r_pl) $ at in
  let instr_pl = iterate_bind instr_pl iterinstrs in
  [ instr_pl ]

(* Rule instruction prosification *)

and prosify_rule_instr (at : region) (ctx : Ctx.t) (id_rel : id)
    (notexp : notexp) (inputs : Hints.Input.t) (iterinstrs : iterinstr list) :
    Pl.block =
  let mixop, exps = notexp in
  let exps_input, exps_output = Hints.Input.split inputs exps in
  let exps_input_pl = prosify_exps ctx exps_input in
  let exps_output_pl = prosify_exps ctx exps_output in
  let prose_in_opt = Ctx.find_hint_prose_in ctx (`Rel id_rel) in
  let prose_out_opt = Ctx.find_hint_prose_out ctx (`Rel id_rel) in
  let rel_call_pl =
    match (prose_in_opt, prose_out_opt) with
    | Some prose_in, Some prose_out ->
        Ctx.validate_hint_alter at prose_in exps_input_pl;
        let prose_out_aligned = Hints.Alter.realign prose_out inputs in
        Ctx.validate_hint_alter at prose_out_aligned exps_output_pl;
        Pl.ProseRelCall
          (`Yield
            (id_rel, prose_in, exps_input_pl, prose_out_aligned, exps_output_pl))
    | _ ->
        let exps_pl = Hints.Input.combine inputs exps_input_pl exps_output_pl in
        Pl.MathRelCall (id_rel, mixop, exps_pl)
  in
  let instr_pl = Pl.RuleI rel_call_pl $ at in
  let instr_pl = iterate_bind instr_pl iterinstrs in
  [ instr_pl ]

(* Result instruction prosification *)

and prosify_result_instr (at : region) (ctx : Ctx.t)
    (rel_signature : rel_signature) (exps : exp list) : Pl.block =
  let exps_pl = prosify_exps ctx exps in
  let nottyp, inputs = rel_signature in
  let _, typs = nottyp.it in
  let result_pl =
    if Hints.Input.is_conditional inputs typs then Pl.ProseResult `Hold
    else
      let id_rel = Ctx.get_namespace ctx in
      match Ctx.find_hint_prose_out ctx (`Rel id_rel) with
      | Some prose_out ->
          let prose_out_aligned = Hints.Alter.realign prose_out inputs in
          Ctx.validate_hint_alter at prose_out_aligned exps_pl;
          Pl.ProseResult (`Yield (prose_out_aligned, exps_pl))
      | None -> Pl.MathResult exps_pl
  in
  let instr_pl = Pl.ResultI result_pl $ at in
  [ instr_pl ]

(* Return instruction prosification *)

and prosify_return_instr (at : region) (ctx : Ctx.t) (exp : exp) : Pl.block =
  let exp_pl = prosify_exp ctx exp in
  let instr_pl = Pl.ReturnI exp_pl $ at in
  [ instr_pl ]

(* Definition prosification *)

let rec prosify_def (ctx : Ctx.t) (def : def) : Pl.def option =
  let wrap_some def = Some def in
  match def.it with
  | ExternTypD _ | TypD _ -> None
  | ExternRelD externrel ->
      prosify_extern_rel_def ctx def.at externrel |> wrap_some
  | RelD rel -> prosify_defined_rel_def ctx def.at rel |> wrap_some
  | ExternDecD externfunc ->
      prosify_extern_func_def ctx def.at externfunc |> wrap_some
  | BuiltinDecD builtinfunc ->
      prosify_builtin_func_def ctx def.at builtinfunc |> wrap_some
  | TableDecD tablefunc ->
      prosify_table_func_def ctx def.at tablefunc |> wrap_some
  | FuncDecD definedfunc ->
      prosify_defined_func_def ctx def.at definedfunc |> wrap_some

(* Relation prosification *)

and prosify_rel_title (ctx : Ctx.t) (id_rel : id)
    (rel_signature : rel_signature) (exps_input : exp list) : Pl.rel_title =
  let nottyp, inputs = rel_signature in
  let _, typs = nottyp.it in
  if Hints.Input.is_conditional inputs typs then
    prosify_rel_hold_title ctx id_rel rel_signature exps_input
  else prosify_rel_yield_title ctx id_rel rel_signature exps_input

and prosify_rel_hold_title (ctx : Ctx.t) (id_rel : id)
    (rel_signature : rel_signature) (exps_input : exp list) : Pl.rel_title =
  let prose_true_opt = Ctx.find_hint_prose_true ctx (`Rel id_rel) in
  let prose_false_opt = Ctx.find_hint_prose_false ctx (`Rel id_rel) in
  let exps_input_pl = prosify_exps ctx exps_input in
  match (prose_true_opt, prose_false_opt) with
  | Some prose_true, Some prose_false ->
      Ctx.validate_hint_alter id_rel.at prose_true exps_input;
      Ctx.validate_hint_alter id_rel.at prose_false exps_input;
      Pl.ProseRelTitle (`Hold (id_rel, prose_true, exps_input_pl))
  | _ -> prosify_rel_math_title ctx id_rel rel_signature exps_input

and prosify_rel_yield_title (ctx : Ctx.t) (id_rel : id)
    (rel_signature : rel_signature) (exps_input : exp list) : Pl.rel_title =
  let prose_in_opt = Ctx.find_hint_prose_in ctx (`Rel id_rel) in
  let prose_out_opt = Ctx.find_hint_prose_out ctx (`Rel id_rel) in
  match (prose_in_opt, prose_out_opt) with
  | Some prose_in, Some prose_out ->
      let nottyp, inputs = rel_signature in
      let _, typs = nottyp.it in
      let typs_input, typs_output = Hints.Input.split inputs typs in
      Ctx.validate_hint_alter id_rel.at prose_in exps_input;
      let prose_out_aligned = Hints.Alter.realign prose_out inputs in
      let exps_input_pl =
        List.map
          (fun typ ->
            let exp, _ = Il.Fresh.fresh_exp_from_typ IdSet.empty typ in
            prosify_exp ctx exp)
          typs_input
      in
      let exps_output_pl =
        List.map
          (fun typ ->
            let exp, _ = Il.Fresh.fresh_exp_from_typ IdSet.empty typ in
            prosify_exp ctx exp)
          typs_output
      in
      Ctx.validate_hint_alter id_rel.at prose_out_aligned exps_output_pl;
      Pl.ProseRelTitle
        (`Yield
          (id_rel, prose_in, exps_input_pl, prose_out_aligned, exps_output_pl))
  | _ -> prosify_rel_math_title ctx id_rel rel_signature exps_input

and prosify_rel_math_title (ctx : Ctx.t) (id_rel : id)
    (rel_signature : rel_signature) (exps_input : exp list) : Pl.rel_title =
  let nottyp, inputs = rel_signature in
  let mixop, _ = nottyp.it in
  let exps_input_pl = prosify_exps ctx exps_input in
  let exps_input_pl_indexed = List.combine inputs exps_input_pl in
  let exps_pl =
    List.init
      (List.length mixop - 1)
      (fun idx ->
        match List.assoc_opt idx exps_input_pl_indexed with
        | Some exp_pl -> exp_pl
        | None -> prosify_hole_exp ())
  in
  Pl.MathRelTitle (id_rel, mixop, exps_pl)

(* Extern relation definition prosification *)

and prosify_extern_rel_def (ctx : Ctx.t) (at : region) (externrel : externrel) :
    Pl.def =
  let id_rel, rel_signature, exps_input, _ = externrel in
  let ctx = Ctx.enter_rel ctx id_rel in
  let rel_title_pl = prosify_rel_title ctx id_rel rel_signature exps_input in
  Pl.ExternRelD rel_title_pl $ at

(* Defined relation definition prosification *)

and collect_rulegroups_instr (instr : instr) :
    (id * rel_signature * exp list * block) list =
  match instr.it with
  | IfI (_, _, block_then, _) -> collect_rulegroups_block block_then
  | HoldI (_, _, _, holdcase) -> (
      match holdcase with
      | BothH (block_hold, block_nothold) ->
          collect_rulegroups_block block_hold
          @ collect_rulegroups_block block_nothold
      | HoldH (block_hold, _) -> collect_rulegroups_block block_hold
      | NotHoldH (block_nothold, _) -> collect_rulegroups_block block_nothold)
  | CaseI (_, cases, _) ->
      let block_group = cases |> List.map snd in
      block_group |> List.map collect_rulegroups_block |> List.concat
  | GroupI (id_rulegroup, rel_signature, exps_input, block) ->
      [ (id_rulegroup, rel_signature, exps_input, block) ]
  | _ -> []

and collect_rulegroups_block (block : block) :
    (id * rel_signature * exp list * block) list =
  block |> List.map collect_rulegroups_instr |> List.concat

and prosify_rulegroup_title (ctx : Ctx.t) (id_rel : id) (id_rulegroup : id)
    (rel_signature : rel_signature) (exps_input : exp list) : Pl.rulegroup_title
    =
  let nottyp, inputs = rel_signature in
  let _, typs = nottyp.it in
  if Hints.Input.is_conditional inputs typs then
    prosify_rulegroup_hold_title ctx id_rel id_rulegroup rel_signature
      exps_input
  else
    prosify_rulegroup_yield_title ctx id_rel id_rulegroup rel_signature
      exps_input

and prosify_rulegroup_hold_title (ctx : Ctx.t) (id_rel : id) (id_rulegroup : id)
    (rel_signature : rel_signature) (exps_input : exp list) : Pl.rulegroup_title
    =
  let prose_true_opt = Ctx.find_hint_prose_true ctx (`Rel id_rel) in
  match prose_true_opt with
  | Some prose_true ->
      let exps_input_pl = prosify_exps ctx exps_input in
      Ctx.validate_hint_alter id_rel.at prose_true exps_input;
      Pl.ProseRuleTitle (`Hold (id_rulegroup, prose_true, exps_input_pl))
  | _ ->
      prosify_rulegroup_math_title ctx id_rel id_rulegroup rel_signature
        exps_input

and prosify_rulegroup_yield_title (ctx : Ctx.t) (id_rel : id)
    (id_rulegroup : id) (rel_signature : rel_signature) (exps_input : exp list)
    : Pl.rulegroup_title =
  let prose_in_opt = Ctx.find_hint_prose_in ctx (`Rel id_rel) in
  match prose_in_opt with
  | Some prose_in ->
      let exps_input_pl = prosify_exps ctx exps_input in
      Ctx.validate_hint_alter id_rel.at prose_in exps_input;
      Pl.ProseRuleTitle (`Yield (id_rulegroup, prose_in, exps_input_pl))
  | None ->
      prosify_rulegroup_math_title ctx id_rel id_rulegroup rel_signature
        exps_input

and prosify_rulegroup_math_title (ctx : Ctx.t) (_id_rel : id)
    (id_rulegroup : id) (rel_signature : rel_signature) (exps_input : exp list)
    : Pl.rulegroup_title =
  let nottyp, inputs = rel_signature in
  let mixop, _ = nottyp.it in
  let exps_input_pl = prosify_exps ctx exps_input in
  let epxs_input_pl_indexed = List.combine inputs exps_input_pl in
  let exps_pl =
    List.init
      (List.length mixop - 1)
      (fun idx ->
        match List.assoc_opt idx epxs_input_pl_indexed with
        | Some exp_pl -> exp_pl
        | None -> prosify_hole_exp ())
  in
  Pl.MathRuleTitle (id_rulegroup, mixop, exps_pl)

and prosify_rulegroup (ctx : Ctx.t) (id_rel : id)
    (rulegroup : id * rel_signature * exp list * block) : Pl.rulegroup =
  let id_rulegroup, rel_signature, exps_input, block = rulegroup in
  let rulegroup_title_pl =
    prosify_rulegroup_title ctx id_rel id_rulegroup rel_signature exps_input
  in
  let ctx =
    let frees =
      IdSet.union (Sl.Free.free_exps exps_input) (Ll.Free.free_block block)
    in
    Ctx.set_free ctx frees
  in
  let block_pl = prosify_block ctx block in
  (rulegroup_title_pl, block_pl)

and prosify_rulegroups (ctx : Ctx.t) (id_rel : id)
    (rulegroups : (id * rel_signature * exp list * block) list) :
    Pl.rulegroup list =
  List.map (prosify_rulegroup ctx id_rel) rulegroups

and prosify_defined_rel_def (ctx : Ctx.t) (at : region) (rel : rel) : Pl.def =
  let id_rel, rel_signature, exps_match, block, elseblock_opt, _ = rel in
  let ctx = Ctx.enter_rel ctx id_rel in
  let rel_title_pl = prosify_rel_title ctx id_rel rel_signature exps_match in
  let block =
    Linearize.linearize_block block
    @ Linearize.linearize_elseblock_opt elseblock_opt
  in
  let rulegroups = collect_rulegroups_block block in
  let rulegroups_pl = prosify_rulegroups ctx id_rel rulegroups in
  Pl.RelD (rel_title_pl, rulegroups_pl) $ at

(* Function prosification *)

and prosify_func_title (ctx : Ctx.t) (id_def : id) (tparams : tparam list)
    (params : param list) (typ_ret : typ) : Pl.func_title =
  let typ_ret_unrolled = Ctx.unroll_typ ctx typ_ret in
  match typ_ret_unrolled.it with
  | BoolT -> prosify_func_title_check ctx id_def tparams params typ_ret
  | _ -> prosify_func_title_yield ctx id_def tparams params typ_ret

and prosify_func_title_check (ctx : Ctx.t) (id_def : id) (tparams : tparam list)
    (params : param list) (typ_ret : typ) : Pl.func_title =
  let prose_true_opt = Ctx.find_hint_prose_true ctx (`Func id_def) in
  let prose_false_opt = Ctx.find_hint_prose_false ctx (`Func id_def) in
  match (prose_true_opt, prose_false_opt) with
  | Some prose_true, Some prose_false ->
      let params_pl = prosify_params ctx params in
      Ctx.validate_hint_alter id_def.at prose_true params_pl;
      Ctx.validate_hint_alter id_def.at prose_false params_pl;
      Pl.ProseFuncTitle (`Check (id_def, prose_true, params_pl))
  | _ -> prosify_func_title_math ctx id_def tparams params typ_ret

and prosify_func_title_yield (ctx : Ctx.t) (id_def : id) (tparams : tparam list)
    (params : param list) (typ_ret : typ) : Pl.func_title =
  let prose_in_opt = Ctx.find_hint_prose_in ctx (`Func id_def) in
  match prose_in_opt with
  | Some prose_in ->
      let params_pl = prosify_params ctx params in
      Ctx.validate_hint_alter id_def.at prose_in params_pl;
      Pl.ProseFuncTitle (`Yield (id_def, prose_in, params_pl))
  | None -> prosify_func_title_math ctx id_def tparams params typ_ret

and prosify_func_title_math (ctx : Ctx.t) (id_def : id) (tparams : tparam list)
    (params : param list) (_typ_ret : typ) : Pl.func_title =
  let params_pl = prosify_params ctx params in
  Pl.MathFuncTitle (id_def, tparams, params_pl)

(* Extern function definition prosification *)

and prosify_extern_func_def (ctx : Ctx.t) (at : region)
    (externfunc : externfunc) : Pl.def =
  let id, tparams, params, typ_ret, _ = externfunc in
  let ctx_local = Ctx.add_tparams ctx tparams in
  let func_title_pl = prosify_func_title ctx_local id tparams params typ_ret in
  Pl.ExternDecD func_title_pl $ at

(* Builtin function definition prosification *)

and prosify_builtin_func_def (ctx : Ctx.t) (at : region)
    (builtinfunc : builtinfunc) : Pl.def =
  let id, tparams, params, typ_ret, _ = builtinfunc in
  let ctx_local = Ctx.add_tparams ctx tparams in
  let func_title_pl = prosify_func_title ctx_local id tparams params typ_ret in
  Pl.BuiltinDecD func_title_pl $ at

(* Table function definition prosification *)

and prosify_tablerow (ctx : Ctx.t) (tablerow : tablerow) : Pl.tablerow =
  let exps_input, exp_output, block = tablerow in
  let block = Linearize.linearize_block block in
  let exps_input_pl = prosify_exps ctx exps_input in
  let exp_output_pl = prosify_exp ctx exp_output in
  let ctx =
    let frees =
      IdSet.union
        (Sl.Free.free_exps exps_input)
        (IdSet.union (Sl.Free.free_exp exp_output) (Ll.Free.free_block block))
    in
    Ctx.set_free ctx frees
  in
  let block_pl = prosify_block ctx block in
  (exps_input_pl, exp_output_pl, block_pl)

and prosify_tablerows (ctx : Ctx.t) (tablerows : tablerow list) :
    Pl.tablerow list =
  List.map (prosify_tablerow ctx) tablerows

and prosify_table_func_def (ctx : Ctx.t) (at : region) (tablefunc : tablefunc) :
    Pl.def =
  let id, params, typ_ret, tablerows, _ = tablefunc in
  let func_title_pl = prosify_func_title ctx id [] params typ_ret in
  let tablerows_pl = prosify_tablerows ctx tablerows in
  Pl.TableDecD (func_title_pl, tablerows_pl) $ at

(* Defined function definition prosification *)

and prosify_defined_func_def (ctx : Ctx.t) (at : region)
    (definedfunc : definedfunc) : Pl.def =
  let id, tparams, params, typ_ret, block, elseblock_opt, _ = definedfunc in
  let ctx_local = Ctx.add_tparams ctx tparams in
  let func_title_pl = prosify_func_title ctx_local id tparams params typ_ret in
  let block =
    Linearize.linearize_block block
    @ Linearize.linearize_elseblock_opt elseblock_opt
  in
  let ctx =
    let frees =
      IdSet.union (Sl.Free.free_params params) (Ll.Free.free_block block)
    in
    Ctx.set_free ctx frees
  in
  let block_pl = prosify_block ctx block in
  Pl.FuncDecD (func_title_pl, block_pl) $ at

(* Entry point *)

let prosify_spec (spec : spec) : Pl.spec =
  let ctx = Ctx.init spec in
  List.filter_map (prosify_def ctx) spec
