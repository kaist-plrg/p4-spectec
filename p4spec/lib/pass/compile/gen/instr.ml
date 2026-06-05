open Lang
open Sl
open Util.Source

(* Instructions *)

let rec compile_instr (ctx : Ctx.t) (instr : instr) : Ctx.t * Ml.expr =
  match instr.it with
  | IfI (exp_cond, iterexps, block, _) ->
      compile_if_instr ctx exp_cond iterexps block
  | HoldI (id, notexp, iterexps, holdcase) ->
      compile_hold_instr ctx id notexp iterexps holdcase
  | CaseI (exp, cases, _) -> compile_case_instr ctx exp cases
  | GroupI (_, _, _, block) -> compile_group_instr ctx block
  | LetI (exp_l, exp_r, iterinstrs, block) ->
      compile_let_instr ctx exp_l exp_r iterinstrs block
  | RuleI (id, notexp, inputs, iterinstrs, block) ->
      compile_rule_instr ctx id notexp inputs iterinstrs block
  | ResultI (_, exps) -> compile_result_instr ctx exps
  | ReturnI exp -> compile_return_instr ctx exp
  | DebugI exp -> compile_debug_instr ctx exp

(* If instruction (no iterexps): [if exp_cond then block]

   [if compile_exp exp_cond then <compile_block block> else raise (Unmatch "if failed")]

   If instruction (list iterexps): iterexps wrapped in reverse order
   List iter: [List.for_all (fun elem -> <inner cond>) guide__star]
   Opt iter:  [match guide__quest with None -> true | Some elem -> <inner cond>] *)

and compile_if_cond (ctx : Ctx.t) (exp_cond : exp) : Ctx.t * Ml.expr =
  (* Base case: compile condition expression directly *)
  Exp.compile_exp ctx exp_cond

and compile_if_cond_list (ctx : Ctx.t) (exp_cond : exp) (vars : var list)
    (iterexps_t : iterexp list) : Ctx.t * Ml.expr =
  let ctx_outer = ctx in
  let n = List.length vars in
  (* Fetch guiding list variables *)
  let ids_guide_ml =
    List.map
      (fun (id, _, iters) -> Ctx.find_binding ctx (id, iters @ [ Il.List ]))
      vars
  in
  (* Create stubs for element vars *)
  let ctx, ids_elem_ml = Stub.OCaml.iterator ~prefix:"iter_cond__" ctx vars in
  (* Combine multiple list guides into a single list of tuples *)
  let ctx, expr_guide_ml =
    match ids_guide_ml with
    | [ id_guide_ml ] -> (ctx, Ml.VarE id_guide_ml)
    | _ ->
        let ctx = Ctx.add_list_arity ctx n in
        ( ctx,
          Ml.AppE
            ( Ml.VarE ("List.combine" ^ string_of_int n),
              List.map (fun id_guide_ml -> Ml.VarE id_guide_ml) ids_guide_ml )
        )
  in
  (* Build element pattern for lambda *)
  let pat_elem_ml =
    match ids_elem_ml with
    | [ id_elem_ml ] -> Ml.VarP id_elem_ml
    | _ ->
        Ml.TupleP (List.map (fun id_elem_ml -> Ml.VarP id_elem_ml) ids_elem_ml)
  in
  (* Compile inner condition with element vars bound *)
  let ctx_inner, expr_inner_ml = compile_if_cond_iter ctx exp_cond iterexps_t in
  (* Promote preamble from inner scope *)
  let ctx = Ctx.promote_preamble ctx_inner ctx_outer in
  (* Build List.for_all *)
  let expr_ml =
    Ml.AppE
      ( Ml.VarE "List.for_all",
        [ Ml.FunE ([ pat_elem_ml ], expr_inner_ml); expr_guide_ml ] )
  in
  (ctx, expr_ml)

and compile_if_cond_opt (ctx : Ctx.t) (exp_cond : exp) (vars : var list)
    (iterexps_t : iterexp list) : Ctx.t * Ml.expr =
  let ctx_outer = ctx in
  let n = List.length vars in
  (* Fetch guiding option variables *)
  let ids_guide_ml =
    List.map
      (fun (id, _, iters) -> Ctx.find_binding ctx (id, iters @ [ Il.Opt ]))
      vars
  in
  (* Create stubs for element vars *)
  let ctx, ids_elem_ml = Stub.OCaml.iterator ~prefix:"iter_cond__" ctx vars in
  (* Combine multiple option guides into an option of a tuple *)
  let ctx, expr_guide_ml =
    match ids_guide_ml with
    | [ id_guide_ml ] -> (ctx, Ml.VarE id_guide_ml)
    | _ ->
        let ctx = Ctx.add_opt_arity ctx n in
        ( ctx,
          Ml.AppE
            ( Ml.VarE ("Option.combine" ^ string_of_int n),
              List.map (fun id_guide_ml -> Ml.VarE id_guide_ml) ids_guide_ml )
        )
  in
  (* Build element pattern for Some branch *)
  let pat_elem_ml =
    match ids_elem_ml with
    | [ id_elem_ml ] -> Ml.VarP id_elem_ml
    | _ ->
        Ml.TupleP (List.map (fun id_elem_ml -> Ml.VarP id_elem_ml) ids_elem_ml)
  in
  (* Compile inner condition with element vars bound *)
  let ctx_inner, expr_inner_ml = compile_if_cond_iter ctx exp_cond iterexps_t in
  (* Promote preamble from inner scope *)
  let ctx = Ctx.promote_preamble ctx_inner ctx_outer in
  (* Build match: None -> true (vacuous), Some elem -> inner *)
  let expr_ml =
    Ml.MatchE
      ( expr_guide_ml,
        [
          (Ml.OptP None, Ml.BoolE true);
          (Ml.OptP (Some pat_elem_ml), expr_inner_ml);
        ] )
  in
  (ctx, expr_ml)

and compile_if_cond_iter (ctx : Ctx.t) (exp_cond : exp)
    (iterexps_rev : iterexp list) : Ctx.t * Ml.expr =
  match iterexps_rev with
  | [] -> compile_if_cond ctx exp_cond
  | (iter, vars) :: iterexps_t -> (
      match iter with
      | Il.List -> compile_if_cond_list ctx exp_cond vars iterexps_t
      | Il.Opt -> compile_if_cond_opt ctx exp_cond vars iterexps_t)

and compile_if_instr (ctx : Ctx.t) (exp_cond : exp) (iterexps : iterexp list)
    (block_then : block) : Ctx.t * Ml.expr =
  let iterexps_rev = List.rev iterexps in
  (* Compile condition, wrapping iterexps from innermost outward *)
  let ctx, expr_cond_ml = compile_if_cond_iter ctx exp_cond iterexps_rev in
  let ctx, expr_then_ml = compile_block ctx block_then in
  ( ctx,
    Ml.IfE (expr_cond_ml, expr_then_ml, Some (Common.raise_unmatch "if failed"))
  )

(* Hold instruction: [if id(notexp) holds then block_hold else block_not_hold]

   Base case (no iterexps):
   [
     let holds__ =
       try let _ = r__id(inputs) in true
       with Unmatch _ -> false
     in
     if holds__ then <block_hold> else <block_not_hold / raise_unmatch>
   ]

   List iter: [List.for_all (fun elem -> <inner_hold>) guide__star]
   Opt iter:  [match guide__quest with None -> true | Some elem -> <inner_hold>]
   (None -> true: vacuously holds when element is absent, same as IfI) *)

and compile_hold_cond (ctx : Ctx.t) (id : id) (notexp : notexp) :
    Ctx.t * Ml.expr =
  let exps_input = Domain.Mixfix.args notexp in
  let ctx, exprs_input_ml = Exp.compile_exps ctx exps_input in
  let id_rel_ml = Names.rel id in
  let expr_call_ml = Ml.AppE (Ml.VarE id_rel_ml, exprs_input_ml) in
  let expr_hold_ml =
    Ml.TryE
      ( Ml.LetE (Ml.WildP, expr_call_ml, Ml.BoolE true),
        [ (Ml.VariantP (`Mono ("Unmatch", [ Ml.WildP ])), Ml.BoolE false) ] )
  in
  (ctx, expr_hold_ml)

and compile_hold_cond_list (ctx : Ctx.t) (id : id) (notexp : notexp)
    (vars : var list) (iterexps_t : iterexp list) : Ctx.t * Ml.expr =
  let ctx_outer = ctx in
  let n = List.length vars in
  (* Fetch guiding list variables *)
  let ids_guide_ml =
    List.map
      (fun (id, _, iters) -> Ctx.find_binding ctx (id, iters @ [ Il.List ]))
      vars
  in
  (* Create stubs for element vars *)
  let ctx, ids_elem_ml = Stub.OCaml.iterator ~prefix:"iter_hold__" ctx vars in
  (* Combine multiple list guides into a single list of tuples *)
  let ctx, expr_guide_ml =
    match ids_guide_ml with
    | [ id_guide_ml ] -> (ctx, Ml.VarE id_guide_ml)
    | _ ->
        let ctx = Ctx.add_list_arity ctx n in
        ( ctx,
          Ml.AppE
            ( Ml.VarE ("List.combine" ^ string_of_int n),
              List.map (fun id_guide_ml -> Ml.VarE id_guide_ml) ids_guide_ml )
        )
  in
  (* Build element pattern for lambda *)
  let pat_elem_ml =
    match ids_elem_ml with
    | [ id_elem_ml ] -> Ml.VarP id_elem_ml
    | _ ->
        Ml.TupleP (List.map (fun id_elem_ml -> Ml.VarP id_elem_ml) ids_elem_ml)
  in
  (* Compile inner hold condition with element vars bound *)
  let ctx, expr_inner_ml = compile_hold_cond_iter ctx id notexp iterexps_t in
  (* Promote preamble from inner scope *)
  let ctx = Ctx.promote_preamble ctx ctx_outer in
  (* Hold holds iff it holds for all list elements *)
  let expr_ml =
    Ml.AppE
      ( Ml.VarE "List.for_all",
        [ Ml.FunE ([ pat_elem_ml ], expr_inner_ml); expr_guide_ml ] )
  in
  (ctx, expr_ml)

and compile_hold_cond_opt (ctx : Ctx.t) (id : id) (notexp : notexp)
    (vars : var list) (iterexps_t : iterexp list) : Ctx.t * Ml.expr =
  let ctx_outer = ctx in
  let n = List.length vars in
  (* Fetch guiding option variables *)
  let ids_guide_ml =
    List.map
      (fun (id, _, iters) -> Ctx.find_binding ctx (id, iters @ [ Il.Opt ]))
      vars
  in
  (* Create stubs for element vars *)
  let ctx, ids_elem_ml = Stub.OCaml.iterator ~prefix:"iter_hold__" ctx vars in
  (* Combine multiple option guides into an option of a tuple *)
  let ctx, expr_guide_ml =
    match ids_guide_ml with
    | [ id_guide_ml ] -> (ctx, Ml.VarE id_guide_ml)
    | _ ->
        let ctx = Ctx.add_opt_arity ctx n in
        ( ctx,
          Ml.AppE
            ( Ml.VarE ("Option.combine" ^ string_of_int n),
              List.map (fun id_guide_ml -> Ml.VarE id_guide_ml) ids_guide_ml )
        )
  in
  (* Build element pattern for Some branch *)
  let pat_elem_ml =
    match ids_elem_ml with
    | [ id_elem_ml ] -> Ml.VarP id_elem_ml
    | _ ->
        Ml.TupleP (List.map (fun id_elem_ml -> Ml.VarP id_elem_ml) ids_elem_ml)
  in
  (* Compile inner hold condition with element vars bound *)
  let ctx, expr_inner_ml = compile_hold_cond_iter ctx id notexp iterexps_t in
  (* Promote preamble from inner scope *)
  let ctx = Ctx.promote_preamble ctx ctx_outer in
  (* Build match: None -> true (vacuous), Some -> inner hold condition *)
  let expr_ml =
    Ml.MatchE
      ( expr_guide_ml,
        [
          (Ml.OptP None, Ml.BoolE true);
          (Ml.OptP (Some pat_elem_ml), expr_inner_ml);
        ] )
  in
  (ctx, expr_ml)

and compile_hold_cond_iter (ctx : Ctx.t) (id : id) (notexp : notexp)
    (iterexps_rev : iterexp list) : Ctx.t * Ml.expr =
  match iterexps_rev with
  | [] -> compile_hold_cond ctx id notexp
  | (iter, vars) :: iterexps_t -> (
      match iter with
      | Il.List -> compile_hold_cond_list ctx id notexp vars iterexps_t
      | Il.Opt -> compile_hold_cond_opt ctx id notexp vars iterexps_t)

and compile_hold_instr (ctx : Ctx.t) (id : id) (notexp : notexp)
    (iterexps : iterexp list) (holdcase : holdcase) : Ctx.t * Ml.expr =
  let ctx_outer = ctx in
  (* Process iterexps innermost-first, matching interpreter's List.rev *)
  let iterexps_rev = List.rev iterexps in
  let ctx, expr_hold_ml = compile_hold_cond_iter ctx id notexp iterexps_rev in
  (* Bind hold__ so holdcase branches can reference it *)
  let ctx, id_hold_ml = Stub.OCaml.var ctx "hold__" in
  let ctx, expr_body_ml =
    match holdcase with
    | BothH (block_hold, block_nothold) ->
        (* Both branches reachable; no Unmatch on either path *)
        let ctx, expr_hold_ml = compile_block ctx block_hold in
        let ctx, expr_nothold_ml = compile_block ctx block_nothold in
        let expr_body_ml =
          Ml.IfE (Ml.VarE id_hold_ml, expr_hold_ml, Some expr_nothold_ml)
        in
        (ctx, expr_body_ml)
    | HoldH (block_hold, _) ->
        (* If condition does not hold: Unmatch -> fall through to next sibling *)
        let ctx, expr_hold_ml = compile_block ctx block_hold in
        let expr_body_ml =
          Ml.IfE
            ( Ml.VarE id_hold_ml,
              expr_hold_ml,
              Some (Common.raise_unmatch "hold failed") )
        in
        (ctx, expr_body_ml)
    | NotHoldH (block_nothold, _) ->
        (* If condition holds: Unmatch -> fall through to next sibling *)
        let ctx, expr_nothold_ml = compile_block ctx block_nothold in
        let expr_body_ml =
          Ml.IfE
            ( Ml.UnopE ("not", Ml.VarE id_hold_ml),
              expr_nothold_ml,
              Some (Common.raise_unmatch "not-hold failed") )
        in
        (ctx, expr_body_ml)
  in
  let ctx = Ctx.promote_preamble ctx ctx_outer in
  let expr_ml = Ml.LetE (Ml.VarP id_hold_ml, expr_hold_ml, expr_body_ml) in
  (ctx, expr_ml)

(* Case instruction: [case exp { guard => block; ... }]

   [
     let case__ = <compile_exp exp> in
     if <compile_guard guard0> then <compile_block block0>
     else if <compile_guard guard1> then <compile_block block1>
     ...
     else raise (Unmatch "no case matched")
   ]

   Guard semantics:
     BoolG true  ->  case__
     BoolG false ->  not case__
     CmpG(op,t,r)->  case__ op r
     SubG typ    ->  case__ <: typ
     MatchG pat  ->  case__ matches pat
     MemG exp_s  ->  case__ in exp_s *)

and compile_guard (ctx : Ctx.t) (exp_scrut : exp) (guard : guard) :
    Ctx.t * Ml.expr =
  match guard with
  | BoolG b ->
      let ctx, expr_ml = Exp.compile_exp ctx exp_scrut in
      let expr_ml = if b then expr_ml else Ml.UnopE ("not", expr_ml) in
      (ctx, expr_ml)
  | CmpG (cmpop, optyp, exp_r) ->
      Exp.compile_cmp_exp ctx cmpop optyp exp_scrut exp_r
  | SubG typ -> Exp.compile_sub_exp ctx exp_scrut typ
  | MatchG pattern -> Exp.compile_match_exp ctx exp_scrut pattern
  | MemG exp_s -> Exp.compile_mem_exp ctx exp_scrut exp_s

and compile_case_instr (ctx : Ctx.t) (exp : exp) (cases : case list) :
    Ctx.t * Ml.expr =
  let ctx_outer = ctx in
  (* Compile scrutinee and bind to a fresh variable so it is evaluated once *)
  let ctx, expr_scrut_ml = Exp.compile_exp ctx exp in
  let ctx, id_scrut_ml = Stub.OCaml.var ctx "case__" in
  let exp_scrut = Stub.SpecTec.var id_scrut_ml (exp.note $ exp.at) in
  (* Build nested if-else chain: first case is outermost, last is innermost *)
  let ctx, expr_body_ml =
    List.fold_right
      (fun (guard, block) (ctx, expr_else_ml) ->
        (* Compile guard condition against the bound scrutinee *)
        let ctx, expr_cond_ml = compile_guard ctx exp_scrut guard in
        let ctx, expr_block_ml = compile_block ctx block in
        let expr_body_ml =
          Ml.IfE (expr_cond_ml, expr_block_ml, Some expr_else_ml)
        in
        (ctx, expr_body_ml))
      cases
      (ctx, Common.raise_unmatch "no case matched")
  in
  (* Promote preamble to outer context *)
  let ctx = Ctx.promote_preamble ctx ctx_outer in
  (* Build let expression *)
  let expr_ml = Ml.LetE (Ml.VarP id_scrut_ml, expr_scrut_ml, expr_body_ml) in
  (ctx, expr_ml)

(* Group instruction: [group { block }]

   [compile_block block] *)

and compile_group_instr (ctx : Ctx.t) (block : block) : Ctx.t * Ml.expr =
  compile_block ctx block

(* Let instruction (no iterinstrs): [let exp_l = exp_r in cont]

   [
     let <bind exp_l = compile_exp exp_r> in
     <cont block>
   ]

   Let instruction (list iterinstrs): [let (a*, ..) = (e* where a* <- x*..) in cont]

   [
     let (a__star, ..) =
       List.splitN (List.map (fun x -> <inner body producing (a,..)>) x__star)
     in
     <cont block>
   ]

   Opt iterinstrs: analogous with Option.map / Option.splitN *)

and compile_let (ctx : Ctx.t) (exp_l : exp) (exp_r : exp)
    (cont : Ctx.t -> Ctx.t * Ml.expr) : Ctx.t * Ml.expr =
  let ctx, expr_r_ml = Exp.compile_exp ctx exp_r in
  let ctx, chain = Bind.compile ctx expr_r_ml exp_l in
  let ctx, expr_result_ml = cont ctx in
  let expr_ml = Chain.apply chain expr_result_ml in
  (ctx, expr_ml)

and compile_let_opt (ctx : Ctx.t) (exp_l : exp) (exp_r : exp)
    (vars_bound : var list) (vars_bind : var list) (iterinstrs : iterinstr list)
    (cont : Ctx.t -> Ctx.t * Ml.expr) : Ctx.t * Ml.expr =
  let ctx_outer = ctx in
  let n_bound = List.length vars_bound in
  let n_bind = List.length vars_bind in
  (* Fetch guiding variables *)
  let ids_guide_ml =
    List.map
      (fun (id, _, iters) -> Ctx.find_binding ctx (id, iters @ [ Il.Opt ]))
      vars_bound
  in
  (* Create stubs for iterators *)
  let ctx, ids_elem_ml =
    Stub.OCaml.iterator ~prefix:"elem_opt__" ctx vars_bound
  in
  (* Build inner continuation that extracts vars_bind as tuple from ctx *)
  let cont_inner ctx =
    let ids_bind_ml =
      List.map
        (fun (id, _, iters) -> Ctx.find_binding ctx (id, iters))
        vars_bind
    in
    ( ctx,
      match ids_bind_ml with
      | [] -> Ml.UnitE
      | [ id_bind_ml ] -> Ml.VarE id_bind_ml
      | _ ->
          Ml.TupleE
            (List.map (fun id_bind_ml -> Ml.VarE id_bind_ml) ids_bind_ml) )
  in
  (* Compile inner body *)
  let ctx, expr_inner_ml =
    compile_let_iter ctx exp_l exp_r iterinstrs cont_inner
  in
  (* Combine a tuple of options into an option of a tuple *)
  let ctx, expr_guide_ml =
    match ids_guide_ml with
    | [ id_guide_ml ] -> (ctx, Ml.VarE id_guide_ml)
    | _ ->
        let ctx = Ctx.add_opt_arity ctx n_bound in
        ( ctx,
          Ml.AppE
            ( Ml.VarE ("Option.combine" ^ string_of_int n_bound),
              List.map (fun id_guide_ml -> Ml.VarE id_guide_ml) ids_guide_ml )
        )
  in
  (* Build lambda for Option.map *)
  let pat_elem_ml =
    match ids_elem_ml with
    | [ id_elem_ml ] -> Ml.VarP id_elem_ml
    | _ ->
        Ml.TupleP (List.map (fun id_elem_ml -> Ml.VarP id_elem_ml) ids_elem_ml)
  in
  (* Build Option.map *)
  let expr_map_ml =
    Ml.AppE
      ( Ml.VarE "Option.map",
        [ Ml.FunE ([ pat_elem_ml ], expr_inner_ml); expr_guide_ml ] )
  in
  (* Name output vars with __quest suffix and register split arity *)
  let ctx = Ctx.add_opt_arity ctx n_bind in
  let ids_out_ml =
    List.map
      (fun (id, _, iters) -> Names.var_of_var (id, iters @ [ Il.Opt ]))
      vars_bind
  in
  (* Build output pattern *)
  let pat_out_ml =
    match ids_out_ml with
    | [ id_out_ml ] -> Ml.VarP id_out_ml
    | _ -> Ml.TupleP (List.map (fun id_out_ml -> Ml.VarP id_out_ml) ids_out_ml)
  in
  (* Split an option of a tuple into a tuple of options *)
  let expr_split_ml =
    Ml.AppE (Ml.VarE ("Option.split" ^ string_of_int n_bind), [ expr_map_ml ])
  in
  (* Add output bindings to ctx *)
  let ctx =
    List.fold_left2
      (fun ctx (id, _, iters) id_out_ml ->
        Ctx.add_binding ctx (id, iters @ [ Il.Opt ]) id_out_ml)
      ctx vars_bind ids_out_ml
  in
  (* Compile continuation *)
  let ctx, expr_cont_ml = cont ctx in
  (* Promote preamble to outer ctx *)
  let ctx = Ctx.promote_preamble ctx ctx_outer in
  (* Build let expression *)
  let expr_ml = Ml.LetE (pat_out_ml, expr_split_ml, expr_cont_ml) in
  (ctx, expr_ml)

and compile_let_list (ctx : Ctx.t) (exp_l : exp) (exp_r : exp)
    (vars_bound : var list) (vars_bind : var list) (iterinstrs : iterinstr list)
    (cont : Ctx.t -> Ctx.t * Ml.expr) : Ctx.t * Ml.expr =
  let ctx_outer = ctx in
  let n_bound = List.length vars_bound in
  let n_bind = List.length vars_bind in
  (* Fetch guiding variables *)
  let ids_guide_ml =
    List.map
      (fun (id, _, iters) -> Ctx.find_binding ctx (id, iters @ [ Il.List ]))
      vars_bound
  in
  (* Create stubs for iterators *)
  let ctx, ids_elem_ml =
    Stub.OCaml.iterator ~prefix:"elem_list__" ctx vars_bound
  in
  (* Build inner continuation that extracts vars_bind as tuple from ctx *)
  let cont_inner ctx =
    let ids_bind_ml =
      List.map
        (fun (id, _, iters) -> Ctx.find_binding ctx (id, iters))
        vars_bind
    in
    ( ctx,
      match ids_bind_ml with
      | [] -> Ml.UnitE
      | [ id_bind_ml ] -> Ml.VarE id_bind_ml
      | _ ->
          Ml.TupleE
            (List.map (fun id_bind_ml -> Ml.VarE id_bind_ml) ids_bind_ml) )
  in
  (* Compile inner body *)
  let ctx, expr_inner_ml =
    compile_let_iter ctx exp_l exp_r iterinstrs cont_inner
  in
  (* Combine multiple list guides into a single list of tuple *)
  let ctx, expr_guide_ml =
    match ids_guide_ml with
    | [ id_guide_ml ] -> (ctx, Ml.VarE id_guide_ml)
    | _ ->
        let ctx = Ctx.add_list_arity ctx n_bound in
        ( ctx,
          Ml.AppE
            ( Ml.VarE ("List.combine" ^ string_of_int n_bound),
              List.map (fun id_guide_ml -> Ml.VarE id_guide_ml) ids_guide_ml )
        )
  in
  (* Build lambda for List.map *)
  let pat_elem_ml =
    match ids_elem_ml with
    | [ id_elem_ml ] -> Ml.VarP id_elem_ml
    | _ ->
        Ml.TupleP (List.map (fun id_elem_ml -> Ml.VarP id_elem_ml) ids_elem_ml)
  in
  (* Build List.map *)
  let expr_map_ml =
    Ml.AppE
      ( Ml.VarE "List.map",
        [ Ml.FunE ([ pat_elem_ml ], expr_inner_ml); expr_guide_ml ] )
  in
  (* Name output vars with __star suffix and register split arity *)
  let ctx = Ctx.add_list_arity ctx n_bind in
  let ids_out_ml =
    List.map
      (fun (id, _, iters) -> Names.var_of_var (id, iters @ [ Il.List ]))
      vars_bind
  in
  (* Build output pattern *)
  let pat_out_ml =
    match ids_out_ml with
    | [ id_out_ml ] -> Ml.VarP id_out_ml
    | _ -> Ml.TupleP (List.map (fun id_out_ml -> Ml.VarP id_out_ml) ids_out_ml)
  in
  (* Split a list of a tuple into a tuple of lists *)
  let expr_split_ml =
    Ml.AppE (Ml.VarE ("List.split" ^ string_of_int n_bind), [ expr_map_ml ])
  in
  (* Add output bindings to ctx *)
  let ctx =
    List.fold_left2
      (fun ctx (id, _, iters) id_out_ml ->
        Ctx.add_binding ctx (id, iters @ [ Il.List ]) id_out_ml)
      ctx vars_bind ids_out_ml
  in
  (* Compile continuation *)
  let ctx, expr_cont_ml = cont ctx in
  (* Promote preamble to outer ctx *)
  let ctx = Ctx.promote_preamble ctx ctx_outer in
  (* Build let expression *)
  let expr_ml = Ml.LetE (pat_out_ml, expr_split_ml, expr_cont_ml) in
  (ctx, expr_ml)

and compile_let_iter (ctx : Ctx.t) (exp_l : exp) (exp_r : exp)
    (iterinstrs_rev : iterinstr list) (cont : Ctx.t -> Ctx.t * Ml.expr) :
    Ctx.t * Ml.expr =
  match iterinstrs_rev with
  | [] -> compile_let ctx exp_l exp_r cont
  | iterinstr_h :: iterinstrs_t -> (
      let iter, vars_bound, vars_bind = iterinstr_h in
      match iter with
      | Il.Opt ->
          compile_let_opt ctx exp_l exp_r vars_bound vars_bind iterinstrs_t cont
      | Il.List ->
          compile_let_list ctx exp_l exp_r vars_bound vars_bind iterinstrs_t
            cont)

and compile_let_instr (ctx : Ctx.t) (exp_l : exp) (exp_r : exp)
    (iterinstrs : iterinstr list) (block_cont_ml : block) : Ctx.t * Ml.expr =
  let iterinstrs_rev = List.rev iterinstrs in
  let cont ctx = compile_block ctx block_cont_ml in
  compile_let_iter ctx exp_l exp_r iterinstrs_rev cont

(* Rule instruction (no iterinstrs): [relation(inputs) ~~ outputs in cont]

   [
     let <bind outputs = relation(inputs)> in
     <cont block>
   ]

   Rule instruction (list iterinstrs): [relation(inputs, x_i_star) ~~ out_i_star in cont]

   [
     let (out_a__star, ..) =
       List.splitN (List.map (fun x -> <inner rule call producing (out_a, ..)>) x__star)
     in
     <cont block with out_a__star, .. in ctx>
   ]

   Opt iterinstrs: analogous with Option.map / Option.splitN *)

and compile_rule (ctx : Ctx.t) (rel_id : id) (notexp : notexp)
    (inputs : Hints.Input.t) (cont : Ctx.t -> Ctx.t * Ml.expr) : Ctx.t * Ml.expr
    =
  let ctx_outer = ctx in
  let exps = Domain.Mixfix.args notexp in
  let exps_input, exps_output = Hints.Input.split inputs exps in
  let ctx, exprs_input_ml = Exp.compile_exps ctx exps_input in
  let id_rel_ml = Names.rel rel_id in
  let expr_call_ml = Ml.AppE (Ml.VarE id_rel_ml, exprs_input_ml) in
  let ctx, chain =
    match exps_output with
    | [] -> (ctx, Chain.make_let Ml.WildP expr_call_ml)
    | [ exp_out ] -> Bind.compile ctx expr_call_ml exp_out
    | exps_out ->
        let n = List.length exps_out in
        let ctx, ids_stub_ml = Stub.OCaml.vars ctx "tup__" n in
        let pat_tuple_ml =
          Ml.TupleP (List.map (fun s -> Ml.VarP s) ids_stub_ml)
        in
        let chain_destructure = Chain.make_let pat_tuple_ml expr_call_ml in
        let ctx, chain_binds =
          List.fold_left2
            (fun (ctx, chain_acc) id_s_ml exp_out ->
              let ctx, chain_b = Bind.compile ctx (Ml.VarE id_s_ml) exp_out in
              (ctx, Chain.connect [ chain_acc; chain_b ]))
            (ctx, Chain.nop) ids_stub_ml exps_out
        in
        (ctx, Chain.connect [ chain_destructure; chain_binds ])
  in
  let ctx, expr_cont_ml = cont ctx in
  let ctx = Ctx.promote_preamble ctx ctx_outer in
  let expr_ml = Chain.apply chain expr_cont_ml in
  (ctx, expr_ml)

and compile_rule_opt (ctx : Ctx.t) (rel_id : id) (notexp : notexp)
    (inputs : Hints.Input.t) (vars_bound : var list) (vars_bind : var list)
    (iterinstrs : iterinstr list) (cont : Ctx.t -> Ctx.t * Ml.expr) :
    Ctx.t * Ml.expr =
  let ctx_outer = ctx in
  let n_bound = List.length vars_bound in
  let n_bind = List.length vars_bind in
  (* Fetch guiding variables *)
  let ids_guide_ml =
    List.map
      (fun (id, _, iters) -> Ctx.find_binding ctx (id, iters @ [ Il.Opt ]))
      vars_bound
  in
  (* Create stubs for iterators *)
  let ctx, ids_elem_ml =
    Stub.OCaml.iterator ~prefix:"elem_opt__" ctx vars_bound
  in
  (* Build inner continuation that extracts vars_bind as tuple from ctx *)
  let cont_inner ctx =
    let ids_bind_ml =
      List.map
        (fun (id, _, iters) -> Ctx.find_binding ctx (id, iters))
        vars_bind
    in
    ( ctx,
      match ids_bind_ml with
      | [] -> Ml.UnitE
      | [ id_bind_ml ] -> Ml.VarE id_bind_ml
      | _ ->
          Ml.TupleE
            (List.map (fun id_bind_ml -> Ml.VarE id_bind_ml) ids_bind_ml) )
  in
  (* Compile inner body *)
  let ctx, expr_inner_ml =
    compile_rule_iter ctx rel_id notexp inputs iterinstrs cont_inner
  in
  (* Combine a tuple of options into an option of a tuple *)
  let ctx, expr_guide_ml =
    match ids_guide_ml with
    | [ id_guide_ml ] -> (ctx, Ml.VarE id_guide_ml)
    | _ ->
        let ctx = Ctx.add_opt_arity ctx n_bound in
        ( ctx,
          Ml.AppE
            ( Ml.VarE ("Option.combine" ^ string_of_int n_bound),
              List.map (fun id_guide_ml -> Ml.VarE id_guide_ml) ids_guide_ml )
        )
  in
  (* Build lambda for Option.map *)
  let pat_elem_ml =
    match ids_elem_ml with
    | [ id_elem_ml ] -> Ml.VarP id_elem_ml
    | _ ->
        Ml.TupleP (List.map (fun id_elem_ml -> Ml.VarP id_elem_ml) ids_elem_ml)
  in
  (* Build Option.map *)
  let expr_map_ml =
    Ml.AppE
      ( Ml.VarE "Option.map",
        [ Ml.FunE ([ pat_elem_ml ], expr_inner_ml); expr_guide_ml ] )
  in
  (* Name output vars with __quest suffix and register split arity *)
  let ctx = Ctx.add_opt_arity ctx n_bind in
  let ids_out_ml =
    List.map
      (fun (id, _, iters) -> Names.var_of_var (id, iters @ [ Il.Opt ]))
      vars_bind
  in
  (* Build output pattern *)
  let pat_out_ml =
    match ids_out_ml with
    | [ id_out_ml ] -> Ml.VarP id_out_ml
    | _ -> Ml.TupleP (List.map (fun id_out_ml -> Ml.VarP id_out_ml) ids_out_ml)
  in
  (* Split an option of a tuple into a tuple of options *)
  let expr_split_ml =
    Ml.AppE (Ml.VarE ("Option.split" ^ string_of_int n_bind), [ expr_map_ml ])
  in
  (* Add output bindings to ctx *)
  let ctx =
    List.fold_left2
      (fun ctx (id, _, iters) id_out_ml ->
        Ctx.add_binding ctx (id, iters @ [ Il.Opt ]) id_out_ml)
      ctx vars_bind ids_out_ml
  in
  (* Compile continuation *)
  let ctx, expr_cont_ml = cont ctx in
  let ctx = Ctx.promote_preamble ctx ctx_outer in
  let expr_ml = Ml.LetE (pat_out_ml, expr_split_ml, expr_cont_ml) in
  (ctx, expr_ml)

and compile_rule_list (ctx : Ctx.t) (rel_id : id) (notexp : notexp)
    (inputs : Hints.Input.t) (vars_bound : var list) (vars_bind : var list)
    (iterinstrs : iterinstr list) (cont : Ctx.t -> Ctx.t * Ml.expr) :
    Ctx.t * Ml.expr =
  let ctx_outer = ctx in
  let n_bound = List.length vars_bound in
  let n_bind = List.length vars_bind in
  (* Fetch guiding variables *)
  let ids_guide_ml =
    List.map
      (fun (id, _, iters) -> Ctx.find_binding ctx (id, iters @ [ Il.List ]))
      vars_bound
  in
  (* Create stubs for iterators *)
  let ctx, ids_elem_ml =
    Stub.OCaml.iterator ~prefix:"elem_list__" ctx vars_bound
  in
  (* Build inner continuation that extracts vars_bind as tuple from ctx *)
  let cont_inner ctx =
    let ids_bind_ml =
      List.map
        (fun (id, _, iters) -> Ctx.find_binding ctx (id, iters))
        vars_bind
    in
    ( ctx,
      match ids_bind_ml with
      | [] -> Ml.UnitE
      | [ id_bind_ml ] -> Ml.VarE id_bind_ml
      | _ ->
          Ml.TupleE
            (List.map (fun id_bind_ml -> Ml.VarE id_bind_ml) ids_bind_ml) )
  in
  (* Compile inner body *)
  let ctx, expr_inner_ml =
    compile_rule_iter ctx rel_id notexp inputs iterinstrs cont_inner
  in
  (* Combine multiple list guides into a single list of tuple *)
  let ctx, expr_guide_ml =
    match ids_guide_ml with
    | [ id_guide_ml ] -> (ctx, Ml.VarE id_guide_ml)
    | _ ->
        let ctx = Ctx.add_list_arity ctx n_bound in
        ( ctx,
          Ml.AppE
            ( Ml.VarE ("List.combine" ^ string_of_int n_bound),
              List.map (fun id_guide_ml -> Ml.VarE id_guide_ml) ids_guide_ml )
        )
  in
  (* Build lambda for List.map *)
  let pat_elem_ml =
    match ids_elem_ml with
    | [ id_elem_ml ] -> Ml.VarP id_elem_ml
    | _ ->
        Ml.TupleP (List.map (fun id_elem_ml -> Ml.VarP id_elem_ml) ids_elem_ml)
  in
  (* Build List.map *)
  let expr_map_ml =
    Ml.AppE
      ( Ml.VarE "List.map",
        [ Ml.FunE ([ pat_elem_ml ], expr_inner_ml); expr_guide_ml ] )
  in
  (* Name output vars with __star suffix and register split arity *)
  let ctx = Ctx.add_list_arity ctx n_bind in
  let ids_out_ml =
    List.map
      (fun (id, _, iters) -> Names.var_of_var (id, iters @ [ Il.List ]))
      vars_bind
  in
  (* Build output pattern *)
  let pat_out_ml =
    match ids_out_ml with
    | [ id_out_ml ] -> Ml.VarP id_out_ml
    | _ -> Ml.TupleP (List.map (fun id_out_ml -> Ml.VarP id_out_ml) ids_out_ml)
  in
  (* Split a list of a tuple into a tuple of lists *)
  let expr_split_ml =
    Ml.AppE (Ml.VarE ("List.split" ^ string_of_int n_bind), [ expr_map_ml ])
  in
  (* Add output bindings to ctx *)
  let ctx =
    List.fold_left2
      (fun ctx (id, _, iters) id_out_ml ->
        Ctx.add_binding ctx (id, iters @ [ Il.List ]) id_out_ml)
      ctx vars_bind ids_out_ml
  in
  (* Compile continuation *)
  let ctx, expr_cont_ml = cont ctx in
  let ctx = Ctx.promote_preamble ctx ctx_outer in
  let expr_ml = Ml.LetE (pat_out_ml, expr_split_ml, expr_cont_ml) in
  (ctx, expr_ml)

and compile_rule_iter (ctx : Ctx.t) (rel_id : id) (notexp : notexp)
    (inputs : Hints.Input.t) (iterinstrs_rev : iterinstr list)
    (cont : Ctx.t -> Ctx.t * Ml.expr) : Ctx.t * Ml.expr =
  match iterinstrs_rev with
  | [] -> compile_rule ctx rel_id notexp inputs cont
  | iterinstr_h :: iterinstrs_t -> (
      let iter, vars_bound, vars_bind = iterinstr_h in
      match iter with
      | Il.Opt ->
          compile_rule_opt ctx rel_id notexp inputs vars_bound vars_bind
            iterinstrs_t cont
      | Il.List ->
          compile_rule_list ctx rel_id notexp inputs vars_bound vars_bind
            iterinstrs_t cont)

and compile_rule_instr (ctx : Ctx.t) (id : id) (notexp : notexp)
    (inputs : Hints.Input.t) (iterinstrs : iterinstr list) (block_cont : block)
    : Ctx.t * Ml.expr =
  let iterinstrs_rev = List.rev iterinstrs in
  let cont ctx = compile_block ctx block_cont in
  compile_rule_iter ctx id notexp inputs iterinstrs_rev cont

(* Result instruction *)

and compile_result_instr (ctx : Ctx.t) (exps : exp list) : Ctx.t * Ml.expr =
  let ctx, exprs_ml = Exp.compile_exps ctx exps in
  let expr_ml =
    match exprs_ml with
    | [] -> Ml.UnitE
    | [ expr_ml ] -> expr_ml
    | _ -> Ml.TupleE exprs_ml
  in
  (ctx, expr_ml)

(* Return instruction *)

and compile_return_instr (ctx : Ctx.t) (exp : exp) : Ctx.t * Ml.expr =
  let ctx, expr_ml = Exp.compile_exp ctx exp in
  (ctx, expr_ml)

(* Debug instruction: [debug exp]

   [raise (Unmatch "debug")] *)

and compile_debug_instr (ctx : Ctx.t) (_exp : exp) : Ctx.t * Ml.expr =
  (ctx, Common.raise_unmatch "debug")

(* Block: [[instr_h; instrs_t..]]

   []                    ->  [raise (Unmatch "empty block")]
   [instr]               ->  [compile_instr instr]
   [instr_h :: instrs_t] ->  [try compile_instr instr_h with Unmatch _ -> compile_block instrs_t] *)

and compile_block (ctx : Ctx.t) (block : block) : Ctx.t * Ml.expr =
  match block with
  | [] -> (ctx, Common.raise_unmatch "empty block")
  | [ instr ] -> compile_instr ctx instr
  | instr_h :: instrs_t ->
      let ctx, expr_h_ml = compile_instr ctx instr_h in
      let ctx, expr_t_ml = compile_block ctx instrs_t in
      let arm_ml = (Ml.VariantP (`Mono ("Unmatch", [ Ml.WildP ])), expr_t_ml) in
      let expr_ml = Ml.TryE (expr_h_ml, [ arm_ml ]) in
      (ctx, expr_ml)
