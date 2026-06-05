open Lang
open Sl

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

(* If instruction *)

and compile_if_instr (_ctx : Ctx.t) (_exp_cond : exp) (_iterexps : iterexp list)
    (_block : block) : Ctx.t * Ml.expr =
  failwith "compile_if_instr"

(* Hold instruction *)

and compile_hold_instr (_ctx : Ctx.t) (_id : id) (_notexp : notexp)
    (_iterexps : iterexp list) (_holdcase : holdcase) : Ctx.t * Ml.expr =
  failwith "compile_hold_instr"

(* Case instruction *)

and compile_case_instr (_ctx : Ctx.t) (_exp : exp) (_cases : case list) :
    Ctx.t * Ml.expr =
  failwith "compile_case_instr"

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
  let expr_ml = Ml.LetE (pat_out_ml, expr_split_ml, expr_cont_ml) in
  (ctx, expr_ml)

and compile_let_list (ctx : Ctx.t) (exp_l : exp) (exp_r : exp)
    (vars_bound : var list) (vars_bind : var list) (iterinstrs : iterinstr list)
    (cont : Ctx.t -> Ctx.t * Ml.expr) : Ctx.t * Ml.expr =
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
  (ctx, Chain.apply chain expr_cont_ml)

and compile_rule_opt (ctx : Ctx.t) (rel_id : id) (notexp : notexp)
    (inputs : Hints.Input.t) (vars_bound : var list) (vars_bind : var list)
    (iterinstrs : iterinstr list) (cont : Ctx.t -> Ctx.t * Ml.expr) :
    Ctx.t * Ml.expr =
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
  let expr_ml = Ml.LetE (pat_out_ml, expr_split_ml, expr_cont_ml) in
  (ctx, expr_ml)

and compile_rule_list (ctx : Ctx.t) (rel_id : id) (notexp : notexp)
    (inputs : Hints.Input.t) (vars_bound : var list) (vars_bind : var list)
    (iterinstrs : iterinstr list) (cont : Ctx.t -> Ctx.t * Ml.expr) :
    Ctx.t * Ml.expr =
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
   [instr_h :: instrs_t] ->  [try compile_instr instr_h with Unmatch _ -> compile_block instrs_t] *)

and compile_block (ctx : Ctx.t) (block : block) : Ctx.t * Ml.expr =
  match block with
  | [] -> (ctx, Common.raise_unmatch "empty block")
  | instr_h :: instrs_t ->
      let ctx, expr_h_ml = compile_instr ctx instr_h in
      let ctx, expr_t_ml = compile_block ctx instrs_t in
      let arm_ml = (Ml.VariantP (`Mono ("Unmatch", [ Ml.WildP ])), expr_t_ml) in
      let expr_ml = Ml.TryE (expr_h_ml, [ arm_ml ]) in
      (ctx, expr_ml)
