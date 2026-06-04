open Domain
open Lang
open Xl
open Sl
open Util.Source

(* Compiling expressions *)

let rec compile_exp (ctx : Ctx.t) (exp : exp) : Ctx.t * Ml.expr =
  let wrap_ctx (expr_ml : Ml.expr) : Ctx.t * Ml.expr = (ctx, expr_ml) in
  let typ_exp = exp.note $ exp.at in
  match exp.it with
  | BoolE b -> compile_bool_exp b |> wrap_ctx
  | NumE num -> compile_num_exp num |> wrap_ctx
  | TextE str -> compile_text_exp str |> wrap_ctx
  | VarE id -> compile_var_exp ctx id |> wrap_ctx
  | UnE (op, optyp, exp) -> compile_unop_exp ctx op optyp exp
  | BinE (op, optyp, exp_l, exp_r) -> compile_binop_exp ctx op optyp exp_l exp_r
  | CmpE (op, optyp, exp_l, exp_r) -> compile_cmp_exp ctx op optyp exp_l exp_r
  | UpCastE (typ, exp) -> compile_upcast_exp ctx typ exp
  | DownCastE (typ, exp) -> compile_downcast_exp ctx typ exp
  | SubE (exp, typ) -> compile_sub_exp ctx exp typ
  | MatchE (exp, pattern) -> compile_match_exp ctx exp pattern
  | TupleE exps -> compile_tuple_exp ctx exps
  | CaseE notexp -> compile_case_exp ctx typ_exp notexp
  | StrE expfields -> compile_str_exp ctx typ_exp expfields
  | OptE exp_opt -> compile_opt_exp ctx exp_opt
  | ListE exps -> compile_list_exp ctx exps
  | ConsE (exp_h, exp_t) -> compile_cons_exp ctx exp_h exp_t
  | CatE (exp_l, exp_r) -> compile_cat_exp ctx typ_exp exp_l exp_r
  | MemE (exp_e, exp_s) -> compile_mem_exp ctx exp_e exp_s
  | LenE exp -> compile_len_exp ctx exp
  | DotE (exp_b, atom) -> compile_dot_exp ctx exp_b atom
  | IdxE (exp_b, exp_i) -> compile_idx_exp ctx exp_b exp_i
  | SliceE (exp_b, exp_l, exp_n) -> compile_slice_exp ctx exp_b exp_l exp_n
  | UpdE (exp_b, path, exp_n) -> compile_upd_exp ctx exp_b path exp_n
  | CallE (id, targs, args) -> compile_call_exp ctx id targs args
  | IterE (exp, iterexp) -> compile_iter_exp ctx typ_exp exp iterexp

and compile_exps (ctx : Ctx.t) (exps : exp list) : Ctx.t * Ml.expr list =
  List.fold_left
    (fun (ctx, exprs_ml) exp ->
      let ctx, expr_ml = compile_exp ctx exp in
      (ctx, exprs_ml @ [ expr_ml ]))
    (ctx, []) exps

(* Boolean expressions *)

and compile_bool_exp (b : bool) : Ml.expr = Ml.BoolE b

(* Numeric expressions *)

and compile_num_exp (num : Xl.Num.t) : Ml.expr =
  Ml.BigintE (Bigint.to_string (Xl.Num.to_int num))

(* Text expressions *)

and compile_text_exp (str : string) : Ml.expr = Ml.StrE str

(* Variable expressions *)

and compile_var_exp (ctx : Ctx.t) (id : id) : Ml.expr =
  let id_ml = Ctx.find_binding ctx (id, []) in
  Ml.VarE id_ml

(* Unary expressions *)

and compile_unop_exp (ctx : Ctx.t) (unop : unop) (_optyp : optyp) (exp : exp) :
    Ctx.t * Ml.expr =
  let ctx, expr_ml = compile_exp ctx exp in
  let expr_ml =
    match unop with
    | `NotOp -> Ml.UnopE ("not", expr_ml)
    | `PlusOp -> expr_ml
    | `MinusOp -> Ml.AppE (Ml.VarE "Bigint.neg", [ expr_ml ])
  in
  (ctx, expr_ml)

(* Binary expressions *)

and compile_binop_bool (binop : Bool.binop) : string =
  match binop with
  | `AndOp -> "&&"
  | `OrOp -> "||"
  | `EquivOp -> "="
  | `ImplOp -> assert false

and compile_binop_num (binop : Num.binop) : string =
  match binop with
  | `AddOp -> "Bigint.( + )"
  | `SubOp -> "Bigint.( - )"
  | `MulOp -> "Bigint.( * )"
  | `DivOp -> "Bigint.( / )"
  | `ModOp -> "Bigint.( % )"
  | `PowOp -> assert false

and compile_binop_exp (ctx : Ctx.t) (binop : binop) (_optyp : optyp)
    (exp_l : exp) (exp_r : exp) : Ctx.t * Ml.expr =
  let ctx, expr_ml_l = compile_exp ctx exp_l in
  let ctx, expr_ml_r = compile_exp ctx exp_r in
  let expr_ml =
    match binop with
    | `ImplOp ->
        let expr_ml_l = Ml.UnopE ("not", expr_ml_l) in
        Ml.BinopE ("||", expr_ml_l, expr_ml_r)
    | (`AndOp | `OrOp | `EquivOp) as binop ->
        let binop_ml = compile_binop_bool binop in
        Ml.BinopE (binop_ml, expr_ml_l, expr_ml_r)
    | `PowOp ->
        let expr_r_ml = Ml.AppE (Ml.VarE "Bigint.to_int_exn", [ expr_ml_r ]) in
        Ml.AppE (Ml.VarE "Bigint.( ** )", [ expr_ml_l; expr_r_ml ])
    | (`AddOp | `SubOp | `MulOp | `DivOp | `ModOp) as binop ->
        let binop_ml = compile_binop_num binop in
        Ml.AppE (Ml.VarE binop_ml, [ expr_ml_l; expr_ml_r ])
  in
  (ctx, expr_ml)

(* Comparison expressions *)

and compile_cmpop_bool (cmpop : Bool.cmpop) : string =
  match cmpop with `EqOp -> "=" | `NeOp -> "<>"

and compile_cmpop_num (cmpop : Num.cmpop) : string =
  match cmpop with
  | `LtOp -> "Bigint.( < )"
  | `GtOp -> "Bigint.( > )"
  | `LeOp -> "Bigint.( <= )"
  | `GeOp -> "Bigint.( >= )"

and compile_cmp_exp (ctx : Ctx.t) (cmpop : cmpop) (optyp : optyp) (exp_l : exp)
    (exp_r : exp) : Ctx.t * Ml.expr =
  let ctx, expr_ml_l = compile_exp ctx exp_l in
  let ctx, expr_ml_r = compile_exp ctx exp_r in
  let expr_ml =
    match (cmpop, optyp) with
    | ((`EqOp | `NeOp) as cmpop), `BoolT ->
        let cmpop_ml = compile_cmpop_bool cmpop in
        Ml.BinopE (cmpop_ml, expr_ml_l, expr_ml_r)
    | `EqOp, _ -> Ml.AppE (Ml.VarE "Bigint.equal", [ expr_ml_l; expr_ml_r ])
    | `NeOp, _ ->
        let expr_ml =
          Ml.AppE (Ml.VarE "Bigint.equal", [ expr_ml_l; expr_ml_r ])
        in
        Ml.UnopE ("not", expr_ml)
    | ((`LtOp | `GtOp | `LeOp | `GeOp) as cmpop), _ ->
        let cmpop_ml = compile_cmpop_num cmpop in
        Ml.AppE (Ml.VarE cmpop_ml, [ expr_ml_l; expr_ml_r ])
  in
  (ctx, expr_ml)

(* Upcast expressions *)

and compile_upcast_exp (ctx : Ctx.t) (typ : typ) (exp : exp) : Ctx.t * Ml.expr =
  let ctx, expr_ml = compile_exp ctx exp in
  let typ_ml = Type.compile_typ ~tparams:[] typ in
  let expr_ml = Ml.CoerceE (expr_ml, typ_ml) in
  (ctx, expr_ml)

(* Downcast expressions *)

(* Type variable downcast *)

and compile_downcast_exp_var (_ctx : Ctx.t) (_id : id) (_targs : targ list)
    (_exp : exp) : Ctx.t * Ml.expr =
  assert false

(* Tuple downcast *)

and compile_downcast_exp_tuple (ctx : Ctx.t) (typs : typ list) (exp : exp) :
    Ctx.t * Ml.expr =
  (* Compile expression *)
  let ctx, expr_ml = compile_exp ctx exp in
  (* Enter nested scope *)
  let ctx = Ctx.push ctx in
  (* Create stub expression for tuple elements *)
  let ctx, ids_stub_ml = typs |> List.length |> Stub.OCaml.tuple ctx in
  let exps_stub = List.map2 Stub.SpecTec.var ids_stub_ml typs in
  (* Temporarily add bindings for stub expressions *)
  let ctx =
    List.fold_left
      (fun ctx id_stub_ml ->
        Ctx.add_binding ctx (id_stub_ml $ no_region, []) id_stub_ml)
      ctx ids_stub_ml
  in
  (* Compile downcast expression for tuple elements *)
  let ctx, expr_elems_ml =
    List.combine exps_stub typs
    |> List.fold_left
         (fun (ctx, expr_elems_ml) (exp_stub, typ) ->
           let ctx, expr_elem_ml = compile_downcast_exp ctx typ exp_stub in
           (ctx, expr_elems_ml @ [ expr_elem_ml ]))
         (ctx, [])
  in
  (* Remove stub expressions from bindings *)
  let ctx =
    List.fold_left
      (fun ctx id_stub_ml ->
        Ctx.remove_binding ctx (id_stub_ml $ no_region, []))
      ctx ids_stub_ml
  in
  (* Leave nested scope *)
  let ctx = Ctx.pop ctx in
  (* Create expression *)
  let expr_ml =
    let pats_ml = List.map (fun id_bind_ml -> Ml.VarP id_bind_ml) ids_stub_ml in
    let pat_ml = Ml.TupleP pats_ml in
    let expr_sub_ml = Ml.TupleE expr_elems_ml in
    Ml.LetE (pat_ml, expr_sub_ml, expr_ml)
  in
  (ctx, expr_ml)

(* Iteration downcast *)

and compile_downcast_exp_iter_opt (ctx : Ctx.t) (typ : typ) (exp : exp) :
    Ctx.t * Ml.expr =
  (* Fetch source type *)
  let typ_src = match exp.note with IterT (typ, Opt) -> typ | _ -> typ in
  (* Compile expression *)
  let ctx, expr_ml = compile_exp ctx exp in
  (* Enter nested scope *)
  let ctx = Ctx.push ctx in
  (* Create stub expression for option element *)
  let ctx, id_stub_ml = Stub.OCaml.iter_opt ctx in
  let exp_stub = Stub.SpecTec.var id_stub_ml typ_src in
  (* Temporarily add binding for stub expression *)
  let ctx = Ctx.add_binding ctx (id_stub_ml $ no_region, []) id_stub_ml in
  (* Compile downcast expression for iterated element *)
  let ctx, expr_elem_ml = compile_downcast_exp ctx typ exp_stub in
  (* Remove stub expression from binding *)
  let ctx = Ctx.remove_binding ctx (id_stub_ml $ no_region, []) in
  (* Leave nested scope *)
  let ctx = Ctx.pop ctx in
  (* Create map on option *)
  let expr_lambda_ml = Ml.FunE ([ Ml.VarP id_stub_ml ], expr_elem_ml) in
  let expr_ml = Ml.AppE (Ml.VarE "Option.map", [ expr_lambda_ml; expr_ml ]) in
  (ctx, expr_ml)

and compile_downcast_exp_iter_list (ctx : Ctx.t) (typ : typ) (exp : exp) :
    Ctx.t * Ml.expr =
  (* Fetch source type *)
  let typ_src = match exp.note with IterT (typ, List) -> typ | _ -> typ in
  (* Compile expression *)
  let ctx, expr_ml = compile_exp ctx exp in
  (* Enter nested scope *)
  let ctx = Ctx.push ctx in
  (* Create stub expression for list element *)
  let ctx, id_stub_ml = Stub.OCaml.iter_list ctx in
  let exp_stub = Stub.SpecTec.var id_stub_ml typ_src in
  (* Temporarily add binding for stub expression *)
  let ctx = Ctx.add_binding ctx (id_stub_ml $ no_region, []) id_stub_ml in
  (* Compile downcast expression for iterated element *)
  let ctx, expr_elem_ml = compile_downcast_exp ctx typ exp_stub in
  (* Remove stub expression from binding *)
  let ctx = Ctx.remove_binding ctx (id_stub_ml $ no_region, []) in
  (* Leave nested scope *)
  let ctx = Ctx.pop ctx in
  (* Create map on list *)
  let expr_lambda_ml = Ml.FunE ([ Ml.VarP id_stub_ml ], expr_elem_ml) in
  let expr_ml = Ml.AppE (Ml.VarE "List.map", [ expr_lambda_ml; expr_ml ]) in
  (ctx, expr_ml)

and compile_downcast_exp_iter (ctx : Ctx.t) (typ : typ) (iter : iter)
    (exp : exp) : Ctx.t * Ml.expr =
  match iter with
  | Opt -> compile_downcast_exp_iter_opt ctx typ exp
  | List -> compile_downcast_exp_iter_list ctx typ exp

and compile_downcast_exp (ctx : Ctx.t) (typ : typ) (exp : exp) : Ctx.t * Ml.expr
    =
  match typ.it with
  | VarT (id, targs) -> compile_downcast_exp_var ctx id targs exp
  | TupleT typs -> compile_downcast_exp_tuple ctx typs exp
  | IterT (typ, iter) -> compile_downcast_exp_iter ctx typ iter exp
  | _ -> compile_exp ctx exp

(* Subtyping check expressions *)

(* Nat subtype check: [exp <: NumT `NatT] *)

and compile_sub_exp_num_nat (ctx : Ctx.t) (exp : exp) : Ctx.t * Ml.expr =
  let ctx, expr_ml = compile_exp ctx exp in
  let expr_zero_ml = Ml.BigintE "0" in
  let expr_ml = Ml.AppE (Ml.VarE "Bigint.( >= )", [ expr_ml; expr_zero_ml ]) in
  (ctx, expr_ml)

(* Variable subtype check *)

and compile_sub_match (_ctx : Ctx.t) (_exp : exp)
    (_ctors_inter : (string * typ list) list) : Ctx.t * Ml.expr =
  assert false

and compile_sub_exp_var_irreflexive (_ctx : Ctx.t) (_exp : exp) (_id : id)
    (_targs : targ list) : Ctx.t * Ml.expr =
  assert false

and compile_sub_exp_var (_ctx : Ctx.t) (_exp : exp) (_id : id)
    (_targs : targ list) : Ctx.t * Ml.expr =
  assert false

(* Tuple subtype check: [exp <: (typ_1, ..., typ_n)] *)

and compile_sub_exp_tuple (ctx : Ctx.t) (exp : exp) (typs : typ list) :
    Ctx.t * Ml.expr =
  let ctx, expr_ml = compile_exp ctx exp in
  let ctx_inner = Ctx.push ctx in
  let ctx_inner, ids_stub_ml = Stub.OCaml.tuple ctx_inner (List.length typs) in
  let ctx_inner =
    List.fold_left
      (fun c id_stub -> Ctx.add_binding c (id_stub $ no_region, []) id_stub)
      ctx_inner ids_stub_ml
  in
  let ctx_inner, exprs_elem_ml =
    List.combine ids_stub_ml typs
    |> List.fold_left
         (fun (c, exprs) (id_stub, typ) ->
           let exp_stub = Stub.SpecTec.var id_stub typ in
           let c, expr_cond = compile_sub_exp c exp_stub typ in
           (c, exprs @ [ expr_cond ]))
         (ctx_inner, [])
  in
  let expr_result =
    match exprs_elem_ml with
    | [] -> Ml.BoolE true
    | _ ->
        let pat_ml = Ml.TupleP (List.map (fun id -> Ml.VarP id) ids_stub_ml) in
        let expr_sub_ml =
          List.fold_left
            (fun acc e -> Ml.BinopE ("&&", acc, e))
            (Ml.BoolE true) exprs_elem_ml
        in
        Ml.LetE (pat_ml, expr_ml, expr_sub_ml)
  in
  let ctx = Ctx.pop ctx_inner in
  (ctx, expr_result)

(* Iteration subtype check *)

(* Option subtype check: [exp <: typ?] *)

and compile_sub_exp_opt (ctx : Ctx.t) (exp : exp) (typ : typ) : Ctx.t * Ml.expr
    =
  let ctx, id_stub_ml = Ctx.fresh ctx "sub_opt__" in
  let ctx_inner = Ctx.add_binding ctx (id_stub_ml $ no_region, []) id_stub_ml in
  let exp_stub = Stub.SpecTec.var id_stub_ml typ in
  let ctx_inner, expr_cond_ml = compile_sub_exp ctx_inner exp_stub typ in
  match expr_cond_ml with
  | Ml.BoolE true -> (ctx_inner, Ml.BoolE true)
  | _ ->
      let ctx_inner, expr_ml = compile_exp ctx_inner exp in
      let arm_none = (Ml.OptP None, Ml.BoolE true) in
      let arm_some = (Ml.OptP (Some (Ml.VarP id_stub_ml)), expr_cond_ml) in
      (ctx_inner, Ml.MatchE (expr_ml, [ arm_none; arm_some ]))

(* List subtype check: [exp <: typ*] *)

and compile_sub_exp_list (ctx : Ctx.t) (exp : exp) (typ : typ) : Ctx.t * Ml.expr
    =
  let ctx, id_stub_ml = Ctx.fresh ctx "sub_elem__" in
  let ctx_inner = Ctx.add_binding ctx (id_stub_ml $ no_region, []) id_stub_ml in
  let exp_stub = Stub.SpecTec.var id_stub_ml typ in
  let ctx_inner, expr_cond_ml = compile_sub_exp ctx_inner exp_stub typ in
  match expr_cond_ml with
  | Ml.BoolE true -> (ctx_inner, Ml.BoolE true)
  | _ ->
      let ctx_inner, expr_ml = compile_exp ctx_inner exp in
      let lambda = Ml.FunE ([ Ml.VarP id_stub_ml ], expr_cond_ml) in
      (ctx_inner, Ml.AppE (Ml.VarE "List.for_all", [ lambda; expr_ml ]))

and compile_sub_exp_iter (ctx : Ctx.t) (exp : exp) (typ : typ) (iter : iter) :
    Ctx.t * Ml.expr =
  match iter with
  | Opt -> compile_sub_exp_opt ctx exp typ
  | List -> compile_sub_exp_list ctx exp typ

and compile_sub_exp (ctx : Ctx.t) (exp : exp) (typ : typ) : Ctx.t * Ml.expr =
  match typ.it with
  | NumT `NatT -> compile_sub_exp_num_nat ctx exp
  | VarT (id, targs) -> compile_sub_exp_var ctx exp id targs
  | TupleT typs -> compile_sub_exp_tuple ctx exp typs
  | IterT (typ, iter) -> compile_sub_exp_iter ctx exp typ iter
  | _ -> (ctx, Ml.BoolE true)

(* Match expressions *)

and compile_match_exp (ctx : Ctx.t) (exp : exp) (pattern : pattern) :
    Ctx.t * Ml.expr =
  let ctx, expr_ml = compile_exp ctx exp in
  let expr_ml =
    match pattern with
    | CaseP mixop ->
        let typ_exp = exp.note $ exp.at in
        let ctor_ml = Ctx.find_ctor ctx typ_exp mixop in
        let arity = Mixfix.arity mixop in
        let pats_ml = List.init arity (fun _ -> Ml.WildP) in
        Ml.MatchE
          ( expr_ml,
            [
              (Ml.VariantP (`Poly (ctor_ml, pats_ml)), Ml.BoolE true);
              (Ml.WildP, Ml.BoolE false);
            ] )
    | ListP `Cons -> Ml.BinopE ("<>", expr_ml, Ml.ListE [])
    | ListP (`Fixed n) ->
        Ml.BinopE
          ( "=",
            Ml.AppE (Ml.VarE "List.length", [ expr_ml ]),
            Ml.LitE (string_of_int n) )
    | ListP `Nil -> Ml.BinopE ("=", expr_ml, Ml.ListE [])
    | OptP `Some -> Ml.AppE (Ml.VarE "Option.is_some", [ expr_ml ])
    | OptP `None -> Ml.AppE (Ml.VarE "Option.is_none", [ expr_ml ])
  in
  (ctx, expr_ml)

(* Tuple expressions *)

and compile_tuple_exp (ctx : Ctx.t) (exps : exp list) : Ctx.t * Ml.expr =
  let ctx, exprs_ml = compile_exps ctx exps in
  let expr_ml = Ml.TupleE exprs_ml in
  (ctx, expr_ml)

(* Case expressions *)

and compile_case_exp (ctx : Ctx.t) (typ_exp : typ) (notexp : notexp) :
    Ctx.t * Ml.expr =
  let mixop, exps = Mixfix.split notexp in
  let ctor_ml = Ctx.find_ctor ctx typ_exp mixop in
  let ctx, exprs_ml = compile_exps ctx exps in
  let expr_ml = Ml.VariantE (ctor_ml, exprs_ml) in
  (ctx, expr_ml)

(* Record expressions *)

and compile_str_exp (ctx : Ctx.t) (typ_exp : typ)
    (expfields : (atom * exp) list) : Ctx.t * Ml.expr =
  let ctx, exprfields_ml =
    List.fold_left
      (fun (ctx, exprfields_ml) (atom, exp) ->
        let field_ml = Names.field atom in
        let ctx, expr_ml = compile_exp ctx exp in
        let exprfield_ml = (field_ml, expr_ml) in
        (ctx, exprfields_ml @ [ exprfield_ml ]))
      (ctx, []) expfields
  in
  let expr_ml = Ml.RecordE exprfields_ml in
  let typ_ml = Type.compile_typ ~tparams:[] typ_exp in
  let expr_ml = Ml.AnnotE (expr_ml, typ_ml) in
  (ctx, expr_ml)

(* Option expressions *)

and compile_opt_exp (ctx : Ctx.t) (exp_opt : exp option) : Ctx.t * Ml.expr =
  match exp_opt with
  | None ->
      let expr_ml = Ml.OptE None in
      (ctx, expr_ml)
  | Some exp ->
      let ctx, expr_ml = compile_exp ctx exp in
      let expr_ml = Ml.OptE (Some expr_ml) in
      (ctx, expr_ml)

(* List expressions *)

and compile_list_exp (ctx : Ctx.t) (exps : exp list) : Ctx.t * Ml.expr =
  let ctx, exprs_ml = compile_exps ctx exps in
  let expr_ml = Ml.ListE exprs_ml in
  (ctx, expr_ml)

(* Cons expressions *)

and compile_cons_exp (ctx : Ctx.t) (exp_h : exp) (exp_t : exp) : Ctx.t * Ml.expr
    =
  let ctx, expr_h_ml = compile_exp ctx exp_h in
  let ctx, expr_t_ml = compile_exp ctx exp_t in
  let expr_ml = Ml.ConsE (expr_h_ml, expr_t_ml) in
  (ctx, expr_ml)

(* Concatenation expressions *)

and compile_cat_exp (ctx : Ctx.t) (typ_exp : typ) (exp_l : exp) (exp_r : exp) :
    Ctx.t * Ml.expr =
  let binop_ml = match typ_exp.it with TextT -> "^" | _ -> "@" in
  let ctx, expr_l_ml = compile_exp ctx exp_l in
  let ctx, expr_r_ml = compile_exp ctx exp_r in
  let expr_ml = Ml.BinopE (binop_ml, expr_l_ml, expr_r_ml) in
  (ctx, expr_ml)

(* Membership expressions *)

and compile_mem_exp (ctx : Ctx.t) (exp_e : exp) (exp_s : exp) : Ctx.t * Ml.expr
    =
  let ctx, expr_e_ml = compile_exp ctx exp_e in
  let ctx, expr_s_ml = compile_exp ctx exp_s in
  let expr_ml = Ml.AppE (Ml.VarE "List.mem", [ expr_e_ml; expr_s_ml ]) in
  (ctx, expr_ml)

(* Length expressions *)

and compile_len_exp (ctx : Ctx.t) (exp : exp) : Ctx.t * Ml.expr =
  let ctx, expr_ml = compile_exp ctx exp in
  let id_len_ml =
    match exp.note with TextT -> "String.length" | _ -> "List.length"
  in
  let expr_ml =
    Ml.AppE
      (Ml.VarE "Bigint.of_int", [ Ml.AppE (Ml.VarE id_len_ml, [ expr_ml ]) ])
  in
  (ctx, expr_ml)

(* Field access expressions *)

and compile_dot_exp (ctx : Ctx.t) (exp_b : exp) (atom : atom) : Ctx.t * Ml.expr
    =
  let field_ml = Names.field atom in
  let ctx, expr_b_ml = compile_exp ctx exp_b in
  let expr_ml = Ml.FieldE (expr_b_ml, field_ml) in
  (ctx, expr_ml)

(* Index expressions *)

and compile_idx_exp (ctx : Ctx.t) (exp_b : exp) (exp_i : exp) : Ctx.t * Ml.expr
    =
  let ctx, expr_b_ml = compile_exp ctx exp_b in
  let ctx, expr_i_ml = compile_exp ctx exp_i in
  let expr_i_ml = Ml.AppE (Ml.VarE "Bigint.to_int_exn", [ expr_i_ml ]) in
  let expr_ml =
    match exp_b.note with
    | TextT ->
        Ml.AppE (Ml.VarE "String.sub", [ expr_b_ml; expr_i_ml; Ml.LitE "1" ])
    | _ -> Ml.AppE (Ml.VarE "List.nth", [ expr_b_ml; expr_i_ml ])
  in
  (ctx, expr_ml)

(* Slice expressions *)

and compile_slice_exp (ctx : Ctx.t) (exp_b : exp) (exp_i : exp) (exp_n : exp) :
    Ctx.t * Ml.expr =
  let ctx, expr_b_ml = compile_exp ctx exp_b in
  let ctx, expr_i_ml = compile_exp ctx exp_i in
  let expr_i_ml = Ml.AppE (Ml.VarE "Bigint.to_int_exn", [ expr_i_ml ]) in
  let ctx, expr_n_ml = compile_exp ctx exp_n in
  let expr_n_ml = Ml.AppE (Ml.VarE "Bigint.to_int_exn", [ expr_n_ml ]) in
  match exp_b.note with
  | TextT ->
      let expr_ml =
        Ml.AppE (Ml.VarE "String.sub", [ expr_b_ml; expr_i_ml; expr_n_ml ])
      in
      (ctx, expr_ml)
  | _ ->
      (* Enter nested block *)
      let ctx = Ctx.push ctx in
      (* Create a stub variable for the index *)
      let ctx, id_stub_ml = Stub.OCaml.slice ctx in
      (* Compile list slice *)
      let expr_ml =
        Ml.AppE
          ( Ml.VarE "List.filteri",
            [
              Ml.FunE
                ( [ Ml.VarP id_stub_ml; Ml.WildP ],
                  Ml.BinopE
                    ( "&&",
                      Ml.BinopE (">=", Ml.VarE id_stub_ml, expr_i_ml),
                      Ml.BinopE
                        ( "<",
                          Ml.VarE id_stub_ml,
                          Ml.BinopE ("+", expr_i_ml, expr_n_ml) ) ) );
              expr_b_ml;
            ] )
      in
      (* Exit nested block *)
      let ctx = Ctx.pop ctx in
      (ctx, expr_ml)

(* Update expressions *)

and compile_upd (_ctx : Ctx.t) (_path : path) (_expr_b_ml : Ml.expr)
    (_expr_n_ml : Ml.expr) : Ctx.t * Ml.expr =
  assert false

and compile_upd_exp (ctx : Ctx.t) (exp_b : exp) (path : path) (exp_n : exp) :
    Ctx.t * Ml.expr =
  let ctx, expr_b_ml = compile_exp ctx exp_b in
  let ctx, expr_n_ml = compile_exp ctx exp_n in
  compile_upd ctx path expr_b_ml expr_n_ml

(* Call expressions *)

and compile_arg (ctx : Ctx.t) (arg : arg) : Ctx.t * Ml.expr =
  match arg.it with
  | ExpA exp ->
      let ctx, expr_ml = compile_exp ctx exp in
      (ctx, expr_ml)
  | DefA id ->
      let id_ml = Names.func id in
      let expr_ml = Ml.VarE id_ml in
      (ctx, expr_ml)

and compile_args (ctx : Ctx.t) (args : arg list) : Ctx.t * Ml.expr list =
  List.fold_left
    (fun (ctx, exprs_ml) arg ->
      let ctx, expr_ml = compile_arg ctx arg in
      (ctx, exprs_ml @ [ expr_ml ]))
    (ctx, []) args

and compile_call_exp (ctx : Ctx.t) (id : id) (_targs : targ list)
    (args : arg list) : Ctx.t * Ml.expr =
  let id_func_ml = Names.func id in
  let ctx, exprs_arg_ml = compile_args ctx args in
  let expr_ml = Ml.AppE (Ml.VarE id_func_ml, exprs_arg_ml) in
  (ctx, expr_ml)

(* Iterator expressions *)

and compile_iter_exp_opt (ctx : Ctx.t) (exp : exp) (vars : var list) :
    Ctx.t * Ml.expr =
  (* Fetch iteration target variables *)
  let ids_opt_ml =
    List.map
      (fun (id, _, iters) -> Ctx.find_binding ctx (id, iters @ [ Il.Opt ]))
      vars
  in
  (* Enter nested block *)
  let ctx = Ctx.push ctx in
  (* Create stub variables for iterated elements *)
  let n = List.length vars in
  let ctx, ids_stub_ml = Stub.OCaml.iter_opts ctx n in
  (* Temporarily add stub variables to context *)
  let ctx =
    List.fold_left2
      (fun ctx (id, _, iters) stub -> Ctx.add_binding ctx (id, iters) stub)
      ctx vars ids_stub_ml
  in
  (* Compile lambda expression *)
  let ctx, expr_lambda_ml =
    let pat_ml =
      match ids_stub_ml with
      | [ id ] -> Ml.VarP id
      | ids -> Ml.TupleP (List.map (fun id -> Ml.VarP id) ids)
    in
    let ctx, expr_body_ml = compile_exp ctx exp in
    let expr_ml = Ml.FunE ([ pat_ml ], expr_body_ml) in
    (ctx, expr_ml)
  in
  (* Remove stub variables from context *)
  let ctx =
    List.fold_left2
      (fun ctx (id, _, iters) _ -> Ctx.remove_binding ctx (id, iters))
      ctx vars ids_stub_ml
  in
  (* Exit nested block *)
  let ctx = Ctx.pop ctx in
  (* Combine iteration targets into a single option, then map over it *)
  let ctx, expr_opt_ml =
    match ids_opt_ml with
    | [ id_ml ] ->
        let expr_opt_ml = Ml.VarE id_ml in
        (ctx, expr_opt_ml)
    | _ ->
        let ctx = Ctx.add_opt_arity ctx n in
        let id_combine_ml = "Option.combine" ^ string_of_int n in
        let exprs_arg_ml =
          List.map (fun id_opt_ml -> Ml.VarE id_opt_ml) ids_opt_ml
        in
        let expr_opt_ml = Ml.AppE (Ml.VarE id_combine_ml, exprs_arg_ml) in
        (ctx, expr_opt_ml)
  in
  let expr_ml =
    Ml.AppE (Ml.VarE "Option.map", [ expr_lambda_ml; expr_opt_ml ])
  in
  (ctx, expr_ml)

and compile_iter_exp_list (ctx : Ctx.t) (exp : exp) (vars : var list) :
    Ctx.t * Ml.expr =
  (* Fetch iteration target variables *)
  let ids_list_ml =
    List.map
      (fun (id, _, iters) -> Ctx.find_binding ctx (id, iters @ [ Il.List ]))
      vars
  in
  (* Enter nested block *)
  let ctx = Ctx.push ctx in
  (* Create stub variables for iterated elements *)
  let n = List.length vars in
  let ctx, ids_stub_ml = Stub.OCaml.iter_lists ctx n in
  (* Temporarily add stub variables to context *)
  let ctx =
    List.fold_left2
      (fun ctx (id, _, iters) stub -> Ctx.add_binding ctx (id, iters) stub)
      ctx vars ids_stub_ml
  in
  (* Compile lambda expression *)
  let ctx, expr_lambda_ml =
    let pat_ml =
      match ids_stub_ml with
      | [ id ] -> Ml.VarP id
      | ids -> Ml.TupleP (List.map (fun id -> Ml.VarP id) ids)
    in
    let ctx, expr_body_ml = compile_exp ctx exp in
    let expr_ml = Ml.FunE ([ pat_ml ], expr_body_ml) in
    (ctx, expr_ml)
  in
  (* Remove stub variables from context *)
  let ctx =
    List.fold_left2
      (fun ctx (id, _, iters) _ -> Ctx.remove_binding ctx (id, iters))
      ctx vars ids_stub_ml
  in
  (* Exit nested block *)
  let ctx = Ctx.pop ctx in
  (* Combine iteration targets into a single list, then map over it *)
  let ctx, expr_list_ml =
    match ids_list_ml with
    | [ id_ml ] ->
        let expr_list_ml = Ml.VarE id_ml in
        (ctx, expr_list_ml)
    | _ ->
        let ctx = Ctx.add_list_arity ctx n in
        let id_combine_ml = "List.combine" ^ string_of_int n in
        let exprs_arg_ml =
          List.map (fun id_list_ml -> Ml.VarE id_list_ml) ids_list_ml
        in
        let expr_list_ml = Ml.AppE (Ml.VarE id_combine_ml, exprs_arg_ml) in
        (ctx, expr_list_ml)
  in
  let expr_ml =
    Ml.AppE (Ml.VarE "List.map", [ expr_lambda_ml; expr_list_ml ])
  in
  (ctx, expr_ml)

and compile_iter_exp (ctx : Ctx.t) (typ_exp : typ) (exp : exp)
    (iterexp : iterexp) : Ctx.t * Ml.expr =
  match
    Common.is_iter_var_exp (Il.IterE (exp, iterexp) $$ (typ_exp.at, typ_exp.it))
  with
  | Some var ->
      let id_ml = Ctx.find_binding ctx var in
      let expr_ml = Ml.VarE id_ml in
      (ctx, expr_ml)
  | None -> (
      let iter, vars = iterexp in
      match iter with
      | Opt -> compile_iter_exp_opt ctx exp vars
      | List -> compile_iter_exp_list ctx exp vars)
