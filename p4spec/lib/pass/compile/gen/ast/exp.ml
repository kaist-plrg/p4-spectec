open Domain
open Lib
open Lang
open Xl
open Sl
module Typ = Runtime.Type
module Typdef = Typ.Typdef
open Util.Source

(* Compiling expressions: [Sl.exp] -> [Ml.expr] *)

let rec compile_exp ~(tparams : string list) (ctx : Ctx.t) (exp : exp) :
    Ctx.t * Ml.expr =
  let wrap_ctx (expr_ml : Ml.expr) : Ctx.t * Ml.expr = (ctx, expr_ml) in
  let typ_exp = exp.note $ exp.at in
  match exp.it with
  | BoolE b -> compile_bool_exp b |> wrap_ctx
  | NumE num -> compile_num_exp num |> wrap_ctx
  | TextE str -> compile_text_exp str |> wrap_ctx
  | VarE id -> compile_var_exp ctx id |> wrap_ctx
  | UnE (op, optyp, exp) -> compile_unop_exp ~tparams ctx op optyp exp
  | BinE (op, optyp, exp_l, exp_r) ->
      compile_binop_exp ~tparams ctx op optyp exp_l exp_r
  | CmpE (op, optyp, exp_l, exp_r) ->
      compile_cmp_exp ~tparams ctx op optyp exp_l exp_r
  | UpCastE (typ, exp) -> compile_upcast_exp ~tparams ctx typ exp
  | DownCastE (typ, exp) -> compile_downcast_exp ~tparams ctx typ exp
  | SubE (exp, typ) -> compile_sub_exp ~tparams ctx exp typ
  | MatchE (exp, pattern) -> compile_match_exp ~tparams ctx exp pattern
  | TupleE exps -> compile_tuple_exp ~tparams ctx exps
  | CaseE notexp -> compile_case_exp ~tparams ctx typ_exp notexp
  | StrE expfields -> compile_str_exp ~tparams ctx typ_exp expfields
  | OptE exp_opt -> compile_opt_exp ~tparams ctx exp_opt
  | ListE exps -> compile_list_exp ~tparams ctx exps
  | ConsE (exp_h, exp_t) -> compile_cons_exp ~tparams ctx exp_h exp_t
  | CatE (exp_l, exp_r) -> compile_cat_exp ~tparams ctx typ_exp exp_l exp_r
  | MemE (exp_e, exp_s) -> compile_mem_exp ~tparams ctx exp_e exp_s
  | LenE exp -> compile_len_exp ~tparams ctx exp
  | DotE (exp_b, atom) -> compile_dot_exp ~tparams ctx exp_b atom
  | IdxE (exp_b, exp_i) -> compile_idx_exp ~tparams ctx exp_b exp_i
  | SliceE (exp_b, exp_l, exp_n) ->
      compile_slice_exp ~tparams ctx exp_b exp_l exp_n
  | UpdE (exp_b, path, exp_n) -> compile_upd_exp ~tparams ctx exp_b path exp_n
  | CallE (id, targs, args) -> compile_call_exp ~tparams ctx id targs args
  | IterE (exp, iterexp) -> compile_iter_exp ~tparams ctx typ_exp exp iterexp

and compile_exps ~(tparams : string list) (ctx : Ctx.t) (exps : exp list) :
    Ctx.t * Ml.expr list =
  List.fold_left
    (fun (ctx, exprs_ml) exp ->
      let ctx, expr_ml = compile_exp ~tparams ctx exp in
      (ctx, exprs_ml @ [ expr_ml ]))
    (ctx, []) exps

(* Boolean expression: [b]

   [b] *)

and compile_bool_exp (b : bool) : Ml.expr = Ml.BoolE b

(* Numeric expression: [n]

   [Bigint n] *)

and compile_num_exp (num : Xl.Num.t) : Ml.expr =
  Ml.BigintE (Bigint.to_string (Xl.Num.to_int num))

(* Text expression: [s]

   [s] *)

and compile_text_exp (str : string) : Ml.expr = Ml.StrE str

(* Variable expression: [x]

   [ctx[x]] *)

and compile_var_exp (ctx : Ctx.t) (id : id) : Ml.expr =
  let id_ml = Ctx.find_binding ctx (id, []) in
  Ml.VarE id_ml

(* Unary expression: [unop exp]

   not  ->  [not expr]
   +    ->  [expr]
   -    ->  [Bigint.neg expr] *)

and compile_unop_exp ~(tparams : string list) (ctx : Ctx.t) (unop : unop)
    (_optyp : optyp) (exp : exp) : Ctx.t * Ml.expr =
  let ctx, expr_ml = compile_exp ~tparams ctx exp in
  let expr_ml =
    match unop with
    | `NotOp -> Ml.UnopE ("not", expr_ml)
    | `PlusOp -> expr_ml
    | `MinusOp -> Ml.AppE (Ml.VarE "Bigint.neg", [ expr_ml ])
  in
  (ctx, expr_ml)

(* Binary expressions: [exp_l binop exp_r] *)

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

(* Binary expression: [exp_l op exp_r]

   ==>        ->  [(not expr_l) || expr_r]
   &&, ||, == ->  [expr_l op expr_r]
   **         ->  [Bigint.( ** ) expr_l expr_r]
   +,-,*,/,%  ->  [Bigint.op expr_l expr_r] *)

and compile_binop_exp ~(tparams : string list) (ctx : Ctx.t) (binop : binop)
    (_optyp : optyp) (exp_l : exp) (exp_r : exp) : Ctx.t * Ml.expr =
  let ctx, expr_ml_l = compile_exp ~tparams ctx exp_l in
  let ctx, expr_ml_r = compile_exp ~tparams ctx exp_r in
  let expr_ml =
    match binop with
    | `ImplOp ->
        let expr_ml_l = Ml.UnopE ("not", expr_ml_l) in
        Ml.BinopE ("||", expr_ml_l, expr_ml_r)
    | (`AndOp | `OrOp | `EquivOp) as binop ->
        let binop_ml = compile_binop_bool binop in
        Ml.BinopE (binop_ml, expr_ml_l, expr_ml_r)
    | `PowOp -> Ml.AppE (Ml.VarE "Bigint.( ** )", [ expr_ml_l; expr_ml_r ])
    | (`AddOp | `SubOp | `MulOp | `DivOp | `ModOp) as binop ->
        let binop_ml = compile_binop_num binop in
        Ml.AppE (Ml.VarE binop_ml, [ expr_ml_l; expr_ml_r ])
  in
  (ctx, expr_ml)

(* Comparison expression: [exp_l cmpop exp_r]

   bool ==/!=     ->  [expr_l = expr_r] / [expr_l <> expr_r]
   num ==         ->  [Bigint.equal expr_l expr_r]
   num !=         ->  [not (Bigint.equal expr_l expr_r)]
   num </>/<=/>=  ->  [Bigint.op expr_l expr_r] *)

and compile_cmpop_bool (cmpop : Bool.cmpop) : string =
  match cmpop with `EqOp -> "=" | `NeOp -> "<>"

and compile_cmpop_num (cmpop : Num.cmpop) : string =
  match cmpop with
  | `LtOp -> "Bigint.( < )"
  | `GtOp -> "Bigint.( > )"
  | `LeOp -> "Bigint.( <= )"
  | `GeOp -> "Bigint.( >= )"

and compile_cmp_exp ~(tparams : string list) (ctx : Ctx.t) (cmpop : cmpop)
    (optyp : optyp) (exp_l : exp) (exp_r : exp) : Ctx.t * Ml.expr =
  let ctx, expr_ml_l = compile_exp ~tparams ctx exp_l in
  let ctx, expr_ml_r = compile_exp ~tparams ctx exp_r in
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

(* Upcast expression: [(T) exp]

   [(expr :> T)] *)

and compile_upcast_exp ~(tparams : string list) (ctx : Ctx.t) (typ : typ)
    (exp : exp) : Ctx.t * Ml.expr =
  let ctx, expr_ml = compile_exp ~tparams ctx exp in
  let typ_ml = Type.compile_typ ~tparams typ in
  let expr_ml = Ml.CoerceE (expr_ml, typ_ml) in
  (ctx, expr_ml)

(* Downcast expressions *)

(* Type variable downcast: [(T) exp]

   T not variant   ->  [expr]
   otherwise       ->  [match (expr : [> T_ctors]) with (`C _ | ...) as v -> v :> T | _ -> raise Unmatch]

   Note: the reflexive case (exp : T) is intentionally NOT short-circuited to a
   plain (expr :> T). Inside iter bodies, the SL type is narrowed post-SubG but
   the OCaml binding type is the wider guide type, so a plain coercion fails. *)

and compile_downcast_exp_var ~(tparams : string list) (ctx : Ctx.t) (id : id)
    (targs : targ list) (exp : exp) : Ctx.t * Ml.expr =
  let ctors_typ = Ctx.find_ctors ctx id in
  let ctx, expr_ml = compile_exp ~tparams ctx exp in
  let typ_target_ml =
    Type.compile_typ ~tparams (Il.VarT (id, targs) $ no_region)
  in
  if ctors_typ = [] then (ctx, expr_ml)
  else
    let typrows_ml =
      List.map
        (fun (ctor_ml, typs) ->
          let typs_inst =
            let td = Ctx.find_typdef ctx id in
            match td with
            | Typdef.Defined (tparams, _) ->
                let theta = TIdMap.of_lists tparams targs in
                Typ.Subst.subst_typs theta typs
            | _ -> typs
          in
          (ctor_ml, Type.compile_typs ~tparams typs_inst))
        ctors_typ
    in
    let expr_scrut_ml = Ml.AnnotE (expr_ml, Ml.OpenRowT typrows_ml) in
    let pats_ml =
      List.map
        (fun (ctor_ml, typs) ->
          let pats = List.map (fun _ -> Ml.WildP) typs in
          Ml.VariantP (`Poly (ctor_ml, pats)))
        ctors_typ
    in
    let pat_or_ml = Ml.OrP pats_ml in
    let ctx, id_downcast_val_ml = Stub.OCaml.var ctx "dc__" in
    let pat_as_ml = Ml.AsP (pat_or_ml, id_downcast_val_ml) in
    let expr_coerce_ml =
      Ml.CoerceE (Ml.VarE id_downcast_val_ml, typ_target_ml)
    in
    ( ctx,
      Ml.MatchE
        ( expr_scrut_ml,
          [
            (pat_as_ml, expr_coerce_ml);
            (Ml.WildP, Common.raise_unmatch "DownCastE: type mismatch");
          ] ) )

(* Tuple downcast: [(T1,..,Tn) exp]

   [let (x1,..,xn) = expr in (dc(T1,x1), .., dc(Tn,xn))] *)

and compile_downcast_exp_tuple ~(tparams : string list) (ctx : Ctx.t)
    (typs : typ list) (exp : exp) : Ctx.t * Ml.expr =
  (* Compile expression *)
  let ctx, expr_ml = compile_exp ~tparams ctx exp in
  let ctx_outer = ctx in
  (* Create stub expression for tuple elements *)
  let ctx, ids_stub_ml = Stub.OCaml.vars ctx "tup__" (List.length typs) in
  let exps_stub = List.map2 Stub.SpecTec.var ids_stub_ml typs in
  (* Compile downcast expression for tuple elements *)
  let ctx, expr_elems_ml =
    List.combine exps_stub typs
    |> List.fold_left
         (fun (ctx, expr_elems_ml) (exp_stub, typ) ->
           let ctx, expr_elem_ml =
             compile_downcast_exp ~tparams ctx typ exp_stub
           in
           (ctx, expr_elems_ml @ [ expr_elem_ml ]))
         (ctx, [])
  in
  (* Promote preamble *)
  let ctx = Ctx.promote_preamble ctx ctx_outer in
  (* Create expression *)
  let expr_ml =
    let pats_ml = List.map (fun id_bind_ml -> Ml.VarP id_bind_ml) ids_stub_ml in
    let pat_ml = Ml.TupleP pats_ml in
    let expr_sub_ml = Ml.TupleE expr_elems_ml in
    Ml.LetE (pat_ml, expr_ml, expr_sub_ml)
  in
  (ctx, expr_ml)

(* Option downcast: [(T?) exp]

   [Option.map (fun x -> dc(T, x)) expr] *)

and compile_downcast_exp_iter_opt ~(tparams : string list) (ctx : Ctx.t)
    (typ : typ) (exp : exp) : Ctx.t * Ml.expr =
  (* Fetch source type *)
  let typ_src = match exp.note with IterT (typ, Opt) -> typ | _ -> typ in
  (* Compile expression *)
  let ctx, expr_ml = compile_exp ~tparams ctx exp in
  let ctx_outer = ctx in
  (* Create stub expression for option element *)
  let ctx, id_stub_ml = Stub.OCaml.var ctx "elem_opt__" in
  let exp_stub = Stub.SpecTec.var id_stub_ml typ_src in
  (* Compile downcast expression for iterated element *)
  let ctx, expr_elem_ml = compile_downcast_exp ~tparams ctx typ exp_stub in
  (* Promote preamble *)
  let ctx = Ctx.promote_preamble ctx ctx_outer in
  (* Create map on option *)
  let expr_lambda_ml = Ml.FunE ([ Ml.VarP id_stub_ml ], expr_elem_ml) in
  let expr_ml = Ml.AppE (Ml.VarE "Option.map", [ expr_lambda_ml; expr_ml ]) in
  (ctx, expr_ml)

(* List downcast: [(T* ) exp]

   [List.map (fun x -> dc(T, x)) expr] *)

and compile_downcast_exp_iter_list ~(tparams : string list) (ctx : Ctx.t)
    (typ : typ) (exp : exp) : Ctx.t * Ml.expr =
  (* Fetch source type *)
  let typ_src = match exp.note with IterT (typ, List) -> typ | _ -> typ in
  (* Compile expression *)
  let ctx, expr_ml = compile_exp ~tparams ctx exp in
  let ctx_outer = ctx in
  (* Create stub expression for list element *)
  let ctx, id_stub_ml = Stub.OCaml.var ctx "elem_list__" in
  let exp_stub = Stub.SpecTec.var id_stub_ml typ_src in
  (* Compile downcast expression for iterated element *)
  let ctx, expr_elem_ml = compile_downcast_exp ~tparams ctx typ exp_stub in
  (* Promote preamble *)
  let ctx = Ctx.promote_preamble ctx ctx_outer in
  (* Create map on list *)
  let expr_lambda_ml = Ml.FunE ([ Ml.VarP id_stub_ml ], expr_elem_ml) in
  let expr_ml = Ml.AppE (Ml.VarE "List.map", [ expr_lambda_ml; expr_ml ]) in
  (ctx, expr_ml)

and compile_downcast_exp_iter ~(tparams : string list) (ctx : Ctx.t) (typ : typ)
    (iter : iter) (exp : exp) : Ctx.t * Ml.expr =
  match iter with
  | Opt -> compile_downcast_exp_iter_opt ~tparams ctx typ exp
  | List -> compile_downcast_exp_iter_list ~tparams ctx typ exp

and compile_downcast_exp ~(tparams : string list) (ctx : Ctx.t) (typ : typ)
    (exp : exp) : Ctx.t * Ml.expr =
  match typ.it with
  | VarT (id, targs) -> compile_downcast_exp_var ~tparams ctx id targs exp
  | TupleT typs -> compile_downcast_exp_tuple ~tparams ctx typs exp
  | IterT (typ, iter) -> compile_downcast_exp_iter ~tparams ctx typ iter exp
  | _ -> compile_exp ~tparams ctx exp

(* Subtyping check expressions *)

(* Nat subtype check: [exp <: NumT `NatT]

   [Bigint.( >= ) expr 0] *)

and compile_sub_exp_num_nat ~(tparams : string list) (ctx : Ctx.t) (exp : exp) :
    Ctx.t * Ml.expr =
  let ctx, expr_ml = compile_exp ~tparams ctx exp in
  let expr_zero_ml = Ml.BigintE "0" in
  let expr_ml = Ml.AppE (Ml.VarE "Bigint.( >= )", [ expr_ml; expr_zero_ml ]) in
  (ctx, expr_ml)

(* Variable subtype check helper: match against ctors_inter = ctors(T) & ctors(exp)

   match (expr : [< C1 p.. | .. | Cn p..]) with
   | `Ci pi.. -> sub(pi) && ..
   | _        -> false *)

and compile_sub_match ~(tparams : string list) (ctx : Ctx.t) (exp : exp)
    (ctors_inter : (Ml.ctor * Il.typ list) list) : Ctx.t * Ml.expr =
  (* Compile expression with type annotation *)
  let ctx, expr_ml = compile_exp ~tparams ctx exp in
  let typrows_ml =
    List.map
      (fun (ctor_ml, typs) -> (ctor_ml, Type.compile_typs ~tparams typs))
      ctors_inter
  in
  let expr_scrut_ml = Ml.AnnotE (expr_ml, Ml.OpenRowT typrows_ml) in
  (* Compile match arms *)
  let ctx, arms_ml =
    List.fold_left
      (fun (ctx, arms_ml) (ctor_ml, typs) ->
        let ctx_outer = ctx in
        (* Create stub expression for case payloads *)
        let n = List.length typs in
        let ctx, ids_stub_ml = Stub.OCaml.vars ctx "pyld__" n in
        (* Compile downcast expression for case payloads *)
        let ctx, exprs_cond_ml =
          List.combine ids_stub_ml typs
          |> List.fold_left
               (fun (ctx, exprs_cond_ml) (id, typ) ->
                 let exp_stub = Stub.SpecTec.var id typ in
                 let ctx, expr_cond_ml =
                   compile_sub_exp ~tparams ctx exp_stub typ
                 in
                 (ctx, exprs_cond_ml @ [ expr_cond_ml ]))
               (ctx, [])
        in
        (* Promote preamble *)
        let ctx = Ctx.promote_preamble ctx ctx_outer in
        let all_true =
          List.for_all
            (function Ml.BoolE true -> true | _ -> false)
            exprs_cond_ml
        in
        let pat_ml, expr_sub_ml =
          if all_true then
            let pats_ml = List.init n (fun _ -> Ml.WildP) in
            (Ml.VariantP (`Poly (ctor_ml, pats_ml)), Ml.BoolE true)
          else
            let pats_ml = List.map (fun id -> Ml.VarP id) ids_stub_ml in
            let expr_ml =
              List.fold_left
                (fun expr_sub_ml expr_cond_ml ->
                  match expr_cond_ml with
                  | Ml.BoolE true -> expr_sub_ml
                  | _ -> Ml.BinopE ("&&", expr_sub_ml, expr_cond_ml))
                (Ml.BoolE true) exprs_cond_ml
            in
            (Ml.VariantP (`Poly (ctor_ml, pats_ml)), expr_ml)
        in
        (ctx, arms_ml @ [ (pat_ml, expr_sub_ml) ]))
      (ctx, []) ctors_inter
  in
  let expr_ml =
    Ml.MatchE (expr_scrut_ml, arms_ml @ [ (Ml.WildP, Ml.BoolE false) ])
  in
  (ctx, expr_ml)

(* Variable subtype check (non-reflexive): [exp <: VarT T] where exp : S != T

   T not variant            ->  true
   ctors(S) & ctors(T) = {} ->  false
   otherwise                ->  compile_sub_match on intersection *)

and compile_sub_exp_var_irreflexive ~(tparams : string list) (ctx : Ctx.t)
    (exp : exp) (id : id) (targs : targ list) : Ctx.t * Ml.expr =
  let ctors_typ = Ctx.find_ctors ctx id in
  if ctors_typ = [] then (ctx, Ml.BoolE true)
  else
    let ctors_typ =
      let td = Ctx.find_typdef ctx id in
      match td with
      | Typdef.Defined (tparams, _) ->
          let theta = TIdMap.of_lists tparams targs in
          List.map
            (fun (ctor_ml, typs) ->
              let typs = Typ.Subst.subst_typs theta typs in
              (ctor_ml, typs))
            ctors_typ
      | _ -> ctors_typ
    in
    let ctors_exp =
      match exp.note with
      | VarT (id_exp, _) -> Ctx.find_ctors ctx id_exp
      | _ -> []
    in
    let ctors_inter =
      List.filter
        (fun (ctor_ml, _) ->
          List.exists (fun (ctor_exp_ml, _) -> ctor_ml = ctor_exp_ml) ctors_exp)
        ctors_typ
    in
    if ctors_inter = [] then (ctx, Ml.BoolE false)
    else compile_sub_match ~tparams ctx exp ctors_inter

(* Variable subtype check: [exp <: VarT T]

   exp : T  ->  true
   exp : S  ->  compile_sub_exp_var_irreflexive *)

and compile_sub_exp_var ~(tparams : string list) (ctx : Ctx.t) (exp : exp)
    (id : id) (targs : targ list) : Ctx.t * Ml.expr =
  match exp.note with
  | VarT (id_exp_typ, _) when id_exp_typ.it = id.it -> (ctx, Ml.BoolE true)
  | _ -> compile_sub_exp_var_irreflexive ~tparams ctx exp id targs

(* Tuple subtype check: [exp <: (typ_1, ..., typ_n)]

   [let (x1,..,xn) = expr in sub(x1,T1) && .. && sub(xn,Tn)] *)

and compile_sub_exp_tuple ~(tparams : string list) (ctx : Ctx.t) (exp : exp)
    (typs : typ list) : Ctx.t * Ml.expr =
  (* Compile expression *)
  let ctx, expr_ml = compile_exp ~tparams ctx exp in
  (* Save context for promotion *)
  let ctx_outer = ctx in
  (* Create stub expression for tuple elements *)
  let ctx, ids_stub_ml = Stub.OCaml.vars ctx "tup__" (List.length typs) in
  (* Compile subtype check for tuple elements *)
  let ctx, exprs_elem_ml =
    List.combine ids_stub_ml typs
    |> List.fold_left
         (fun (ctx, exprs_elem_ml) (id_stub, typ) ->
           let exp_stub = Stub.SpecTec.var id_stub typ in
           let ctx, expr_ml = compile_sub_exp ~tparams ctx exp_stub typ in
           (ctx, exprs_elem_ml @ [ expr_ml ]))
         (ctx, [])
  in
  (* Promote preamble *)
  let ctx = Ctx.promote_preamble ctx ctx_outer in
  (* Create result expression *)
  let expr_ml =
    match exprs_elem_ml with
    | [] -> Ml.BoolE true
    | _ ->
        let pat_ml =
          Ml.TupleP
            (List.map (fun id_stub_ml -> Ml.VarP id_stub_ml) ids_stub_ml)
        in
        let expr_sub_ml =
          List.fold_left
            (fun acc e -> Ml.BinopE ("&&", acc, e))
            (Ml.BoolE true) exprs_elem_ml
        in
        Ml.LetE (pat_ml, expr_ml, expr_sub_ml)
  in
  (ctx, expr_ml)

(* Iteration subtype check *)

(* Option subtype check: [exp <: typ?]

   [match expr with None -> true | Some x -> sub(x, T)] *)

and compile_sub_exp_opt ~(tparams : string list) (ctx : Ctx.t) (exp : exp)
    (typ : typ) : Ctx.t * Ml.expr =
  (* Compile expression *)
  let ctx, expr_ml = compile_exp ~tparams ctx exp in
  (* Save context for promotion *)
  let ctx_outer = ctx in
  (* Create stub expression for option element *)
  let ctx, id_stub_ml = Stub.OCaml.var ctx "elem_opt__" in
  let exp_stub = Stub.SpecTec.var id_stub_ml typ in
  (* Compile subtype check for option element *)
  let ctx, expr_cond_ml = compile_sub_exp ~tparams ctx exp_stub typ in
  (* Promote preamble *)
  let ctx = Ctx.promote_preamble ctx ctx_outer in
  (* Create match expression *)
  let expr_ml =
    match expr_cond_ml with
    | Ml.BoolE true -> Ml.BoolE true
    | _ ->
        let arm_none_ml = (Ml.OptP None, Ml.BoolE true) in
        let arm_some_ml = (Ml.OptP (Some (Ml.VarP id_stub_ml)), expr_cond_ml) in
        Ml.MatchE (expr_ml, [ arm_none_ml; arm_some_ml ])
  in
  (ctx, expr_ml)

(* List subtype check: [exp <: typ*]

   [List.for_all (fun x -> sub(x, T)) expr] *)

and compile_sub_exp_list ~(tparams : string list) (ctx : Ctx.t) (exp : exp)
    (typ : typ) : Ctx.t * Ml.expr =
  (* Compile expression *)
  let ctx, expr_ml = compile_exp ~tparams ctx exp in
  (* Save context for promotion *)
  let ctx_outer = ctx in
  (* Create stub expression for list element *)
  let ctx, id_stub_ml = Stub.OCaml.var ctx "elem_list__" in
  let exp_stub = Stub.SpecTec.var id_stub_ml typ in
  (* Compile subtype check for list element *)
  let ctx, expr_cond_ml = compile_sub_exp ~tparams ctx exp_stub typ in
  (* Promote preamble *)
  let ctx = Ctx.promote_preamble ctx ctx_outer in
  (* Create result expression *)
  let expr_ml =
    match expr_cond_ml with
    | Ml.BoolE true -> Ml.BoolE true
    | _ ->
        let expr_lambda_ml = Ml.FunE ([ Ml.VarP id_stub_ml ], expr_cond_ml) in
        Ml.AppE (Ml.VarE "List.for_all", [ expr_lambda_ml; expr_ml ])
  in
  (ctx, expr_ml)

and compile_sub_exp_iter ~(tparams : string list) (ctx : Ctx.t) (exp : exp)
    (typ : typ) (iter : iter) : Ctx.t * Ml.expr =
  match iter with
  | Opt -> compile_sub_exp_opt ~tparams ctx exp typ
  | List -> compile_sub_exp_list ~tparams ctx exp typ

and compile_sub_exp ~(tparams : string list) (ctx : Ctx.t) (exp : exp)
    (typ : typ) : Ctx.t * Ml.expr =
  match typ.it with
  | NumT `NatT -> compile_sub_exp_num_nat ~tparams ctx exp
  | VarT (id, targs) -> compile_sub_exp_var ~tparams ctx exp id targs
  | TupleT typs -> compile_sub_exp_tuple ~tparams ctx exp typs
  | IterT (typ, iter) -> compile_sub_exp_iter ~tparams ctx exp typ iter
  | _ -> (ctx, Ml.BoolE true)

(* Pattern match expression: [exp matches pattern]

   CaseP op        ->  [match expr with `Ctor _ -> true | _ -> false]
   ListP Cons      ->  [expr <> []]
   ListP (Fixed n) ->  [List.length expr = n]
   ListP Nil       ->  [expr = []]
   OptP Some       ->  [Option.is_some expr]
   OptP None       ->  [Option.is_none expr] *)

and compile_match_exp ~(tparams : string list) (ctx : Ctx.t) (exp : exp)
    (pattern : pattern) : Ctx.t * Ml.expr =
  let ctx, expr_ml = compile_exp ~tparams ctx exp in
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

(* Tuple expression: [(exp1, .., expn)]

   [(expr1, .., exprn)] *)

and compile_tuple_exp ~(tparams : string list) (ctx : Ctx.t) (exps : exp list) :
    Ctx.t * Ml.expr =
  let ctx, exprs_ml = compile_exps ~tparams ctx exps in
  let expr_ml = Ml.TupleE exprs_ml in
  (ctx, expr_ml)

(* Case expression: [op(exp1, .., expn)]

   [`Ctor(expr1, .., exprn)] *)

and compile_case_exp ~(tparams : string list) (ctx : Ctx.t) (typ_exp : typ)
    (notexp : notexp) : Ctx.t * Ml.expr =
  let mixop, exps = Mixfix.split notexp in
  let ctor_ml = Ctx.find_ctor ctx typ_exp mixop in
  let ctx, exprs_ml = compile_exps ~tparams ctx exps in
  let expr_ml = Ml.VariantE (ctor_ml, exprs_ml) in
  (ctx, expr_ml)

(* Record expression: [{a1=exp1, .., an=expn}]

   [({field1=expr1; ..; fieldn=exprn} : T)] *)

and compile_str_exp ~(tparams : string list) (ctx : Ctx.t) (typ_exp : typ)
    (expfields : (atom * exp) list) : Ctx.t * Ml.expr =
  let ctx, exprfields_ml =
    List.fold_left
      (fun (ctx, exprfields_ml) (atom, exp) ->
        let field_ml = Names.field atom in
        let ctx, expr_ml = compile_exp ~tparams ctx exp in
        let exprfield_ml = (field_ml, expr_ml) in
        (ctx, exprfields_ml @ [ exprfield_ml ]))
      (ctx, []) expfields
  in
  let expr_ml = Ml.RecordE exprfields_ml in
  let typ_ml = Type.compile_typ ~tparams typ_exp in
  let expr_ml = Ml.AnnotE (expr_ml, typ_ml) in
  (ctx, expr_ml)

(* Option expression: [exp?]

   None    ->  [None]
   Some e  ->  [Some expr] *)

and compile_opt_exp ~(tparams : string list) (ctx : Ctx.t)
    (exp_opt : exp option) : Ctx.t * Ml.expr =
  match exp_opt with
  | None ->
      let expr_ml = Ml.OptE None in
      (ctx, expr_ml)
  | Some exp ->
      let ctx, expr_ml = compile_exp ~tparams ctx exp in
      let expr_ml = Ml.OptE (Some expr_ml) in
      (ctx, expr_ml)

(* List expression: [[exp1, .., expn]]

   [[expr1; ..; exprn]] *)

and compile_list_exp ~(tparams : string list) (ctx : Ctx.t) (exps : exp list) :
    Ctx.t * Ml.expr =
  let ctx, exprs_ml = compile_exps ~tparams ctx exps in
  let expr_ml = Ml.ListE exprs_ml in
  (ctx, expr_ml)

(* Cons expression: [exp_h :: exp_t]

   [expr_h :: expr_t] *)

and compile_cons_exp ~(tparams : string list) (ctx : Ctx.t) (exp_h : exp)
    (exp_t : exp) : Ctx.t * Ml.expr =
  let ctx, expr_h_ml = compile_exp ~tparams ctx exp_h in
  let ctx, expr_t_ml = compile_exp ~tparams ctx exp_t in
  let expr_ml = Ml.ConsE (expr_h_ml, expr_t_ml) in
  (ctx, expr_ml)

(* Concatenation expression: [exp_l ++ exp_r]

   text  ->  [expr_l ^ expr_r]
   list  ->  [expr_l @ expr_r] *)

and compile_cat_exp ~(tparams : string list) (ctx : Ctx.t) (typ_exp : typ)
    (exp_l : exp) (exp_r : exp) : Ctx.t * Ml.expr =
  let binop_ml = match typ_exp.it with TextT -> "^" | _ -> "@" in
  let ctx, expr_l_ml = compile_exp ~tparams ctx exp_l in
  let ctx, expr_r_ml = compile_exp ~tparams ctx exp_r in
  let expr_ml = Ml.BinopE (binop_ml, expr_l_ml, expr_r_ml) in
  (ctx, expr_ml)

(* Membership expression: [exp_e <- exp_s]

   [List.mem expr_e expr_s] *)

and compile_mem_exp ~(tparams : string list) (ctx : Ctx.t) (exp_e : exp)
    (exp_s : exp) : Ctx.t * Ml.expr =
  let ctx, expr_e_ml = compile_exp ~tparams ctx exp_e in
  let ctx, expr_s_ml = compile_exp ~tparams ctx exp_s in
  let expr_ml = Ml.AppE (Ml.VarE "List.mem", [ expr_e_ml; expr_s_ml ]) in
  (ctx, expr_ml)

(* Length expression: [|exp|]

   text  ->  [Bigint.of_int (String.length expr)]
   list  ->  [Bigint.of_int (List.length expr)] *)

and compile_len_exp ~(tparams : string list) (ctx : Ctx.t) (exp : exp) :
    Ctx.t * Ml.expr =
  let ctx, expr_ml = compile_exp ~tparams ctx exp in
  let id_len_ml =
    match exp.note with TextT -> "String.length" | _ -> "List.length"
  in
  let expr_ml =
    Ml.AppE
      (Ml.VarE "Bigint.of_int", [ Ml.AppE (Ml.VarE id_len_ml, [ expr_ml ]) ])
  in
  (ctx, expr_ml)

(* Field access expression: [exp_b.atom]

   [expr_b.field] *)

and compile_dot_exp ~(tparams : string list) (ctx : Ctx.t) (exp_b : exp)
    (atom : atom) : Ctx.t * Ml.expr =
  let field_ml = Names.field atom in
  let ctx, expr_b_ml = compile_exp ~tparams ctx exp_b in
  let expr_ml = Ml.FieldE (expr_b_ml, field_ml) in
  (ctx, expr_ml)

(* Index expression: [exp_b[exp_i]]

   text  ->  [String.sub expr_b (to_int i) 1]
   list  ->  [List.nth expr_b (to_int i)] *)

and compile_idx_exp ~(tparams : string list) (ctx : Ctx.t) (exp_b : exp)
    (exp_i : exp) : Ctx.t * Ml.expr =
  let ctx, expr_b_ml = compile_exp ~tparams ctx exp_b in
  let ctx, expr_i_ml = compile_exp ~tparams ctx exp_i in
  let expr_i_ml = Ml.AppE (Ml.VarE "Bigint.to_int_exn", [ expr_i_ml ]) in
  let expr_ml =
    match exp_b.note with
    | TextT ->
        Ml.AppE (Ml.VarE "String.sub", [ expr_b_ml; expr_i_ml; Ml.LitE "1" ])
    | _ -> Ml.AppE (Ml.VarE "List.nth", [ expr_b_ml; expr_i_ml ])
  in
  (ctx, expr_ml)

(* Slice expression: [exp_b[exp_i : exp_n]]

   text  ->  [String.sub expr_b (to_int i) (to_int n)]
   list  ->  [List.filteri (fun j _ -> i <= j && j < i+n) expr_b] *)

and compile_slice_exp ~(tparams : string list) (ctx : Ctx.t) (exp_b : exp)
    (exp_i : exp) (exp_n : exp) : Ctx.t * Ml.expr =
  let ctx, expr_b_ml = compile_exp ~tparams ctx exp_b in
  let ctx, expr_i_ml = compile_exp ~tparams ctx exp_i in
  let expr_i_ml = Ml.AppE (Ml.VarE "Bigint.to_int_exn", [ expr_i_ml ]) in
  let ctx, expr_n_ml = compile_exp ~tparams ctx exp_n in
  let expr_n_ml = Ml.AppE (Ml.VarE "Bigint.to_int_exn", [ expr_n_ml ]) in
  match exp_b.note with
  | TextT ->
      let expr_ml =
        Ml.AppE (Ml.VarE "String.sub", [ expr_b_ml; expr_i_ml; expr_n_ml ])
      in
      (ctx, expr_ml)
  | _ ->
      let ctx_outer = ctx in
      let ctx, id_stub_ml = Stub.OCaml.var ctx "elem_list__" in
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
      let ctx = Ctx.promote_preamble ctx ctx_outer in
      (ctx, expr_ml)

(* Update expressions *)

(* Path read helper: [access(path, expr_b)]

   RootP         ->  [expr_b]
   DotP(p, f)    ->  [access(p, expr_b).field]
   IdxP(p, i)    ->  [access(p, expr_b)[i]]
   SliceP(p,i,n) ->  [access(p, expr_b)[i..i+n-1]] *)

and compile_access_path_idx ~(tparams : string list) (ctx : Ctx.t) (path : path)
    (exp_i : exp) (expr_b_ml : Ml.expr) : Ctx.t * Ml.expr =
  let ctx, expr_inner_ml = compile_access_path ~tparams ctx path expr_b_ml in
  let ctx, expr_i_ml = compile_exp ~tparams ctx exp_i in
  let expr_i_ml = Ml.AppE (Ml.VarE "Bigint.to_int_exn", [ expr_i_ml ]) in
  let expr_ml =
    match path.note with
    | Il.TextT ->
        Ml.AppE (Ml.VarE "String.sub", [ expr_inner_ml; expr_i_ml; Ml.LitE "1" ])
    | _ -> Ml.AppE (Ml.VarE "List.nth", [ expr_inner_ml; expr_i_ml ])
  in
  (ctx, expr_ml)

and compile_access_path_slice ~(tparams : string list) (ctx : Ctx.t)
    (path : path) (exp_i : exp) (exp_n : exp) (expr_b_ml : Ml.expr) :
    Ctx.t * Ml.expr =
  let ctx, expr_inner_ml = compile_access_path ~tparams ctx path expr_b_ml in
  let ctx, expr_i_ml = compile_exp ~tparams ctx exp_i in
  let expr_i_ml = Ml.AppE (Ml.VarE "Bigint.to_int_exn", [ expr_i_ml ]) in
  let ctx, expr_n_ml = compile_exp ~tparams ctx exp_n in
  let expr_n_ml = Ml.AppE (Ml.VarE "Bigint.to_int_exn", [ expr_n_ml ]) in
  match path.note with
  | Il.TextT ->
      ( ctx,
        Ml.AppE (Ml.VarE "String.sub", [ expr_inner_ml; expr_i_ml; expr_n_ml ])
      )
  | _ ->
      let ctx_outer = ctx in
      let ctx, id_j_ml = Stub.OCaml.var ctx "j" in
      let ctx = Ctx.promote_preamble ctx ctx_outer in
      let expr_ml =
        Ml.AppE
          ( Ml.VarE "List.filteri",
            [
              Ml.FunE
                ( [ Ml.VarP id_j_ml; Ml.WildP ],
                  Ml.BinopE
                    ( "&&",
                      Ml.BinopE ("<=", expr_i_ml, Ml.VarE id_j_ml),
                      Ml.BinopE
                        ( "<",
                          Ml.VarE id_j_ml,
                          Ml.BinopE ("+", expr_i_ml, expr_n_ml) ) ) );
              expr_inner_ml;
            ] )
      in
      (ctx, expr_ml)

and compile_access_path ~(tparams : string list) (ctx : Ctx.t) (path : path)
    (expr_b_ml : Ml.expr) : Ctx.t * Ml.expr =
  match path.it with
  | RootP -> (ctx, expr_b_ml)
  | DotP (path, atom) ->
      let ctx, expr_ml = compile_access_path ~tparams ctx path expr_b_ml in
      let field_ml = Names.field atom in
      let expr_ml = Ml.FieldE (expr_ml, field_ml) in
      (ctx, expr_ml)
  | IdxP (path, exp_i) ->
      compile_access_path_idx ~tparams ctx path exp_i expr_b_ml
  | SliceP (path, exp_i, exp_n) ->
      compile_access_path_slice ~tparams ctx path exp_i exp_n expr_b_ml

(* Path write helper: [update(path, expr_b, expr_n)]

   RootP         ->  [expr_n]
   DotP(p, f)    ->  [update(p, expr_b, {access(p, expr_b) with field=expr_n})]
   IdxP(p, i)    ->  [update(p, expr_b, List.mapi ...)]
   SliceP(p,i,n) ->  [update(p, expr_b, List.mapi ...)] *)

(* Index update: [exp_b[i] <- exp_n]

   text  ->  [expr_b[0..i-1] ^ expr_n ^ expr_b[i+1..]]
   list  ->  [List.mapi (fun j x -> if j = i then expr_n else x) expr_b] *)

and compile_upd_idx_text (ctx : Ctx.t) (expr_ml : Ml.expr) (expr_i_ml : Ml.expr)
    (expr_n_ml : Ml.expr) : Ctx.t * Ml.expr =
  let expr_len_ml = Ml.AppE (Ml.VarE "String.length", [ expr_ml ]) in
  let expr_h_ml =
    Ml.AppE (Ml.VarE "String.sub", [ expr_ml; Ml.LitE "0"; expr_i_ml ])
  in
  let expr_t_ml =
    Ml.AppE
      ( Ml.VarE "String.sub",
        [
          expr_ml;
          Ml.BinopE ("+", expr_i_ml, Ml.LitE "1");
          Ml.BinopE ("-", Ml.BinopE ("-", expr_len_ml, expr_i_ml), Ml.LitE "1");
        ] )
  in
  let expr_ml =
    Ml.BinopE ("^", Ml.BinopE ("^", expr_h_ml, expr_n_ml), expr_t_ml)
  in
  (ctx, expr_ml)

and compile_upd_idx_list (ctx : Ctx.t) (expr_ml : Ml.expr) (expr_i_ml : Ml.expr)
    (expr_n_ml : Ml.expr) : Ctx.t * Ml.expr =
  let ctx_outer = ctx in
  let ctx, id_j_ml = Stub.OCaml.var ctx "j" in
  let ctx, id_x_ml = Stub.OCaml.var ctx "x" in
  let ctx = Ctx.promote_preamble ctx ctx_outer in
  let expr_ml =
    Ml.AppE
      ( Ml.VarE "List.mapi",
        [
          Ml.FunE
            ( [ Ml.VarP id_j_ml; Ml.VarP id_x_ml ],
              Ml.IfE
                ( Ml.BinopE ("=", Ml.VarE id_j_ml, expr_i_ml),
                  expr_n_ml,
                  Some (Ml.VarE id_x_ml) ) );
          expr_ml;
        ] )
  in
  (ctx, expr_ml)

and compile_upd_idx ~(tparams : string list) (ctx : Ctx.t) (path : path)
    (exp_i : exp) (expr_b_ml : Ml.expr) (expr_n_ml : Ml.expr) : Ctx.t * Ml.expr
    =
  let ctx, expr_ml = compile_access_path ~tparams ctx path expr_b_ml in
  let ctx, expr_i_ml = compile_exp ~tparams ctx exp_i in
  let expr_i_ml = Ml.AppE (Ml.VarE "Bigint.to_int_exn", [ expr_i_ml ]) in
  let ctx, expr_ml =
    match path.note with
    | Il.TextT -> compile_upd_idx_text ctx expr_ml expr_i_ml expr_n_ml
    | _ -> compile_upd_idx_list ctx expr_ml expr_i_ml expr_n_ml
  in
  compile_upd ~tparams ctx path expr_b_ml expr_ml

(* Slice update: [exp_b[i : n] <- exp_n]

   text  ->  [expr_b[0..i-1] ^ expr_n ^ expr_b[i+n..]]
   list  ->  [List.mapi (fun j x -> if i <= j && j < i+n then List.nth expr_n (j-i) else x) expr_b] *)

and compile_upd_slice_text (ctx : Ctx.t) (expr_ml : Ml.expr)
    (expr_i_ml : Ml.expr) (expr_n_len_ml : Ml.expr) (expr_n_ml : Ml.expr) :
    Ctx.t * Ml.expr =
  let expr_len_ml = Ml.AppE (Ml.VarE "String.length", [ expr_ml ]) in
  let expr_h_ml =
    Ml.AppE (Ml.VarE "String.sub", [ expr_ml; Ml.LitE "0"; expr_i_ml ])
  in
  let expr_t_ml =
    Ml.AppE
      ( Ml.VarE "String.sub",
        [
          expr_ml;
          Ml.BinopE ("+", expr_i_ml, expr_n_len_ml);
          Ml.BinopE ("-", Ml.BinopE ("-", expr_len_ml, expr_i_ml), expr_n_len_ml);
        ] )
  in
  let expr_ml =
    Ml.BinopE ("^", Ml.BinopE ("^", expr_h_ml, expr_n_ml), expr_t_ml)
  in
  (ctx, expr_ml)

and compile_upd_slice_list (ctx : Ctx.t) (expr_ml : Ml.expr)
    (expr_i_ml : Ml.expr) (expr_n_len_ml : Ml.expr) (expr_n_ml : Ml.expr) :
    Ctx.t * Ml.expr =
  let ctx_outer = ctx in
  let ctx, id_j_ml = Stub.OCaml.var ctx "j" in
  let ctx, id_x_ml = Stub.OCaml.var ctx "x" in
  let ctx = Ctx.promote_preamble ctx ctx_outer in
  let expr_idx_hi_ml = Ml.BinopE ("+", expr_i_ml, expr_n_len_ml) in
  let expr_ml =
    Ml.AppE
      ( Ml.VarE "List.mapi",
        [
          Ml.FunE
            ( [ Ml.VarP id_j_ml; Ml.VarP id_x_ml ],
              Ml.IfE
                ( Ml.BinopE
                    ( "&&",
                      Ml.BinopE ("<=", expr_i_ml, Ml.VarE id_j_ml),
                      Ml.BinopE ("<", Ml.VarE id_j_ml, expr_idx_hi_ml) ),
                  Ml.AppE
                    ( Ml.VarE "List.nth",
                      [ expr_n_ml; Ml.BinopE ("-", Ml.VarE id_j_ml, expr_i_ml) ]
                    ),
                  Some (Ml.VarE id_x_ml) ) );
          expr_ml;
        ] )
  in
  (ctx, expr_ml)

and compile_upd_slice ~(tparams : string list) (ctx : Ctx.t) (path : path)
    (exp_i : exp) (exp_n_len : exp) (expr_b_ml : Ml.expr) (expr_n_ml : Ml.expr)
    : Ctx.t * Ml.expr =
  let ctx, expr_ml = compile_access_path ~tparams ctx path expr_b_ml in
  let ctx, expr_i_ml = compile_exp ~tparams ctx exp_i in
  let expr_i_ml = Ml.AppE (Ml.VarE "Bigint.to_int_exn", [ expr_i_ml ]) in
  let ctx, expr_n_len_ml = compile_exp ~tparams ctx exp_n_len in
  let expr_n_len_ml =
    Ml.AppE (Ml.VarE "Bigint.to_int_exn", [ expr_n_len_ml ])
  in
  let ctx, expr_ml =
    match path.note with
    | Il.TextT ->
        compile_upd_slice_text ctx expr_ml expr_i_ml expr_n_len_ml expr_n_ml
    | _ -> compile_upd_slice_list ctx expr_ml expr_i_ml expr_n_len_ml expr_n_ml
  in
  compile_upd ~tparams ctx path expr_b_ml expr_ml

(* Record field update: [exp_b.atom <- exp_n]

   [update(p, expr_b, {access(p, expr_b) with field = expr_n})] *)

and compile_upd_dot ~(tparams : string list) (ctx : Ctx.t) (path : path)
    (atom : atom) (expr_b_ml : Ml.expr) (expr_n_ml : Ml.expr) : Ctx.t * Ml.expr
    =
  let ctx, expr_ml = compile_access_path ~tparams ctx path expr_b_ml in
  let expr_ml =
    let field_ml = Names.field atom in
    Ml.RecordUpdateE (expr_ml, [ (field_ml, expr_n_ml) ])
  in
  compile_upd ~tparams ctx path expr_b_ml expr_ml

and compile_upd ~(tparams : string list) (ctx : Ctx.t) (path : path)
    (expr_b_ml : Ml.expr) (expr_n_ml : Ml.expr) : Ctx.t * Ml.expr =
  match path.it with
  | RootP -> (ctx, expr_n_ml)
  | DotP (path, atom) ->
      compile_upd_dot ~tparams ctx path atom expr_b_ml expr_n_ml
  | IdxP (path, exp_i) ->
      compile_upd_idx ~tparams ctx path exp_i expr_b_ml expr_n_ml
  | SliceP (path, exp_i, exp_n_len) ->
      compile_upd_slice ~tparams ctx path exp_i exp_n_len expr_b_ml expr_n_ml

and compile_upd_exp ~(tparams : string list) (ctx : Ctx.t) (exp_b : exp)
    (path : path) (exp_n : exp) : Ctx.t * Ml.expr =
  let ctx, expr_b_ml = compile_exp ~tparams ctx exp_b in
  let ctx, expr_n_ml = compile_exp ~tparams ctx exp_n in
  compile_upd ~tparams ctx path expr_b_ml expr_n_ml

(* Call expressions: [id(args)]

   [f_id expr_args] *)

and compile_arg ~(tparams : string list) (ctx : Ctx.t) (arg : arg) :
    Ctx.t * Ml.expr =
  match arg.it with
  | ExpA exp ->
      let ctx, expr_ml = compile_exp ~tparams ctx exp in
      (ctx, expr_ml)
  | DefA id ->
      let id_ml = Names.func id in
      let expr_ml =
        match Ctx.find_func_tparams ctx id.it with
        | Some callee_tparams when callee_tparams <> [] ->
            List.iter
              (fun (tparam : Il.tparam) ->
                if not (List.mem tparam.it tparams) then
                  failwith
                    (Printf.sprintf
                       "compile_arg: %s: callee type parameter %s is not among \
                        the caller's type parameters"
                       id.it tparam.it))
              callee_tparams;
            let exprs_converter_ml =
              List.concat_map
                (fun (tparam : Il.tparam) ->
                  let typ_tparam = Il.VarT (tparam, []) $ no_region in
                  let converter =
                    Interface.Converter.resolve ctx tparams typ_tparam
                  in
                  let expr_typ_ml =
                    Interface.Dynamic_gen.make_typ_expr ~tparams typ_tparam
                  in
                  [ converter.marshal; converter.unmarshal; expr_typ_ml ])
                callee_tparams
            in
            Ml.AppE (Ml.VarE id_ml, exprs_converter_ml)
        | Some _ | None -> Ml.VarE id_ml
      in
      (ctx, expr_ml)

and compile_args ~(tparams : string list) (ctx : Ctx.t) (args : arg list) :
    Ctx.t * Ml.expr list =
  List.fold_left
    (fun (ctx, exprs_ml) arg ->
      let ctx, expr_ml = compile_arg ~tparams ctx arg in
      (ctx, exprs_ml @ [ expr_ml ]))
    (ctx, []) args

and compile_call_exp ~(tparams : string list) (ctx : Ctx.t) (id : id)
    (targs : targ list) (args : arg list) : Ctx.t * Ml.expr =
  let id_func_ml = Names.func id in
  let ctx, exprs_arg_ml = compile_args ~tparams ctx args in
  match Ctx.find_func_tparams ctx id.it with
  | Some callee_tparams
    when callee_tparams <> [] && List.length callee_tparams = List.length targs
    ->
      let exprs_converter_ml =
        List.concat_map
          (fun targ ->
            let converter = Interface.Converter.resolve ctx tparams targ in
            let expr_typ_ml =
              Interface.Dynamic_gen.make_typ_expr ~tparams targ
            in
            [ converter.marshal; converter.unmarshal; expr_typ_ml ])
          targs
      in
      (ctx, Ml.AppE (Ml.VarE id_func_ml, exprs_converter_ml @ exprs_arg_ml))
  | _ -> (ctx, Ml.AppE (Ml.VarE id_func_ml, exprs_arg_ml))

(* Iterator expressions *)

(* Option iterator expression: [exp{x?}]

   single var  ->  [Option.map (fun x -> expr) x?]
   multi-var   ->  [Option.fold_N_1 (fun x .. -> expr) x? y? ..]
                   (fuses [Option.map f (Option.combineN x? y? ..)]) *)

and compile_iter_exp_opt ~(tparams : string list) (ctx : Ctx.t) (exp : exp)
    (vars : var list) : Ctx.t * Ml.expr =
  (* Fetch iteration target variables *)
  let ids_opt_ml =
    List.map
      (fun (id, _, iters) -> Ctx.find_binding ctx (id, iters @ [ Il.Opt ]))
      vars
  in
  let ctx_outer = ctx in
  (* Create stub variables for iterated elements *)
  let ctx, ids_stub_ml = Stub.OCaml.iterator ~prefix:"elem_opt__" ctx vars in
  let n = List.length ids_opt_ml in
  (* Compile body expression *)
  let ctx, expr_body_ml = compile_exp ~tparams ctx exp in
  let ctx, expr_ml =
    if n >= 2 then
      (* Fuse [Option.map f (combineN o0 ..)] into [Option.fold_N_1 f o0 ..] *)
      Common.make_opt_fold ctx ids_opt_ml ids_stub_ml expr_body_ml n 1
    else
      (* Single guide: a plain Option.map, no combine to eliminate. *)
      let pat_ml =
        match ids_stub_ml with
        | [ id_ml ] -> Ml.VarP id_ml
        | ids_ml -> Ml.TupleP (List.map (fun id_ml -> Ml.VarP id_ml) ids_ml)
      in
      let expr_lambda_ml = Ml.FunE ([ pat_ml ], expr_body_ml) in
      let ctx, expr_opt_ml =
        match ids_opt_ml with
        | [ id_ml ] -> (ctx, Ml.VarE id_ml)
        | _ ->
            let ctx = Ctx.add_opt_combine ctx n in
            let id_combine_ml = "Option.combine" ^ string_of_int n in
            let exprs_arg_ml =
              List.map (fun id_opt_ml -> Ml.VarE id_opt_ml) ids_opt_ml
            in
            (ctx, Ml.AppE (Ml.VarE id_combine_ml, exprs_arg_ml))
      in
      (ctx, Ml.AppE (Ml.VarE "Option.map", [ expr_lambda_ml; expr_opt_ml ]))
  in
  (* Promote preamble *)
  let ctx = Ctx.promote_preamble ctx ctx_outer in
  (ctx, expr_ml)

(* List iterator expression: [exp{x*}]

   single var  ->  [List.map (fun x -> expr) x*]
   multi-var   ->  [List.fold_left_N_1 (fun x .. -> expr) x* y* ..]
                   (fuses [List.map f (List.combineN x* y* ..)]) *)

and compile_iter_exp_list ~(tparams : string list) (ctx : Ctx.t) (exp : exp)
    (vars : var list) : Ctx.t * Ml.expr =
  (* Save outer context for promotion *)
  let ctx_outer = ctx in
  (* Fetch iteration target variables *)
  let ids_list_ml =
    List.map
      (fun (id, _, iters) -> Ctx.find_binding ctx (id, iters @ [ Il.List ]))
      vars
  in
  (* Create stub variables for iterated elements *)
  let ctx, ids_stub_ml = Stub.OCaml.iterator ~prefix:"elem_list__" ctx vars in
  let n = List.length ids_list_ml in
  (* Compile body expression *)
  let ctx, expr_body_ml = compile_exp ~tparams ctx exp in
  let ctx, expr_ml =
    if n >= 2 then
      (* Fuse [List.map f (combineN l0 ..)] into a single [fold_left_N_1 f l0 ..] *)
      Common.make_list_fold ctx ids_list_ml ids_stub_ml expr_body_ml n 1
    else
      (* Single guide: a plain List.map, no combine to eliminate. *)
      let pat_ml =
        match ids_stub_ml with
        | [ id_ml ] -> Ml.VarP id_ml
        | ids_ml -> Ml.TupleP (List.map (fun id_ml -> Ml.VarP id_ml) ids_ml)
      in
      let expr_lambda_ml = Ml.FunE ([ pat_ml ], expr_body_ml) in
      let ctx, expr_list_ml =
        match ids_list_ml with
        | [ id_ml ] -> (ctx, Ml.VarE id_ml)
        | _ ->
            let ctx = Ctx.add_list_combine ctx n in
            let id_combine_ml = "List.combine" ^ string_of_int n in
            let exprs_arg_ml =
              List.map (fun id_list_ml -> Ml.VarE id_list_ml) ids_list_ml
            in
            (ctx, Ml.AppE (Ml.VarE id_combine_ml, exprs_arg_ml))
      in
      (ctx, Ml.AppE (Ml.VarE "List.map", [ expr_lambda_ml; expr_list_ml ]))
  in
  (* Promote preamble *)
  let ctx = Ctx.promote_preamble ctx ctx_outer in
  (ctx, expr_ml)

and compile_iter_exp ~(tparams : string list) (ctx : Ctx.t) (typ_exp : typ)
    (exp : exp) (iterexp : iterexp) : Ctx.t * Ml.expr =
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
      | Opt -> compile_iter_exp_opt ~tparams ctx exp vars
      | List -> compile_iter_exp_list ~tparams ctx exp vars)
