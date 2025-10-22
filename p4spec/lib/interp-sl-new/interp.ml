open Domain.Lib
open Xl
open Sl.Ast
module InputHint = Runtime_static.Rel.InputHint
module Typ = Runtime_dynamic.Typ
module Value = Runtime_dynamic.Value
module Cache = Runtime_dynamic.Cache
module Rel = Runtime_dynamic_sl.Rel
open Runtime_dynamic_sl.Envs
module Sim = Runtime_simulator.Simulator
module Dep = Runtime_testgen.Dep
module SCov = Runtime_testgen.Cov.Single
module MCov = Runtime_testgen.Cov.Multiple
open Error
module F = Format
open Util.Source

(* Option monad *)

let ( let* ) = Option.bind

(* Cache *)

let func_cache = ref (Cache.Cache.create ~size:10000)
let rule_cache = ref (Cache.Cache.create ~size:10000)

module Make (Arch : Sim.ARCH) : Sim.INTERP = struct
  (* Assignments *)

  (* Assigning a value to an expression *)

  let rec assign_exp (ctx : Ctx.t) (exp : exp) (value : value) : Ctx.t =
    let note = value.note.typ in
    match (exp.it, value.it) with
    | VarE id, _ ->
        let ctx = Ctx.add_value Local ctx (id, []) value in
        ctx
    | TupleE exps_inner, TupleV values_inner ->
        let ctx = assign_exps ctx exps_inner values_inner in
        List.iter
          (fun value_inner ->
            Ctx.add_edge ctx value_inner value Dep.Edges.Assign)
          values_inner;
        ctx
    | CaseE notexp, CaseV (_mixop_value, values_inner) ->
        let _mixop_exp, exps_inner = notexp in
        let ctx = assign_exps ctx exps_inner values_inner in
        List.iter
          (fun value_inner ->
            Ctx.add_edge ctx value_inner value Dep.Edges.Assign)
          values_inner;
        ctx
    | OptE exp_opt, OptV value_opt -> (
        match (exp_opt, value_opt) with
        | Some exp_inner, Some value_inner ->
            let ctx = assign_exp ctx exp_inner value_inner in
            Ctx.add_edge ctx value_inner value Dep.Edges.Assign;
            ctx
        | None, None -> ctx
        | _ -> assert false)
    | ListE exps_inner, ListV values_inner ->
        let ctx = assign_exps ctx exps_inner values_inner in
        List.iter
          (fun value_inner ->
            Ctx.add_edge ctx value_inner value Dep.Edges.Assign)
          values_inner;
        ctx
    | ConsE (exp_h, exp_t), ListV values_inner ->
        let value_h = List.hd values_inner in
        let value_t =
          let vid = Value.fresh () in
          let typ = note in
          Il.Ast.(ListV (List.tl values_inner) $$$ { vid; typ })
        in
        Ctx.add_node ctx value_t;
        let ctx = assign_exp ctx exp_h value_h in
        Ctx.add_edge ctx value_h value Dep.Edges.Assign;
        let ctx = assign_exp ctx exp_t value_t in
        Ctx.add_edge ctx value_t value Dep.Edges.Assign;
        ctx
    | IterE (_, (Opt, vars)), OptV None ->
        (* Per iterated variable, make an option out of the value *)
        List.fold_left
          (fun ctx (id, typ, iters) ->
            let value_sub =
              let vid = Value.fresh () in
              let typ = Typ.iterate typ (iters @ [ Il.Ast.Opt ]) in
              Il.Ast.(OptV None $$$ { vid; typ = typ.it })
            in
            Ctx.add_node ctx value_sub;
            Ctx.add_edge ctx value_sub value Dep.Edges.Assign;
            Ctx.add_value Local ctx (id, iters @ [ Il.Ast.Opt ]) value_sub)
          ctx vars
    | IterE (exp, (Opt, vars)), OptV (Some value) ->
        (* Assign the value to the iterated expression *)
        let ctx = assign_exp ctx exp value in
        (* Per iterated variable, make an option out of the value *)
        List.fold_left
          (fun ctx (id, typ, iters) ->
            let value_sub =
              let value = Ctx.find_value Local ctx (id, iters) in
              let vid = Value.fresh () in
              let typ = Typ.iterate typ (iters @ [ Il.Ast.Opt ]) in
              Il.Ast.(OptV (Some value) $$$ { vid; typ = typ.it })
            in
            Ctx.add_node ctx value_sub;
            Ctx.add_edge ctx value_sub value Dep.Edges.Assign;
            Ctx.add_value Local ctx (id, iters @ [ Il.Ast.Opt ]) value_sub)
          ctx vars
    | IterE (exp, (List, vars)), ListV values ->
        (* Map over the value list elements,
           and assign each value to the iterated expression *)
        let ctxs =
          List.fold_left
            (fun ctxs value ->
              let ctx = Ctx.localize_clear ctx in
              let ctx = assign_exp ctx exp value in
              ctxs @ [ ctx ])
            [] values
        in
        (* Per iterated variable, collect its elementwise value,
           then make a sequence out of them *)
        List.fold_left
          (fun ctx (id, typ, iters) ->
            let values =
              List.map (fun ctx -> Ctx.find_value Local ctx (id, iters)) ctxs
            in
            let value_sub =
              let vid = Value.fresh () in
              let typ = Typ.iterate typ (iters @ [ Il.Ast.List ]) in
              Il.Ast.(ListV values $$$ { vid; typ = typ.it })
            in
            Ctx.add_node ctx value_sub;
            Ctx.add_edge ctx value_sub value Dep.Edges.Assign;
            Ctx.add_value Local ctx (id, iters @ [ Il.Ast.List ]) value_sub)
          ctx vars
    | _ ->
        error exp.at
          (F.asprintf "(TODO) match failed %s <- %s"
             (Sl.Print.string_of_exp exp)
             (Sl.Print.string_of_value ~short:true value))

  and assign_exps (ctx : Ctx.t) (exps : exp list) (values : value list) : Ctx.t
      =
    check
      (List.length exps = List.length values)
      (over_region (List.map at exps))
      (F.asprintf
         "mismatch in number of expressions and values while assigning, \
          expected %d value(s) but got %d"
         (List.length exps) (List.length values));
    List.fold_left2 assign_exp ctx exps values

  (* Assigning a value to an argument *)

  and assign_arg (ctx_caller : Ctx.t) (ctx_callee : Ctx.t) (arg : arg)
      (value : value) : Ctx.t =
    match arg.it with
    | ExpA exp -> assign_arg_exp ctx_callee exp value
    | DefA id -> assign_arg_def ctx_caller ctx_callee id value

  and assign_args (ctx_caller : Ctx.t) (ctx_callee : Ctx.t) (args : arg list)
      (values : value list) : Ctx.t =
    check
      (List.length args = List.length values)
      (over_region (List.map at args))
      (F.asprintf
         "mismatch in number of arguments and values while assigning, expected \
          %d value(s) but got %d"
         (List.length args) (List.length values));
    List.fold_left2 (assign_arg ctx_caller) ctx_callee args values

  and assign_arg_exp (ctx : Ctx.t) (exp : exp) (value : value) : Ctx.t =
    assign_exp ctx exp value

  and assign_arg_def (ctx_caller : Ctx.t) (ctx_callee : Ctx.t) (id : id)
      (value : value) : Ctx.t =
    match value.it with
    | FuncV id_f ->
        let func = Ctx.find_func Local ctx_caller id_f in
        Ctx.add_func Local ctx_callee id func
    | _ ->
        error id.at
          (F.asprintf "cannot assign a value %s to a definition %s"
             (Sl.Print.string_of_value ~short:true value)
             id.it)

  (* Expression evaluation *)

  (* DownCastE and SubE performs subtype checks that are not guaranteed by the type system,
      because in SpecTec assignment should be able to revert the type cast expression

       - Numeric subtyping:
         - e.g., -- if (int) n = $foo() when $foo() returns a positive integer +2
       - Variant subtyping:
         - e.g., -- if (typ) objtyp = $foo() when $foo() returns a variant of objtyp specifically
       - Tuple subtyping: recursive, but the type system guarantees that their lengths are equal
       - Iteration subtyping

     Note that structs are invariant in SpecTec, so we do not need to check for subtyping *)

  let rec eval_exp (ctx : Ctx.t) (exp : exp) : value =
    let at, note = (exp.at, exp.note) in
    match exp.it with
    | BoolE b -> eval_bool_exp note ctx b
    | NumE n -> eval_num_exp note ctx n
    | TextE s -> eval_text_exp note ctx s
    | VarE id -> eval_var_exp note ctx id
    | UnE (unop, optyp, exp) -> eval_un_exp note ctx unop optyp exp
    | BinE (binop, optyp, exp_l, exp_r) ->
        eval_bin_exp note ctx binop optyp exp_l exp_r
    | CmpE (cmpop, optyp, exp_l, exp_r) ->
        eval_cmp_exp note ctx cmpop optyp exp_l exp_r
    | UpCastE (typ, exp) -> eval_upcast_exp note ctx typ exp
    | DownCastE (typ, exp) -> eval_downcast_exp note ctx typ exp
    | SubE (exp, typ) -> eval_sub_exp note ctx exp typ
    | MatchE (exp, pattern) -> eval_match_exp note ctx exp pattern
    | TupleE exps -> eval_tuple_exp note ctx exps
    | CaseE notexp -> eval_case_exp note ctx notexp
    | StrE fields -> eval_str_exp note ctx fields
    | OptE exp_opt -> eval_opt_exp note ctx exp_opt
    | ListE exps -> eval_list_exp note ctx exps
    | ConsE (exp_h, exp_t) -> eval_cons_exp note ctx exp_h exp_t
    | CatE (exp_l, exp_r) -> eval_cat_exp note ctx at exp_l exp_r
    | MemE (exp_e, exp_s) -> eval_mem_exp note ctx exp_e exp_s
    | LenE exp -> eval_len_exp note ctx exp
    | DotE (exp_b, atom) -> eval_dot_exp note ctx exp_b atom
    | IdxE (exp_b, exp_i) -> eval_idx_exp note ctx exp_b exp_i
    | SliceE (exp_b, exp_l, exp_h) -> eval_slice_exp note ctx exp_b exp_l exp_h
    | UpdE (exp_b, path, exp_f) -> eval_upd_exp note ctx exp_b path exp_f
    | CallE (id, targs, args) -> eval_call_exp note ctx id targs args
    | IterE (exp, iterexp) -> eval_iter_exp note ctx exp iterexp

  and eval_exps (ctx : Ctx.t) (exps : exp list) : value list =
    List.map (eval_exp ctx) exps

  (* Boolean expression evaluation *)

  and eval_bool_exp (note : typ') (ctx : Ctx.t) (b : bool) : value =
    let value_res =
      let vid = Value.fresh () in
      let typ = note in
      Il.Ast.(BoolV b $$$ { vid; typ })
    in
    Ctx.add_node ctx value_res;
    List.iter
      (fun value_input ->
        Ctx.add_edge ctx value_res value_input Dep.Edges.Control)
      (Ctx.find_values_input Ctx.Local ctx);
    value_res

  (* Numeric expression evaluation *)

  and eval_num_exp (note : typ') (ctx : Ctx.t) (n : Num.t) : value =
    let value_res =
      let vid = Value.fresh () in
      let typ = note in
      Il.Ast.(NumV n $$$ { vid; typ })
    in
    Ctx.add_node ctx value_res;
    List.iter
      (fun value_input ->
        Ctx.add_edge ctx value_res value_input Dep.Edges.Control)
      (Ctx.find_values_input Ctx.Local ctx);
    value_res

  (* Text expression evaluation *)

  and eval_text_exp (note : typ') (ctx : Ctx.t) (s : string) : value =
    let value_res =
      let vid = Value.fresh () in
      let typ = note in
      Il.Ast.(TextV s $$$ { vid; typ })
    in
    Ctx.add_node ctx value_res;
    List.iter
      (fun value_input ->
        Ctx.add_edge ctx value_res value_input Dep.Edges.Control)
      (Ctx.find_values_input Ctx.Local ctx);
    value_res

  (* Variable expression evaluation *)

  and eval_var_exp (_note : typ') (ctx : Ctx.t) (id : id) : value =
    Ctx.find_value Local ctx (id, [])

  (* Unary expression evaluation *)

  and eval_un_bool (unop : Bool.unop) (value : value) : value' =
    match unop with `NotOp -> Il.Ast.BoolV (not (Value.get_bool value))

  and eval_un_num (unop : Num.unop) (value : value) : value' =
    let num = Value.get_num value in
    let num = Num.un unop num in
    Il.Ast.NumV num

  and eval_un_exp (note : typ') (ctx : Ctx.t) (unop : unop) (_optyp : optyp)
      (exp : exp) : value =
    let value = eval_exp ctx exp in
    let value_res =
      match unop with
      | #Bool.unop as unop -> eval_un_bool unop value
      | #Num.unop as unop -> eval_un_num unop value
    in
    let value_res =
      let vid = Value.fresh () in
      let typ = note in
      Il.Ast.(value_res $$$ { vid; typ })
    in
    Ctx.add_node ctx value_res;
    Ctx.add_edge ctx value_res value (Dep.Edges.Op (UnOp unop));
    value_res

  (* Binary expression evaluation *)

  and eval_bin_bool (binop : Bool.binop) (value_l : value) (value_r : value) :
      value' =
    let bool_l = Value.get_bool value_l in
    let bool_r = Value.get_bool value_r in
    match binop with
    | `AndOp -> Il.Ast.BoolV (bool_l && bool_r)
    | `OrOp -> Il.Ast.BoolV (bool_l || bool_r)
    | `ImplOp -> Il.Ast.BoolV ((not bool_l) || bool_r)
    | `EquivOp -> Il.Ast.BoolV (bool_l = bool_r)

  and eval_bin_num (binop : Num.binop) (value_l : value) (value_r : value) :
      value' =
    let num_l = Value.get_num value_l in
    let num_r = Value.get_num value_r in
    Il.Ast.NumV (Num.bin binop num_l num_r)

  and eval_bin_exp (note : typ') (ctx : Ctx.t) (binop : binop) (_optyp : optyp)
      (exp_l : exp) (exp_r : exp) : value =
    let value_l = eval_exp ctx exp_l in
    let value_r = eval_exp ctx exp_r in
    let value_res =
      match binop with
      | #Bool.binop as binop -> eval_bin_bool binop value_l value_r
      | #Num.binop as binop -> eval_bin_num binop value_l value_r
    in
    let value_res =
      let vid = Value.fresh () in
      let typ = note in
      Il.Ast.(value_res $$$ { vid; typ })
    in
    Ctx.add_node ctx value_res;
    Ctx.add_edge ctx value_res value_l (Dep.Edges.Op (BinOp binop));
    Ctx.add_edge ctx value_res value_r (Dep.Edges.Op (BinOp binop));
    value_res

  (* Comparison expression evaluation *)

  and eval_cmp_bool (cmpop : Bool.cmpop) (value_l : value) (value_r : value) :
      value' =
    let eq = Value.eq value_l value_r in
    match cmpop with `EqOp -> Il.Ast.BoolV eq | `NeOp -> Il.Ast.BoolV (not eq)

  and eval_cmp_num (cmpop : Num.cmpop) (value_l : value) (value_r : value) :
      value' =
    let num_l = Value.get_num value_l in
    let num_r = Value.get_num value_r in
    Il.Ast.BoolV (Num.cmp cmpop num_l num_r)

  and eval_cmp_exp (note : typ') (ctx : Ctx.t) (cmpop : cmpop) (_optyp : optyp)
      (exp_l : exp) (exp_r : exp) : value =
    let value_l = eval_exp ctx exp_l in
    let value_r = eval_exp ctx exp_r in
    let value_res =
      match cmpop with
      | #Bool.cmpop as cmpop -> eval_cmp_bool cmpop value_l value_r
      | #Num.cmpop as cmpop -> eval_cmp_num cmpop value_l value_r
    in
    let value_res =
      let vid = Value.fresh () in
      let typ = note in
      Il.Ast.(value_res $$$ { vid; typ })
    in
    Ctx.add_node ctx value_res;
    Ctx.add_edge ctx value_res value_l (Dep.Edges.Op (CmpOp cmpop));
    Ctx.add_edge ctx value_res value_r (Dep.Edges.Op (CmpOp cmpop));
    value_res

  (* Upcast expression evaluation *)

  and upcast (ctx : Ctx.t) (typ : typ) (value : value) : value =
    match typ.it with
    | NumT `IntT -> (
        match value.it with
        | NumV (`Nat n) ->
            let value_res =
              let vid = Value.fresh () in
              let typ = typ.it in
              Il.Ast.(NumV (`Int n) $$$ { vid; typ })
            in
            Ctx.add_node ctx value_res;
            Ctx.add_edge ctx value_res value (Dep.Edges.Op (CastOp typ));
            value_res
        | NumV (`Int _) -> value
        | _ -> assert false)
    | VarT (tid, targs) -> (
        let tparams, deftyp = Ctx.find_typdef Local ctx tid in
        match deftyp.it with
        | PlainT typ ->
            let theta = List.combine tparams targs |> TIdMap.of_list in
            let typ = Typ.subst_typ theta typ in
            upcast ctx typ value
        | _ -> value)
    | TupleT typs -> (
        match value.it with
        | TupleV values ->
            let values = List.map2 (upcast ctx) typs values in
            let value_res =
              let vid = Value.fresh () in
              let typ = typ.it in
              Il.Ast.(TupleV values $$$ { vid; typ })
            in
            Ctx.add_node ctx value_res;
            Ctx.add_edge ctx value_res value (Dep.Edges.Op (CastOp typ));
            value_res
        | _ -> assert false)
    | _ -> value

  and eval_upcast_exp (_note : typ') (ctx : Ctx.t) (typ : typ) (exp : exp) :
      value =
    let value = eval_exp ctx exp in
    upcast ctx typ value

  (* Downcast expression evaluation *)

  and downcast (ctx : Ctx.t) (typ : typ) (value : value) : value =
    match typ.it with
    | NumT `NatT -> (
        match value.it with
        | NumV (`Nat _) -> value
        | NumV (`Int i) when Bigint.(i >= zero) ->
            let value_res =
              let vid = Value.fresh () in
              let typ = typ.it in
              Il.Ast.(NumV (`Nat i) $$$ { vid; typ })
            in
            Ctx.add_node ctx value_res;
            Ctx.add_edge ctx value_res value (Dep.Edges.Op (CastOp typ));
            value_res
        | _ -> assert false)
    | VarT (tid, targs) -> (
        let tparams, deftyp = Ctx.find_typdef Local ctx tid in
        match deftyp.it with
        | PlainT typ ->
            let theta = List.combine tparams targs |> TIdMap.of_list in
            let typ = Typ.subst_typ theta typ in
            downcast ctx typ value
        | _ -> value)
    | TupleT typs -> (
        match value.it with
        | TupleV values ->
            let values = List.map2 (downcast ctx) typs values in
            let value_res =
              let vid = Value.fresh () in
              let typ = typ.it in
              Il.Ast.(TupleV values $$$ { vid; typ })
            in
            Ctx.add_node ctx value_res;
            Ctx.add_edge ctx value_res value (Dep.Edges.Op (CastOp typ));
            value_res
        | _ -> assert false)
    | _ -> value

  and eval_downcast_exp (_note : typ') (ctx : Ctx.t) (typ : typ) (exp : exp) :
      value =
    let value = eval_exp ctx exp in
    downcast ctx typ value

  (* Subtype check expression evaluation *)

  and subtyp (ctx : Ctx.t) (typ : typ) (value : value) : bool =
    match typ.it with
    | NumT `NatT -> (
        match value.it with
        | NumV (`Nat _) -> true
        | NumV (`Int i) -> Bigint.(i >= zero)
        | _ -> assert false)
    | VarT (tid, targs) -> (
        let tparams, deftyp = Ctx.find_typdef Local ctx tid in
        let theta = List.combine tparams targs |> TIdMap.of_list in
        match (deftyp.it, value.it) with
        | PlainT typ, _ ->
            let typ = Typ.subst_typ theta typ in
            subtyp ctx typ value
        | VariantT typcases, CaseV (mixop_v, _) ->
            List.exists
              (fun (nottyp, _) ->
                let mixop_t, _ = nottyp.it in
                Mixop.eq mixop_t mixop_v)
              typcases
        | _ -> true)
    | TupleT typs -> (
        match value.it with
        | TupleV values ->
            List.length typs = List.length values
            && List.for_all2 (subtyp ctx) typs values
        | _ -> false)
    | _ -> true

  and eval_sub_exp (note : typ') (ctx : Ctx.t) (exp : exp) (typ : typ) : value =
    let value = eval_exp ctx exp in
    let sub = subtyp ctx typ value in
    let value_res =
      let vid = Value.fresh () in
      let typ = note in
      Il.Ast.(BoolV sub $$$ { vid; typ })
    in
    Ctx.add_node ctx value_res;
    Ctx.add_edge ctx value_res value (Dep.Edges.Op (SubOp typ));
    value_res

  (* Pattern match check expression evaluation *)

  and eval_match_exp (note : typ') (ctx : Ctx.t) (exp : exp) (pattern : pattern)
      : value =
    let value = eval_exp ctx exp in
    let matches =
      match (pattern, value.it) with
      | CaseP mixop_p, CaseV (mixop_v, _) -> Mixop.eq mixop_p mixop_v
      | ListP listpattern, ListV values -> (
          let len_v = List.length values in
          match listpattern with
          | `Cons -> len_v > 0
          | `Fixed len_p -> len_v = len_p
          | `Nil -> len_v = 0)
      | OptP `Some, OptV (Some _) -> true
      | OptP `None, OptV None -> true
      | _ -> false
    in
    let value_res =
      let vid = Value.fresh () in
      let typ = note in
      Il.Ast.(BoolV matches $$$ { vid; typ })
    in
    Ctx.add_node ctx value_res;
    Ctx.add_edge ctx value_res value (Dep.Edges.Op (MatchOp pattern));
    value_res

  (* Tuple expression evaluation *)

  and eval_tuple_exp (note : typ') (ctx : Ctx.t) (exps : exp list) : value =
    let values = eval_exps ctx exps in
    let value_res =
      let vid = Value.fresh () in
      let typ = note in
      Il.Ast.(TupleV values $$$ { vid; typ })
    in
    Ctx.add_node ctx value_res;
    if List.length values = 0 then
      List.iter
        (fun value_input ->
          Ctx.add_edge ctx value_res value_input Dep.Edges.Control)
        (Ctx.find_values_input Ctx.Local ctx);
    value_res

  (* Case expression evaluation *)

  and eval_case_exp (note : typ') (ctx : Ctx.t) (notexp : notexp) : value =
    let mixop, exps = notexp in
    let values = eval_exps ctx exps in
    let value_res =
      let vid = Value.fresh () in
      let typ = note in
      Il.Ast.(CaseV (mixop, values) $$$ { vid; typ })
    in
    Ctx.add_node ctx value_res;
    if List.length values = 0 then
      List.iter
        (fun value_input ->
          Ctx.add_edge ctx value_res value_input Dep.Edges.Control)
        (Ctx.find_values_input Ctx.Local ctx);
    value_res

  (* Struct expression evaluation *)

  and eval_str_exp (note : typ') (ctx : Ctx.t) (fields : (atom * exp) list) :
      value =
    let atoms, exps = List.split fields in
    let values = eval_exps ctx exps in
    let fields = List.combine atoms values in
    let value_res =
      let vid = Value.fresh () in
      let typ = note in
      Il.Ast.(StructV fields $$$ { vid; typ })
    in
    Ctx.add_node ctx value_res;
    if List.length values = 0 then
      List.iter
        (fun value_input ->
          Ctx.add_edge ctx value_res value_input Dep.Edges.Control)
        (Ctx.find_values_input Ctx.Local ctx);
    value_res

  (* Option expression evaluation *)

  and eval_opt_exp (note : typ') (ctx : Ctx.t) (exp_opt : exp option) : value =
    let value_opt = Option.map (eval_exp ctx) exp_opt in
    let value_res =
      let vid = Value.fresh () in
      let typ = note in
      Il.Ast.(OptV value_opt $$$ { vid; typ })
    in
    Ctx.add_node ctx value_res;
    if Option.is_none value_opt then
      List.iter
        (fun value_input ->
          Ctx.add_edge ctx value_res value_input Dep.Edges.Control)
        (Ctx.find_values_input Ctx.Local ctx);
    value_res

  (* List expression evaluation *)

  and eval_list_exp (note : typ') (ctx : Ctx.t) (exps : exp list) : value =
    let values = eval_exps ctx exps in
    let value_res =
      let vid = Value.fresh () in
      let typ = note in
      Il.Ast.(ListV values $$$ { vid; typ })
    in
    Ctx.add_node ctx value_res;
    if List.length values = 0 then
      List.iter
        (fun value_input ->
          Ctx.add_edge ctx value_res value_input Dep.Edges.Control)
        (Ctx.find_values_input Ctx.Local ctx);
    value_res

  (* Cons expression evaluation *)

  and eval_cons_exp (note : typ') (ctx : Ctx.t) (exp_h : exp) (exp_t : exp) :
      value =
    let value_h = eval_exp ctx exp_h in
    let value_t = eval_exp ctx exp_t in
    let values_t = Value.get_list value_t in
    let value_res =
      let vid = Value.fresh () in
      let typ = note in
      Il.Ast.(ListV (value_h :: values_t) $$$ { vid; typ })
    in
    Ctx.add_node ctx value_res;
    value_res

  (* Concatenation expression evaluation *)

  and eval_cat_exp (note : typ') (ctx : Ctx.t) (at : region) (exp_l : exp)
      (exp_r : exp) : value =
    let value_l = eval_exp ctx exp_l in
    let value_r = eval_exp ctx exp_r in
    let value_res =
      match (value_l.it, value_r.it) with
      | TextV s_l, TextV s_r -> Il.Ast.TextV (s_l ^ s_r)
      | ListV values_l, ListV values_r -> Il.Ast.ListV (values_l @ values_r)
      | _ -> error at "concatenation expects either two texts or two lists"
    in
    let value_res =
      let vid = Value.fresh () in
      let typ = note in
      Il.Ast.(value_res $$$ { vid; typ })
    in
    Ctx.add_node ctx value_res;
    Ctx.add_edge ctx value_res value_l (Dep.Edges.Op CatOp);
    Ctx.add_edge ctx value_res value_r (Dep.Edges.Op CatOp);
    value_res

  (* Membership expression evaluation *)

  and eval_mem_exp (note : typ') (ctx : Ctx.t) (exp_e : exp) (exp_s : exp) :
      value =
    let value_e = eval_exp ctx exp_e in
    let value_s = eval_exp ctx exp_s in
    let values_s = Value.get_list value_s in
    let value_res =
      let vid = Value.fresh () in
      let typ = note in
      Il.Ast.(BoolV (List.exists (Value.eq value_e) values_s) $$$ { vid; typ })
    in
    Ctx.add_node ctx value_res;
    Ctx.add_edge ctx value_res value_e (Dep.Edges.Op MemOp);
    Ctx.add_edge ctx value_res value_s (Dep.Edges.Op MemOp);
    value_res

  (* Length expression evaluation *)

  and eval_len_exp (note : typ') (ctx : Ctx.t) (exp : exp) : value =
    let value = eval_exp ctx exp in
    let len = value |> Value.get_list |> List.length |> Bigint.of_int in
    let value_res =
      let vid = Value.fresh () in
      let typ = note in
      Il.Ast.(NumV (`Nat len) $$$ { vid; typ })
    in
    Ctx.add_node ctx value_res;
    Ctx.add_edge ctx value_res value (Dep.Edges.Op LenOp);
    value_res

  (* Dot expression evaluation *)

  and eval_dot_exp (_note : typ') (ctx : Ctx.t) (exp_b : exp) (atom : atom) :
      value =
    let value_b = eval_exp ctx exp_b in
    let fields = Value.get_struct value_b in
    let value_res =
      fields
      |> List.map (fun (atom, value) -> (atom.it, value))
      |> List.assoc atom.it
    in
    value_res

  (* Index expression evaluation *)

  and eval_idx_exp (_note : typ') (ctx : Ctx.t) (exp_b : exp) (exp_i : exp) :
      value =
    let value_b = eval_exp ctx exp_b in
    let value_i = eval_exp ctx exp_i in
    let values = Value.get_list value_b in
    let idx = value_i |> Value.get_num |> Num.to_int |> Bigint.to_int_exn in
    let value_res = List.nth values idx in
    value_res

  (* Slice expression evaluation *)

  and eval_slice_exp (note : typ') (ctx : Ctx.t) (exp_b : exp) (exp_i : exp)
      (exp_n : exp) : value =
    let value_b = eval_exp ctx exp_b in
    let values = Value.get_list value_b in
    let value_i = eval_exp ctx exp_i in
    let idx_l = value_i |> Value.get_num |> Num.to_int |> Bigint.to_int_exn in
    let value_n = eval_exp ctx exp_n in
    let idx_n = value_n |> Value.get_num |> Num.to_int |> Bigint.to_int_exn in
    let idx_h = idx_l + idx_n in
    let values_slice =
      List.mapi
        (fun idx value ->
          if idx_l <= idx && idx < idx_h then Some value else None)
        values
      |> List.filter_map Fun.id
    in
    let value_res =
      let vid = Value.fresh () in
      let typ = note in
      Il.Ast.(ListV values_slice $$$ { vid; typ })
    in
    Ctx.add_node ctx value_res;
    value_res

  (* Update expression evaluation *)

  and eval_access_path (value_b : value) (path : path) : value =
    match path.it with
    | RootP -> value_b
    | DotP (path, atom) ->
        let value = eval_access_path value_b path in
        let fields = value |> Value.get_struct in
        fields
        |> List.map (fun (atom, value) -> (atom.it, value))
        |> List.assoc atom.it
    | _ -> failwith "(TODO) access_path"

  and eval_update_path (ctx : Ctx.t) (value_b : value) (path : path)
      (value_n : value) : value =
    match path.it with
    | RootP -> value_n
    | DotP (path, atom) ->
        let value = eval_access_path value_b path in
        let fields = value |> Value.get_struct in
        let fields =
          List.map
            (fun (atom_f, value_f) ->
              if atom_f.it = atom.it then (atom_f, value_n)
              else (atom_f, value_f))
            fields
        in
        let value =
          let vid = Value.fresh () in
          let typ = path.note in
          Il.Ast.(StructV fields $$$ { vid; typ })
        in
        Ctx.add_node ctx value;
        eval_update_path ctx value_b path value
    | _ -> failwith "(TODO eval_update_path)"

  and eval_upd_exp (_note : typ') (ctx : Ctx.t) (exp_b : exp) (path : path)
      (exp_f : exp) : value =
    let value_b = eval_exp ctx exp_b in
    let value_f = eval_exp ctx exp_f in
    eval_update_path ctx value_b path value_f

  (* Function call expression evaluation *)

  and eval_call_exp (_note : typ') (ctx : Ctx.t) (id : id) (targs : targ list)
      (args : arg list) : value =
    invoke_func ctx id targs args

  (* Iterated expression evaluation *)

  and eval_iter_exp_opt (note : typ') (ctx : Ctx.t) (exp : exp)
      (vars : var list) : value =
    let ctx_sub_opt = Ctx.sub_opt ctx vars in
    let value_res =
      match ctx_sub_opt with
      | Some ctx_sub ->
          let value = eval_exp ctx_sub exp in
          let vid = Value.fresh () in
          let typ = note in
          Il.Ast.(OptV (Some value) $$$ { vid; typ })
      | None ->
          let vid = Value.fresh () in
          let typ = note in
          Il.Ast.(OptV None $$$ { vid; typ })
    in
    Ctx.add_node ctx value_res;
    List.iter
      (fun (id, _typ, iters) ->
        let value_sub = Ctx.find_value Local ctx (id, iters @ [ Il.Ast.Opt ]) in
        Ctx.add_edge ctx value_res value_sub Dep.Edges.Iter)
      vars;
    value_res

  and eval_iter_exp_list (note : typ') (ctx : Ctx.t) (exp : exp)
      (vars : var list) : value =
    let ctxs_sub = Ctx.sub_list ctx vars in
    let values = List.map (fun ctx_sub -> eval_exp ctx_sub exp) ctxs_sub in
    let value_res =
      let vid = Value.fresh () in
      let typ = note in
      Il.Ast.(ListV values $$$ { vid; typ })
    in
    Ctx.add_node ctx value_res;
    List.iter
      (fun (id, _typ, iters) ->
        let value_sub =
          Ctx.find_value Local ctx (id, iters @ [ Il.Ast.List ])
        in
        Ctx.add_edge ctx value_res value_sub Dep.Edges.Iter)
      vars;
    value_res

  and eval_iter_exp (note : typ') (ctx : Ctx.t) (exp : exp) (iterexp : iterexp)
      : value =
    let iter, vars = iterexp in
    match iter with
    | Opt -> eval_iter_exp_opt note ctx exp vars
    | List -> eval_iter_exp_list note ctx exp vars

  (* Argument evaluation *)

  and eval_arg (ctx : Ctx.t) (arg : arg) : value =
    match arg.it with
    | ExpA exp -> eval_exp ctx exp
    | DefA id ->
        let value_res =
          let vid = Value.fresh () in
          let typ = Il.Ast.FuncT in
          Il.Ast.(FuncV id $$$ { vid; typ })
        in
        Ctx.add_node ctx value_res;
        value_res

  and eval_args (ctx : Ctx.t) (args : arg list) : value list =
    List.map (eval_arg ctx) args

  (* Instruction evaluation *)

  and eval_instr (ctx : Ctx.t) (instr : instr) : Ctx.t * Sign.t =
    match instr.it with
    | IfI (exp_cond, iterexps, instrs_then, phantom_opt) ->
        eval_if_instr ctx exp_cond iterexps instrs_then phantom_opt
    | HoldI (id, notexp, iterexps, holdcase) ->
        eval_hold_instr ctx id notexp iterexps holdcase
    | CaseI (exp, cases, phantom_opt) ->
        eval_case_instr ctx exp cases phantom_opt
    | OtherwiseI instr -> eval_instr ctx instr
    | GroupI (id_group, exps_group, instrs_group) ->
        eval_group_instr ctx id_group exps_group instrs_group
    | LetI (exp_l, exp_r, iterexps) -> eval_let_instr ctx exp_l exp_r iterexps
    | RuleI (id, notexp, iterexps) -> eval_rule_instr ctx id notexp iterexps
    | ResultI exps -> eval_result_instr ctx exps
    | ReturnI exp -> eval_return_instr ctx exp
    | DebugI exp -> eval_debug_instr ctx exp

  and eval_instrs (ctx : Ctx.t) (sign : Sign.t) (instrs : instr list) :
      Ctx.t * Sign.t =
    List.fold_left
      (fun (ctx, sign) instr ->
        match sign with Sign.Cont -> eval_instr ctx instr | _ -> (ctx, sign))
      (ctx, sign) instrs

  (* If instruction evaluation *)

  and eval_if_cond (ctx : Ctx.t) (exp_cond : exp) : bool * value =
    let value_cond = eval_exp ctx exp_cond in
    let cond = Value.get_bool value_cond in
    (cond, value_cond)

  and eval_if_cond_list (ctx : Ctx.t) (exp_cond : exp) (vars : var list)
      (iterexps : iterexp list) : bool * value list =
    let ctxs_sub = Ctx.sub_list ctx vars in
    List.fold_left
      (fun (cond, values_cond) ctx_sub ->
        if not cond then (cond, values_cond)
        else
          let cond, value_cond = eval_if_cond_iter' ctx_sub exp_cond iterexps in
          let values_cond = values_cond @ [ value_cond ] in
          (cond, values_cond))
      (true, []) ctxs_sub

  and eval_if_cond_iter' (ctx : Ctx.t) (exp_cond : exp)
      (iterexps : iterexp list) : bool * value =
    match iterexps with
    | [] -> eval_if_cond ctx exp_cond
    | iterexp_h :: iterexps_t -> (
        let iter_h, vars_h = iterexp_h in
        match iter_h with
        | Opt -> error no_region "(TODO)"
        | List ->
            let cond, values_cond =
              eval_if_cond_list ctx exp_cond vars_h iterexps_t
            in
            let value_cond =
              let vid = Value.fresh () in
              let typ = Il.Ast.IterT (Il.Ast.BoolT $ no_region, Il.Ast.List) in
              Il.Ast.(ListV values_cond $$$ { vid; typ })
            in
            Ctx.add_node ctx value_cond;
            List.iter
              (fun (id, _typ, iters) ->
                let value_sub =
                  Ctx.find_value Local ctx (id, iters @ [ Il.Ast.List ])
                in
                Ctx.add_edge ctx value_cond value_sub Dep.Edges.Iter)
              vars_h;
            (cond, value_cond))

  and eval_if_cond_iter (ctx : Ctx.t) (exp_cond : exp) (iterexps : iterexp list)
      : bool * value =
    let iterexps = List.rev iterexps in
    eval_if_cond_iter' ctx exp_cond iterexps

  and eval_if_instr (ctx : Ctx.t) (exp_cond : exp) (iterexps : iterexp list)
      (instrs_then : instr list) (phantom_opt : phantom option) : Ctx.t * Sign.t
      =
    let cond, value_cond = eval_if_cond_iter ctx exp_cond iterexps in
    let vid = value_cond.note.vid in
    (match phantom_opt with
    | Some (pid, _) -> Ctx.cover ctx (not cond) pid vid
    | None -> ());
    if cond then eval_instrs ctx Cont instrs_then else (ctx, Cont)

  (* Hold instruction evaluation *)

  and eval_hold_cond (ctx : Ctx.t) (id : id) (notexp : notexp) : bool * value =
    let _, exps_input = notexp in
    let values_input = eval_exps ctx exps_input in
    let hold = invoke_rel ctx id values_input |> Option.is_some in
    let value_res =
      let vid = Value.fresh () in
      let typ = Il.Ast.BoolT in
      Il.Ast.(BoolV hold $$$ { vid; typ })
    in
    Ctx.add_node ctx value_res;
    List.iteri
      (fun idx value_input ->
        Ctx.add_edge ctx value_res value_input (Dep.Edges.Rel (id, idx)))
      values_input;
    (hold, value_res)

  and eval_hold_cond_list (ctx : Ctx.t) (id : id) (notexp : notexp)
      (vars : var list) (iterexps : iterexp list) : bool * value list =
    let ctxs_sub = Ctx.sub_list ctx vars in
    List.fold_left
      (fun (cond, values_cond) ctx_sub ->
        if not cond then (cond, values_cond)
        else
          let cond, value_cond =
            eval_hold_cond_iter' ctx_sub id notexp iterexps
          in
          let values_cond = values_cond @ [ value_cond ] in
          (cond, values_cond))
      (true, []) ctxs_sub

  and eval_hold_cond_iter' (ctx : Ctx.t) (id : id) (notexp : notexp)
      (iterexps : iterexp list) : bool * value =
    match iterexps with
    | [] -> eval_hold_cond ctx id notexp
    | iterexp_h :: iterexps_t -> (
        let iter_h, vars_h = iterexp_h in
        match iter_h with
        | Opt -> error no_region "(TODO)"
        | List ->
            let cond, values_cond =
              eval_hold_cond_list ctx id notexp vars_h iterexps_t
            in
            let value_cond =
              let vid = Value.fresh () in
              let typ = Il.Ast.IterT (Il.Ast.BoolT $ no_region, Il.Ast.List) in
              Il.Ast.(ListV values_cond $$$ { vid; typ })
            in
            Ctx.add_node ctx value_cond;
            List.iter
              (fun (id, _typ, iters) ->
                let value_sub =
                  Ctx.find_value Local ctx (id, iters @ [ Il.Ast.List ])
                in
                Ctx.add_edge ctx value_cond value_sub Dep.Edges.Iter)
              vars_h;
            (cond, value_cond))

  and eval_hold_cond_iter (ctx : Ctx.t) (id : id) (notexp : notexp)
      (iterexps : iterexp list) : bool * value =
    let iterexps = List.rev iterexps in
    eval_hold_cond_iter' ctx id notexp iterexps

  and eval_hold_instr (ctx : Ctx.t) (id : id) (notexp : notexp)
      (iterexps : iterexp list) (holdcase : holdcase) : Ctx.t * Sign.t =
    (* Copy the current coverage information *)
    let cover_backup = !(ctx.testing.cover) in
    (* Evaluate the hold condition *)
    let cond, value_cond = eval_hold_cond_iter ctx id notexp iterexps in
    (* Evaluate the hold case, and restore the coverage information
       if the expected behavior is the relation not holding *)
    let vid = value_cond.note.vid in
    match holdcase with
    | BothH (instrs_hold, instrs_not_hold) ->
        if cond then eval_instrs ctx Cont instrs_hold
        else (
          ctx.testing.cover := cover_backup;
          eval_instrs ctx Cont instrs_not_hold)
    | HoldH (instrs_hold, phantom_opt) ->
        (match phantom_opt with
        | Some (pid, _) -> Ctx.cover ctx (not cond) pid vid
        | None -> ());
        if cond then eval_instrs ctx Cont instrs_hold else (ctx, Cont)
    | NotHoldH (instrs_not_hold, phantom_opt) ->
        ctx.testing.cover := cover_backup;
        (match phantom_opt with
        | Some (pid, _) -> Ctx.cover ctx cond pid vid
        | None -> ());
        if not cond then eval_instrs ctx Cont instrs_not_hold else (ctx, Cont)

  (* Case analysis instruction evaluation *)

  and eval_cases (ctx : Ctx.t) (exp : exp) (cases : case list) :
      instr list option * value =
    cases
    |> List.fold_left
         (fun (block_match, values_cond) (guard, block) ->
           match block_match with
           | Some _ -> (block_match, values_cond)
           | None ->
               let exp_cond =
                 match guard with
                 | BoolG true -> exp.it
                 | BoolG false -> Il.Ast.UnE (`NotOp, `BoolT, exp)
                 | CmpG (cmpop, optyp, exp_r) ->
                     Il.Ast.CmpE (cmpop, optyp, exp, exp_r)
                 | SubG typ -> Il.Ast.SubE (exp, typ)
                 | MatchG pattern -> Il.Ast.MatchE (exp, pattern)
                 | MemG exp_s -> Il.Ast.MemE (exp, exp_s)
               in
               let exp_cond = exp_cond $$ (exp.at, Il.Ast.BoolT) in
               let value_cond = eval_exp ctx exp_cond in
               let values_cond = values_cond @ [ value_cond ] in
               let cond = Value.get_bool value_cond in
               if cond then (Some block, values_cond) else (None, values_cond))
         (None, [])
    |> fun (block_match, values_cond) ->
    let value_cond =
      let vid = Value.fresh () in
      let typ = Il.Ast.IterT (Il.Ast.BoolT $ no_region, Il.Ast.List) in
      Il.Ast.(ListV values_cond $$$ { vid; typ })
    in
    Ctx.add_node ctx value_cond;
    (block_match, value_cond)

  and eval_case_instr (ctx : Ctx.t) (exp : exp) (cases : case list)
      (phantom_opt : phantom option) : Ctx.t * Sign.t =
    let instrs_opt, value_cond = eval_cases ctx exp cases in
    let vid = value_cond.note.vid in
    (match phantom_opt with
    | Some (pid, _) -> Ctx.cover ctx (Option.is_none instrs_opt) pid vid
    | None -> ());
    match instrs_opt with
    | Some instrs -> eval_instrs ctx Cont instrs
    | None -> (ctx, Cont)

  (* Group instruction evaluation *)

  and eval_group_instr (ctx : Ctx.t) (id_group : id) (_exps_group : exp list)
      (instrs_group : instr list) : Ctx.t * Sign.t =
    let ctx_group, sign_group = eval_instrs ctx Cont instrs_group in
    match sign_group with
    | Cont -> (ctx, Cont)
    | Res values_output -> (ctx_group, Res values_output)
    | Ret _ -> error id_group.at "cannot return from try instruction"

  (* Let instruction evaluation *)

  and eval_let (ctx : Ctx.t) (exp_l : exp) (exp_r : exp) : Ctx.t =
    let value = eval_exp ctx exp_r in
    assign_exp ctx exp_l value

  and eval_let_opt (ctx : Ctx.t) (exp_l : exp) (exp_r : exp) (vars : var list)
      (iterexps : iterexp list) : Ctx.t =
    (* Discriminate between bound and binding variables *)
    let vars_bound, vars_binding =
      List.partition
        (fun (id, _typ, iters) ->
          Ctx.bound_value Local ctx (id, iters @ [ Il.Ast.Opt ]))
        vars
    in
    let ctx_sub_opt = Ctx.sub_opt ctx vars_bound in
    let ctx, values_binding =
      match ctx_sub_opt with
      (* If the bound variable supposed to guide the iteration is already empty,
         then the binding variables are also empty *)
      | None ->
          let values_binding =
            List.map
              (fun (_id_binding, typ_binding, iters_binding) ->
                let value_binding =
                  let vid = Value.fresh () in
                  let typ =
                    Typ.iterate typ_binding (iters_binding @ [ Il.Ast.Opt ])
                  in
                  Il.Ast.(OptV None $$$ { vid; typ = typ.it })
                in
                Ctx.add_node ctx value_binding;
                List.iter
                  (fun (id, _typ, iters) ->
                    let value_sub =
                      Ctx.find_value Local ctx (id, iters @ [ Il.Ast.Opt ])
                    in
                    Ctx.add_edge ctx value_binding value_sub Dep.Edges.Iter)
                  vars_bound;
                value_binding)
              vars_binding
          in
          (ctx, values_binding)
      (* Otherwise, evaluate the premise for the subcontext *)
      | Some ctx_sub ->
          let ctx_sub = eval_let_iter' ctx_sub exp_l exp_r iterexps in
          let values_binding =
            List.map
              (fun (id_binding, typ_binding, iters_binding) ->
                let value_binding =
                  Ctx.find_value Local ctx_sub (id_binding, iters_binding)
                in
                let value_binding =
                  let vid = Value.fresh () in
                  let typ =
                    Typ.iterate typ_binding (iters_binding @ [ Il.Ast.Opt ])
                  in
                  Il.Ast.(OptV (Some value_binding) $$$ { vid; typ = typ.it })
                in
                Ctx.add_node ctx value_binding;
                List.iter
                  (fun (id, _typ, iters) ->
                    let value_sub =
                      Ctx.find_value Local ctx (id, iters @ [ Il.Ast.Opt ])
                    in
                    Ctx.add_edge ctx value_binding value_sub Dep.Edges.Iter)
                  vars_bound;
                value_binding)
              vars_binding
          in
          (ctx, values_binding)
    in
    (* Finally, bind the resulting values *)
    List.fold_left2
      (fun ctx (id_binding, _typ_binding, iters_binding) value_binding ->
        Ctx.add_value Local ctx
          (id_binding, iters_binding @ [ Il.Ast.Opt ])
          value_binding)
      ctx vars_binding values_binding

  and eval_let_list (ctx : Ctx.t) (exp_l : exp) (exp_r : exp) (vars : var list)
      (iterexps : iterexp list) : Ctx.t =
    (* Discriminate between bound and binding variables *)
    let vars_bound, vars_binding =
      List.partition
        (fun (id, _typ, iters) ->
          Ctx.bound_value Local ctx (id, iters @ [ Il.Ast.List ]))
        vars
    in
    (* Create a subcontext for each batch of bound values *)
    let ctxs_sub = Ctx.sub_list ctx vars_bound in
    let ctx, values_binding =
      match ctxs_sub with
      (* If the bound variable supposed to guide the iteration is already empty,
         then the binding variables are also empty *)
      | [] ->
          let values_binding =
            List.init (List.length vars_binding) (fun _ -> [])
          in
          (ctx, values_binding)
      (* Otherwise, evaluate the premise for each batch of bound values,
         and collect the resulting binding batches *)
      | _ ->
          let ctx, values_binding_batch =
            List.fold_left
              (fun (ctx, values_binding_batch) ctx_sub ->
                let ctx_sub = eval_let_iter' ctx_sub exp_l exp_r iterexps in
                let value_binding_batch =
                  List.map
                    (fun (id_binding, _typ_binding, iters_binding) ->
                      Ctx.find_value Local ctx_sub (id_binding, iters_binding))
                    vars_binding
                in
                let values_binding_batch =
                  values_binding_batch @ [ value_binding_batch ]
                in
                (ctx, values_binding_batch))
              (ctx, []) ctxs_sub
          in
          let values_binding = values_binding_batch |> Ctx.transpose in
          (ctx, values_binding)
    in
    (* Finally, bind the resulting binding batches *)
    List.fold_left2
      (fun ctx (id_binding, typ_binding, iters_binding) values_binding ->
        let value_binding =
          let vid = Value.fresh () in
          let typ = Typ.iterate typ_binding (iters_binding @ [ Il.Ast.List ]) in
          Il.Ast.(ListV values_binding $$$ { vid; typ = typ.it })
        in
        Ctx.add_node ctx value_binding;
        List.iter
          (fun (id, _typ, iters) ->
            let value_sub =
              Ctx.find_value Local ctx (id, iters @ [ Il.Ast.List ])
            in
            Ctx.add_edge ctx value_binding value_sub Dep.Edges.Iter)
          vars_bound;
        Ctx.add_value Local ctx
          (id_binding, iters_binding @ [ Il.Ast.List ])
          value_binding)
      ctx vars_binding values_binding

  and eval_let_iter' (ctx : Ctx.t) (exp_l : exp) (exp_r : exp)
      (iterexps : iterexp list) : Ctx.t =
    match iterexps with
    | [] -> eval_let ctx exp_l exp_r
    | iterexp_h :: iterexps_t -> (
        let iter_h, vars_h = iterexp_h in
        match iter_h with
        | Opt -> eval_let_opt ctx exp_l exp_r vars_h iterexps_t
        | List -> eval_let_list ctx exp_l exp_r vars_h iterexps_t)

  and eval_let_iter (ctx : Ctx.t) (exp_l : exp) (exp_r : exp)
      (iterexps : iterexp list) : Ctx.t =
    let iterexps = List.rev iterexps in
    eval_let_iter' ctx exp_l exp_r iterexps

  and eval_let_instr (ctx : Ctx.t) (exp_l : exp) (exp_r : exp)
      (iterexps : iterexp list) : Ctx.t * Sign.t =
    let ctx = eval_let_iter ctx exp_l exp_r iterexps in
    (ctx, Cont)

  (* Rule instruction evaluation *)

  and eval_rule (ctx : Ctx.t) (id : id) (notexp : notexp) : Ctx.t =
    let rel = Ctx.find_rel Local ctx id in
    let exps_input, exps_output =
      let _, inputs, _, _ = rel in
      let _, exps = notexp in
      InputHint.split_exps_without_idx inputs exps
    in
    let values_input = eval_exps ctx exps_input in
    let values_output =
      match invoke_rel ctx id values_input with
      | Some values_output -> values_output
      | None -> error id.at (F.asprintf "relation %s was not matched" id.it)
    in
    assign_exps ctx exps_output values_output

  and eval_rule_opt (_ctx : Ctx.t) (_id : id) (_notexp : notexp)
      (_vars : var list) (_iterexps : iterexp list) : Ctx.t =
    failwith "(TODO) eval_rule_opt"

  and eval_rule_list (ctx : Ctx.t) (id : id) (notexp : notexp) (vars : var list)
      (iterexps : iterexp list) : Ctx.t =
    (* Discriminate between bound and binding variables *)
    let vars_bound, vars_binding =
      List.partition
        (fun (id, _typ, iters) ->
          Ctx.bound_value Local ctx (id, iters @ [ Il.Ast.List ]))
        vars
    in
    (* Create a subcontext for each batch of bound values *)
    let ctxs_sub = Ctx.sub_list ctx vars_bound in
    let ctx, values_binding =
      match ctxs_sub with
      (* If the bound variable supposed to guide the iteration is already empty,
         then the binding variables are also empty *)
      | [] ->
          let values_binding =
            List.init (List.length vars_binding) (fun _ -> [])
          in
          (ctx, values_binding)
      (* Otherwise, evaluate the premise for each batch of bound values,
         and collect the resulting binding batches *)
      | _ ->
          let ctx, values_binding_batch =
            List.fold_left
              (fun (ctx, values_binding_batch) ctx_sub ->
                let ctx_sub = eval_rule_iter' ctx_sub id notexp iterexps in
                let value_binding_batch =
                  List.map
                    (fun (id_binding, _typ_binding, iters_binding) ->
                      Ctx.find_value Local ctx_sub (id_binding, iters_binding))
                    vars_binding
                in
                let values_binding_batch =
                  values_binding_batch @ [ value_binding_batch ]
                in
                (ctx, values_binding_batch))
              (ctx, []) ctxs_sub
          in
          let values_binding = values_binding_batch |> Ctx.transpose in
          (ctx, values_binding)
    in
    (* Finally, bind the resulting binding batches *)
    List.fold_left2
      (fun ctx (id_binding, typ_binding, iters_binding) values_binding ->
        let value_binding =
          let vid = Value.fresh () in
          let typ = Typ.iterate typ_binding (iters_binding @ [ Il.Ast.List ]) in
          Il.Ast.(ListV values_binding $$$ { vid; typ = typ.it })
        in
        Ctx.add_node ctx value_binding;
        List.iter
          (fun (id, _typ, iters) ->
            let value_sub =
              Ctx.find_value Local ctx (id, iters @ [ Il.Ast.List ])
            in
            Ctx.add_edge ctx value_binding value_sub Dep.Edges.Iter)
          vars_bound;
        Ctx.add_value Local ctx
          (id_binding, iters_binding @ [ Il.Ast.List ])
          value_binding)
      ctx vars_binding values_binding

  and eval_rule_iter' (ctx : Ctx.t) (id : id) (notexp : notexp)
      (iterexps : iterexp list) : Ctx.t =
    match iterexps with
    | [] -> eval_rule ctx id notexp
    | iterexp_h :: iterexps_t -> (
        let iter_h, vars_h = iterexp_h in
        match iter_h with
        | Opt -> eval_rule_opt ctx id notexp vars_h iterexps_t
        | List -> eval_rule_list ctx id notexp vars_h iterexps_t)

  and eval_rule_iter (ctx : Ctx.t) (id : id) (notexp : notexp)
      (iterexps : iterexp list) : Ctx.t =
    let iterexps = List.rev iterexps in
    eval_rule_iter' ctx id notexp iterexps

  and eval_rule_instr (ctx : Ctx.t) (id : id) (notexp : notexp)
      (iterexps : iterexp list) : Ctx.t * Sign.t =
    let ctx = eval_rule_iter ctx id notexp iterexps in
    (ctx, Cont)

  (* Result instruction evaluation *)

  and eval_result_instr (ctx : Ctx.t) (exps : exp list) : Ctx.t * Sign.t =
    let values = eval_exps ctx exps in
    (ctx, Res values)

  (* Return instruction evaluation *)

  and eval_return_instr (ctx : Ctx.t) (exp : exp) : Ctx.t * Sign.t =
    let value = eval_exp ctx exp in
    (ctx, Ret value)

  (* Debug instruction evaluation *)

  and eval_debug_instr (ctx : Ctx.t) (exp : exp) : Ctx.t * Sign.t =
    let value = eval_exp ctx exp in
    print_endline
    @@ F.sprintf "%s: %s" (string_of_region exp.at) (Il.Print.string_of_exp exp);
    print_endline @@ Il.Print.string_of_value value;
    (ctx, Cont)

  (* Invoke a relation *)

  and invoke_rel (ctx : Ctx.t) (id : id) (values_input : value list) :
      value list option =
    let _, _, exps_input, instrs = Ctx.find_rel Local ctx id in
    match instrs with
    | [] -> invoke_rel_arch ctx id exps_input values_input
    | _ -> invoke_rel_def ctx id exps_input instrs values_input

  and invoke_rel_arch (_ctx : Ctx.t) (id : id) (_exps_input : exp list)
      (values_input : value list) : value list option =
    match id.it with
    | "ExternMethodCall_eval" ->
        Some (Arch.eval_extern_method_call values_input)
    | _ -> error id.at (F.asprintf "unknown arch relation %s" id.it)

  and invoke_rel_def (ctx : Ctx.t) (id : id) (exps_input : exp list)
      (instrs : instr list) (values_input : value list) : value list option =
    let invoke_rel_def' () =
      let ctx_local = Ctx.localize_rule ctx id values_input in
      let ctx_local = assign_exps ctx_local exps_input values_input in
      let ctx_local, sign = eval_instrs ctx_local Cont instrs in
      match sign with
      | Res values_output ->
          List.iteri
            (fun idx_arg value_input ->
              List.iter
                (fun value_output ->
                  Ctx.add_edge ctx value_output value_input
                    (Dep.Edges.Rel (id, idx_arg)))
                values_output)
            values_input;
          Some values_output
      | _ -> None
    in
    if (not ctx.testing.derive) && Cache.is_cached_rule id.it then (
      let cache_result = Cache.Cache.find !rule_cache (id.it, values_input) in
      match cache_result with
      | Some values_output -> Some values_output
      | None ->
          let* values_output = invoke_rel_def' () in
          Cache.Cache.add !rule_cache (id.it, values_input) values_output;
          Some values_output)
    else invoke_rel_def' ()

  (* Invoke a function *)

  and invoke_func (ctx : Ctx.t) (id : id) (targs : targ list) (args : arg list)
      : value =
    if Builtin.is_builtin id then invoke_func_builtin ctx id targs args
    else invoke_func_def ctx id targs args

  and invoke_func_builtin (ctx : Ctx.t) (id : id) (targs : targ list)
      (args : arg list) : value =
    let values_input = eval_args ctx args in
    let value_output = Builtin.invoke ctx id targs values_input in
    List.iteri
      (fun idx_arg value_input ->
        Ctx.add_edge ctx value_output value_input (Dep.Edges.Func (id, idx_arg)))
      values_input;
    value_output

  and invoke_func_def (ctx : Ctx.t) (id : id) (targs : targ list)
      (args : arg list) : value =
    let values_input = eval_args ctx args in
    invoke_func_def' ctx id targs values_input

  and invoke_func_def' (ctx : Ctx.t) (id : id) (targs : targ list)
      (values_input : value list) : value =
    let tparams, args_input, instrs = Ctx.find_func Local ctx id in
    check (instrs <> []) id.at "function has no instructions";
    let tdenv_local =
      check
        (List.length targs = List.length tparams)
        id.at "arity mismatch in type arguments";
      let targs =
        match targs with
        | [] -> []
        | targs ->
            let theta =
              (TDEnv.bindings ctx.global.tdenv
              @
              match ctx.local with
              | Empty | Rel _ -> []
              | Func { tdenv; _ } -> TDEnv.bindings tdenv)
              |> List.filter_map (fun (tid, (_tparams, deftyp)) ->
                     match deftyp.it with
                     | Il.Ast.PlainT typ -> Some (tid, typ)
                     | _ -> None)
              |> TIdMap.of_list
            in
            List.map (Typ.subst_typ theta) targs
      in
      List.fold_left2
        (fun tdenv_local tparam targ ->
          TDEnv.add tparam ([], Il.Ast.PlainT targ $ targ.at) tdenv_local)
        TDEnv.empty tparams targs
    in
    let ctx_local = Ctx.localize_func ctx id values_input tdenv_local in
    let invoke_func_def'' () =
      let ctx_local = assign_args ctx ctx_local args_input values_input in
      let _ctx_local, sign = eval_instrs ctx_local Cont instrs in
      match sign with
      | Ret value_output ->
          List.iteri
            (fun idx_arg value_input ->
              Ctx.add_edge ctx value_output value_input
                (Dep.Edges.Func (id, idx_arg)))
            values_input;
          value_output
      | _ -> error id.at (F.asprintf "function %s was not matched" id.it)
    in
    if (not ctx.testing.derive) && Cache.is_cached_func id.it then (
      let cache_result = Cache.Cache.find !func_cache (id.it, values_input) in
      match cache_result with
      | Some value_output -> value_output
      | None ->
          let value_output = invoke_func_def'' () in
          Cache.Cache.add !func_cache (id.it, values_input) value_output;
          value_output)
    else invoke_func_def'' ()

  (* Load definitions into the context *)

  let load_def (ctx : Ctx.t) (def : def) : Ctx.t =
    match def.it with
    | TypD (id, tparams, deftyp) ->
        let typdef = (tparams, deftyp) in
        Ctx.add_typdef Global ctx id typdef
    | RelD (id, (mixop, inputs), relmatch, relpaths, _) ->
        let rel = (mixop, inputs, relmatch, relpaths) in
        Ctx.add_rel Global ctx id rel
    | DecD (id, tparams, args_input, instrs, _) ->
        let func = (tparams, args_input, instrs) in
        Ctx.add_func Global ctx id func

  let load_spec (ctx : Ctx.t) (spec : spec) : Ctx.t =
    List.fold_left load_def ctx spec

  (* Entry point *)

  let eval_rel_call (spec : spec) (relname : string) (values_input : value list)
      : value list =
    Builtin.init ();
    Value.refresh ();
    Cache.Cache.clear !func_cache;
    Cache.Cache.clear !rule_cache;
    let graph = Dep.Graph.empty () in
    let vid = -1 in
    let cover = ref (SCov.init IdSet.empty spec) in
    let ctx = Ctx.empty ~derive:false "FOO.P4" graph vid cover in
    let ctx = load_spec ctx spec in
    match invoke_rel ctx (relname $ no_region) values_input with
    | Some values_output -> values_output
    | None -> error no_region (F.asprintf "relation %s was not matched" relname)

  let eval_rel_call_program (spec : spec) (relname : string)
      (includes_p4 : string list) (filename_p4 : string) : value list =
    Builtin.init ();
    Value.refresh ();
    Cache.Cache.clear !func_cache;
    Cache.Cache.clear !rule_cache;
    let cover = ref (SCov.init IdSet.empty spec) in
    let value_program = Interface.Parse.parse_file includes_p4 filename_p4 in
    let graph = Dep.Graph.assemble_graph value_program in
    let ctx =
      Ctx.empty ~derive:false filename_p4 graph value_program.note.vid cover
    in
    let ctx = load_spec ctx spec in
    match invoke_rel ctx (relname $ no_region) [ value_program ] with
    | Some values_output -> values_output
    | None -> error no_region (F.asprintf "relation %s was not matched" relname)

  let eval_func_call (spec : spec) (funcname : string) (targs : targ list)
      (values_input : value list) : value =
    Builtin.init ();
    Value.refresh ();
    Cache.Cache.clear !func_cache;
    Cache.Cache.clear !rule_cache;
    let graph = Dep.Graph.empty () in
    let vid = -1 in
    let cover = ref (SCov.init IdSet.empty spec) in
    let ctx = Ctx.empty ~derive:false "FOO.P4" graph vid cover in
    let ctx = load_spec ctx spec in
    invoke_func_def' ctx (funcname $ no_region) targs values_input
end
