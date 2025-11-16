open Domain.Lib
open Xl
open Sl.Ast
module InputHint = Runtime_static.Rel.InputHint
module Typ = Runtime_dynamic.Typ
module TypDef = Runtime_dynamic.Typdef
module Value = Runtime_dynamic.Value
module Rel = Runtime_dynamic_sl.Rel
module Func = Runtime_dynamic_sl.Func
module Cache = Runtime_dynamic.Cache
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

(* Continuation helpers *)

let rec map_cps (f : 'a -> ('b -> 'r) -> 'r) (xs : 'a list) (k : 'b list -> 'r)
    : 'r =
  match xs with
  | [] -> k []
  | x_h :: xs_t ->
      f x_h (fun r_h -> map_cps f xs_t (fun rs_t -> k (r_h :: rs_t)))

let rec fold_left_cps (f : 'a -> 'b -> ('a -> 'r) -> 'r) (acc : 'a)
    (xs : 'b list) (k : 'a -> 'r) : 'r =
  match xs with
  | [] -> k acc
  | x_h :: xs_t -> f acc x_h (fun acc_h -> fold_left_cps f acc_h xs_t k)

let rec fold_left2_cps (f : 'a -> 'b -> 'c -> ('a -> 'r) -> 'r) (acc : 'a)
    (xs : 'b list) (ys : 'c list) (k : 'a -> 'r) : 'r =
  match (xs, ys) with
  | [], [] -> k acc
  | x_h :: xs_t, y_h :: ys_t ->
      f acc x_h y_h (fun acc_h -> fold_left2_cps f acc_h xs_t ys_t k)
  | _ -> failwith "fold_left2_cps: lists have different lengths"

(* Cache *)

let func_cache = ref (Cache.Cache.create ~size:10000)
let rule_cache = ref (Cache.Cache.create ~size:10000)

module Make (Arch : Sim.ARCH) : Sim.INTERP_SL = struct
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
          List.map
            (fun value ->
              let ctx = Ctx.localize_clear ctx in
              assign_exp ctx exp value)
            values
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

  let rec eval_exp (ctx : Ctx.t) (exp : exp) (k : value -> 'r) : 'r =
    let at, note = (exp.at, exp.note) in
    match exp.it with
    | BoolE b -> eval_bool_exp note ctx b k
    | NumE n -> eval_num_exp note ctx n k
    | TextE s -> eval_text_exp note ctx s k
    | VarE id -> eval_var_exp note ctx id k
    | UnE (unop, optyp, exp) -> eval_un_exp note ctx unop optyp exp k
    | BinE (binop, optyp, exp_l, exp_r) ->
        eval_bin_exp note ctx binop optyp exp_l exp_r k
    | CmpE (cmpop, optyp, exp_l, exp_r) ->
        eval_cmp_exp note ctx cmpop optyp exp_l exp_r k
    | UpCastE (typ, exp) -> eval_upcast_exp note ctx typ exp k
    | DownCastE (typ, exp) -> eval_downcast_exp note ctx typ exp k
    | SubE (exp, typ) -> eval_sub_exp note ctx exp typ k
    | MatchE (exp, pattern) -> eval_match_exp note ctx exp pattern k
    | TupleE exps -> eval_tuple_exp note ctx exps k
    | CaseE notexp -> eval_case_exp note ctx notexp k
    | StrE fields -> eval_str_exp note ctx fields k
    | OptE exp_opt -> eval_opt_exp note ctx exp_opt k
    | ListE exps -> eval_list_exp note ctx exps k
    | ConsE (exp_h, exp_t) -> eval_cons_exp note ctx exp_h exp_t k
    | CatE (exp_l, exp_r) -> eval_cat_exp note ctx at exp_l exp_r k
    | MemE (exp_e, exp_s) -> eval_mem_exp note ctx exp_e exp_s k
    | LenE exp -> eval_len_exp note ctx exp k
    | DotE (exp_b, atom) -> eval_dot_exp note ctx exp_b atom k
    | IdxE (exp_b, exp_i) -> eval_idx_exp note ctx exp_b exp_i k
    | SliceE (exp_b, exp_l, exp_h) ->
        eval_slice_exp note ctx exp_b exp_l exp_h k
    | UpdE (exp_b, path, exp_f) -> eval_upd_exp note ctx exp_b path exp_f k
    | CallE (id, targs, args) -> eval_call_exp note ctx id targs args k
    | IterE (exp, iterexp) -> eval_iter_exp note ctx exp iterexp k

  and eval_exps (ctx : Ctx.t) (exps : exp list) (k : value list -> 'r) : 'r =
    map_cps (eval_exp ctx) exps k

  (* Boolean expression evaluation *)

  and eval_bool_exp (note : typ') (ctx : Ctx.t) (b : bool) (k : value -> 'r) :
      'r =
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
    k value_res

  (* Numeric expression evaluation *)

  and eval_num_exp (note : typ') (ctx : Ctx.t) (n : Num.t) (k : value -> 'r) :
      'r =
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
    k value_res

  (* Text expression evaluation *)

  and eval_text_exp (note : typ') (ctx : Ctx.t) (s : string) (k : value -> 'r) :
      'r =
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
    k value_res

  (* Variable expression evaluation *)

  and eval_var_exp (_note : typ') (ctx : Ctx.t) (id : id) (k : value -> 'r) : 'r
      =
    let value = Ctx.find_value Local ctx (id, []) in
    k value

  (* Unary expression evaluation *)

  and eval_un_bool (unop : Bool.unop) (value : value) : value' =
    match unop with `NotOp -> Il.Ast.BoolV (not (Value.get_bool value))

  and eval_un_num (unop : Num.unop) (value : value) : value' =
    let num = Value.get_num value in
    let num = Num.un unop num in
    Il.Ast.NumV num

  and eval_un_exp (note : typ') (ctx : Ctx.t) (unop : unop) (_optyp : optyp)
      (exp : exp) (k : value -> 'r) : 'r =
    eval_exp ctx exp (fun value ->
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
        k value_res)

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
      (exp_l : exp) (exp_r : exp) (k : value -> 'r) : 'r =
    eval_exp ctx exp_l (fun value_l ->
        eval_exp ctx exp_r (fun value_r ->
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
            k value_res))

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
      (exp_l : exp) (exp_r : exp) (k : value -> 'r) : 'r =
    eval_exp ctx exp_l (fun value_l ->
        eval_exp ctx exp_r (fun value_r ->
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
            k value_res))

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
        let tparams, deftyp = Ctx.find_defined_typdef Local ctx tid in
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

  and eval_upcast_exp (_note : typ') (ctx : Ctx.t) (typ : typ) (exp : exp)
      (k : value -> 'r) : 'r =
    eval_exp ctx exp (fun value ->
        let value = upcast ctx typ value in
        k value)

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
        let tparams, deftyp = Ctx.find_defined_typdef Local ctx tid in
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

  and eval_downcast_exp (_note : typ') (ctx : Ctx.t) (typ : typ) (exp : exp)
      (k : value -> 'r) : 'r =
    eval_exp ctx exp (fun value ->
        let value = downcast ctx typ value in
        k value)

  (* Subtype check expression evaluation *)

  and subtyp (ctx : Ctx.t) (typ : typ) (value : value) : bool =
    match typ.it with
    | NumT `NatT -> (
        match value.it with
        | NumV (`Nat _) -> true
        | NumV (`Int i) -> Bigint.(i >= zero)
        | _ -> assert false)
    | VarT (tid, targs) -> (
        let tparams, deftyp = Ctx.find_defined_typdef Local ctx tid in
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

  and eval_sub_exp (note : typ') (ctx : Ctx.t) (exp : exp) (typ : typ)
      (k : value -> 'r) : 'r =
    eval_exp ctx exp (fun value ->
        let sub = subtyp ctx typ value in
        let value_res =
          let vid = Value.fresh () in
          let typ = note in
          Il.Ast.(BoolV sub $$$ { vid; typ })
        in
        Ctx.add_node ctx value_res;
        Ctx.add_edge ctx value_res value (Dep.Edges.Op (SubOp typ));
        k value_res)

  (* Pattern match check expression evaluation *)

  and eval_match_exp (note : typ') (ctx : Ctx.t) (exp : exp) (pattern : pattern)
      (k : value -> 'r) : 'r =
    eval_exp ctx exp (fun value ->
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
        k value_res)

  (* Tuple expression evaluation *)

  and eval_tuple_exp (note : typ') (ctx : Ctx.t) (exps : exp list)
      (k : value -> 'r) : 'r =
    eval_exps ctx exps (fun values ->
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
        k value_res)

  (* Case expression evaluation *)

  and eval_case_exp (note : typ') (ctx : Ctx.t) (notexp : notexp)
      (k : value -> 'r) : 'r =
    let mixop, exps = notexp in
    eval_exps ctx exps (fun values ->
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
        k value_res)

  (* Struct expression evaluation *)

  and eval_str_exp (note : typ') (ctx : Ctx.t) (fields : (atom * exp) list)
      (k : value -> 'r) : 'r =
    let atoms, exps = List.split fields in
    eval_exps ctx exps (fun values ->
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
        k value_res)

  (* Option expression evaluation *)

  and eval_opt_exp_some (note : typ') (ctx : Ctx.t) (exp : exp)
      (k : value -> 'r) : 'r =
    eval_exp ctx exp (fun value_inner ->
        let value_res =
          let vid = Value.fresh () in
          let typ = note in
          Il.Ast.(OptV (Some value_inner) $$$ { vid; typ })
        in
        Ctx.add_node ctx value_res;
        k value_res)

  and eval_opt_exp_none (note : typ') (ctx : Ctx.t) (k : value -> 'r) : 'r =
    let value_res =
      let vid = Value.fresh () in
      let typ = note in
      Il.Ast.(OptV None $$$ { vid; typ })
    in
    Ctx.add_node ctx value_res;
    List.iter
      (fun value_input ->
        Ctx.add_edge ctx value_res value_input Dep.Edges.Control)
      (Ctx.find_values_input Ctx.Local ctx);
    k value_res

  and eval_opt_exp (note : typ') (ctx : Ctx.t) (exp_opt : exp option)
      (k : value -> 'r) : 'r =
    match exp_opt with
    | Some exp -> eval_opt_exp_some note ctx exp k
    | None -> eval_opt_exp_none note ctx k

  (* List expression evaluation *)

  and eval_list_exp (note : typ') (ctx : Ctx.t) (exps : exp list)
      (k : value -> 'r) : 'r =
    eval_exps ctx exps (fun values ->
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
        k value_res)

  (* Cons expression evaluation *)

  and eval_cons_exp (note : typ') (ctx : Ctx.t) (exp_h : exp) (exp_t : exp)
      (k : value -> 'r) : 'r =
    eval_exp ctx exp_h (fun value_h ->
        eval_exp ctx exp_t (fun value_t ->
            let values_t = Value.get_list value_t in
            let value_res =
              let vid = Value.fresh () in
              let typ = note in
              Il.Ast.(ListV (value_h :: values_t) $$$ { vid; typ })
            in
            Ctx.add_node ctx value_res;
            k value_res))

  (* Concatenation expression evaluation *)

  and eval_cat_exp (note : typ') (ctx : Ctx.t) (at : region) (exp_l : exp)
      (exp_r : exp) (k : value -> 'r) : 'r =
    eval_exp ctx exp_l (fun value_l ->
        eval_exp ctx exp_r (fun value_r ->
            let value_res =
              match (value_l.it, value_r.it) with
              | TextV s_l, TextV s_r -> Il.Ast.TextV (s_l ^ s_r)
              | ListV values_l, ListV values_r ->
                  Il.Ast.ListV (values_l @ values_r)
              | _ ->
                  error at "concatenation expects either two texts or two lists"
            in
            let value_res =
              let vid = Value.fresh () in
              let typ = note in
              Il.Ast.(value_res $$$ { vid; typ })
            in
            Ctx.add_node ctx value_res;
            Ctx.add_edge ctx value_res value_l (Dep.Edges.Op CatOp);
            Ctx.add_edge ctx value_res value_r (Dep.Edges.Op CatOp);
            k value_res))

  (* Membership expression evaluation *)

  and eval_mem_exp (note : typ') (ctx : Ctx.t) (exp_e : exp) (exp_s : exp)
      (k : value -> 'r) : 'r =
    eval_exp ctx exp_e (fun value_e ->
        eval_exp ctx exp_s (fun value_s ->
            let values_s = Value.get_list value_s in
            let value_res =
              let vid = Value.fresh () in
              let typ = note in
              Il.Ast.(
                BoolV (List.exists (Value.eq value_e) values_s) $$$ { vid; typ })
            in
            Ctx.add_node ctx value_res;
            Ctx.add_edge ctx value_res value_e (Dep.Edges.Op MemOp);
            Ctx.add_edge ctx value_res value_s (Dep.Edges.Op MemOp);
            k value_res))

  (* Length expression evaluation *)

  and eval_len_exp (note : typ') (ctx : Ctx.t) (exp : exp) (k : value -> 'r) :
      'r =
    eval_exp ctx exp (fun value ->
        let len = value |> Value.get_list |> List.length |> Bigint.of_int in
        let value_res =
          let vid = Value.fresh () in
          let typ = note in
          Il.Ast.(NumV (`Nat len) $$$ { vid; typ })
        in
        Ctx.add_node ctx value_res;
        Ctx.add_edge ctx value_res value (Dep.Edges.Op LenOp);
        k value_res)

  (* Dot expression evaluation *)

  and eval_dot_exp (_note : typ') (ctx : Ctx.t) (exp_b : exp) (atom : atom)
      (k : value -> 'r) : 'r =
    eval_exp ctx exp_b (fun value_b ->
        let fields = Value.get_struct value_b in
        let value_res =
          fields
          |> List.map (fun (atom, value) -> (atom.it, value))
          |> List.assoc atom.it
        in
        k value_res)

  (* Index expression evaluation *)

  and eval_idx_exp (_note : typ') (ctx : Ctx.t) (exp_b : exp) (exp_i : exp)
      (k : value -> 'r) : 'r =
    eval_exp ctx exp_b (fun value_b ->
        eval_exp ctx exp_i (fun value_i ->
            let values = Value.get_list value_b in
            let idx =
              value_i |> Value.get_num |> Num.to_int |> Bigint.to_int_exn
            in
            let value_res = List.nth values idx in
            k value_res))

  (* Slice expression evaluation *)

  and eval_slice_exp (note : typ') (ctx : Ctx.t) (exp_b : exp) (exp_i : exp)
      (exp_n : exp) (k : value -> 'r) : 'r =
    eval_exp ctx exp_b (fun value_b ->
        eval_exp ctx exp_i (fun value_i ->
            eval_exp ctx exp_n (fun value_n ->
                let values = Value.get_list value_b in
                let idx_l =
                  value_i |> Value.get_num |> Num.to_int |> Bigint.to_int_exn
                in
                let idx_n =
                  value_n |> Value.get_num |> Num.to_int |> Bigint.to_int_exn
                in
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
                k value_res)))

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
      (exp_f : exp) (k : value -> 'r) : 'r =
    eval_exp ctx exp_b (fun value_b ->
        eval_exp ctx exp_f (fun value_f ->
            let value_res = eval_update_path ctx value_b path value_f in
            k value_res))

  (* Function call expression evaluation *)

  and eval_call_exp (_note : typ') (ctx : Ctx.t) (id : id) (targs : targ list)
      (args : arg list) (k : value -> 'r) : 'r =
    invoke_func ctx id targs args (function
      | Some value_output -> k value_output
      | None -> error id.at (F.asprintf "function %s was not matched" id.it))

  (* Iterated expression evaluation *)

  and eval_iter_exp_opt (note : typ') (ctx : Ctx.t) (exp : exp)
      (vars : var list) (k : value -> 'r) : 'r =
    let ctx_sub_opt = Ctx.sub_opt ctx vars in
    match ctx_sub_opt with
    | Some ctx_sub ->
        eval_exp ctx_sub exp (fun value ->
            let value_res =
              let vid = Value.fresh () in
              let typ = note in
              Il.Ast.(OptV (Some value) $$$ { vid; typ })
            in
            Ctx.add_node ctx value_res;
            List.iter
              (fun (id, _typ, iters) ->
                let value_sub =
                  Ctx.find_value Local ctx (id, iters @ [ Il.Ast.Opt ])
                in
                Ctx.add_edge ctx value_res value_sub Dep.Edges.Iter)
              vars;
            k value_res)
    | None ->
        let vid = Value.fresh () in
        let typ = note in
        let value_res = Il.Ast.(OptV None $$$ { vid; typ }) in
        Ctx.add_node ctx value_res;
        List.iter
          (fun (id, _typ, iters) ->
            let value_sub =
              Ctx.find_value Local ctx (id, iters @ [ Il.Ast.Opt ])
            in
            Ctx.add_edge ctx value_res value_sub Dep.Edges.Iter)
          vars;
        k value_res

  and eval_iter_exp_list (note : typ') (ctx : Ctx.t) (exp : exp)
      (vars : var list) (k : value -> 'r) : 'r =
    let ctxs_sub = Ctx.sub_list ctx vars in
    let f_map ctx_sub k_map = eval_exp ctx_sub exp k_map in
    let k_wrap values =
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
      k value_res
    in
    map_cps f_map ctxs_sub k_wrap

  and eval_iter_exp (note : typ') (ctx : Ctx.t) (exp : exp) (iterexp : iterexp)
      (k : value -> 'r) : 'r =
    let iter, vars = iterexp in
    match iter with
    | Opt -> eval_iter_exp_opt note ctx exp vars k
    | List -> eval_iter_exp_list note ctx exp vars k

  (* Argument evaluation *)

  and eval_arg (ctx : Ctx.t) (arg : arg) (k : value -> 'r) : 'r =
    match arg.it with
    | ExpA exp -> eval_exp ctx exp k
    | DefA id ->
        let value_res =
          let vid = Value.fresh () in
          let typ = Il.Ast.FuncT in
          Il.Ast.(FuncV id $$$ { vid; typ })
        in
        Ctx.add_node ctx value_res;
        k value_res

  and eval_args (ctx : Ctx.t) (args : arg list) (k : value list -> 'r) : 'r =
    map_cps (eval_arg ctx) args k

  (* Instruction evaluation *)

  and eval_instr (ctx : Ctx.t) (instr : instr) (k : Ctx.t * Sign.t -> 'r) : 'r =
    match instr.it with
    | IfI (exp_cond, iterexps, instrs_then, phantom_opt) ->
        eval_if_instr ctx exp_cond iterexps instrs_then phantom_opt k
    | HoldI (id, notexp, iterexps, holdcase) ->
        eval_hold_instr ctx id notexp iterexps holdcase k
    | CaseI (exp, cases, phantom_opt) ->
        eval_case_instr ctx exp cases phantom_opt k
    | OtherwiseI instr -> eval_instr ctx instr k
    | GroupI (id_group, exps_group, instrs_group) ->
        eval_group_instr ctx id_group exps_group instrs_group k
    | LetI (exp_l, exp_r, iterexps) -> eval_let_instr ctx exp_l exp_r iterexps k
    | RuleI (id, notexp, iterexps) -> eval_rule_instr ctx id notexp iterexps k
    | ResultI exps -> eval_result_instr ctx exps k
    | ReturnI exp -> eval_return_instr ctx exp k
    | DebugI exp -> eval_debug_instr ctx exp k

  and eval_instrs (ctx : Ctx.t) (instrs : instr list) (k : Ctx.t * Sign.t -> 'r)
      : 'r =
    let f (ctx, sign) instr k_fold =
      match (sign : Sign.t) with
      | Cont -> eval_instr ctx instr k_fold
      | _ -> k_fold (ctx, sign)
    in
    fold_left_cps f (ctx, Cont) instrs k

  (* If instruction evaluation *)

  and eval_if_cond (ctx : Ctx.t) (exp_cond : exp) (k : bool * value -> 'r) : 'r
      =
    eval_exp ctx exp_cond (fun value_cond ->
        let cond = Value.get_bool value_cond in
        k (cond, value_cond))

  and eval_if_cond_list (ctx : Ctx.t) (exp_cond : exp) (vars : var list)
      (iterexps : iterexp list) (k : bool * value list -> 'r) : 'r =
    let ctxs_sub = Ctx.sub_list ctx vars in
    let f (cond, values_cond_rev) ctx_sub k_fold =
      if not cond then k_fold (cond, values_cond_rev)
      else
        eval_if_cond_iter' ctx_sub exp_cond iterexps (fun (cond, value_cond) ->
            let values_cond_rev = value_cond :: values_cond_rev in
            k_fold (cond, values_cond_rev))
    in
    let k_wrap (cond, values_cond_rev) =
      let values_cond = List.rev values_cond_rev in
      k (cond, values_cond)
    in
    fold_left_cps f (true, []) ctxs_sub k_wrap

  and eval_if_cond_iter' (ctx : Ctx.t) (exp_cond : exp)
      (iterexps : iterexp list) (k : bool * value -> 'r) : 'r =
    match iterexps with
    | [] -> eval_if_cond ctx exp_cond k
    | iterexp_h :: iterexps_t -> (
        let iter_h, vars_h = iterexp_h in
        match iter_h with
        | Opt -> error no_region "(TODO)"
        | List ->
            eval_if_cond_list ctx exp_cond vars_h iterexps_t
              (fun (cond, values_cond) ->
                let value_cond =
                  let vid = Value.fresh () in
                  let typ =
                    Il.Ast.IterT (Il.Ast.BoolT $ no_region, Il.Ast.List)
                  in
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
                k (cond, value_cond)))

  and eval_if_cond_iter (ctx : Ctx.t) (exp_cond : exp) (iterexps : iterexp list)
      (k : bool * value -> 'r) : 'r =
    let iterexps = List.rev iterexps in
    eval_if_cond_iter' ctx exp_cond iterexps k

  and eval_if_instr (ctx : Ctx.t) (exp_cond : exp) (iterexps : iterexp list)
      (instrs_then : instr list) (phantom_opt : phantom option)
      (k : Ctx.t * Sign.t -> 'r) : 'r =
    (* Evaluate the if condition and mark phantom *)
    eval_if_cond_iter ctx exp_cond iterexps (fun (cond, value_cond) ->
        let vid = value_cond.note.vid in
        (match phantom_opt with
        | Some (pid, _) -> Ctx.cover ctx (not cond) pid vid
        | None -> ());
        (* Evaluate the then branch if the condition holds *)
        if cond then eval_instrs ctx instrs_then k else k (ctx, Cont))

  (* Hold instruction evaluation *)

  and eval_hold_cond (ctx : Ctx.t) (id : id) (notexp : notexp)
      (k : bool * value -> 'r) : 'r =
    let _, exps_input = notexp in
    eval_exps ctx exps_input (fun values_input ->
        invoke_rel ctx id values_input (fun value_output_opt ->
            let hold = Option.is_some value_output_opt in
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
            k (hold, value_res)))

  and eval_hold_cond_list (ctx : Ctx.t) (id : id) (notexp : notexp)
      (vars : var list) (iterexps : iterexp list) (k : bool * value list -> 'r)
      : 'r =
    let ctxs_sub = Ctx.sub_list ctx vars in
    let f (cond, values_cond_rev) ctx_sub k_fold =
      if not cond then k_fold (cond, values_cond_rev)
      else
        eval_hold_cond_iter' ctx_sub id notexp iterexps
          (fun (cond, value_cond) ->
            let values_cond_rev = value_cond :: values_cond_rev in
            k_fold (cond, values_cond_rev))
    in
    let k_wrap (cond, values_cond_rev) =
      let values_cond = List.rev values_cond_rev in
      k (cond, values_cond)
    in
    fold_left_cps f (true, []) ctxs_sub k_wrap

  and eval_hold_cond_iter' (ctx : Ctx.t) (id : id) (notexp : notexp)
      (iterexps : iterexp list) (k : bool * value -> 'r) : 'r =
    match iterexps with
    | [] -> eval_hold_cond ctx id notexp k
    | iterexp_h :: iterexps_t -> (
        let iter_h, vars_h = iterexp_h in
        match iter_h with
        | Opt -> error no_region "(TODO)"
        | List ->
            eval_hold_cond_list ctx id notexp vars_h iterexps_t
              (fun (cond, values_cond) ->
                let value_cond =
                  let vid = Value.fresh () in
                  let typ =
                    Il.Ast.IterT (Il.Ast.BoolT $ no_region, Il.Ast.List)
                  in
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
                k (cond, value_cond)))

  and eval_hold_cond_iter (ctx : Ctx.t) (id : id) (notexp : notexp)
      (iterexps : iterexp list) (k : bool * value -> 'r) : 'r =
    let iterexps = List.rev iterexps in
    eval_hold_cond_iter' ctx id notexp iterexps k

  and eval_hold_instr (ctx : Ctx.t) (id : id) (notexp : notexp)
      (iterexps : iterexp list) (holdcase : holdcase) (k : Ctx.t * Sign.t -> 'r)
      : 'r =
    (* Copy the current coverage information *)
    let cover_backup = !(ctx.coverage) in
    (* Evaluate the hold condition *)
    eval_hold_cond_iter ctx id notexp iterexps (fun (cond, value_cond) ->
        (* Evaluate the hold case, and restore the coverage information
           if the expected behavior is the relation not holding *)
        let vid = value_cond.note.vid in
        match holdcase with
        | BothH (instrs_hold, instrs_not_hold) ->
            if cond then eval_instrs ctx instrs_hold k
            else (
              ctx.coverage := cover_backup;
              eval_instrs ctx instrs_not_hold k)
        | HoldH (instrs_hold, phantom_opt) ->
            (match phantom_opt with
            | Some (pid, _) -> Ctx.cover ctx (not cond) pid vid
            | None -> ());
            if cond then eval_instrs ctx instrs_hold k else k (ctx, Cont)
        | NotHoldH (instrs_not_hold, phantom_opt) ->
            ctx.coverage := cover_backup;
            (match phantom_opt with
            | Some (pid, _) -> Ctx.cover ctx cond pid vid
            | None -> ());
            if not cond then eval_instrs ctx instrs_not_hold k else k (ctx, Cont))

  (* Case analysis instruction evaluation *)

  and eval_cases (ctx : Ctx.t) (exp : exp) (cases : case list)
      (k : instr list option * value -> 'r) : 'r =
    let f (block_match, values_cond_rev) case k_fold =
      match block_match with
      | Some _ -> k_fold (block_match, values_cond_rev)
      | None ->
          let guard, block = case in
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
          eval_exp ctx exp_cond (fun value_cond ->
              let values_cond_rev = value_cond :: values_cond_rev in
              let cond = Value.get_bool value_cond in
              if cond then k_fold (Some block, values_cond_rev)
              else k_fold (None, values_cond_rev))
    in
    let k_wrap (block_match, values_cond_rev) =
      let values_cond = List.rev values_cond_rev in
      let value_cond =
        let vid = Value.fresh () in
        let typ = Il.Ast.IterT (Il.Ast.BoolT $ no_region, Il.Ast.List) in
        Il.Ast.(ListV values_cond $$$ { vid; typ })
      in
      Ctx.add_node ctx value_cond;
      k (block_match, value_cond)
    in
    fold_left_cps f (None, []) cases k_wrap

  and eval_case_instr (ctx : Ctx.t) (exp : exp) (cases : case list)
      (phantom_opt : phantom option) (k : Ctx.t * Sign.t -> 'r) : 'r =
    (* Evaluate case match and mark phantom *)
    eval_cases ctx exp cases (fun (instrs_opt, value_cond) ->
        let vid = value_cond.note.vid in
        (match phantom_opt with
        | Some (pid, _) -> Ctx.cover ctx (Option.is_none instrs_opt) pid vid
        | None -> ());
        (* Evaluate the matching case if any *)
        match instrs_opt with
        | Some instrs -> eval_instrs ctx instrs k
        | None -> k (ctx, Cont))

  (* Group instruction evaluation *)

  and eval_group_instr (ctx : Ctx.t) (id_group : id) (_exps_group : exp list)
      (instrs_group : instr list) (k : Ctx.t * Sign.t -> 'r) : 'r =
    eval_instrs ctx instrs_group (fun (ctx_group, sign_group) ->
        match sign_group with
        | Cont -> k (ctx, Cont)
        | Res values_output -> k (ctx_group, Res values_output)
        | Ret _ -> error id_group.at "cannot return from group instruction")

  (* Let instruction evaluation *)

  and eval_let (ctx : Ctx.t) (exp_l : exp) (exp_r : exp) (k : Ctx.t -> 'r) : 'r
      =
    eval_exp ctx exp_r (fun value ->
        let ctx = assign_exp ctx exp_l value in
        k ctx)

  and eval_let_opt (ctx : Ctx.t) (exp_l : exp) (exp_r : exp) (vars : var list)
      (iterexps : iterexp list) (k : Ctx.t -> 'r) : 'r =
    (* Discriminate between bound and binding variables *)
    let vars_bound, vars_binding =
      List.partition
        (fun (id, _typ, iters) ->
          Ctx.bound_value Local ctx (id, iters @ [ Il.Ast.Opt ]))
        vars
    in
    (* Create a subcontext for the bound values *)
    let ctx_sub_opt = Ctx.sub_opt ctx vars_bound in
    (* Main binding logic *)
    let do_bind values_binding =
      let f_fold ctx var_binding value_binding k_fold =
        let id_binding, _typ_binding, iters_binding = var_binding in
        let ctx =
          Ctx.add_value Local ctx
            (id_binding, iters_binding @ [ Il.Ast.Opt ])
            value_binding
        in
        k_fold ctx
      in
      fold_left2_cps f_fold ctx vars_binding values_binding k
    in
    (* Collect the binding values and invoke binder *)
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
        do_bind values_binding
    (* Otherwise, evaluate the premise for the subcontext *)
    | Some ctx_sub ->
        eval_let_iter' ctx_sub exp_l exp_r iterexps (fun ctx_sub ->
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
            do_bind values_binding)

  and eval_let_list (ctx : Ctx.t) (exp_l : exp) (exp_r : exp) (vars : var list)
      (iterexps : iterexp list) (k : Ctx.t -> 'r) : 'r =
    (* Discriminate between bound and binding variables *)
    let vars_bound, vars_binding =
      List.partition
        (fun (id, _typ, iters) ->
          Ctx.bound_value Local ctx (id, iters @ [ Il.Ast.List ]))
        vars
    in
    (* Create a subcontext for each batch of bound values *)
    let ctxs_sub = Ctx.sub_list ctx vars_bound in
    (* Main binding logic *)
    let do_bind values_binding_batch =
      let f_fold ctx var_binding values_binding k_fold =
        let id_binding, typ_binding, iters_binding = var_binding in
        let value_binding =
          let vid = Value.fresh () in
          let typ = Typ.iterate typ_binding (iters_binding @ [ Il.Ast.List ]) in
          Il.Ast.(ListV values_binding $$$ { vid; typ = typ.it })
        in
        Ctx.add_node ctx value_binding;
        let ctx =
          Ctx.add_value Local ctx
            (id_binding, iters_binding @ [ Il.Ast.List ])
            value_binding
        in
        k_fold ctx
      in
      fold_left2_cps f_fold ctx vars_binding values_binding_batch k
    in
    (* Collect the binding values and invoke binder *)
    match ctxs_sub with
    (* If the bound variable supposed to guide the iteration is already empty,
       then the binding variables are also empty *)
    | [] ->
        let values_binding_batch =
          List.init (List.length vars_binding) (fun _ -> [])
        in
        do_bind values_binding_batch
    (* Otherwise, evaluate the premise for each batch of bound values,
       and collect the resulting binding batches *)
    | _ ->
        let f_map ctx_sub k_map =
          eval_let_iter' ctx_sub exp_l exp_r iterexps (fun ctx_sub ->
              let values_binding =
                List.map
                  (fun (id_binding, _typ_binding, iters_binding) ->
                    Ctx.find_value Local ctx_sub (id_binding, iters_binding))
                  vars_binding
              in
              k_map values_binding)
        in
        let k_wrap values_binding_batch =
          let values_binding_batch = values_binding_batch |> Ctx.transpose in
          do_bind values_binding_batch
        in
        map_cps f_map ctxs_sub k_wrap

  and eval_let_iter' (ctx : Ctx.t) (exp_l : exp) (exp_r : exp)
      (iterexps : iterexp list) (k : Ctx.t -> 'r) : 'r =
    match iterexps with
    | [] -> eval_let ctx exp_l exp_r k
    | iterexp_h :: iterexps_t -> (
        let iter_h, vars_h = iterexp_h in
        match iter_h with
        | Opt -> eval_let_opt ctx exp_l exp_r vars_h iterexps_t k
        | List -> eval_let_list ctx exp_l exp_r vars_h iterexps_t k)

  and eval_let_iter (ctx : Ctx.t) (exp_l : exp) (exp_r : exp)
      (iterexps : iterexp list) (k : Ctx.t -> 'r) : 'r =
    let iterexps = List.rev iterexps in
    eval_let_iter' ctx exp_l exp_r iterexps k

  and eval_let_instr (ctx : Ctx.t) (exp_l : exp) (exp_r : exp)
      (iterexps : iterexp list) (k : Ctx.t * Sign.t -> 'r) : 'r =
    eval_let_iter ctx exp_l exp_r iterexps (fun ctx -> k (ctx, Cont))

  (* Rule instruction evaluation *)

  and eval_rule (ctx : Ctx.t) (id : id) (notexp : notexp) (k : Ctx.t -> 'r) : 'r
      =
    let exps_input, exps_output =
      let inputs = Ctx.find_rel_inputs Local ctx id in
      let _, exps = notexp in
      InputHint.split_exps_without_idx inputs exps
    in
    eval_exps ctx exps_input (fun values_input ->
        invoke_rel ctx id values_input (function
          | Some value_output ->
              let values_output = Value.get_tuple value_output in
              let ctx = assign_exps ctx exps_output values_output in
              k ctx
          | None -> error id.at (F.asprintf "relation %s was not matched" id.it)))

  and eval_rule_opt (_ctx : Ctx.t) (_id : id) (_notexp : notexp)
      (_vars : var list) (_iterexps : iterexp list) (_k : Ctx.t -> 'r) : 'r =
    failwith "(TODO) eval_rule_opt"

  and eval_rule_list (ctx : Ctx.t) (id : id) (notexp : notexp) (vars : var list)
      (iterexps : iterexp list) (k : Ctx.t -> 'r) : 'r =
    (* Discriminate between bound and binding variables *)
    let vars_bound, vars_binding =
      List.partition
        (fun (id, _typ, iters) ->
          Ctx.bound_value Local ctx (id, iters @ [ Il.Ast.List ]))
        vars
    in
    (* Create a subcontext for each batch of bound values *)
    let ctxs_sub = Ctx.sub_list ctx vars_bound in
    (* Main binding logic *)
    let do_bind values_binding_batch =
      let f_fold ctx var_binding values_binding k_fold =
        let id_binding, typ_binding, iters_binding = var_binding in
        let value_binding =
          let vid = Value.fresh () in
          let typ = Typ.iterate typ_binding (iters_binding @ [ Il.Ast.List ]) in
          Il.Ast.(ListV values_binding $$$ { vid; typ = typ.it })
        in
        Ctx.add_node ctx value_binding;
        let ctx =
          Ctx.add_value Local ctx
            (id_binding, iters_binding @ [ Il.Ast.List ])
            value_binding
        in
        k_fold ctx
      in
      fold_left2_cps f_fold ctx vars_binding values_binding_batch k
    in
    (* Collect the binding values and invoke binder *)
    match ctxs_sub with
    (* If the bound variable supposed to guide the iteration is already empty,
       then the binding variables are also empty *)
    | [] ->
        let values_binding_batch =
          List.init (List.length vars_binding) (fun _ -> [])
        in
        do_bind values_binding_batch
    (* Otherwise, evaluate the premise for each batch of bound values,
       and collect the resulting binding batches *)
    | _ ->
        let f_map ctx_sub k_map =
          eval_rule_iter' ctx_sub id notexp iterexps (fun ctx_sub ->
              let values_binding =
                List.map
                  (fun (id_binding, _typ_binding, iters_binding) ->
                    Ctx.find_value Local ctx_sub (id_binding, iters_binding))
                  vars_binding
              in
              k_map values_binding)
        in
        let k_wrap values_binding_batch =
          let values_binding_batch = values_binding_batch |> Ctx.transpose in
          do_bind values_binding_batch
        in
        map_cps f_map ctxs_sub k_wrap

  and eval_rule_iter' (ctx : Ctx.t) (id : id) (notexp : notexp)
      (iterexps : iterexp list) (k : Ctx.t -> 'r) : 'r =
    match iterexps with
    | [] -> eval_rule ctx id notexp k
    | iterexp_h :: iterexps_t -> (
        let iter_h, vars_h = iterexp_h in
        match iter_h with
        | Opt -> eval_rule_opt ctx id notexp vars_h iterexps_t k
        | List -> eval_rule_list ctx id notexp vars_h iterexps_t k)

  and eval_rule_iter (ctx : Ctx.t) (id : id) (notexp : notexp)
      (iterexps : iterexp list) (k : Ctx.t -> 'r) : 'r =
    let iterexps = List.rev iterexps in
    eval_rule_iter' ctx id notexp iterexps k

  and eval_rule_instr (ctx : Ctx.t) (id : id) (notexp : notexp)
      (iterexps : iterexp list) (k : Ctx.t * Sign.t -> 'r) : 'r =
    eval_rule_iter ctx id notexp iterexps (fun ctx -> k (ctx, Cont))

  (* Result instruction evaluation *)

  and eval_result_instr (ctx : Ctx.t) (exps : exp list)
      (k : Ctx.t * Sign.t -> 'r) : 'r =
    eval_exps ctx exps (fun values -> k (ctx, Res values))

  (* Return instruction evaluation *)

  and eval_return_instr (ctx : Ctx.t) (exp : exp) (k : Ctx.t * Sign.t -> 'r) :
      'r =
    eval_exp ctx exp (fun value -> k (ctx, Ret value))

  (* Debug instruction evaluation *)

  and eval_debug_instr (ctx : Ctx.t) (exp : exp) (k : Ctx.t * Sign.t -> 'r) : 'r
      =
    eval_exp ctx exp (fun value ->
        print_endline
        @@ F.sprintf "%s: %s" (string_of_region exp.at)
             (Il.Print.string_of_exp exp);
        print_endline @@ Il.Print.string_of_value value;
        k (ctx, Cont))

  (* Invoke a relation *)

  and invoke_rel (ctx : Ctx.t) (id : id) (values_input : value list)
      (k : value option -> 'r) : 'r =
    let rel = Ctx.find_rel Local ctx id in
    match rel with
    | Rel.Extern _ -> invoke_extern_rel ctx id values_input k
    | Rel.Defined (_, exps_input, instrs) ->
        invoke_defined_rel ctx id exps_input instrs values_input k

  and invoke_extern_rel (_ctx : Ctx.t) (id : id) (values_input : value list)
      (k : value option -> 'r) : 'r =
    let values_output =
      match id.it with
      | "ExternFunctionCall_eval" -> Arch.eval_extern_func_call values_input
      | "ExternMethodCall_eval" -> Arch.eval_extern_method_call values_input
      | _ -> error id.at (F.asprintf "unimplemented extern relation %s" id.it)
    in
    List.iteri
      (fun idx_arg value_input ->
        List.iter
          (fun value_output ->
            Ctx.add_edge _ctx value_output value_input
              (Dep.Edges.Rel (id, idx_arg)))
          values_output)
      values_input;
    let value_output =
      let vid = Value.fresh () in
      let typ =
        Il.Ast.(
          TupleT
            (List.map (fun value -> value.note.typ $ no_region) values_output))
      in
      Il.Ast.(TupleV values_output $$$ { vid; typ })
    in
    k (Some value_output)

  and invoke_defined_rel (ctx : Ctx.t) (id : id) (exps_input : exp list)
      (instrs : instr list) (values_input : value list) (k : value option -> 'r)
      : 'r =
    (* Main invocation logic *)
    let invoke_defined_rel' (k : value option -> 'r) =
      let ctx_local = Ctx.localize_rule ctx id values_input in
      let ctx_local = assign_exps ctx_local exps_input values_input in
      eval_instrs ctx_local instrs (fun (_ctx_local, sign) ->
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
              let value_output =
                let vid = Value.fresh () in
                let typ =
                  Il.Ast.(
                    TupleT
                      (List.map
                         (fun value -> value.note.typ $ no_region)
                         values_output))
                in
                Il.Ast.(TupleV values_output $$$ { vid; typ })
              in
              k (Some value_output)
          | _ -> k None)
    in
    (* Cache lookup *)
    if (not (Ctx.deriving ctx)) && Cache.is_cached_rule id.it then
      let cache_result = Cache.Cache.find !rule_cache (id.it, values_input) in
      match cache_result with
      | Some values_output ->
          let value_output =
            let vid = Value.fresh () in
            let typ =
              Il.Ast.(
                TupleT
                  (List.map
                     (fun value -> value.note.typ $ no_region)
                     values_output))
            in
            Il.Ast.(TupleV values_output $$$ { vid; typ })
          in
          k (Some value_output)
      | None ->
          invoke_defined_rel' (function
            | Some value_output ->
                let values_output = Value.get_tuple value_output in
                Cache.Cache.add !rule_cache (id.it, values_input) values_output;
                k (Some value_output)
            | None -> k None)
    else invoke_defined_rel' k

  (* Invoke a function *)

  and invoke_func (ctx : Ctx.t) (id : id) (targs : targ list) (args : arg list)
      (k : value option -> 'r) : 'r =
    (* Evaluate type arguments *)
    let targs =
      match targs with
      | [] -> []
      | targs ->
          let theta =
            let tdenv_local =
              match ctx.local with
              | Empty | Rel _ -> TIdMap.empty
              | Func { tdenv; _ } -> tdenv
            in
            TDEnv.fold
              (fun tid typdef theta ->
                match typdef with
                | TypDef.Defined ([], { it = Il.Ast.PlainT typ; _ }) ->
                    TIdMap.add tid typ theta
                | _ -> theta)
              tdenv_local TIdMap.empty
          in
          List.map (Typ.subst_typ theta) targs
    in
    (* Evaluate arguments *)
    eval_args ctx args (fun values_input ->
        invoke_func' ctx id targs values_input k)

  and invoke_func' (ctx : Ctx.t) (id : id) (targs : targ list)
      (values_input : value list) (k : value option -> 'r) : 'r =
    let func = Ctx.find_func Local ctx id in
    match func with
    | Func.Extern -> invoke_extern_func ctx id targs values_input k
    | Func.Builtin -> invoke_builtin_func ctx id targs values_input k
    | Func.Defined (tparams, args_input, instrs) ->
        invoke_defined_func ctx id tparams args_input instrs targs values_input
          k

  and invoke_extern_func (ctx : Ctx.t) (id : id) (_targs : targ list)
      (values_input : value list) (k : value option -> 'r) : 'r =
    let value_output =
      match id.it with
      | "init_externState" -> Arch.eval_extern_init values_input
      | _ -> error id.at (F.asprintf "unimplemented extern function %s" id.it)
    in
    List.iteri
      (fun idx_arg value_input ->
        Ctx.add_edge ctx value_output value_input (Dep.Edges.Func (id, idx_arg)))
      values_input;
    k (Some value_output)

  and invoke_builtin_func (ctx : Ctx.t) (id : id) (targs : targ list)
      (values_input : value list) (k : value option -> 'r) : 'r =
    let value_output = Builtin.invoke ctx id targs values_input in
    List.iteri
      (fun idx_arg value_input ->
        Ctx.add_edge ctx value_output value_input (Dep.Edges.Func (id, idx_arg)))
      values_input;
    k (Some value_output)

  and invoke_defined_func (ctx : Ctx.t) (id : id) (tparams : tparam list)
      (args_input : arg list) (instrs : instr list) (targs : targ list)
      (values_input : value list) (k : value option -> 'r) : 'r =
    (* Main invocation logic *)
    let invoke_defined_func' (k : value option -> 'r) =
      let tdenv_local =
        check
          (List.length targs = List.length tparams)
          id.at "arity mismatch in type arguments";
        List.fold_left2
          (fun tdenv_local tparam targ ->
            let td = TypDef.Defined ([], Il.Ast.PlainT targ $ targ.at) in
            TDEnv.add tparam td tdenv_local)
          TDEnv.empty tparams targs
      in
      let ctx_local = Ctx.localize_func ctx id values_input tdenv_local in
      let ctx_local = assign_args ctx ctx_local args_input values_input in
      eval_instrs ctx_local instrs (fun (_ctx_local, sign) ->
          match sign with
          | Ret value_output ->
              List.iteri
                (fun idx_arg value_input ->
                  Ctx.add_edge ctx value_output value_input
                    (Dep.Edges.Func (id, idx_arg)))
                values_input;
              k (Some value_output)
          | _ -> k None)
    in
    (* Cache lookup *)
    if (not (Ctx.deriving ctx)) && Cache.is_cached_func id.it then
      let cache_result = Cache.Cache.find !func_cache (id.it, values_input) in
      match cache_result with
      | Some value_output -> k (Some value_output)
      | None ->
          invoke_defined_func' (function
            | Some value_output ->
                Cache.Cache.add !func_cache (id.it, values_input) value_output;
                k (Some value_output)
            | None -> k None)
    else invoke_defined_func' k

  (* Load definitions into the context *)

  let load_def (ctx : Ctx.t) (def : def) : Ctx.t =
    match def.it with
    | ExternTypD (id, _) ->
        let td = TypDef.Extern in
        Ctx.add_typdef Global ctx id td
    | TypD (id, tparams, deftyp, _) ->
        let td = TypDef.Defined (tparams, deftyp) in
        Ctx.add_typdef Global ctx id td
    | ExternRelD (id, (_, inputs), _, _) ->
        let rel = Rel.Extern inputs in
        Ctx.add_rel Global ctx id rel
    | RelD (id, (_, inputs), relmatch, relpaths, _) ->
        let rel = Rel.Defined (inputs, relmatch, relpaths) in
        Ctx.add_rel Global ctx id rel
    | ExternDecD (id, _, _, _, _) ->
        let func = Func.Extern in
        Ctx.add_func Global ctx id func
    | BuiltinDecD (id, _, _, _, _) ->
        let func = Func.Builtin in
        Ctx.add_func Global ctx id func
    | DecD (id, tparams, args_input, _typ, instrs, _) ->
        let func = Func.Defined (tparams, args_input, instrs) in
        Ctx.add_func Global ctx id func

  let load_spec (ctx : Ctx.t) (spec : spec) : Ctx.t =
    List.fold_left load_def ctx spec

  (* Entry points for evaluation *)

  let do_eval_rel (ctx : Ctx.t) (spec : spec) (relname : string)
      (values_input : value list) : value list =
    let ctx = load_spec ctx spec in
    invoke_rel ctx (relname $ no_region) values_input (function
      | Some value_output -> value_output
      | None ->
          error no_region (F.asprintf "relation %s was not matched" relname))
    |> Value.get_tuple

  let do_eval_func (ctx : Ctx.t) (spec : spec) (funcname : string)
      (targs : targ list) (values_input : value list) : value =
    let ctx = load_spec ctx spec in
    invoke_func' ctx (funcname $ no_region) targs values_input (function
      | Some value_output -> value_output
      | None ->
          error no_region (F.asprintf "function %s was not matched" funcname))

  let eval_program ~(derive : bool) (spec : spec) (relname : string)
      (includes_p4 : string list) (filename_p4 : string) : Sim.program_result =
    Builtin.init ();
    Value.refresh ();
    Cache.Cache.clear !func_cache;
    Cache.Cache.clear !rule_cache;
    let cover = ref (SCov.init spec) in
    try
      let value_program = Interface.Parse.parse_file includes_p4 filename_p4 in
      let graph = Dep.Graph.assemble_graph value_program in
      let vdg = Ctx.{ graph; vid_program = value_program.note.vid } in
      let ctx = Ctx.empty_end_to_end ~derive vdg cover in
      let values_output = do_eval_rel ctx spec relname [ value_program ] in
      Sim.Pass (values_output, graph, value_program.note.vid, !(ctx.coverage))
    with
    | Util.Error.ParseError (at, msg) -> Sim.IllFormed (at, msg, !cover)
    | Util.Error.InterpError (at, msg) -> Sim.Fail (at, msg, !cover)

  let eval_rel (spec : spec) (relname : string) (values_input : value list) :
      Sim.rel_result =
    Builtin.init ();
    Value.refresh ();
    Cache.Cache.clear !func_cache;
    Cache.Cache.clear !rule_cache;
    let cover = ref (SCov.init spec) in
    let ctx = Ctx.empty_partial cover in
    try
      let values_output = do_eval_rel ctx spec relname values_input in
      Sim.Pass (values_output, !(ctx.coverage))
    with Util.Error.InterpError (at, msg) ->
      Sim.Fail (at, msg, !(ctx.coverage))

  let eval_func (spec : spec) (funcname : string) (targs : targ list)
      (values_input : value list) : Sim.func_result =
    Builtin.init ();
    Value.refresh ();
    Cache.Cache.clear !func_cache;
    Cache.Cache.clear !rule_cache;
    let cover = ref (SCov.init spec) in
    let ctx = Ctx.empty_partial cover in
    try
      let value_output = do_eval_func ctx spec funcname targs values_input in
      Sim.Pass (value_output, !(ctx.coverage))
    with Util.Error.InterpError (at, msg) ->
      Sim.Fail (at, msg, !(ctx.coverage))

  (* Entry point for coverage *)

  let cover_programs (spec : spec) (relname : string)
      (includes_p4 : string list) (filenames_p4 : string list) : MCov.Cover.t =
    let cover_multi = MCov.init spec in
    List.fold_left
      (fun cover_multi filename_p4 ->
        let wellformed, welltyped, cover_single =
          match
            eval_program ~derive:false spec relname includes_p4 filename_p4
          with
          | Pass (_, _, _, cover_single) -> (true, true, cover_single)
          | Fail (_, _, cover_single) -> (true, false, cover_single)
          | IllFormed (_, _, cover_single) -> (false, false, cover_single)
        in
        MCov.extend cover_multi filename_p4 wellformed welltyped cover_single)
      cover_multi filenames_p4
end
