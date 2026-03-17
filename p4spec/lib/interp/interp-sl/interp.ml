open Domain
open Lib
open Lang
open Xl
open Sl
module ICov_single = Coverage.Instr.Single
module ICov_multi = Coverage.Instr.Multi
module DCov_single = Coverage.Dangling.Single
module DCov_multi = Coverage.Dangling.Multi
open Runtime.Dynamic_Sl
open Envs
module Sim = Runtime.Sim.Simulator
module Dep = Runtime.Testgen_neg.Dep
module Hook = Inst.Hook
open Error
open Backtrace
open Nondet
module F = Format
open Util.Source

(* Cache *)

let func_cache = ref (Cache.Cache.create ~size:10000)
let rel_cache = ref (Cache.Cache.create ~size:10000)

module Make (Arch : Sim.ARCH) : Sim.INTERP_SL = struct
  (* Assignments *)

  (* Assigning a value to an expression *)

  let rec assign_exp (ctx : Ctx.t) (exp : exp) (value : value) : Ctx.t =
    let note = value.note.typ in
    match (exp.it, value.it) with
    | VarE id, _ -> Ctx.add_value ctx (id, []) value
    | TupleE exps_inner, TupleV values_inner ->
        let ctx = assign_exps ctx exps_inner values_inner in
        List.iter
          (fun value_inner ->
            Hook.on_value_dependency value_inner value Dep.Edges.Assign)
          values_inner;
        ctx
    | CaseE (_, exps_inner), CaseV (_mixop_value, values_inner) ->
        let ctx = assign_exps ctx exps_inner values_inner in
        List.iter
          (fun value_inner ->
            Hook.on_value_dependency value_inner value Dep.Edges.Assign)
          values_inner;
        ctx
    | OptE exp_opt, OptV value_opt -> (
        match (exp_opt, value_opt) with
        | Some exp_inner, Some value_inner ->
            let ctx = assign_exp ctx exp_inner value_inner in
            Hook.on_value_dependency value_inner value Dep.Edges.Assign;
            ctx
        | None, None -> ctx
        | _ -> assert false)
    | ListE exps_inner, ListV values_inner ->
        let ctx = assign_exps ctx exps_inner values_inner in
        List.iter
          (fun value_inner ->
            Hook.on_value_dependency value_inner value Dep.Edges.Assign)
          values_inner;
        ctx
    | ConsE (exp_h, exp_t), ListV values_inner ->
        let value_h = List.hd values_inner in
        let value_t = Value.make note (ListV (List.tl values_inner)) in
        Hook.on_value value_t;
        let ctx = assign_exp ctx exp_h value_h in
        Hook.on_value_dependency value_h value Dep.Edges.Assign;
        let ctx = assign_exp ctx exp_t value_t in
        Hook.on_value_dependency value_t value Dep.Edges.Assign;
        ctx
    | IterE (_, (Opt, vars)), OptV None ->
        (* Per iterated variable, make an option out of the value *)
        List.fold_left
          (fun ctx (id, typ, iters) ->
            let value_sub =
              let typ = Typ.iterate typ (iters @ [ Il.Opt ]) in
              Value.make typ.it (OptV None)
            in
            Hook.on_value value_sub;
            Hook.on_value_dependency value_sub value Dep.Edges.Assign;
            Ctx.add_value ctx (id, iters @ [ Il.Opt ]) value_sub)
          ctx vars
    | IterE (exp, (Opt, vars)), OptV (Some value) ->
        (* Assign the value to the iterated expression *)
        let ctx = assign_exp ctx exp value in
        (* Per iterated variable, make an option out of the value *)
        List.fold_left
          (fun ctx (id, typ, iters) ->
            let value_sub =
              let value = Ctx.find_value ctx (id, iters) in
              let typ = Typ.iterate typ (iters @ [ Il.Opt ]) in
              Value.make typ.it (OptV (Some value))
            in
            Hook.on_value value_sub;
            Hook.on_value_dependency value_sub value Dep.Edges.Assign;
            Ctx.add_value ctx (id, iters @ [ Il.Opt ]) value_sub)
          ctx vars
    | IterE (exp, (List, vars)), ListV values ->
        (* Map over the value list elements,
           and assign each value to the iterated expression *)
        let ctxs =
          List.map
            (fun value ->
              let ctx_sub = Ctx.localize_clear ctx in
              assign_exp ctx_sub exp value)
            values
        in
        (* Per iterated variable, collect its elementwise value,
           then make a sequence out of them *)
        List.fold_left
          (fun ctx (id, typ, iters) ->
            let values =
              List.map (fun ctx -> Ctx.find_value ctx (id, iters)) ctxs
            in
            let value_sub =
              let typ = Typ.iterate typ (iters @ [ Il.List ]) in
              Value.make typ.it (ListV values)
            in
            Hook.on_value value_sub;
            Hook.on_value_dependency value_sub value Dep.Edges.Assign;
            Ctx.add_value ctx (id, iters @ [ Il.List ]) value_sub)
          ctx vars
    | _ ->
        back_err exp.at
          (F.asprintf "match failed %s <- %s"
             (Sl.Print.string_of_exp exp)
             (Sl.Print.string_of_value ~short:true value))

  and assign_exps (ctx : Ctx.t) (exps : exp list) (values : value list) : Ctx.t
      =
    check_back_err
      (List.length exps = List.length values)
      (over_region (List.map at exps))
      (F.asprintf
         "mismatch in number of expressions and values while assigning, \
          expected %d value(s) but got %d"
         (List.length exps) (List.length values));
    List.fold_left2 assign_exp ctx exps values

  (* Assigning a value to a parameter *)

  and assign_param (ctx_caller : Ctx.t) (ctx_callee : Ctx.t) (param : param)
      (value : value) : Ctx.t =
    match param.it with
    | ExpP (_typ, exp) -> assign_param_exp ctx_callee exp value
    | DefP id -> assign_param_def ctx_caller ctx_callee id value

  and assign_params (ctx_caller : Ctx.t) (ctx_callee : Ctx.t)
      (params : param list) (values : value list) : Ctx.t =
    check_back_err
      (List.length params = List.length values)
      (over_region (List.map at params))
      (F.asprintf
         "mismatch in number of parameters and values while assigning, \
          expected %d value(s) but got %d"
         (List.length params) (List.length values));
    List.fold_left2 (assign_param ctx_caller) ctx_callee params values

  and assign_param_exp (ctx : Ctx.t) (exp : exp) (value : value) : Ctx.t =
    assign_exp ctx exp value

  and assign_param_def (ctx_caller : Ctx.t) (ctx_callee : Ctx.t) (id : id)
      (value : value) : Ctx.t =
    match value.it with
    | FuncV id_f ->
        let _, func = Ctx.find_func ctx_caller id_f in
        Ctx.add_func ctx_callee id func
    | _ ->
        back_err id.at
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
    try eval_exp' ctx exp
    with Backtrace backtrace ->
      back_nest exp.at
        (F.asprintf "%s failed" (Sl.Print.string_of_exp exp))
        backtrace

  and eval_exp' (ctx : Ctx.t) (exp : exp) : value =
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
    let value_res = Value.make note (BoolV b) in
    Hook.on_value value_res;
    List.iter
      (fun value_input ->
        Hook.on_value_dependency value_res value_input Dep.Edges.Control)
      (Ctx.find_values_input ctx);
    value_res

  (* Numeric expression evaluation *)

  and eval_num_exp (note : typ') (ctx : Ctx.t) (n : Num.t) : value =
    let value_res = Value.make note (NumV n) in
    Hook.on_value value_res;
    List.iter
      (fun value_input ->
        Hook.on_value_dependency value_res value_input Dep.Edges.Control)
      (Ctx.find_values_input ctx);
    value_res

  (* Text expression evaluation *)

  and eval_text_exp (note : typ') (ctx : Ctx.t) (s : string) : value =
    let value_res = Value.make note (TextV s) in
    Hook.on_value value_res;
    List.iter
      (fun value_input ->
        Hook.on_value_dependency value_res value_input Dep.Edges.Control)
      (Ctx.find_values_input ctx);
    value_res

  (* Variable expression evaluation *)

  and eval_var_exp (_note : typ') (ctx : Ctx.t) (id : id) : value =
    Ctx.find_value ctx (id, [])

  (* Unary expression evaluation *)

  and eval_un_bool (unop : Bool.unop) (value : value) : value' =
    match unop with `NotOp -> Il.BoolV (not (Value.get_bool value))

  and eval_un_num (unop : Num.unop) (value : value) : value' =
    let num = Value.get_num value in
    let num = Num.un unop num in
    Il.NumV num

  and eval_un_exp (note : typ') (ctx : Ctx.t) (unop : unop) (_optyp : optyp)
      (exp : exp) : value =
    let value = eval_exp ctx exp in
    let value_res =
      match unop with
      | #Bool.unop as unop -> eval_un_bool unop value
      | #Num.unop as unop -> eval_un_num unop value
    in
    let value_res = Value.make note value_res in
    Hook.on_value value_res;
    Hook.on_value_dependency value_res value (Dep.Edges.Op (UnOp unop));
    value_res

  (* Binary expression evaluation *)

  and eval_bin_bool (binop : Bool.binop) (value_l : value) (value_r : value) :
      value' =
    let bool_l = Value.get_bool value_l in
    let bool_r = Value.get_bool value_r in
    match binop with
    | `AndOp -> Il.BoolV (bool_l && bool_r)
    | `OrOp -> Il.BoolV (bool_l || bool_r)
    | `ImplOp -> Il.BoolV ((not bool_l) || bool_r)
    | `EquivOp -> Il.BoolV (bool_l = bool_r)

  and eval_bin_num (binop : Num.binop) (value_l : value) (value_r : value) :
      value' =
    let num_l = Value.get_num value_l in
    let num_r = Value.get_num value_r in
    Il.NumV (Num.bin binop num_l num_r)

  and eval_bin_exp (note : typ') (ctx : Ctx.t) (binop : binop) (_optyp : optyp)
      (exp_l : exp) (exp_r : exp) : value =
    let value_l = eval_exp ctx exp_l in
    let value_r = eval_exp ctx exp_r in
    let value_res =
      match binop with
      | #Bool.binop as binop -> eval_bin_bool binop value_l value_r
      | #Num.binop as binop -> eval_bin_num binop value_l value_r
    in
    let value_res = Value.make note value_res in
    Hook.on_value value_res;
    Hook.on_value_dependency value_res value_l (Dep.Edges.Op (BinOp binop));
    Hook.on_value_dependency value_res value_r (Dep.Edges.Op (BinOp binop));
    value_res

  (* Comparison expression evaluation *)

  and eval_cmp_bool (cmpop : Bool.cmpop) (value_l : value) (value_r : value) :
      value' =
    let eq = Value.eq value_l value_r in
    match cmpop with `EqOp -> Il.BoolV eq | `NeOp -> Il.BoolV (not eq)

  and eval_cmp_num (cmpop : Num.cmpop) (value_l : value) (value_r : value) :
      value' =
    let num_l = Value.get_num value_l in
    let num_r = Value.get_num value_r in
    Il.BoolV (Num.cmp cmpop num_l num_r)

  and eval_cmp_exp (note : typ') (ctx : Ctx.t) (cmpop : cmpop) (_optyp : optyp)
      (exp_l : exp) (exp_r : exp) : value =
    let value_l = eval_exp ctx exp_l in
    let value_r = eval_exp ctx exp_r in
    let value_res =
      match cmpop with
      | #Bool.cmpop as cmpop -> eval_cmp_bool cmpop value_l value_r
      | #Num.cmpop as cmpop -> eval_cmp_num cmpop value_l value_r
    in
    let value_res = Value.make note value_res in
    Hook.on_value value_res;
    Hook.on_value_dependency value_res value_l (Dep.Edges.Op (CmpOp cmpop));
    Hook.on_value_dependency value_res value_r (Dep.Edges.Op (CmpOp cmpop));
    value_res

  (* Upcast expression evaluation *)

  and upcast (ctx : Ctx.t) (typ : typ) (value : value) : value =
    let back_err_upcast () =
      back_err typ.at
        (F.asprintf "cannot upcast value %s to type %s"
           (Sl.Print.string_of_value ~short:true value)
           (Sl.Print.string_of_typ typ))
    in
    match typ.it with
    | NumT `IntT -> (
        match value.it with
        | NumV (`Nat n) ->
            let value_res = Value.make typ.it (NumV (`Int n)) in
            Hook.on_value value_res;
            Hook.on_value_dependency value_res value (Dep.Edges.Op (CastOp typ));
            value_res
        | NumV (`Int _) -> value
        | _ -> back_err_upcast ())
    | VarT (tid, targs) -> (
        let tparams, deftyp = Ctx.find_defined_typdef ctx tid in
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
            let value_res = Value.make typ.it (TupleV values) in
            Hook.on_value value_res;
            Hook.on_value_dependency value_res value (Dep.Edges.Op (CastOp typ));
            value_res
        | _ -> back_err_upcast ())
    | _ -> value

  and eval_upcast_exp (_note : typ') (ctx : Ctx.t) (typ : typ) (exp : exp) :
      value =
    let value = eval_exp ctx exp in
    upcast ctx typ value

  (* Downcast expression evaluation *)

  and downcast (ctx : Ctx.t) (typ : typ) (value : value) : value =
    let back_err_downcast () =
      back_err typ.at
        (F.asprintf "cannot downcast value %s to type %s"
           (Sl.Print.string_of_value ~short:true value)
           (Sl.Print.string_of_typ typ))
    in
    match typ.it with
    | NumT `NatT -> (
        match value.it with
        | NumV (`Nat _) -> value
        | NumV (`Int i) when Bigint.(i >= zero) ->
            let value_res = Value.make typ.it (NumV (`Nat i)) in
            Hook.on_value value_res;
            Hook.on_value_dependency value_res value (Dep.Edges.Op (CastOp typ));
            value_res
        | _ -> back_err_downcast ())
    | VarT (tid, targs) -> (
        let tparams, deftyp = Ctx.find_defined_typdef ctx tid in
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
            let value_res = Value.make typ.it (TupleV values) in
            Hook.on_value value_res;
            Hook.on_value_dependency value_res value (Dep.Edges.Op (CastOp typ));
            value_res
        | _ -> back_err_downcast ())
    | _ -> value

  and eval_downcast_exp (_note : typ') (ctx : Ctx.t) (typ : typ) (exp : exp) :
      value =
    let value = eval_exp ctx exp in
    downcast ctx typ value

  (* Subtype check expression evaluation *)

  and subtyp (ctx : Ctx.t) (typ : typ) (value : value) : bool =
    match typ.it with
    | BoolT -> ( match value.it with BoolV _ -> true | _ -> false)
    | NumT `NatT -> (
        match value.it with
        | NumV (`Nat _) -> true
        | NumV (`Int i) -> Bigint.(i >= zero)
        | _ -> false)
    | NumT `IntT -> ( match value.it with NumV _ -> true | _ -> false)
    | TextT -> ( match value.it with TextV _ -> true | _ -> false)
    | VarT (tid, targs) -> (
        let typdef = Ctx.find_typdef ctx tid in
        match typdef with
        | Param -> assert false
        | Extern -> ( match value.it with ExternV _ -> true | _ -> false)
        | Defined (tparams, deftyp) -> (
            let theta = List.combine tparams targs |> TIdMap.of_list in
            match (deftyp.it, value.it) with
            | PlainT typ, _ ->
                let typ = Typ.subst_typ theta typ in
                subtyp ctx typ value
            | VariantT typcases, CaseV (mixop_v, values_inner) ->
                List.exists
                  (fun typcase ->
                    let nottyp, _hints = typcase in
                    let mixop_t, typs_inner = nottyp.it in
                    Mixop.eq mixop_t mixop_v
                    &&
                    let typs_inner =
                      List.map (Typ.subst_typ theta) typs_inner
                    in
                    subtyps ctx typs_inner values_inner)
                  typcases
            | _ -> true))
    | TupleT typs -> (
        match value.it with
        | TupleV values ->
            List.length typs = List.length values
            && List.for_all2 (subtyp ctx) typs values
        | _ -> false)
    | IterT (typ_inner, Opt) -> (
        match value.it with
        | OptV value_opt -> (
            match value_opt with
            | Some value_inner -> subtyp ctx typ_inner value_inner
            | None -> true)
        | _ -> false)
    | IterT (typ_inner, List) -> (
        match value.it with
        | ListV values -> List.for_all (subtyp ctx typ_inner) values
        | _ -> false)
    | _ -> false

  and subtyps (ctx : Ctx.t) (typs : typ list) (values : value list) : bool =
    List.length typs = List.length values
    && List.for_all2 (subtyp ctx) typs values

  and eval_sub_exp (note : typ') (ctx : Ctx.t) (exp : exp) (typ : typ) : value =
    let value = eval_exp ctx exp in
    let sub = subtyp ctx typ value in
    let value_res = Value.make note (BoolV sub) in
    Hook.on_value value_res;
    Hook.on_value_dependency value_res value (Dep.Edges.Op (SubOp typ));
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
    let value_res = Value.make note (BoolV matches) in
    Hook.on_value value_res;
    Hook.on_value_dependency value_res value (Dep.Edges.Op (MatchOp pattern));
    value_res

  (* Tuple expression evaluation *)

  and eval_tuple_exp (note : typ') (ctx : Ctx.t) (exps : exp list) : value =
    let values = eval_exps ctx exps in
    let value_res = Value.make note (TupleV values) in
    Hook.on_value value_res;
    if List.length values = 0 then
      List.iter
        (fun value_input ->
          Hook.on_value_dependency value_res value_input Dep.Edges.Control)
        (Ctx.find_values_input ctx);
    value_res

  (* Case expression evaluation *)

  and eval_case_exp (note : typ') (ctx : Ctx.t) (notexp : notexp) : value =
    let mixop, exps = notexp in
    let values = eval_exps ctx exps in
    let value_res = Value.make note (CaseV (mixop, values)) in
    Hook.on_value value_res;
    if List.length values = 0 then
      List.iter
        (fun value_input ->
          Hook.on_value_dependency value_res value_input Dep.Edges.Control)
        (Ctx.find_values_input ctx);
    value_res

  (* Struct expression evaluation *)

  and eval_str_exp (note : typ') (ctx : Ctx.t) (fields : (atom * exp) list) :
      value =
    let atoms, exps = List.split fields in
    let values = eval_exps ctx exps in
    let fields = List.combine atoms values in
    let value_res = Value.make note (StructV fields) in
    Hook.on_value value_res;
    if List.length values = 0 then
      List.iter
        (fun value_input ->
          Hook.on_value_dependency value_res value_input Dep.Edges.Control)
        (Ctx.find_values_input ctx);
    value_res

  (* Option expression evaluation *)

  and eval_opt_exp (note : typ') (ctx : Ctx.t) (exp_opt : exp option) : value =
    let value_opt = Option.map (eval_exp ctx) exp_opt in
    let value_res = Value.make note (OptV value_opt) in
    Hook.on_value value_res;
    if Option.is_none value_opt then
      List.iter
        (fun value_input ->
          Hook.on_value_dependency value_res value_input Dep.Edges.Control)
        (Ctx.find_values_input ctx);
    value_res

  (* List expression evaluation *)

  and eval_list_exp (note : typ') (ctx : Ctx.t) (exps : exp list) : value =
    let values = eval_exps ctx exps in
    let value_res = Value.make note (ListV values) in
    Hook.on_value value_res;
    if List.length values = 0 then
      List.iter
        (fun value_input ->
          Hook.on_value_dependency value_res value_input Dep.Edges.Control)
        (Ctx.find_values_input ctx);
    value_res

  (* Cons expression evaluation *)

  and eval_cons_exp (note : typ') (ctx : Ctx.t) (exp_h : exp) (exp_t : exp) :
      value =
    let value_h = eval_exp ctx exp_h in
    let value_t = eval_exp ctx exp_t in
    let values_t = Value.get_list value_t in
    let value_res = Value.make note (ListV (value_h :: values_t)) in
    Hook.on_value value_res;
    value_res

  (* Concatenation expression evaluation *)

  and eval_cat_exp (note : typ') (ctx : Ctx.t) (at : region) (exp_l : exp)
      (exp_r : exp) : value =
    let value_l = eval_exp ctx exp_l in
    let value_r = eval_exp ctx exp_r in
    let value_res =
      match (value_l.it, value_r.it) with
      | TextV s_l, TextV s_r -> Il.TextV (s_l ^ s_r)
      | ListV values_l, ListV values_r -> Il.ListV (values_l @ values_r)
      | _ ->
          back_err at
            (F.asprintf
               "concatenation expects either two texts or two lists, but got \
                %s and %s"
               (Sl.Print.string_of_value ~short:true value_l)
               (Sl.Print.string_of_value ~short:true value_r))
    in
    let value_res = Value.make note value_res in
    Hook.on_value value_res;
    Hook.on_value_dependency value_res value_l (Dep.Edges.Op CatOp);
    Hook.on_value_dependency value_res value_r (Dep.Edges.Op CatOp);
    value_res

  (* Membership expression evaluation *)

  and eval_mem_exp (note : typ') (ctx : Ctx.t) (exp_e : exp) (exp_s : exp) :
      value =
    let value_e = eval_exp ctx exp_e in
    let value_s = eval_exp ctx exp_s in
    let values_s = Value.get_list value_s in
    let value_res =
      Value.make note (BoolV (List.exists (Value.eq value_e) values_s))
    in
    Hook.on_value value_res;
    Hook.on_value_dependency value_res value_e (Dep.Edges.Op MemOp);
    Hook.on_value_dependency value_res value_s (Dep.Edges.Op MemOp);
    value_res

  (* Length expression evaluation *)

  and eval_len_exp (note : typ') (ctx : Ctx.t) (exp : exp) : value =
    let value = eval_exp ctx exp in
    let len =
      match value.it with
      | TextV s -> s |> String.length |> Bigint.of_int
      | ListV values -> values |> List.length |> Bigint.of_int
      | _ ->
          back_err exp.at
            (F.asprintf
               "length operation expects either a text or a list, but got %s"
               (Sl.Print.string_of_value ~short:true value))
    in
    let value_res = Value.make note (NumV (`Nat len)) in
    Hook.on_value value_res;
    Hook.on_value_dependency value_res value (Dep.Edges.Op LenOp);
    value_res

  (* Dot expression evaluation *)

  and eval_dot_exp (_note : typ') (ctx : Ctx.t) (exp_b : exp) (atom : atom) :
      value =
    let value_b = eval_exp ctx exp_b in
    let fields = Value.get_struct value_b in
    let value_res =
      fields
      |> List.find (fun (atom_field, _) -> Atom.eq atom_field.it atom.it)
      |> snd
    in
    value_res

  (* Index expression evaluation *)

  and eval_idx_exp (_note : typ') (ctx : Ctx.t) (exp_b : exp) (exp_i : exp) :
      value =
    let value_b = eval_exp ctx exp_b in
    let value_i = eval_exp ctx exp_i in
    let idx = value_i |> Value.get_num |> Num.to_int |> Bigint.to_int_exn in
    match value_b.it with
    | TextV s when idx < 0 || idx >= String.length s ->
        back_err exp_i.at
          (F.asprintf "index %d out of bounds [0, %d)" idx (String.length s))
    | TextV s ->
        let s = String.get s idx |> String.make 1 in
        let value_res = Value.make Il.TextT (TextV s) in
        Hook.on_value value_res;
        value_res
    | ListV values when idx < 0 || idx >= List.length values ->
        back_err exp_i.at
          (F.asprintf "index %d out of bounds [0, %d)" idx (List.length values))
    | ListV values -> List.nth values idx
    | _ ->
        back_err exp_b.at
          (F.asprintf "indexing expects either a text or a list, but got %s"
             (Sl.Print.string_of_value ~short:true value_b))

  (* Sl.ce expression evaluation *)

  and eval_slice_exp (note : typ') (ctx : Ctx.t) (exp_b : exp) (exp_i : exp)
      (exp_n : exp) : value =
    let value_b = eval_exp ctx exp_b in
    let value_i = eval_exp ctx exp_i in
    let idx_l = value_i |> Value.get_num |> Num.to_int |> Bigint.to_int_exn in
    let value_n = eval_exp ctx exp_n in
    let idx_n = value_n |> Value.get_num |> Num.to_int |> Bigint.to_int_exn in
    let idx_h = idx_l + idx_n in
    match value_b.it with
    | TextV s when idx_l < 0 || idx_h > String.length s ->
        back_err exp_n.at
          (F.asprintf "slice [%d, %d) out of bounds [0, %d)" idx_l idx_h
             (String.length s))
    | TextV s ->
        let s_slice = String.sub s idx_l (idx_h - idx_l) in
        let value_res = Value.make Il.TextT (TextV s_slice) in
        Hook.on_value value_res;
        value_res
    | ListV values when idx_l < 0 || idx_h > List.length values ->
        back_err exp_n.at
          (F.asprintf "slice [%d, %d) out of bounds [0, %d)" idx_l idx_h
             (List.length values))
    | ListV values ->
        let values_slice =
          List.mapi
            (fun idx value ->
              if idx_l <= idx && idx < idx_h then Some value else None)
            values
          |> List.filter_map Fun.id
        in
        let value_res = Value.make note (ListV values_slice) in
        Hook.on_value value_res;
        value_res
    | _ ->
        back_err exp_b.at
          (F.asprintf "slicing expects either a text or a list, but got %s"
             (Sl.Print.string_of_value ~short:true value_b))

  (* Update expression evaluation *)

  and eval_access_path (ctx : Ctx.t) (value_b : value) (path : path) : value =
    match path.it with
    | RootP -> value_b
    | IdxP (path, exp_i) -> (
        let value = eval_access_path ctx value_b path in
        let value_i = eval_exp ctx exp_i in
        let idx = value_i |> Value.get_num |> Num.to_int |> Bigint.to_int_exn in
        match value.it with
        | TextV s when idx < 0 || idx >= String.length s ->
            back_err exp_i.at
              (F.asprintf "index %d out of bounds [0, %d)" idx (String.length s))
        | TextV s ->
            let s = String.get s idx |> String.make 1 in
            let value_res = Value.make Il.TextT (TextV s) in
            Hook.on_value value_res;
            value_res
        | ListV values when idx < 0 || idx >= List.length values ->
            back_err exp_i.at
              (F.asprintf "index %d out of bounds [0, %d)" idx
                 (List.length values))
        | ListV values -> List.nth values idx
        | _ ->
            back_err path.at
              (F.asprintf "indexing expects either a text or a list, but got %s"
                 (Sl.Print.string_of_value ~short:true value)))
    | SliceP (path, exp_i, exp_n) -> (
        let value = eval_access_path ctx value_b path in
        let value_i = eval_exp ctx exp_i in
        let idx_l =
          value_i |> Value.get_num |> Num.to_int |> Bigint.to_int_exn
        in
        let value_n = eval_exp ctx exp_n in
        let idx_n =
          value_n |> Value.get_num |> Num.to_int |> Bigint.to_int_exn
        in
        let idx_h = idx_l + idx_n in
        match value.it with
        | TextV s when idx_l < 0 || idx_h > String.length s ->
            back_err exp_n.at
              (F.asprintf "slice [%d, %d) out of bounds [0, %d)" idx_l idx_h
                 (String.length s))
        | TextV s ->
            let s_slice = String.sub s idx_l (idx_h - idx_l) in
            let value_res = Value.make Il.TextT (TextV s_slice) in
            Hook.on_value value_res;
            value_res
        | ListV values when idx_l < 0 || idx_h > List.length values ->
            back_err exp_n.at
              (F.asprintf "slice [%d, %d) out of bounds [0, %d)" idx_l idx_h
                 (List.length values))
        | ListV values ->
            let values_slice =
              List.mapi
                (fun idx value ->
                  if idx_l <= idx && idx < idx_h then Some value else None)
                values
              |> List.filter_map Fun.id
            in
            let value_res = Value.make path.note (ListV values_slice) in
            Hook.on_value value_res;
            value_res
        | _ ->
            back_err path.at
              (F.asprintf "slicing expects either a text or a list, but got %s"
                 (Sl.Print.string_of_value ~short:true value)))
    | DotP (path, atom) ->
        let value = eval_access_path ctx value_b path in
        let fields = value |> Value.get_struct in
        fields
        |> List.find (fun (atom_field, _) -> Atom.eq atom_field.it atom.it)
        |> snd

  and eval_update_path (ctx : Ctx.t) (value_b : value) (path : path)
      (value_n : value) : value =
    match path.it with
    | RootP -> value_n
    | IdxP (path, exp_i) -> (
        let value = eval_access_path ctx value_b path in
        let value_i = eval_exp ctx exp_i in
        let idx_target =
          value_i |> Value.get_num |> Num.to_int |> Bigint.to_int_exn
        in
        match value.it with
        | TextV s when idx_target < 0 || idx_target >= String.length s ->
            back_err exp_i.at
              (F.asprintf "index %d out of bounds [0, %d)" idx_target
                 (String.length s))
        | TextV s ->
            let s_n = Value.get_text value_n in
            if String.length s_n <> 1 then
              back_err exp_i.at
                (F.asprintf
                   "updating a character requires a single-character text, but \
                    got %s"
                   (Sl.Print.string_of_value ~short:true value_n))
            else
              let s_updated =
                String.sub s 0 idx_target ^ s_n
                ^ String.sub s (idx_target + 1)
                    (String.length s - idx_target - 1)
              in
              let value = Value.make Il.TextT (TextV s_updated) in
              Hook.on_value value;
              eval_update_path ctx value_b path value
        | ListV values when idx_target < 0 || idx_target >= List.length values
          ->
            back_err exp_i.at
              (F.asprintf "index %d out of bounds [0, %d)" idx_target
                 (List.length values))
        | ListV values ->
            let values_updated =
              List.mapi
                (fun idx value -> if idx = idx_target then value_n else value)
                values
            in
            let value = Value.make path.note (ListV values_updated) in
            Hook.on_value value;
            eval_update_path ctx value_b path value
        | _ ->
            back_err path.at
              (F.asprintf "indexing expects either a text or a list, but got %s"
                 (Sl.Print.string_of_value ~short:true value)))
    | SliceP (path, exp_i, exp_n) -> (
        let value = eval_access_path ctx value_b path in
        let value_i = eval_exp ctx exp_i in
        let idx_l =
          value_i |> Value.get_num |> Num.to_int |> Bigint.to_int_exn
        in
        let value_n = eval_exp ctx exp_n in
        let idx_n =
          value_n |> Value.get_num |> Num.to_int |> Bigint.to_int_exn
        in
        let idx_h = idx_l + idx_n in
        match value.it with
        | TextV s when idx_l < 0 || idx_h > String.length s ->
            back_err exp_n.at
              (F.asprintf "slice [%d, %d) out of bounds [0, %d)" idx_l idx_h
                 (String.length s))
        | TextV s ->
            let s_n = Value.get_text value_n in
            if String.length s_n <> idx_n then
              back_err exp_n.at
                (F.asprintf
                   "updating a slice of length %d requires a text of length \
                    %d, but got %s"
                   idx_n (String.length s_n)
                   (Sl.Print.string_of_value ~short:true value_n))
            else
              let s_updated =
                String.sub s 0 idx_l ^ s_n
                ^ String.sub s idx_h (String.length s - idx_h)
              in
              let value = Value.make Il.TextT (TextV s_updated) in
              Hook.on_value value;
              eval_update_path ctx value_b path value
        | ListV values when idx_l < 0 || idx_h > List.length values ->
            back_err exp_n.at
              (F.asprintf "slice [%d, %d) out of bounds [0, %d)" idx_l idx_h
                 (List.length values))
        | ListV values ->
            let values_n = Value.get_list value_n in
            if List.length values_n <> idx_n then
              back_err exp_n.at
                (F.asprintf
                   "updating a slice of length %d requires a list of length \
                    %d, but got %s"
                   idx_n (List.length values_n)
                   (Sl.Print.string_of_value ~short:true value_n))
            else
              let values_updated =
                List.mapi
                  (fun idx value ->
                    if idx_l <= idx && idx < idx_h then
                      List.nth values_n (idx - idx_l)
                    else value)
                  values
              in
              let value = Value.make path.note (ListV values_updated) in
              Hook.on_value value;
              eval_update_path ctx value_b path value
        | _ ->
            back_err path.at
              (F.asprintf "slicing expects either a text or a list, but got %s"
                 (Sl.Print.string_of_value ~short:true value)))
    | DotP (path, atom) ->
        let value = eval_access_path ctx value_b path in
        let fields = value |> Value.get_struct in
        let fields =
          List.map
            (fun (atom_f, value_f) ->
              if atom_f.it = atom.it then (atom_f, value_n)
              else (atom_f, value_f))
            fields
        in
        let value = Value.make path.note (StructV fields) in
        Hook.on_value value;
        eval_update_path ctx value_b path value

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
          Value.make note (OptV (Some value))
      | None -> Value.make note (OptV None)
    in
    Hook.on_value value_res;
    List.iter
      (fun (id, _typ, iters) ->
        let value_sub = Ctx.find_value ctx (id, iters @ [ Il.Opt ]) in
        Hook.on_value_dependency value_res value_sub Dep.Edges.Iter)
      vars;
    value_res

  and eval_iter_exp_list (note : typ') (ctx : Ctx.t) (exp : exp)
      (vars : var list) : value =
    let ctxs_sub = Ctx.sub_list ctx vars in
    let values = List.map (fun ctx_sub -> eval_exp ctx_sub exp) ctxs_sub in
    let value_res = Value.make note (ListV values) in
    Hook.on_value value_res;
    List.iter
      (fun (id, _typ, iters) ->
        let value_sub = Ctx.find_value ctx (id, iters @ [ Il.List ]) in
        Hook.on_value_dependency value_res value_sub Dep.Edges.Iter)
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
    try eval_arg' ctx arg
    with Backtrace backtrace ->
      back_nest arg.at
        (F.asprintf "%s failed" (Sl.Print.string_of_arg arg))
        backtrace

  and eval_arg' (ctx : Ctx.t) (arg : arg) : value =
    match arg.it with
    | ExpA exp -> eval_exp ctx exp
    | DefA id ->
        let value_res = Value.make Il.FuncT (FuncV id) in
        Hook.on_value value_res;
        value_res

  and eval_args (ctx : Ctx.t) (args : arg list) : value list =
    List.map (eval_arg ctx) args

  (* Instruction evaluation *)

  and eval_instr (ctx : Ctx.t) (instr : instr) : Flow.t =
    Hook.on_instr instr;
    try eval_instr' ctx instr
    with Backtrace backtrace ->
      backtrace
      |> back_nest instr.at
           (F.asprintf "%s failed" (Sl.Print.string_of_instr ~short:true instr))

  and eval_instr' (ctx : Ctx.t) (instr : instr) : Flow.t =
    match instr.it with
    | IfI (exp_cond, iterexps, block_then, phantom_opt) ->
        eval_if_instr ctx exp_cond iterexps block_then phantom_opt
    | HoldI (id, notexp, iterexps, holdcase) ->
        eval_hold_instr ctx id notexp iterexps holdcase
    | CaseI (exp, cases, phantom_opt) ->
        eval_case_instr ctx exp cases phantom_opt
    | GroupI (id_group, rel_signature, exps_group, block) ->
        eval_group_instr ctx id_group rel_signature exps_group block
    | LetI (exp_l, exp_r, iterinstrs, block) ->
        eval_let_instr ctx exp_l exp_r iterinstrs block
    | RuleI (id, notexp, inputs, iterinstrs, block) ->
        eval_rule_instr ctx id notexp inputs iterinstrs block
    | ResultI (rel_signature, exps) -> eval_result_instr ctx rel_signature exps
    | ReturnI exp -> eval_return_instr ctx exp
    | DebugI exp -> eval_debug_instr ctx exp

  and eval_block (ctx : Ctx.t) (block : block) : Flow.t =
    if !Ctx.is_det then eval_block_deterministic ctx block
    else eval_block_sequential ctx block

  and eval_block_deterministic (ctx : Ctx.t) (block : block) : Flow.t =
    let eval_instr_deterministic (flow_pre : Flow.t) (instr : instr) : Flow.t =
      let open Flow in
      try
        let flow_post = eval_instr ctx instr in
        match flow_pre with
        | Cont traces_pre -> (
            match flow_post with
            | Cont traces_post -> Cont (traces_pre @ traces_post)
            | _ -> flow_post)
        | Res _ -> (
            match flow_post with
            | Cont _ -> flow_pre
            | Res _ -> nondet instr.at
            | Ret _ -> back_err instr.at "cannot have both result and return")
        | Ret _ -> (
            match flow_post with
            | Cont _ -> flow_pre
            | Res _ -> back_err instr.at "cannot have both return and result"
            | Ret _ -> nondet instr.at)
      with Backtrace (Unmatch _) -> flow_pre
    in
    try
      List.fold_left
        (fun flow_pre instr -> eval_instr_deterministic flow_pre instr)
        (Flow.Cont []) block
    with Nondet at -> back_err at "nondeterministic instruction evaluation"

  and eval_block_sequential (ctx : Ctx.t) (block : block) : Flow.t =
    List.fold_left
      (fun flow_pre instr ->
        match flow_pre with
        | Flow.Cont _ -> eval_instr ctx instr
        | _ -> flow_pre)
      (Flow.Cont []) block

  and eval_elseblock_opt (ctx : Ctx.t) (flow : Flow.t)
      (elseblock_opt : elseblock option) : Flow.t =
    match flow with
    | Flow.Cont traces -> (
        match elseblock_opt with
        | Some block_else -> eval_block ctx block_else
        | None -> Flow.Cont traces)
    | _ -> flow

  (* If instruction evaluation *)

  and eval_if_cond (ctx : Ctx.t) (exp_cond : exp) : bool * value =
    let value_cond = eval_exp ctx exp_cond in
    let cond = Value.get_bool value_cond in
    (cond, value_cond)

  and eval_if_cond_opt (ctx : Ctx.t) (exp_cond : exp) (vars : var list)
      (iterexps : iterexp list) : bool * value option =
    let ctx_sub_opt = Ctx.sub_opt ctx vars in
    match ctx_sub_opt with
    | Some ctx_sub ->
        let cond, value_cond = eval_if_cond_iter ctx_sub exp_cond iterexps in
        (cond, Some value_cond)
    | None -> (false, None)

  and eval_if_cond_list (ctx : Ctx.t) (exp_cond : exp) (vars : var list)
      (iterexps : iterexp list) : bool * value list =
    let ctxs_sub = Ctx.sub_list ctx vars in
    let cond, values_cond_rev =
      List.fold_left
        (fun (cond, values_cond_rev) ctx_sub ->
          if not cond then (cond, values_cond_rev)
          else
            let cond, value_cond =
              eval_if_cond_iter' ctx_sub exp_cond iterexps
            in
            let values_cond_rev = value_cond :: values_cond_rev in
            (cond, values_cond_rev))
        (true, []) ctxs_sub
    in
    let values_cond = List.rev values_cond_rev in
    (cond, values_cond)

  and eval_if_cond_iter' (ctx : Ctx.t) (exp_cond : exp)
      (iterexps : iterexp list) : bool * value =
    match iterexps with
    | [] -> eval_if_cond ctx exp_cond
    | iterexp_h :: iterexps_t -> (
        let iter_h, vars_h = iterexp_h in
        match iter_h with
        | Opt ->
            let cond, value_cond_opt =
              eval_if_cond_opt ctx exp_cond vars_h iterexps_t
            in
            let value_cond =
              Value.make
                (Il.IterT (Il.BoolT $ no_region, Il.Opt))
                (OptV value_cond_opt)
            in
            Hook.on_value value_cond;
            List.iter
              (fun (id, _typ, iters) ->
                let value_sub = Ctx.find_value ctx (id, iters @ [ Il.Opt ]) in
                Hook.on_value_dependency value_cond value_sub Dep.Edges.Iter)
              vars_h;
            (cond, value_cond)
        | List ->
            let cond, values_cond =
              eval_if_cond_list ctx exp_cond vars_h iterexps_t
            in
            let value_cond =
              Value.make
                (Il.IterT (Il.BoolT $ no_region, Il.List))
                (ListV values_cond)
            in
            Hook.on_value value_cond;
            List.iter
              (fun (id, _typ, iters) ->
                let value_sub = Ctx.find_value ctx (id, iters @ [ Il.List ]) in
                Hook.on_value_dependency value_cond value_sub Dep.Edges.Iter)
              vars_h;
            (cond, value_cond))

  and eval_if_cond_iter (ctx : Ctx.t) (exp_cond : exp) (iterexps : iterexp list)
      : bool * value =
    let iterexps = List.rev iterexps in
    eval_if_cond_iter' ctx exp_cond iterexps

  and eval_if_instr (ctx : Ctx.t) (exp_cond : exp) (iterexps : iterexp list)
      (block_then : block) (phantom_opt : phantom option) : Flow.t =
    (* Evaluate the if condition and mark phantom *)
    let cond, value_cond = eval_if_cond_iter ctx exp_cond iterexps in
    (match phantom_opt with
    | Some pid -> Hook.on_instr_dangling (not cond) pid value_cond
    | None -> ());
    (* Evaluate the then branch if the condition holds *)
    if cond then eval_block ctx block_then else Cont []

  (* Hold instruction evaluation *)

  and eval_hold_cond (ctx : Ctx.t) (id : id) (notexp : notexp) : bool * value =
    let _, exps_input = notexp in
    let values_input = eval_exps ctx exps_input in
    let hold =
      try
        let _ = invoke_rel ctx id values_input in
        true
      with
      | Backtrace (Unmatch _) -> false
      | Backtrace backtrace ->
          back_nest id.at "hold condition evaluation failed" backtrace
    in
    let value_res = Value.make Il.BoolT (BoolV hold) in
    Hook.on_value value_res;
    List.iteri
      (fun idx value_input ->
        Hook.on_value_dependency value_res value_input (Dep.Edges.Rel (id, idx)))
      values_input;
    (hold, value_res)

  and eval_hold_cond_opt (ctx : Ctx.t) (id : id) (notexp : notexp)
      (vars : var list) (iterexps : iterexp list) : bool * value option =
    let ctx_sub_opt = Ctx.sub_opt ctx vars in
    match ctx_sub_opt with
    | Some ctx_sub ->
        let cond, value_cond = eval_hold_cond_iter ctx_sub id notexp iterexps in
        (cond, Some value_cond)
    | None -> (false, None)

  and eval_hold_cond_list (ctx : Ctx.t) (id : id) (notexp : notexp)
      (vars : var list) (iterexps : iterexp list) : bool * value list =
    let ctxs_sub = Ctx.sub_list ctx vars in
    let cond, values_cond_rev =
      List.fold_left
        (fun (cond, values_cond_rev) ctx_sub ->
          if not cond then (cond, values_cond_rev)
          else
            let cond, value_cond =
              eval_hold_cond_iter' ctx_sub id notexp iterexps
            in
            let values_cond_rev = value_cond :: values_cond_rev in
            (cond, values_cond_rev))
        (true, []) ctxs_sub
    in
    let values_cond = List.rev values_cond_rev in
    (cond, values_cond)

  and eval_hold_cond_iter' (ctx : Ctx.t) (id : id) (notexp : notexp)
      (iterexps : iterexp list) : bool * value =
    match iterexps with
    | [] -> eval_hold_cond ctx id notexp
    | iterexp_h :: iterexps_t -> (
        let iter_h, vars_h = iterexp_h in
        match iter_h with
        | Opt ->
            let cond, value_cond_opt =
              eval_hold_cond_opt ctx id notexp vars_h iterexps_t
            in
            let value_cond =
              Value.make
                (Il.IterT (Il.BoolT $ no_region, Il.Opt))
                (OptV value_cond_opt)
            in
            Hook.on_value value_cond;
            List.iter
              (fun (id, _typ, iters) ->
                let value_sub = Ctx.find_value ctx (id, iters @ [ Il.Opt ]) in
                Hook.on_value_dependency value_cond value_sub Dep.Edges.Iter)
              vars_h;
            (cond, value_cond)
        | List ->
            let cond, values_cond =
              eval_hold_cond_list ctx id notexp vars_h iterexps_t
            in
            let value_cond =
              Value.make
                (Il.IterT (Il.BoolT $ no_region, Il.List))
                (ListV values_cond)
            in
            Hook.on_value value_cond;
            List.iter
              (fun (id, _typ, iters) ->
                let value_sub = Ctx.find_value ctx (id, iters @ [ Il.List ]) in
                Hook.on_value_dependency value_cond value_sub Dep.Edges.Iter)
              vars_h;
            (cond, value_cond))

  and eval_hold_cond_iter (ctx : Ctx.t) (id : id) (notexp : notexp)
      (iterexps : iterexp list) : bool * value =
    let iterexps = List.rev iterexps in
    eval_hold_cond_iter' ctx id notexp iterexps

  and eval_hold_instr (ctx : Ctx.t) (id : id) (notexp : notexp)
      (iterexps : iterexp list) (holdcase : holdcase) : Flow.t =
    (* Backup in case of failure *)
    Hook.backup ();
    (* Evaluate the hold condition *)
    let cond, value_cond = eval_hold_cond_iter ctx id notexp iterexps in
    (* Evaluate the hold case, and restore the coverage information
       if the expected behavior is the relation not holding *)
    match holdcase with
    | BothH (block_hold, block_not_hold) ->
        if cond then eval_block ctx block_hold
        else (
          Hook.restore ();
          eval_block ctx block_not_hold)
    | HoldH (block_hold, phantom_opt) ->
        (match phantom_opt with
        | Some pid -> Hook.on_instr_dangling (not cond) pid value_cond
        | None -> ());
        if cond then eval_block ctx block_hold else Cont []
    | NotHoldH (block_not_hold, phantom_opt) ->
        Hook.restore ();
        (match phantom_opt with
        | Some pid -> Hook.on_instr_dangling cond pid value_cond
        | None -> ());
        if not cond then eval_block ctx block_not_hold else Cont []

  (* Case analysis instruction evaluation *)

  and eval_cases (ctx : Ctx.t) (exp : exp) (cases : case list) :
      block option * value =
    let block_match, values_cond_rev =
      List.fold_left
        (fun (block_match, values_cond_rev) (guard, block) ->
          match block_match with
          | Some _ -> (block_match, values_cond_rev)
          | None ->
              let exp_cond =
                match guard with
                | BoolG true -> exp.it
                | BoolG false -> Il.UnE (`NotOp, `BoolT, exp)
                | CmpG (cmpop, optyp, exp_r) ->
                    Il.CmpE (cmpop, optyp, exp, exp_r)
                | SubG typ -> Il.SubE (exp, typ)
                | MatchG pattern -> Il.MatchE (exp, pattern)
                | MemG exp_s -> Il.MemE (exp, exp_s)
              in
              let exp_cond = exp_cond $$ (exp.at, Il.BoolT) in
              let value_cond = eval_exp ctx exp_cond in
              let values_cond_rev = value_cond :: values_cond_rev in
              let cond = Value.get_bool value_cond in
              if cond then (Some block, values_cond_rev)
              else (None, values_cond_rev))
        (None, []) cases
    in
    let values_cond = List.rev values_cond_rev in
    let value_cond =
      Value.make (Il.IterT (Il.BoolT $ no_region, Il.List)) (ListV values_cond)
    in
    Hook.on_value value_cond;
    (block_match, value_cond)

  and eval_case_instr (ctx : Ctx.t) (exp : exp) (cases : case list)
      (phantom_opt : phantom option) : Flow.t =
    (* Evaluate the matching case and mark phantom *)
    let block_opt, value_cond = eval_cases ctx exp cases in
    (match phantom_opt with
    | Some pid ->
        let matched = Option.is_some block_opt in
        Hook.on_instr_dangling (not matched) pid value_cond
    | None -> ());
    (* Evaluate the matching case if any *)
    match block_opt with Some block -> eval_block ctx block | None -> Cont []

  (* Group instruction evaluation *)

  and eval_group_instr (ctx : Ctx.t) (_id_group : id)
      (_rel_signature : rel_signature) (_exps_group : exp list)
      (block : instr list) : Flow.t =
    eval_block ctx block

  (* Let instruction evaluation *)

  and eval_let (ctx : Ctx.t) (exp_l : exp) (exp_r : exp) : Ctx.t =
    let value = eval_exp ctx exp_r in
    assign_exp ctx exp_l value

  and eval_let_opt (ctx : Ctx.t) (exp_l : exp) (exp_r : exp)
      (vars_bound : var list) (vars_bind : var list)
      (iterinstrs : iterinstr list) : Ctx.t =
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
                  let typ =
                    Typ.iterate typ_binding (iters_binding @ [ Il.Opt ])
                  in
                  Value.make typ.it (OptV None)
                in
                Hook.on_value value_binding;
                List.iter
                  (fun (id, _typ, iters) ->
                    let value_sub =
                      Ctx.find_value ctx (id, iters @ [ Il.Opt ])
                    in
                    Hook.on_value_dependency value_binding value_sub
                      Dep.Edges.Iter)
                  vars_bound;
                value_binding)
              vars_bind
          in
          (ctx, values_binding)
      (* Otherwise, evaluate the premise for the subcontext *)
      | Some ctx_sub ->
          let ctx_sub = eval_let_iter' ctx_sub exp_l exp_r iterinstrs in
          let values_binding =
            List.map
              (fun (id_binding, typ_binding, iters_binding) ->
                let value_binding =
                  Ctx.find_value ctx_sub (id_binding, iters_binding)
                in
                let value_binding =
                  let typ =
                    Typ.iterate typ_binding (iters_binding @ [ Il.Opt ])
                  in
                  Value.make typ.it (OptV (Some value_binding))
                in
                Hook.on_value value_binding;
                List.iter
                  (fun (id, _typ, iters) ->
                    let value_sub =
                      Ctx.find_value ctx (id, iters @ [ Il.Opt ])
                    in
                    Hook.on_value_dependency value_binding value_sub
                      Dep.Edges.Iter)
                  vars_bound;
                value_binding)
              vars_bind
          in
          (ctx, values_binding)
    in
    (* Finally, bind the resulting values *)
    List.fold_left2
      (fun ctx (id_binding, _typ_binding, iters_binding) value_binding ->
        Ctx.add_value ctx (id_binding, iters_binding @ [ Il.Opt ]) value_binding)
      ctx vars_bind values_binding

  and eval_let_list (ctx : Ctx.t) (exp_l : exp) (exp_r : exp)
      (vars_bound : var list) (vars_bind : var list)
      (iterinstrs : iterinstr list) : Ctx.t =
    (* Create a subcontext for each batch of bound values *)
    let ctxs_sub = Ctx.sub_list ctx vars_bound in
    let values_binding =
      match ctxs_sub with
      (* If the bound variable supposed to guide the iteration is already empty,
         then the binding variables are also empty *)
      | [] -> List.init (List.length vars_bind) (fun _ -> [])
      (* Otherwise, evaluate the premise for each batch of bound values,
         and collect the resulting binding batches *)
      | _ ->
          let values_binding_batch =
            List.map
              (fun ctx_sub ->
                let ctx_sub = eval_let_iter' ctx_sub exp_l exp_r iterinstrs in
                List.map
                  (fun (id_binding, _typ_binding, iters_binding) ->
                    Ctx.find_value ctx_sub (id_binding, iters_binding))
                  vars_bind)
              ctxs_sub
          in
          values_binding_batch |> Ctx.transpose
    in
    (* Finally, bind the resulting binding batches *)
    List.fold_left2
      (fun ctx (id_binding, typ_binding, iters_binding) values_binding ->
        let value_binding =
          let typ = Typ.iterate typ_binding (iters_binding @ [ Il.List ]) in
          Value.make typ.it (ListV values_binding)
        in
        Hook.on_value value_binding;
        List.iter
          (fun (id, _typ, iters) ->
            let value_sub = Ctx.find_value ctx (id, iters @ [ Il.List ]) in
            Hook.on_value_dependency value_binding value_sub Dep.Edges.Iter)
          vars_bound;
        Ctx.add_value ctx
          (id_binding, iters_binding @ [ Il.List ])
          value_binding)
      ctx vars_bind values_binding

  and eval_let_iter' (ctx : Ctx.t) (exp_l : exp) (exp_r : exp)
      (iterinstrs : iterinstr list) : Ctx.t =
    match iterinstrs with
    | [] -> eval_let ctx exp_l exp_r
    | iterinstr_h :: iterinstrs_t -> (
        let iter_h, vars_bound_h, vars_bind_h = iterinstr_h in
        match iter_h with
        | Opt ->
            eval_let_opt ctx exp_l exp_r vars_bound_h vars_bind_h iterinstrs_t
        | List ->
            eval_let_list ctx exp_l exp_r vars_bound_h vars_bind_h iterinstrs_t)

  and eval_let_iter (ctx : Ctx.t) (exp_l : exp) (exp_r : exp)
      (iterinstrs : iterinstr list) : Ctx.t =
    let iterinstrs = List.rev iterinstrs in
    eval_let_iter' ctx exp_l exp_r iterinstrs

  and eval_let_instr (ctx : Ctx.t) (exp_l : exp) (exp_r : exp)
      (iterinstrs : iterinstr list) (block : block) : Flow.t =
    try
      let ctx = eval_let_iter ctx exp_l exp_r iterinstrs in
      eval_block ctx block
    with Backtrace (Unmatch traces) -> Cont traces

  (* Rule instruction evaluation *)

  and eval_rule (ctx : Ctx.t) (id : id) (notexp : notexp)
      (inputs : Hints.Input.t) : Ctx.t =
    let _, exps = notexp in
    let exps_input, exps_output = Hints.Input.split inputs exps in
    let values_input = eval_exps ctx exps_input in
    let values_output = invoke_rel ctx id values_input in
    assign_exps ctx exps_output values_output

  and eval_rule_opt (ctx : Ctx.t) (id : id) (notexp : notexp)
      (inputs : Hints.Input.t) (vars_bound : var list) (vars_bind : var list)
      (iterinstrs : iterinstr list) : Ctx.t =
    (* Create a subcontext for the bound values *)
    let ctx_sub_opt = Ctx.sub_opt ctx vars_bound in
    let ctx, values_binding =
      match ctx_sub_opt with
      (* If the bound variable supposed to guide the iteration is already empty,
         then the binding variables are also empty *)
      | None ->
          let values_binding =
            List.map
              (fun (_id_binding, typ_binding, iters_binding) ->
                let typ =
                  Typ.iterate typ_binding (iters_binding @ [ Il.Opt ])
                in
                Value.make typ.it (OptV None))
              vars_bind
          in
          (ctx, values_binding)
      (* Otherwise, evaluate the rule for the subcontext *)
      | Some ctx_sub ->
          let ctx_sub = eval_rule_iter' ctx_sub id notexp inputs iterinstrs in
          let values_binding =
            List.map
              (fun (id_binding, typ_binding, iters_binding) ->
                let value_binding =
                  Ctx.find_value ctx_sub (id_binding, iters_binding)
                in
                let typ =
                  Typ.iterate typ_binding (iters_binding @ [ Il.Opt ])
                in
                Value.make typ.it (OptV (Some value_binding)))
              vars_bind
          in
          (ctx, values_binding)
    in
    List.fold_left2
      (fun ctx (id_binding, _typ_binding, iters_binding) value_binding ->
        Hook.on_value value_binding;
        List.iter
          (fun (id, _typ, iters) ->
            let value_sub = Ctx.find_value ctx (id, iters @ [ Il.Opt ]) in
            Hook.on_value_dependency value_binding value_sub Dep.Edges.Iter)
          vars_bound;
        Ctx.add_value ctx (id_binding, iters_binding @ [ Il.Opt ]) value_binding)
      ctx vars_bind values_binding

  and eval_rule_list (ctx : Ctx.t) (id : id) (notexp : notexp)
      (inputs : Hints.Input.t) (vars_bound : var list) (vars_bind : var list)
      (iterinstrs : iterinstr list) : Ctx.t =
    (* Create a subcontext for each batch of bound values *)
    let ctxs_sub = Ctx.sub_list ctx vars_bound in
    let values_binding =
      match ctxs_sub with
      (* If the bound variable supposed to guide the iteration is already empty,
         then the binding variables are also empty *)
      | [] -> List.init (List.length vars_bind) (fun _ -> [])
      (* Otherwise, evaluate the premise for each batch of bound values,
         and collect the resulting binding batches *)
      | _ ->
          let values_binding_batch =
            List.map
              (fun ctx_sub ->
                let ctx_sub =
                  eval_rule_iter' ctx_sub id notexp inputs iterinstrs
                in
                List.map
                  (fun (id_binding, _typ_binding, iters_binding) ->
                    Ctx.find_value ctx_sub (id_binding, iters_binding))
                  vars_bind)
              ctxs_sub
          in
          values_binding_batch |> Ctx.transpose
    in
    (* Finally, bind the resulting binding batches *)
    List.fold_left2
      (fun ctx (id_binding, typ_binding, iters_binding) values_binding ->
        let value_binding =
          let typ = Typ.iterate typ_binding (iters_binding @ [ Il.List ]) in
          Value.make typ.it (ListV values_binding)
        in
        Hook.on_value value_binding;
        List.iter
          (fun (id, _typ, iters) ->
            let value_sub = Ctx.find_value ctx (id, iters @ [ Il.List ]) in
            Hook.on_value_dependency value_binding value_sub Dep.Edges.Iter)
          vars_bound;
        Ctx.add_value ctx
          (id_binding, iters_binding @ [ Il.List ])
          value_binding)
      ctx vars_bind values_binding

  and eval_rule_iter' (ctx : Ctx.t) (id : id) (notexp : notexp)
      (inputs : Hints.Input.t) (iterinstrs : iterinstr list) : Ctx.t =
    match iterinstrs with
    | [] -> eval_rule ctx id notexp inputs
    | iterinstr_h :: iterinstrs_t -> (
        let iter_h, vars_bound_h, vars_bind_h = iterinstr_h in
        match iter_h with
        | Opt ->
            eval_rule_opt ctx id notexp inputs vars_bound_h vars_bind_h
              iterinstrs_t
        | List ->
            eval_rule_list ctx id notexp inputs vars_bound_h vars_bind_h
              iterinstrs_t)

  and eval_rule_iter (ctx : Ctx.t) (id : id) (notexp : notexp)
      (inputs : Hints.Input.t) (iterinstrs : iterinstr list) : Ctx.t =
    let iterinstrs = List.rev iterinstrs in
    eval_rule_iter' ctx id notexp inputs iterinstrs

  and eval_rule_instr (ctx : Ctx.t) (id : id) (notexp : notexp)
      (inputs : Hints.Input.t) (iterinstrs : iterinstr list) (block : block) :
      Flow.t =
    try
      let ctx = eval_rule_iter ctx id notexp inputs iterinstrs in
      eval_block ctx block
    with Backtrace (Unmatch traces) -> Cont traces

  (* Result instruction evaluation *)

  and eval_result_instr (ctx : Ctx.t) (_rel_signature : rel_signature)
      (exps : exp list) : Flow.t =
    try
      let values = eval_exps ctx exps in
      Flow.Res values
    with Backtrace (Unmatch traces) -> Flow.Cont traces

  (* Return instruction evaluation *)

  and eval_return_instr (ctx : Ctx.t) (exp : exp) : Flow.t =
    try
      let value = eval_exp ctx exp in
      Flow.Ret value
    with Backtrace (Unmatch traces) -> Flow.Cont traces

  (* Debug instruction evaluation *)

  and eval_debug_instr (ctx : Ctx.t) (exp : exp) : Flow.t =
    try
      let value = eval_exp ctx exp in
      print_endline
      @@ F.sprintf "%s: %s" (string_of_region exp.at)
           (Il.Print.string_of_exp exp);
      print_endline @@ Il.Print.string_of_value value;
      Flow.Cont []
    with Backtrace (Unmatch traces) -> Flow.Cont traces

  (* Invoke a relation *)

  and invoke_rel (ctx : Ctx.t) (id : id) (values_input : value list) :
      value list =
    try
      Hook.on_rel_enter id values_input;
      let values_output = invoke_rel' ctx id values_input in
      Hook.on_rel_exit id;
      values_output
    with Backtrace backtrace ->
      Hook.on_rel_exit id;
      back_nest id.at (F.asprintf "relation %s failed" id.it) backtrace

  and invoke_rel' (ctx : Ctx.t) (id : id) (values_input : value list) :
      value list =
    let rel = Ctx.find_rel ctx id in
    match rel with
    | Rel.Extern -> invoke_extern_rel id values_input
    | Rel.Defined (exps_input, block, elseblock_opt) ->
        invoke_defined_rel ctx id exps_input block elseblock_opt values_input

  and invoke_extern_rel (id : id) (values_input : value list) : value list =
    let invoke_extern_rel' (id : id) (values_input : value list) : value list =
      let values_output =
        match id.it with
        | "ExternFunctionCall_eval_lctk" ->
            Arch.eval_extern_func_lctk_call values_input
        | "ExternFunctionCall_eval" -> Arch.eval_extern_func_call values_input
        | "ExternMethodCall_eval" -> Arch.eval_extern_method_call values_input
        | _ ->
            back_err id.at (F.asprintf "unimplemented extern relation %s" id.it)
      in
      List.iteri
        (fun idx_arg value_input ->
          List.iter
            (fun value_output ->
              Hook.on_value_dependency value_output value_input
                (Dep.Edges.Rel (id, idx_arg)))
            values_output)
        values_input;
      values_output
    in
    try invoke_extern_rel' id values_input
    with Util.Error.ArchError (at, msg) -> back_unmatch at msg

  and invoke_defined_rel (ctx : Ctx.t) (id : id) (exps_input : exp list)
      (block : block) (elseblock_opt : elseblock option)
      (values_input : value list) : value list =
    let invoke_defined_rel' () =
      let ctx_local = Ctx.localize_rule ctx id values_input in
      let ctx_local = assign_exps ctx_local exps_input values_input in
      let flow = eval_block ctx_local block in
      let flow = eval_elseblock_opt ctx_local flow elseblock_opt in
      match flow with
      | Res values_output ->
          List.iteri
            (fun idx_arg value_input ->
              List.iter
                (fun value_output ->
                  Hook.on_value_dependency value_output value_input
                    (Dep.Edges.Rel (id, idx_arg)))
                values_output)
            values_input;
          values_output
      | Ret _ -> back_err id.at "relation cannot return a value"
      | Cont traces -> Unmatch traces |> back
    in
    if Hook.is_cache_on () && Cache.is_cached_rel id.it then (
      let cache_result = Cache.Cache.find !rel_cache (id.it, values_input) in
      match cache_result with
      | Some values_output -> values_output
      | None ->
          let builtin_ctr_before = !Builtin.Fresh.ctr in
          let values_output = invoke_defined_rel' () in
          let builtin_ctr_after = !Builtin.Fresh.ctr in
          (* Cache if the relation does not create a side-effect *)
          if builtin_ctr_before = builtin_ctr_after then
            Cache.Cache.add !rel_cache (id.it, values_input) values_output;
          values_output)
    else invoke_defined_rel' ()

  (* Invoke a function *)

  and is_high_order_func (values_input : value list) : bool =
    List.exists
      (fun value_input ->
        match value_input.it with Il.FuncV _ -> true | _ -> false)
      values_input

  and invoke_func (ctx : Ctx.t) (id : id) (targs : targ list) (args : arg list)
      : value =
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
                | Typdef.Defined ([], { it = Il.PlainT typ; _ }) ->
                    TIdMap.add tid typ theta
                | _ -> theta)
              tdenv_local TIdMap.empty
          in
          List.map (Typ.subst_typ theta) targs
    in
    let values_input = eval_args ctx args in
    invoke_func_with_values ctx id targs values_input

  and invoke_func_with_values (ctx : Ctx.t) (id : id) (targs : targ list)
      (values_input : value list) : value =
    try
      Hook.on_func_enter id values_input;
      let cursor, func = Ctx.find_func ctx id in
      let anon = cursor = Ctx.Local in
      let value_output =
        match func with
        | Func.Extern -> invoke_extern_func ~anon id targs values_input
        | Func.Builtin -> invoke_builtin_func ~anon id targs values_input
        | Func.Table (params, tablerows) ->
            invoke_table_func ~anon ctx id params tablerows values_input
        | Func.Defined (tparams, params, block, elseblock_opt) ->
            invoke_defined_func ~anon ctx id tparams params block elseblock_opt
              targs values_input
      in
      Hook.on_func_exit id;
      value_output
    with Backtrace backtrace ->
      Hook.on_func_exit id;
      back_nest id.at (F.asprintf "function %s failed" id.it) backtrace

  and invoke_extern_func ~(anon : bool) (id : id) (targs : targ list)
      (values_input : value list) : value =
    anon |> ignore;
    let invoke_extern_func' (id : id) (_targs : targ list)
        (values_input : value list) : value =
      let value_output =
        match id.it with
        | "init_objectState" -> Arch.eval_extern_init values_input
        | "init_archState" -> Arch.init_arch_state
        | _ ->
            back_err id.at (F.asprintf "unimplemented extern function %s" id.it)
      in
      List.iteri
        (fun idx_arg value_input ->
          Hook.on_value_dependency value_output value_input
            (Dep.Edges.Func (id, idx_arg)))
        values_input;
      value_output
    in
    try invoke_extern_func' id targs values_input
    with Util.Error.ArchError (at, msg) -> back_unmatch at msg

  and invoke_builtin_func ~(anon : bool) (id : id) (targs : targ list)
      (values_input : value list) : value =
    let invoke_builtin_func' () =
      let value_output =
        try
          Builtin.Call.invoke
            (fun value -> Hook.on_value value)
            id targs values_input
        with Util.Error.BuiltinError (at, msg) -> back_unmatch at msg
      in
      List.iteri
        (fun idx_arg value_input ->
          Hook.on_value_dependency value_output value_input
            (Dep.Edges.Func (id, idx_arg)))
        values_input;
      value_output
    in
    if
      Hook.is_cache_on () && Cache.is_cached_func id.it && (not anon)
      && not (is_high_order_func values_input)
    then (
      let cache_result = Cache.Cache.find !func_cache (id.it, values_input) in
      match cache_result with
      | Some value_output -> value_output
      | None ->
          let builtin_ctr_before = !Builtin.Fresh.ctr in
          let value_output = invoke_builtin_func' () in
          let builtin_ctr_after = !Builtin.Fresh.ctr in
          (* Cache if the builtin function does not create a side-effect *)
          if builtin_ctr_before = builtin_ctr_after then
            Cache.Cache.add !func_cache (id.it, values_input) value_output;
          value_output)
    else invoke_builtin_func' ()

  and invoke_table_func ~(anon : bool) (ctx : Ctx.t) (id : id)
      (params : param list) (tablerows : tablerow list)
      (values_input : value list) : value =
    let invoke_table_func' () =
      let ctx_local = Ctx.localize_func ctx id values_input TDEnv.empty in
      let ctx_local = assign_params ctx ctx_local params values_input in
      let instrs = List.concat_map (fun (_, _, instrs) -> instrs) tablerows in
      let flow = eval_block_sequential ctx_local instrs in
      match flow with
      | Ret value_output ->
          List.iteri
            (fun idx_arg value_input ->
              Hook.on_value_dependency value_output value_input
                (Dep.Edges.Func (id, idx_arg)))
            values_input;
          value_output
      | _ -> back_err id.at "table did not return a value"
    in
    if Hook.is_cache_on () && Cache.is_cached_func id.it && not anon then (
      let cache_result = Cache.Cache.find !func_cache (id.it, values_input) in
      match cache_result with
      | Some value_output -> value_output
      | None ->
          let builtin_ctr_before = !Builtin.Fresh.ctr in
          let value_output = invoke_table_func' () in
          let builtin_ctr_after = !Builtin.Fresh.ctr in
          (* Cache if the table function does not create a side-effect *)
          if builtin_ctr_before = builtin_ctr_after then
            Cache.Cache.add !func_cache (id.it, values_input) value_output;
          value_output)
    else invoke_table_func' ()

  and invoke_defined_func ~(anon : bool) (ctx : Ctx.t) (id : id)
      (tparams : tparam list) (params : param list) (block : block)
      (elseblock_opt : elseblock option) (targs : targ list)
      (values_input : value list) : value =
    let tdenv_local =
      check
        (List.length targs = List.length tparams)
        id.at "arity mismatch in type arguments";
      List.fold_left2
        (fun tdenv_local tparam targ ->
          let td = Typdef.Defined ([], Il.PlainT targ $ targ.at) in
          TDEnv.add tparam td tdenv_local)
        TDEnv.empty tparams targs
    in
    let ctx_local = Ctx.localize_func ctx id values_input tdenv_local in
    let invoke_defined_func' () =
      let ctx_local = assign_params ctx ctx_local params values_input in
      let flow = eval_block ctx_local block in
      let flow = eval_elseblock_opt ctx_local flow elseblock_opt in
      match flow with
      | Ret value_output ->
          List.iteri
            (fun idx_arg value_input ->
              Hook.on_value_dependency value_output value_input
                (Dep.Edges.Func (id, idx_arg)))
            values_input;
          value_output
      | Res _ -> back_err id.at "relation cannot return a value"
      | Cont traces -> Unmatch traces |> back
    in
    if
      Hook.is_cache_on () && Cache.is_cached_func id.it && (not anon)
      && not (is_high_order_func values_input)
    then (
      let cache_result = Cache.Cache.find !func_cache (id.it, values_input) in
      match cache_result with
      | Some value_output -> value_output
      | None ->
          let builtin_ctr_before = !Builtin.Fresh.ctr in
          let value_output = invoke_defined_func' () in
          let builtin_ctr_after = !Builtin.Fresh.ctr in
          (* Cache if the function does not create a side-effect *)
          if builtin_ctr_before = builtin_ctr_after then
            Cache.Cache.add !func_cache (id.it, values_input) value_output;
          value_output)
    else invoke_defined_func' ()

  (* Entry points for evaluation *)

  let clear () : unit =
    Value.refresh ();
    Cache.Cache.reset !func_cache;
    Cache.Cache.reset !rel_cache

  let do_eval_rel (relname : string) (values_input : value list) : value list =
    try
      let ctx = Ctx.empty () in
      let values_ouput = invoke_rel ctx (relname $ no_region) values_input in
      values_ouput
    with Backtrace backtrace ->
      let failtraces = back_failtraces backtrace in
      let msg = Util.Attempt.string_of_failtraces_short failtraces in
      error no_region msg

  let do_eval_func (funcname : string) (targs : targ list)
      (values_input : value list) : value =
    try
      let ctx = Ctx.empty () in
      let value_output =
        invoke_func_with_values ctx (funcname $ no_region) targs values_input
      in
      value_output
    with Backtrace backtrace ->
      let failtraces = back_failtraces backtrace in
      let msg = Util.Attempt.string_of_failtraces_short failtraces in
      error no_region msg

  let eval_program (relname : string) (includes_p4 : string list)
      (filename_p4 : string) : Sim.program_result =
    clear ();
    try
      let value_program = Interface.Parse.parse_file includes_p4 filename_p4 in
      Hook.on_program value_program;
      let values_output = do_eval_rel relname [ value_program ] in
      Sim.Pass values_output
    with
    | Util.Error.ParseError (at, msg) -> Sim.Fail (`Syntax (at, msg))
    | Util.Error.InterpError (at, msg) | Util.Error.ArchError (at, msg) ->
        Sim.Fail (`Runtime (at, msg))

  let eval_rel (relname : string) (values_input : value list) : Sim.rel_result =
    clear ();
    try
      let values_output = do_eval_rel relname values_input in
      Sim.Pass values_output
    with Util.Error.InterpError (at, msg) | Util.Error.ArchError (at, msg) ->
      Sim.Fail (at, msg)

  let eval_func (funcname : string) (targs : targ list)
      (values_input : value list) : Sim.func_result =
    clear ();
    try
      let value_output = do_eval_func funcname targs values_input in
      Sim.Pass value_output
    with Util.Error.InterpError (at, msg) | Util.Error.ArchError (at, msg) ->
      Sim.Fail (at, msg)

  (* Initialization *)

  let init ~(cache : bool) ~(det : bool) (spec : spec) : unit =
    if cache then Hook.cache_on () else Hook.cache_off ();
    let printer value =
      let henv = Interface.Hint.hints_of_spec_sl spec in
      Format.asprintf "%a" (Interface.Unparse.pp_value henv) value
    in
    Builtin.Call.init printer;
    Ctx.init ~det spec
end
