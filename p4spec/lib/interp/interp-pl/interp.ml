open Domain
module Mixfix = Domain.Mixfix
open Lib
open Lang
open Xl
open Pl
module Type = Runtime.Type
module Typ = Type.Typ
module CCache = Runtime.Dynamic.Caches.CallCache
open Runtime.Dynamic_Pl
open Envs
module Run = Runtime.Dynamic_Runner.Signature
module Dep = Runtime.Testgen_neg.Dep
module Hook = Inst.Hook
open Interp_common.Error
open Interp_common.Backtrace
open Interp_common.Nondet
module Flow = Interp_common.Flow
module F = Format
open Util.Source

module Make (Interface : Run.INTERFACE) (Extern : Run.EXTERN) () :
  Run.INTERP_PL = struct
  (* Build context *)

  module Ctx = Ctx.Make ()

  (* Tier-handler signatures *)

  type 'instr_tier eval_instr_tier = Ctx.t -> 'instr_tier -> Ctx.t * Flow.t

  type 'instr_tier string_of_instr_tier =
    ?short:bool -> ?level:int -> ?index:int -> 'instr_tier -> string

  (* The two threaded handlers, then whatever body the block walker consumes *)

  type ('instr_tier, 'body) eval_block_fn =
    'instr_tier eval_instr_tier ->
    'instr_tier string_of_instr_tier ->
    Ctx.t ->
    'body ->
    Ctx.t * Flow.t

  (* Build caches *)

  let func_cache = ref (CCache.create ~size:(256 * 1024))
  let rel_cache = ref (CCache.create ~size:(256 * 1024))
  let sub_cache = Hashtbl.create 4096

  (* Cache toggle *)

  let cache_enabled = ref false

  module Cache = struct
    let cache_on () =
      cache_enabled := true;
      Extern.Cache.cache_on ()

    let cache_off () =
      cache_enabled := false;
      Extern.Cache.cache_off ()
  end

  (* Checkers *)

  let check_rel_inputs (ctx : Ctx.t) (id_rel : id) (values_input : value list) :
      unit =
    let nottyp, inputs = Ctx.find_rel_signature ctx id_rel in
    let typs = Mixfix.args nottyp.it in
    let typs = List.map (fun i -> List.nth typs i) inputs in
    check
      (Value.Match.subs (Ctx.find_typdef_opt ctx)
         (Ctx.find_func_signature ctx)
         typs values_input)
      id_rel.at
      (F.sprintf "relation input of %s does not match the expected type"
         id_rel.it)

  let check_rel_outputs (ctx : Ctx.t) (id_rel : id) (nottyp : nottyp)
      (inputs : Hints.Input.t) (values_output : value list) : unit =
    let typs = Mixfix.args nottyp.it in
    let typs =
      typs
      |> List.mapi (fun idx typ ->
             if List.mem idx inputs then None else Some typ)
      |> List.filter_map Fun.id
    in
    check
      (Value.Match.subs (Ctx.find_typdef_opt ctx)
         (Ctx.find_func_signature ctx)
         typs values_output)
      id_rel.at
      (F.sprintf "relation output of %s does not match the expected type"
         id_rel.it)

  let check_func_inputs (ctx : Ctx.t) (id_func : id) (targs : targ list)
      (values_input : value list) : unit =
    let tparams, typs_params, _ = Ctx.find_func_signature ctx id_func in
    check
      (List.length targs = List.length tparams)
      id_func.at
      (F.sprintf "arity mismatch in type arguments of %s" id_func.it);
    let tdenv_local =
      List.fold_left2
        (fun tdenv_local tparam targ ->
          let td = Type.Typdef.Defined ([], Il.PlainT targ $ targ.at) in
          TDEnv.add tparam td tdenv_local)
        TDEnv.empty tparams targs
    in
    let ctx_local = Ctx.localize_func ctx id_func values_input tdenv_local in
    check
      (Value.Match.subs
         (Ctx.find_typdef_opt ctx_local)
         (Ctx.find_func_signature ctx_local)
         typs_params values_input)
      id_func.at
      (F.sprintf "function argument of %s does not match the parameter type"
         id_func.it)

  let check_func_output (ctx : Ctx.t) (id_func : id) (tparams : tparam list)
      (typ_output : typ) (targs : targ list) (value_output : value) : unit =
    let theta = TIdMap.of_lists tparams targs in
    let typ_output = Type.Subst.subst_typ theta typ_output in
    check
      (Value.Match.sub sub_cache (Ctx.find_typdef_opt ctx)
         (Ctx.find_func_signature ctx)
         typ_output value_output)
      id_func.at
      (F.sprintf "return value of function %s does not match the expected type"
         id_func.it)

  (* Assignments *)

  (* Assigning a value to an expression *)

  let rec assign_exp (ctx : Ctx.t) (exp : exp) (value : value) : Ctx.t =
    let typ_value = value.note.typ $ exp.node.at in
    match (exp.node.it, value.it) with
    | VarE id, _ -> Ctx.add_value ctx (id, []) value
    | TupleE exps_inner, TupleV values_inner ->
        let ctx = assign_exps ctx exps_inner values_inner in
        List.iter
          (fun value_inner ->
            Hook.on_value_dependency value_inner value Dep.Edges.Assign)
          values_inner;
        ctx
    | CaseE notexp, CaseV valuecase ->
        let exps_inner = Mixfix.args notexp in
        let values_inner = Mixfix.args valuecase in
        let ctx = assign_exps ctx exps_inner values_inner in
        List.iter
          (fun value_inner ->
            Hook.on_value_dependency value_inner value Dep.Edges.Assign)
          values_inner;
        ctx
    | StrE expfields, StructV valuefields ->
        let exps_inner = List.map snd expfields in
        let values_inner = List.map snd valuefields in
        let ctx = assign_exps ctx exps_inner values_inner in
        List.iter
          (fun value_inner ->
            Hook.on_value_dependency value_inner value Dep.Edges.Assign)
          values_inner;
        ctx
    | OptE (Some exp_inner), OptV (Some value_inner) ->
        let ctx = assign_exp ctx exp_inner value_inner in
        Hook.on_value_dependency value_inner value Dep.Edges.Assign;
        ctx
    | OptE None, OptV None -> ctx
    | ListE exps_inner, ListV values_inner ->
        let ctx = assign_exps ctx exps_inner values_inner in
        List.iter
          (fun value_inner ->
            Hook.on_value_dependency value_inner value Dep.Edges.Assign)
          values_inner;
        ctx
    | ConsE (exp_h, exp_t), ListV (value_h :: values_inner) ->
        let value_t = Value.Make.list typ_value values_inner in
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
            let typ = Typ.Make.iterate typ (iters @ [ Il.Opt ]) in
            let value_sub = Value.Make.opt typ None in
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
            let typ = Typ.Make.iterate typ (iters @ [ Il.Opt ]) in
            let value = Ctx.find_value ctx (id, iters) in
            let value_sub = Value.Make.opt typ (Some value) in
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
            let typ = Typ.Make.iterate typ (iters @ [ Il.List ]) in
            let values =
              List.map (fun ctx -> Ctx.find_value ctx (id, iters)) ctxs
            in
            let value_sub = Value.Make.list typ values in
            Hook.on_value value_sub;
            Hook.on_value_dependency value_sub value Dep.Edges.Assign;
            Ctx.add_value ctx (id, iters @ [ Il.List ]) value_sub)
          ctx vars
    | _ ->
        back_err exp.node.at
          (F.asprintf "match failed %s <- %s"
             (Pl.Print.string_of_exp exp)
             (Sl.Print.string_of_value ~short:true value))

  and assign_exps (ctx : Ctx.t) (exps : exp list) (values : value list) : Ctx.t
      =
    if List.length exps <> List.length values then
      back_err
        (over_region (List.map (fun e -> e.Annot.node.at) exps))
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
    | DefP (id, _, _, _) -> assign_param_def ctx_caller ctx_callee id value

  and assign_params (ctx_caller : Ctx.t) (ctx_callee : Ctx.t)
      (params : param list) (values : value list) : Ctx.t =
    if List.length params <> List.length values then
      back_err
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
      back_nest exp.node.at
        (fun () -> F.asprintf "%s failed" (Pl.Print.string_of_exp exp))
        backtrace

  and eval_exp' (ctx : Ctx.t) (exp : exp) : value =
    let typ_note = exp.node.note $ exp.node.at in
    match exp.node.it with
    | BoolE b -> eval_bool_exp typ_note ctx b
    | NumE n -> eval_num_exp typ_note ctx n
    | TextE s -> eval_text_exp typ_note ctx s
    | VarE id -> eval_var_exp typ_note ctx id
    | UnE (unop, optyp, exp) -> eval_un_exp typ_note ctx unop optyp exp
    | BinE (binop, optyp, exp_l, exp_r) ->
        eval_bin_exp typ_note ctx binop optyp exp_l exp_r
    | CmpE (cmpop, optyp, exp_l, exp_r) ->
        eval_cmp_exp typ_note ctx cmpop optyp exp_l exp_r
    | UpCastE (typ, exp) -> eval_upcast_exp typ_note ctx typ exp
    | DownCastE (typ, exp) -> eval_downcast_exp typ_note ctx typ exp
    | SubE (exp, typ, subcheck) -> eval_sub_exp typ_note ctx exp typ subcheck
    | MatchE (exp, pattern) -> eval_match_exp typ_note ctx exp pattern
    | TupleE exps -> eval_tuple_exp typ_note ctx exps
    | CaseE typ_notexp -> eval_case_exp typ_note ctx typ_notexp
    | StrE fields -> eval_str_exp typ_note ctx fields
    | OptE exp_opt -> eval_opt_exp typ_note ctx exp_opt
    | ListE exps -> eval_list_exp typ_note ctx exps
    | ConsE (exp_h, exp_t) -> eval_cons_exp typ_note ctx exp_h exp_t
    | CatE (exp_l, exp_r) -> eval_cat_exp typ_note ctx exp_l exp_r
    | MemE (exp_e, exp_s) -> eval_mem_exp typ_note ctx exp_e exp_s
    | LenE exp -> eval_len_exp typ_note ctx exp
    | DotE (exp_b, atom) -> eval_dot_exp typ_note ctx exp_b atom
    | IdxE (exp_b, exp_i) -> eval_idx_exp typ_note ctx exp_b exp_i
    | SliceE (exp_b, exp_l, exp_h) ->
        eval_slice_exp typ_note ctx exp_b exp_l exp_h
    | UpdE (exp_b, path, exp_f) -> eval_upd_exp typ_note ctx exp_b path exp_f
    | CallE (id, targs, args) -> eval_call_exp typ_note ctx id targs args
    | IterE (exp, iterexp) -> eval_iter_exp typ_note ctx exp iterexp

  and eval_exps (ctx : Ctx.t) (exps : exp list) : value list =
    List.map (eval_exp ctx) exps

  (* Boolean expression evaluation *)

  and eval_bool_exp (_typ_note : typ) (ctx : Ctx.t) (b : bool) : value =
    let value_res = Value.Make.bool b in
    Hook.on_value value_res;
    if Hook.is_active () then
      List.iter
        (fun value_input ->
          Hook.on_value_dependency value_res value_input Dep.Edges.Control)
        (Ctx.find_values_input ctx);
    value_res

  (* Numeric expression evaluation *)

  and eval_num_exp (_typ_note : typ) (ctx : Ctx.t) (n : Num.t) : value =
    let value_res = Value.Make.num n in
    Hook.on_value value_res;
    if Hook.is_active () then
      List.iter
        (fun value_input ->
          Hook.on_value_dependency value_res value_input Dep.Edges.Control)
        (Ctx.find_values_input ctx);
    value_res

  (* Text expression evaluation *)

  and eval_text_exp (_typ_note : typ) (ctx : Ctx.t) (s : string) : value =
    let value_res = Value.Make.text s in
    Hook.on_value value_res;
    if Hook.is_active () then
      List.iter
        (fun value_input ->
          Hook.on_value_dependency value_res value_input Dep.Edges.Control)
        (Ctx.find_values_input ctx);
    value_res

  (* Variable expression evaluation *)

  and eval_var_exp (_typ_note : typ) (ctx : Ctx.t) (id : id) : value =
    Ctx.find_value ctx (id, [])

  (* Unary expression evaluation *)

  and eval_un_bool (unop : Bool.unop) (value : value) : value =
    match unop with
    | `NotOp -> value |> Value.Get.bool |> not |> Value.Make.bool

  and eval_un_num (unop : Num.unop) (value : value) : value =
    value |> Value.Get.num |> Num.un unop |> Value.Make.num

  and eval_un_exp (_typ_note : typ) (ctx : Ctx.t) (unop : unop) (_optyp : optyp)
      (exp : exp) : value =
    let value = eval_exp ctx exp in
    let value_res =
      match unop with
      | #Bool.unop as unop -> eval_un_bool unop value
      | #Num.unop as unop -> eval_un_num unop value
    in
    Hook.on_value value_res;
    Hook.on_value_dependency value_res value (Dep.Edges.Op (UnOp unop));
    value_res

  (* Binary expression evaluation *)

  and eval_bin_bool (binop : Bool.binop) (value_l : value) (value_r : value) :
      value =
    let b_l = Value.Get.bool value_l in
    let b_r = Value.Get.bool value_r in
    match binop with
    | `AndOp -> Value.Make.bool (b_l && b_r)
    | `OrOp -> Value.Make.bool (b_l || b_r)
    | `ImplOp -> Value.Make.bool ((not b_l) || b_r)
    | `EquivOp -> Value.Make.bool (b_l = b_r)

  and eval_bin_num (binop : Num.binop) (value_l : value) (value_r : value) :
      value =
    let num_l = Value.Get.num value_l in
    let num_r = Value.Get.num value_r in
    Value.Make.num (Num.bin binop num_l num_r)

  and eval_bin_exp (_typ_note : typ) (ctx : Ctx.t) (binop : binop)
      (_optyp : optyp) (exp_l : exp) (exp_r : exp) : value =
    let value_l = eval_exp ctx exp_l in
    let value_r = eval_exp ctx exp_r in
    let value_res =
      match binop with
      | #Bool.binop as binop -> eval_bin_bool binop value_l value_r
      | #Num.binop as binop -> eval_bin_num binop value_l value_r
    in
    Hook.on_value value_res;
    Hook.on_value_dependency value_res value_l (Dep.Edges.Op (BinOp binop));
    Hook.on_value_dependency value_res value_r (Dep.Edges.Op (BinOp binop));
    value_res

  (* Comparison expression evaluation *)

  and eval_cmp_bool (cmpop : Bool.cmpop) (value_l : value) (value_r : value) :
      value =
    let eq = Value.eq value_l value_r in
    match cmpop with
    | `EqOp -> Value.Make.bool eq
    | `NeOp -> Value.Make.bool (not eq)

  and eval_cmp_num (cmpop : Num.cmpop) (value_l : value) (value_r : value) :
      value =
    let num_l = Value.Get.num value_l in
    let num_r = Value.Get.num value_r in
    Value.Make.bool (Num.cmp cmpop num_l num_r)

  and eval_cmp_exp (_typ_note : typ) (ctx : Ctx.t) (cmpop : cmpop)
      (_optyp : optyp) (exp_l : exp) (exp_r : exp) : value =
    let value_l = eval_exp ctx exp_l in
    let value_r = eval_exp ctx exp_r in
    let value_res =
      match cmpop with
      | #Bool.cmpop as cmpop -> eval_cmp_bool cmpop value_l value_r
      | #Num.cmpop as cmpop -> eval_cmp_num cmpop value_l value_r
    in
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
            let value_res = Value.Make.int n in
            Hook.on_value value_res;
            Hook.on_value_dependency value_res value (Dep.Edges.Op (CastOp typ));
            value_res
        | NumV (`Int _) -> value
        | _ -> back_err_upcast ())
    | VarT (tid, targs) -> (
        let tparams, deftyp = Ctx.find_defined_typdef ctx tid in
        match deftyp.it with
        | PlainT typ ->
            let theta = TIdMap.of_lists tparams targs in
            let typ = Type.Subst.subst_typ theta typ in
            upcast ctx typ value
        | _ -> value)
    | TupleT typs -> (
        match value.it with
        | TupleV values ->
            let values = List.map2 (upcast ctx) typs values in
            let value_res = Value.Make.tuple typ values in
            Hook.on_value value_res;
            Hook.on_value_dependency value_res value (Dep.Edges.Op (CastOp typ));
            value_res
        | _ -> back_err_upcast ())
    | _ -> value

  and eval_upcast_exp (_typ_note : typ) (ctx : Ctx.t) (typ : typ) (exp : exp) :
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
            let value_res = Value.Make.nat i in
            Hook.on_value value_res;
            Hook.on_value_dependency value_res value (Dep.Edges.Op (CastOp typ));
            value_res
        | _ -> back_err_downcast ())
    | VarT (tid, targs) -> (
        let tparams, deftyp = Ctx.find_defined_typdef ctx tid in
        match deftyp.it with
        | PlainT typ ->
            let theta = TIdMap.of_lists tparams targs in
            let typ = Type.Subst.subst_typ theta typ in
            downcast ctx typ value
        | _ -> value)
    | TupleT typs -> (
        match value.it with
        | TupleV values ->
            let values = List.map2 (downcast ctx) typs values in
            let value_res = Value.Make.tuple typ values in
            Hook.on_value value_res;
            Hook.on_value_dependency value_res value (Dep.Edges.Op (CastOp typ));
            value_res
        | _ -> back_err_downcast ())
    | _ -> value

  and eval_downcast_exp (_typ_note : typ) (ctx : Ctx.t) (typ : typ) (exp : exp)
      : value =
    let value = eval_exp ctx exp in
    downcast ctx typ value

  (* Subtype check expression evaluation *)

  and eval_sub_exp (_typ_note : typ) (ctx : Ctx.t) (exp : exp) (typ : typ)
      (subcheck : subcheck) : value =
    let value = eval_exp ctx exp in
    let sub =
      Value.Match.check sub_cache (Ctx.find_typdef_opt ctx)
        (Ctx.find_func_signature ctx)
        subcheck value
    in
    let value_res = Value.Make.bool sub in
    Hook.on_value value_res;
    Hook.on_value_dependency value_res value (Dep.Edges.Op (SubOp typ));
    value_res

  (* Pattern match check expression evaluation *)

  and eval_match_exp (_typ_note : typ) (ctx : Ctx.t) (exp : exp)
      (pattern : pattern) : value =
    let value = eval_exp ctx exp in
    let matches =
      match (pattern, value.it) with
      | CaseP mixop_p, CaseV valuecase -> Mixfix.eq_mixop mixop_p valuecase
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
    let value_res = Value.Make.bool matches in
    Hook.on_value value_res;
    Hook.on_value_dependency value_res value (Dep.Edges.Op (MatchOp pattern));
    value_res

  (* Tuple expression evaluation *)

  and eval_tuple_exp (typ_note : typ) (ctx : Ctx.t) (exps : exp list) : value =
    let values = eval_exps ctx exps in
    let value_res = Value.Make.tuple typ_note values in
    Hook.on_value value_res;
    if List.length values = 0 then
      List.iter
        (fun value_input ->
          Hook.on_value_dependency value_res value_input Dep.Edges.Control)
        (Ctx.find_values_input ctx);
    value_res

  (* Case expression evaluation *)

  and eval_case_exp (typ_note : typ) (ctx : Ctx.t) (notexp : notexp) : value =
    let mixop, exps = Mixfix.split notexp in
    let values = eval_exps ctx exps in
    let value_res = Value.Make.case typ_note (Mixfix.fill mixop values) in
    Hook.on_value value_res;
    if List.length values = 0 then
      List.iter
        (fun value_input ->
          Hook.on_value_dependency value_res value_input Dep.Edges.Control)
        (Ctx.find_values_input ctx);
    value_res

  (* Struct expression evaluation *)

  and eval_str_exp (typ_note : typ) (ctx : Ctx.t) (fields : (atom * exp) list) :
      value =
    let atoms, exps = List.split fields in
    let values = eval_exps ctx exps in
    let valuefields = List.combine atoms values in
    let value_res = Value.Make.str typ_note valuefields in
    Hook.on_value value_res;
    if List.length values = 0 then
      List.iter
        (fun value_input ->
          Hook.on_value_dependency value_res value_input Dep.Edges.Control)
        (Ctx.find_values_input ctx);
    value_res

  (* Option expression evaluation *)

  and eval_opt_exp (typ_note : typ) (ctx : Ctx.t) (exp_opt : exp option) : value
      =
    let value_opt = Option.map (eval_exp ctx) exp_opt in
    let value_res = Value.Make.opt typ_note value_opt in
    Hook.on_value value_res;
    if Option.is_none value_opt then
      List.iter
        (fun value_input ->
          Hook.on_value_dependency value_res value_input Dep.Edges.Control)
        (Ctx.find_values_input ctx);
    value_res

  (* List expression evaluation *)

  and eval_list_exp (typ_note : typ) (ctx : Ctx.t) (exps : exp list) : value =
    let values = eval_exps ctx exps in
    let value_res = Value.Make.list typ_note values in
    Hook.on_value value_res;
    if List.length values = 0 then
      List.iter
        (fun value_input ->
          Hook.on_value_dependency value_res value_input Dep.Edges.Control)
        (Ctx.find_values_input ctx);
    value_res

  (* Cons expression evaluation *)

  and eval_cons_exp (typ_note : typ) (ctx : Ctx.t) (exp_h : exp) (exp_t : exp) :
      value =
    let value_h = eval_exp ctx exp_h in
    let value_t = eval_exp ctx exp_t in
    let values_t = Value.Get.list value_t in
    let value_res = Value.Make.list typ_note (value_h :: values_t) in
    Hook.on_value value_res;
    value_res

  (* Concatenation expression evaluation *)

  and eval_cat_exp (typ_note : typ) (ctx : Ctx.t) (exp_l : exp) (exp_r : exp) :
      value =
    let value_l = eval_exp ctx exp_l in
    let value_r = eval_exp ctx exp_r in
    let value_res =
      match (value_l.it, value_r.it) with
      | TextV s_l, TextV s_r -> Value.Make.text (s_l ^ s_r)
      | ListV values_l, ListV values_r ->
          Value.Make.list typ_note (values_l @ values_r)
      | _ ->
          back_err
            (over_region [ exp_l.node.at; exp_r.node.at ])
            (F.asprintf
               "concatenation expects either two texts or two lists, but got \
                %s and %s"
               (Sl.Print.string_of_value ~short:true value_l)
               (Sl.Print.string_of_value ~short:true value_r))
    in
    Hook.on_value value_res;
    Hook.on_value_dependency value_res value_l (Dep.Edges.Op CatOp);
    Hook.on_value_dependency value_res value_r (Dep.Edges.Op CatOp);
    value_res

  (* Membership expression evaluation *)

  and eval_mem_exp (_typ_note : typ) (ctx : Ctx.t) (exp_e : exp) (exp_s : exp) :
      value =
    let value_e = eval_exp ctx exp_e in
    let value_s = eval_exp ctx exp_s in
    let values_s = Value.Get.list value_s in
    let value_res = Value.Make.bool (List.exists (Value.eq value_e) values_s) in
    Hook.on_value value_res;
    Hook.on_value_dependency value_res value_e (Dep.Edges.Op MemOp);
    Hook.on_value_dependency value_res value_s (Dep.Edges.Op MemOp);
    value_res

  (* Length expression evaluation *)

  and eval_len_exp (_typ_note : typ) (ctx : Ctx.t) (exp : exp) : value =
    let value = eval_exp ctx exp in
    let len =
      match value.it with
      | TextV s -> s |> String.length |> Bigint.of_int
      | ListV values -> values |> List.length |> Bigint.of_int
      | _ ->
          back_err exp.node.at
            (F.asprintf
               "length operation expects either a text or a list, but got %s"
               (Sl.Print.string_of_value ~short:true value))
    in
    let value_res = Value.Make.nat len in
    Hook.on_value value_res;
    Hook.on_value_dependency value_res value (Dep.Edges.Op LenOp);
    value_res

  (* Dot expression evaluation *)

  and eval_dot_exp (_typ_note : typ) (ctx : Ctx.t) (exp_b : exp) (atom : atom) :
      value =
    let value_b = eval_exp ctx exp_b in
    let valuefields = Value.Get.str value_b in
    let value_res =
      valuefields
      |> List.find (fun (atom_field, _) -> Atom.eq atom_field.it atom.it)
      |> snd
    in
    value_res

  (* Index expression evaluation *)

  and eval_idx_exp (_typ_note : typ) (ctx : Ctx.t) (exp_b : exp) (exp_i : exp) :
      value =
    let value_b = eval_exp ctx exp_b in
    let value_i = eval_exp ctx exp_i in
    let idx = value_i |> Value.Get.num |> Num.to_int |> Bigint.to_int_exn in
    match value_b.it with
    | TextV s when idx < 0 || idx >= String.length s ->
        back_err exp_i.node.at
          (F.asprintf "index %d out of bounds [0, %d)" idx (String.length s))
    | TextV s ->
        let s = String.get s idx |> String.make 1 in
        let value_res = Value.Make.text s in
        Hook.on_value value_res;
        value_res
    | ListV values when idx < 0 || idx >= List.length values ->
        back_err exp_i.node.at
          (F.asprintf "index %d out of bounds [0, %d)" idx (List.length values))
    | ListV values -> List.nth values idx
    | _ ->
        back_err exp_b.node.at
          (F.asprintf "indexing expects either a text or a list, but got %s"
             (Sl.Print.string_of_value ~short:true value_b))

  (* Sl.ce expression evaluation *)

  and eval_slice_exp (typ_note : typ) (ctx : Ctx.t) (exp_b : exp) (exp_i : exp)
      (exp_n : exp) : value =
    let value_b = eval_exp ctx exp_b in
    let value_i = eval_exp ctx exp_i in
    let idx_l = value_i |> Value.Get.num |> Num.to_int |> Bigint.to_int_exn in
    let value_n = eval_exp ctx exp_n in
    let idx_n = value_n |> Value.Get.num |> Num.to_int |> Bigint.to_int_exn in
    let idx_h = idx_l + idx_n in
    match value_b.it with
    | TextV s when idx_l < 0 || idx_h > String.length s ->
        back_err exp_n.node.at
          (F.asprintf "slice [%d, %d) out of bounds [0, %d)" idx_l idx_h
             (String.length s))
    | TextV s ->
        let s_slice = String.sub s idx_l (idx_h - idx_l) in
        let value_res = Value.Make.text s_slice in
        Hook.on_value value_res;
        value_res
    | ListV values when idx_l < 0 || idx_h > List.length values ->
        back_err exp_n.node.at
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
        let value_res = Value.Make.list typ_note values_slice in
        Hook.on_value value_res;
        value_res
    | _ ->
        back_err exp_b.node.at
          (F.asprintf "slicing expects either a text or a list, but got %s"
             (Sl.Print.string_of_value ~short:true value_b))

  (* Update expression evaluation *)

  and eval_access_path (ctx : Ctx.t) (value_b : value) (path : path) : value =
    match path.it with
    | RootP -> value_b
    | IdxP (path, exp_i) -> (
        let value = eval_access_path ctx value_b path in
        let value_i = eval_exp ctx exp_i in
        let idx = value_i |> Value.Get.num |> Num.to_int |> Bigint.to_int_exn in
        match value.it with
        | TextV s when idx < 0 || idx >= String.length s ->
            back_err exp_i.node.at
              (F.asprintf "index %d out of bounds [0, %d)" idx (String.length s))
        | TextV s ->
            let s = String.get s idx |> String.make 1 in
            let value_res = Value.Make.text s in
            Hook.on_value value_res;
            value_res
        | ListV values when idx < 0 || idx >= List.length values ->
            back_err exp_i.node.at
              (F.asprintf "index %d out of bounds [0, %d)" idx
                 (List.length values))
        | ListV values -> List.nth values idx
        | _ ->
            back_err path.at
              (F.asprintf "indexing expects either a text or a list, but got %s"
                 (Sl.Print.string_of_value ~short:true value)))
    | SliceP (path, exp_i, exp_n) -> (
        let typ = path.note $ path.at in
        let value = eval_access_path ctx value_b path in
        let value_i = eval_exp ctx exp_i in
        let idx_l =
          value_i |> Value.Get.num |> Num.to_int |> Bigint.to_int_exn
        in
        let value_n = eval_exp ctx exp_n in
        let idx_n =
          value_n |> Value.Get.num |> Num.to_int |> Bigint.to_int_exn
        in
        let idx_h = idx_l + idx_n in
        match value.it with
        | TextV s when idx_l < 0 || idx_h > String.length s ->
            back_err exp_n.node.at
              (F.asprintf "slice [%d, %d) out of bounds [0, %d)" idx_l idx_h
                 (String.length s))
        | TextV s ->
            let s_slice = String.sub s idx_l (idx_h - idx_l) in
            let value_res = Value.Make.text s_slice in
            Hook.on_value value_res;
            value_res
        | ListV values when idx_l < 0 || idx_h > List.length values ->
            back_err exp_n.node.at
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
            let value_res = Value.Make.list typ values_slice in
            Hook.on_value value_res;
            value_res
        | _ ->
            back_err path.at
              (F.asprintf "slicing expects either a text or a list, but got %s"
                 (Sl.Print.string_of_value ~short:true value)))
    | DotP (path, atom) ->
        let value = eval_access_path ctx value_b path in
        let fields = value |> Value.Get.str in
        fields
        |> List.find (fun (atom_field, _) -> Atom.eq atom_field.it atom.it)
        |> snd

  and eval_update_path (ctx : Ctx.t) (value_b : value) (path : path)
      (value_upd : value) : value =
    match path.it with
    | RootP -> value_upd
    | IdxP (path, exp_i) -> (
        let typ = path.note $ path.at in
        let value = eval_access_path ctx value_b path in
        let value_i = eval_exp ctx exp_i in
        let idx_target =
          value_i |> Value.Get.num |> Num.to_int |> Bigint.to_int_exn
        in
        match value.it with
        | TextV s when idx_target < 0 || idx_target >= String.length s ->
            back_err exp_i.node.at
              (F.asprintf "index %d out of bounds [0, %d)" idx_target
                 (String.length s))
        | TextV s ->
            let s_n = Value.Get.text value_upd in
            if String.length s_n <> 1 then
              back_err exp_i.node.at
                (F.asprintf
                   "updating a character requires a single-character text, but \
                    got %s"
                   (Sl.Print.string_of_value ~short:true value_upd))
            else
              let s_updated =
                String.sub s 0 idx_target ^ s_n
                ^ String.sub s (idx_target + 1)
                    (String.length s - idx_target - 1)
              in
              let value = Value.Make.text s_updated in
              Hook.on_value value;
              eval_update_path ctx value_b path value
        | ListV values when idx_target < 0 || idx_target >= List.length values
          ->
            back_err exp_i.node.at
              (F.asprintf "index %d out of bounds [0, %d)" idx_target
                 (List.length values))
        | ListV values ->
            let values_updated =
              List.mapi
                (fun idx value -> if idx = idx_target then value_upd else value)
                values
            in
            let value = Value.Make.list typ values_updated in
            Hook.on_value value;
            eval_update_path ctx value_b path value
        | _ ->
            back_err path.at
              (F.asprintf "indexing expects either a text or a list, but got %s"
                 (Sl.Print.string_of_value ~short:true value)))
    | SliceP (path, exp_i, exp_n) -> (
        let typ = path.note $ path.at in
        let value = eval_access_path ctx value_b path in
        let value_i = eval_exp ctx exp_i in
        let idx_l =
          value_i |> Value.Get.num |> Num.to_int |> Bigint.to_int_exn
        in
        let value_n = eval_exp ctx exp_n in
        let idx_n =
          value_n |> Value.Get.num |> Num.to_int |> Bigint.to_int_exn
        in
        let idx_h = idx_l + idx_n in
        match value.it with
        | TextV s when idx_l < 0 || idx_h > String.length s ->
            back_err exp_n.node.at
              (F.asprintf "slice [%d, %d) out of bounds [0, %d)" idx_l idx_h
                 (String.length s))
        | TextV s ->
            let s_upd = Value.Get.text value_upd in
            if String.length s_upd <> idx_n then
              back_err exp_n.node.at
                (F.asprintf
                   "updating a slice of length %d requires a text of length \
                    %d, but got %s"
                   idx_n (String.length s_upd)
                   (Sl.Print.string_of_value ~short:true value_upd))
            else
              let s_upd =
                String.sub s 0 idx_l ^ s_upd
                ^ String.sub s idx_h (String.length s - idx_h)
              in
              let value = Value.Make.text s_upd in
              Hook.on_value value;
              eval_update_path ctx value_b path value
        | ListV values when idx_l < 0 || idx_h > List.length values ->
            back_err exp_n.node.at
              (F.asprintf "slice [%d, %d) out of bounds [0, %d)" idx_l idx_h
                 (List.length values))
        | ListV values ->
            let values_upd = Value.Get.list value_upd in
            if List.length values_upd <> idx_n then
              back_err exp_n.node.at
                (F.asprintf
                   "updating a slice of length %d requires a list of length \
                    %d, but got %s"
                   idx_n (List.length values_upd)
                   (Sl.Print.string_of_value ~short:true value_upd))
            else
              let values_upd =
                List.mapi
                  (fun idx value ->
                    if idx_l <= idx && idx < idx_h then
                      List.nth values_upd (idx - idx_l)
                    else value)
                  values
              in
              let value = Value.Make.list typ values_upd in
              Hook.on_value value;
              eval_update_path ctx value_b path value
        | _ ->
            back_err path.at
              (F.asprintf "slicing expects either a text or a list, but got %s"
                 (Sl.Print.string_of_value ~short:true value)))
    | DotP (path, atom) ->
        let typ = path.note $ path.at in
        let value = eval_access_path ctx value_b path in
        let valuefields = value |> Value.Get.str in
        let valuefields =
          List.map
            (fun (atom_f, value_f) ->
              if Atom.eq atom_f.it atom.it then (atom_f, value_upd)
              else (atom_f, value_f))
            valuefields
        in
        let value = Value.Make.str typ valuefields in
        Hook.on_value value;
        eval_update_path ctx value_b path value

  and eval_upd_exp (_typ_note : typ) (ctx : Ctx.t) (exp_b : exp) (path : path)
      (exp_f : exp) : value =
    let value_b = eval_exp ctx exp_b in
    let value_f = eval_exp ctx exp_f in
    eval_update_path ctx value_b path value_f

  (* Function call expression evaluation *)

  and eval_call_exp (_typ_note : typ) (ctx : Ctx.t) (id : id)
      (targs : targ list) (args : arg list) : value =
    invoke_func ctx id targs args

  (* Iterated expression evaluation *)

  and eval_iter_exp_opt (typ_note : typ) (ctx : Ctx.t) (exp : exp)
      (vars : var list) : value =
    let ctx_sub_opt = Ctx.sub_opt ctx vars in
    let value_res =
      match ctx_sub_opt with
      | Some ctx_sub ->
          let value = eval_exp ctx_sub exp in
          Value.Make.opt typ_note (Some value)
      | None -> Value.Make.opt typ_note None
    in
    Hook.on_value value_res;
    List.iter
      (fun (id, _typ, iters) ->
        let value_sub = Ctx.find_value ctx (id, iters @ [ Il.Opt ]) in
        Hook.on_value_dependency value_res value_sub Dep.Edges.Iter)
      vars;
    value_res

  and eval_iter_exp_list (typ_note : typ) (ctx : Ctx.t) (exp : exp)
      (vars : var list) : value =
    let ctxs_sub = Ctx.sub_list ctx vars in
    let values = List.map (fun ctx_sub -> eval_exp ctx_sub exp) ctxs_sub in
    let value_res = Value.Make.list typ_note values in
    Hook.on_value value_res;
    List.iter
      (fun (id, _typ, iters) ->
        let value_sub = Ctx.find_value ctx (id, iters @ [ Il.List ]) in
        Hook.on_value_dependency value_res value_sub Dep.Edges.Iter)
      vars;
    value_res

  and eval_iter_exp (typ_note : typ) (ctx : Ctx.t) (exp : exp)
      (iterexp : iterexp) : value =
    let iter, vars = iterexp in
    match iter with
    | Opt -> eval_iter_exp_opt typ_note ctx exp vars
    | List -> eval_iter_exp_list typ_note ctx exp vars

  (* Argument evaluation *)

  and eval_arg (ctx : Ctx.t) (arg : arg) : value =
    try eval_arg' ctx arg
    with Backtrace backtrace ->
      back_nest arg.at
        (fun () -> F.asprintf "%s failed" (Pl.Print.string_of_arg arg))
        backtrace

  and eval_arg' (ctx : Ctx.t) (arg : arg) : value =
    match arg.it with
    | ExpA exp -> eval_exp ctx exp
    | DefA id ->
        let tparams, typs_params, typ = Ctx.find_func_signature ctx id in
        let value_res = Value.Make.func id tparams typs_params typ in
        Hook.on_value value_res;
        value_res

  and eval_args (ctx : Ctx.t) (args : arg list) : value list =
    List.map (eval_arg ctx) args

  (* Instruction evaluation *)

  and eval_instr (eval_instr_tier : 'instr_tier eval_instr_tier)
      (string_of_instr_tier : 'instr_tier string_of_instr_tier) (ctx : Ctx.t)
      (instr : 'instr_tier instr) : Ctx.t * Flow.t =
    try eval_instr' eval_instr_tier string_of_instr_tier ctx instr
    with Backtrace backtrace ->
      backtrace
      |> back_nest instr.node.at (fun () ->
             F.asprintf "%s failed"
               (Pl.Print.string_of_instr string_of_instr_tier instr))

  and eval_instr' (eval_instr_tier : 'instr_tier eval_instr_tier)
      (string_of_instr_tier : 'instr_tier string_of_instr_tier) (ctx : Ctx.t)
      (instr : 'instr_tier instr) : Ctx.t * Flow.t =
    let iid = instr.node.note.iid in
    match instr.node.it with
    | IfI (exp_cond, iterexps, block_then, dangle) ->
        eval_if_instr eval_instr_tier string_of_instr_tier iid ctx exp_cond
          iterexps block_then dangle
    | HoldI (id, notexp, iterexps, holdcase) ->
        eval_hold_instr eval_instr_tier string_of_instr_tier iid ctx id notexp
          iterexps holdcase
    | CaseI (exp, cases, dangle) ->
        eval_case_instr eval_instr_tier string_of_instr_tier iid ctx exp cases
          dangle
    | LetI (exp_l, exp_r, iterinstrs) ->
        eval_let_instr ctx exp_l exp_r iterinstrs
    | DebugI exp -> eval_debug_instr ctx exp
    | DestructI (fields, exp) -> eval_destruct_instr ctx fields exp
    | CheckLetSubI (typ_target, subcheck, exp_l, exp_r, instr_then) ->
        eval_check_let_sub_instr eval_instr_tier string_of_instr_tier ctx
          typ_target subcheck exp_l exp_r instr_then
    | CheckLetMatchI (pattern, exp_l, exp_r, instr_then) ->
        eval_check_let_match_instr eval_instr_tier string_of_instr_tier ctx
          pattern exp_l exp_r instr_then
    | OptionGetI (exp_l, exp_r, block) ->
        eval_option_get_instr eval_instr_tier string_of_instr_tier ctx exp_l
          exp_r block
    | TierI instr_tier -> eval_instr_tier ctx instr_tier

  and eval_block : 'instr_tier. ('instr_tier, 'instr_tier block) eval_block_fn =
   fun eval_instr_tier string_of_instr_tier ctx block ->
    let _, flow =
      List.fold_left
        (fun (ctx, flow) instr ->
          match flow with
          | Flow.Cont traces_pre -> (
              let ctx_post, flow_post =
                eval_instr eval_instr_tier string_of_instr_tier ctx instr
              in
              match flow_post with
              (* Retain the deeper trace chain so an exhausted
                 block reports the branch that progressed furthest *)
              | Flow.Cont traces_post ->
                  let traces =
                    if List.length traces_post >= List.length traces_pre then
                      traces_post
                    else traces_pre
                  in
                  (ctx_post, Flow.Cont traces)
              | _ -> (ctx_post, flow_post))
          | _ -> (ctx, flow))
        (ctx, Flow.Cont []) block
    in
    (ctx, flow)

  and eval_elseblock_opt :
        'instr_tier. ('instr_tier, 'instr_tier block option) eval_block_fn =
   fun eval_instr_tier string_of_instr_tier ctx elseblock_opt ->
    match elseblock_opt with
    | Some block_else ->
        eval_block eval_instr_tier string_of_instr_tier ctx block_else
    | None -> (ctx, Flow.Cont [])

  (* If instruction evaluation *)

  and eval_if_cond (ctx : Ctx.t) (exp_cond : exp) : bool * value =
    let value_cond = eval_exp ctx exp_cond in
    let cond = Value.Get.bool value_cond in
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
            let typ_cond_opt = Typ.Make.opt Typ.Make.bool in
            let value_cond = Value.Make.opt typ_cond_opt value_cond_opt in
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
            let typ_cond_list = Typ.Make.list Typ.Make.bool in
            let value_cond = Value.Make.list typ_cond_list values_cond in
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

  and eval_if_instr (eval_instr_tier : 'instr_tier eval_instr_tier)
      (string_of_instr_tier : 'instr_tier string_of_instr_tier) (iid : iid)
      (ctx : Ctx.t) (exp_cond : exp) (iterexps : iterexp list)
      (block_then : 'instr_tier block) (dangle : dangle) : Ctx.t * Flow.t =
    (* Evaluate the if condition and mark dangle *)
    let cond, value_cond = eval_if_cond_iter ctx exp_cond iterexps in
    if dangle then Hook.on_instr_dangling (not cond) iid value_cond;
    (* Evaluate the then branch if the condition holds *)
    if cond then eval_block eval_instr_tier string_of_instr_tier ctx block_then
    else
      let trace =
        ( exp_cond.node.at,
          fun () ->
            F.asprintf "condition %s was not met"
              (Pl.Print.string_of_exp exp_cond) )
      in
      (ctx, Flow.Cont [ trace ])

  (* Hold instruction evaluation *)

  and eval_hold_cond (ctx : Ctx.t) (id : id) (notexp : notexp) : bool * value =
    let exps_input = Mixfix.args notexp in
    let values_input = eval_exps ctx exps_input in
    let hold =
      try
        let _ = invoke_rel ctx id values_input in
        true
      with
      | Backtrace (Unmatch _) -> false
      | Backtrace backtrace ->
          back_nest id.at
            (fun () -> "hold condition evaluation failed")
            backtrace
    in
    let value_res = Value.Make.bool hold in
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
            let typ_cond_opt = Typ.Make.opt Typ.Make.bool in
            let value_cond = Value.Make.opt typ_cond_opt value_cond_opt in
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
            let typ_cond_list = Typ.Make.list Typ.Make.bool in
            let value_cond = Value.Make.list typ_cond_list values_cond in
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

  and eval_hold_instr (eval_instr_tier : 'instr_tier eval_instr_tier)
      (string_of_instr_tier : 'instr_tier string_of_instr_tier) (iid : iid)
      (ctx : Ctx.t) (id : id) (notexp : notexp) (iterexps : iterexp list)
      (holdcase : 'instr_tier holdcase) : Ctx.t * Flow.t =
    (* Backup in case of failure *)
    Hook.backup ();
    (* Evaluate the hold condition *)
    let cond, value_cond = eval_hold_cond_iter ctx id notexp iterexps in
    (* Evaluate the hold case, and restore the coverage information
       if the expected behavior is the relation not holding *)
    match holdcase with
    | BothH (block_hold, block_not_hold) ->
        if cond then
          eval_block eval_instr_tier string_of_instr_tier ctx block_hold
        else (
          Hook.restore ();
          eval_block eval_instr_tier string_of_instr_tier ctx block_not_hold)
    | HoldH (block_hold, dangle) ->
        if dangle then Hook.on_instr_dangling (not cond) iid value_cond;
        if cond then
          eval_block eval_instr_tier string_of_instr_tier ctx block_hold
        else
          let trace =
            (id.at, fun () -> F.asprintf "condition hold %s was not met" id.it)
          in
          (ctx, Flow.Cont [ trace ])
    | NotHoldH (block_not_hold, dangle) ->
        Hook.restore ();
        if dangle then Hook.on_instr_dangling cond iid value_cond;
        if not cond then
          eval_block eval_instr_tier string_of_instr_tier ctx block_not_hold
        else
          let trace =
            ( id.at,
              fun () -> F.asprintf "condition not-hold %s was not met" id.it )
          in
          (ctx, Flow.Cont [ trace ])

  (* Case analysis instruction evaluation *)

  and eval_cases (ctx : Ctx.t) (exp : exp) (cases : 'instr_tier case list) :
      (Ctx.t * 'instr_tier block) option * value =
    let value_exp = eval_exp ctx exp in
    let id_tmp = "~case" $ no_region in
    let ctx = Ctx.add_value ctx (id_tmp, []) value_exp in
    let exp =
      Pl.VarE id_tmp $$ (exp.node.at, exp.node.note) |> Annot.no_hints
    in
    let bind_target (ctx : Ctx.t) (guard : guard) : Ctx.t =
      match guard with
      | CheckLetSubG (_, _, target) ->
          let typ_target = target.node.note $ target.node.at in
          let value' = downcast ctx typ_target value_exp in
          assign_exp ctx target value'
      | CheckLetMatchG (_, target) -> assign_exp ctx target value_exp
      | _ -> ctx
    in
    let block_match, values_cond_rev =
      List.fold_left
        (fun (block_match, values_cond_rev) (guard, block) ->
          match block_match with
          | Some _ -> (block_match, values_cond_rev)
          | None ->
              let exp_cond =
                match guard with
                | BoolG true -> exp.node.it
                | BoolG false -> Pl.UnE (`NotOp, `BoolT, exp)
                | CmpG (cmpop, optyp, exp_r) ->
                    Pl.CmpE (cmpop, optyp, exp, exp_r)
                | SubG (typ, subcheck) | CheckLetSubG (typ, subcheck, _) ->
                    Pl.SubE (exp, typ, subcheck)
                | MatchG pattern | CheckLetMatchG (pattern, _) ->
                    Pl.MatchE (exp, pattern)
                | MemG exp_s -> Pl.MemE (exp, exp_s)
              in
              let exp_cond =
                exp_cond $$ (exp.node.at, Il.BoolT) |> Annot.no_hints
              in
              let value_cond = eval_exp ctx exp_cond in
              let values_cond_rev = value_cond :: values_cond_rev in
              let cond = Value.Get.bool value_cond in
              if cond then
                let ctx_arm = bind_target ctx guard in
                (Some (ctx_arm, block), values_cond_rev)
              else (None, values_cond_rev))
        (None, []) cases
    in
    let typ_cond_list = Typ.Make.list Typ.Make.bool in
    let values_cond = List.rev values_cond_rev in
    let value_cond = Value.Make.list typ_cond_list values_cond in
    Hook.on_value value_cond;
    (block_match, value_cond)

  and eval_case_instr (eval_instr_tier : 'instr_tier eval_instr_tier)
      (string_of_instr_tier : 'instr_tier string_of_instr_tier) (iid : iid)
      (ctx : Ctx.t) (exp : exp) (cases : 'instr_tier case list)
      (dangle : dangle) : Ctx.t * Flow.t =
    (* Evaluate the matching case and mark dangle *)
    let block_opt, value_cond = eval_cases ctx exp cases in
    (if dangle then
       let matched = Option.is_some block_opt in
       Hook.on_instr_dangling (not matched) iid value_cond);
    (* Evaluate the matching case if any *)
    match block_opt with
    | Some (ctx, block) ->
        eval_block eval_instr_tier string_of_instr_tier ctx block
    | None ->
        let trace =
          ( exp.node.at,
            fun () ->
              F.asprintf "no case matched for %s" (Pl.Print.string_of_exp exp)
          )
        in
        (ctx, Flow.Cont [ trace ])

  (* Backtracking-block instruction evaluation *)

  and eval_block_deterministic :
        'instr_tier. ('instr_tier, 'instr_tier arm list) eval_block_fn =
   fun eval_instr_tier string_of_instr_tier ctx arms ->
    let eval_arm_deterministic (ctx_pre : Ctx.t) (flow_pre : Flow.t)
        (arm : 'instr_tier arm) : Ctx.t * Flow.t =
      let at = match arm with instr :: _ -> instr.node.at | [] -> no_region in
      let open Flow in
      try
        let ctx_post, flow_post =
          eval_block eval_instr_tier string_of_instr_tier ctx_pre arm
        in
        match flow_pre with
        | Cont traces_pre -> (
            match flow_post with
            | Cont traces_post ->
                (* Retain the deeper trace chain, to report
                   the arm that progressed furthest *)
                let traces =
                  if List.length traces_post >= List.length traces_pre then
                    traces_post
                  else traces_pre
                in
                let flow = Cont traces in
                (ctx_post, flow)
            | _ -> (ctx_post, flow_post))
        | Res _ -> (
            match flow_post with
            | Cont _ -> (ctx_pre, flow_pre)
            | Res _ -> nondet at
            | Ret _ -> back_err at "cannot have both result and return"
            | Tailcall_func _ | Tailcall_rel _ -> assert false)
        | Ret _ -> (
            match flow_post with
            | Cont _ -> (ctx_pre, flow_pre)
            | Res _ -> back_err at "cannot have both return and result"
            | Ret _ -> nondet at
            | Tailcall_func _ | Tailcall_rel _ -> assert false)
        | Tailcall_func _ | Tailcall_rel _ -> assert false
      with Backtrace (Unmatch _) -> (ctx_pre, flow_pre)
    in
    try
      let _, flow =
        List.fold_left
          (fun (ctx_pre, flow_pre) arm ->
            eval_arm_deterministic ctx_pre flow_pre arm)
          (ctx, Flow.Cont []) arms
      in
      (ctx, flow)
    with Nondet at -> back_err at "nondeterministic instruction evaluation"

  and eval_block_sequential :
        'instr_tier. ('instr_tier, 'instr_tier arm list) eval_block_fn =
   fun eval_instr_tier string_of_instr_tier ctx arms ->
    let _, flow =
      List.fold_left
        (fun (ctx, flow) arm ->
          match flow with
          | Flow.Cont traces -> (
              try eval_block eval_instr_tier string_of_instr_tier ctx arm
              with Backtrace (Unmatch traces_post) ->
                (* Retain the deeper trace chain, to report
                   the arm that progressed furthest *)
                let traces =
                  if List.length traces_post >= List.length traces then
                    traces_post
                  else traces
                in
                let flow = Flow.Cont traces in
                (ctx, flow))
          | _ -> (ctx, flow))
        (ctx, Flow.Cont []) arms
    in
    (ctx, flow)

  and eval_block_instr :
        'instr_tier. ('instr_tier, 'instr_tier arm list) eval_block_fn =
   fun eval_instr_tier string_of_instr_tier ctx arms ->
    if !Ctx.is_det then
      eval_block_deterministic eval_instr_tier string_of_instr_tier ctx arms
    else eval_block_sequential eval_instr_tier string_of_instr_tier ctx arms

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
                    Typ.Make.iterate typ_binding (iters_binding @ [ Il.Opt ])
                  in
                  Value.Make.opt typ None
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
                    Typ.Make.iterate typ_binding (iters_binding @ [ Il.Opt ])
                  in
                  Value.Make.opt typ (Some value_binding)
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
          let typ =
            Typ.Make.iterate typ_binding (iters_binding @ [ Il.List ])
          in
          Value.Make.list typ values_binding
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
      (iterinstrs : iterinstr list) : Ctx.t * Flow.t =
    let ctx = eval_let_iter ctx exp_l exp_r iterinstrs in
    (ctx, Flow.Cont [])

  (* Rule instruction evaluation *)

  and eval_rule (ctx : Ctx.t) (id : id) (notexp : notexp)
      (inputs : Hints.Input.t) : Ctx.t =
    let exps = Mixfix.args notexp in
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
                  Typ.Make.iterate typ_binding (iters_binding @ [ Il.Opt ])
                in
                Value.Make.opt typ None)
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
                  Typ.Make.iterate typ_binding (iters_binding @ [ Il.Opt ])
                in
                Value.Make.opt typ (Some value_binding))
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
          let typ =
            Typ.Make.iterate typ_binding (iters_binding @ [ Il.List ])
          in
          Value.Make.list typ values_binding
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
      (inputs : Hints.Input.t) (iterinstrs : iterinstr list) : Ctx.t * Flow.t =
    let ctx = eval_rule_iter ctx id notexp inputs iterinstrs in
    (ctx, Flow.Cont [])

  (* Result instruction evaluation *)

  and eval_result_instr (ctx : Ctx.t) (_rel_signature : rel_signature)
      (exps : exp list) : Ctx.t * Flow.t =
    let values = eval_exps ctx exps in
    let flow = Flow.Res values in
    (ctx, flow)

  (* Return instruction evaluation *)

  and eval_return_instr (ctx : Ctx.t) (exp : exp) : Ctx.t * Flow.t =
    let value = eval_exp ctx exp in
    let flow = Flow.Ret value in
    (ctx, flow)

  (* Debug instruction evaluation *)

  and eval_debug_instr (ctx : Ctx.t) (exp : exp) : Ctx.t * Flow.t =
    let value = eval_exp ctx exp in
    string_of_region exp.node.at ^ ": " ^ Pl.Print.string_of_exp exp
    |> print_endline;
    let region = string_of_region value.at in
    (if region = "" then "" else region ^ ": ") ^ Il.Print.string_of_value value
    |> print_endline;
    (ctx, Flow.Cont [])

  (* Destruct instruction evaluation *)

  and eval_destruct_instr (ctx : Ctx.t) (fields : (string option * exp) list)
      (exp : exp) : Ctx.t * Flow.t =
    let value = eval_exp ctx exp in
    match value.it with
    | CaseV valuecase
      when List.length fields = List.length (Mixfix.args valuecase) ->
        let values = Mixfix.args valuecase in
        let ctx =
          List.fold_left2
            (fun ctx (_, exp_target) value -> assign_exp ctx exp_target value)
            ctx fields values
        in
        let flow = Flow.Cont [] in
        (ctx, flow)
    | _ ->
        back_err exp.node.at
          (F.asprintf "destructure failed: %s"
             (Sl.Print.string_of_value ~short:true value))

  (* Check-let on sub instruction evaluation *)

  and eval_check_let_sub_instr (eval_instr_tier : 'instr_tier eval_instr_tier)
      (string_of_instr_tier : 'instr_tier string_of_instr_tier) (ctx : Ctx.t)
      (typ_target : typ) (subcheck : subcheck) (exp_l : exp) (exp_r : exp)
      (block_inner : 'instr_tier block) : Ctx.t * Flow.t =
    let value = eval_exp ctx exp_r in
    let sub =
      Value.Match.check sub_cache (Ctx.find_typdef_opt ctx)
        (Ctx.find_func_signature ctx)
        subcheck value
    in
    if sub then
      let value = downcast ctx typ_target value in
      let ctx_opt =
        try
          let ctx = assign_exp ctx exp_l value in
          Some ctx
        with Backtrace (Err _) -> None
      in
      match ctx_opt with
      | Some ctx ->
          eval_block eval_instr_tier string_of_instr_tier ctx block_inner
      | None ->
          let trace =
            ( exp_r.node.at,
              fun () ->
                F.asprintf "binding %s failed" (Pl.Print.string_of_exp exp_l) )
          in
          (ctx, Flow.Cont [ trace ])
    else
      let trace =
        ( exp_r.node.at,
          fun () ->
            F.asprintf "%s is not a subtype of %s"
              (Pl.Print.string_of_exp exp_r)
              (Sl.Print.string_of_typ typ_target) )
      in
      (ctx, Flow.Cont [ trace ])

  (* Check-let on match instruction evaluation *)

  and eval_check_let_match_instr (eval_instr_tier : 'instr_tier eval_instr_tier)
      (string_of_instr_tier : 'instr_tier string_of_instr_tier) (ctx : Ctx.t)
      (pattern : pattern) (exp_l : exp) (exp_r : exp)
      (block_inner : 'instr_tier block) : Ctx.t * Flow.t =
    let value = eval_exp ctx exp_r in
    let matches =
      match (pattern, value.it) with
      | CaseP mixop_p, CaseV valuecase -> Mixfix.eq_mixop mixop_p valuecase
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
    if matches then
      let ctx = assign_exp ctx exp_l value in
      eval_block eval_instr_tier string_of_instr_tier ctx block_inner
    else
      let trace =
        ( exp_r.node.at,
          fun () ->
            F.asprintf "%s does not match the expected pattern"
              (Pl.Print.string_of_exp exp_r) )
      in
      (ctx, Flow.Cont [ trace ])

  (* Option-get instruction evaluation *)

  and eval_option_get_instr (eval_instr_tier : 'instr_tier eval_instr_tier)
      (string_of_instr_tier : 'instr_tier string_of_instr_tier) (ctx : Ctx.t)
      (exp_l : exp) (exp_r : exp) (block : 'instr_tier block) : Ctx.t * Flow.t =
    let value = eval_exp ctx exp_r in
    match value.it with
    | OptV (Some value_inner) ->
        let ctx = assign_exp ctx exp_l value_inner in
        eval_block eval_instr_tier string_of_instr_tier ctx block
    | _ ->
        let trace =
          ( exp_r.node.at,
            fun () ->
              F.asprintf "%s evaluated to an empty option"
                (Pl.Print.string_of_exp exp_r) )
        in
        (ctx, Flow.Cont [ trace ])

  (* Tier-specific instruction evaluation *)

  and eval_instr_dispatch (ctx : Ctx.t) (instr_dispatch : instr_dispatch) :
      Ctx.t * Flow.t =
    match instr_dispatch with
    | GroupI (_id_group, _id_rel, _rel_signature, _exps_group, block) ->
        eval_block_group ctx block
    | RouteI arms ->
        eval_block_instr eval_instr_dispatch Pl.Print.string_of_instr_dispatch
          ctx arms

  and eval_block_dispatch (ctx : Ctx.t) (block : block_dispatch) :
      Ctx.t * Flow.t =
    eval_block eval_instr_dispatch Pl.Print.string_of_instr_dispatch ctx block

  and eval_instr_group (ctx : Ctx.t) (instr_group : instr_group) :
      Ctx.t * Flow.t =
    match instr_group with
    | ResultI (rel_signature, exps) -> eval_result_instr ctx rel_signature exps
    | ReturnI exp -> eval_return_instr ctx exp
    | RuleI (id, notexp, inputs, iterinstrs) ->
        eval_rule_instr ctx id notexp inputs iterinstrs
    | BacktrackI arms ->
        eval_block_instr eval_instr_group Pl.Print.string_of_instr_group ctx
          arms

  and eval_block_group (ctx : Ctx.t) (block : block_group) : Ctx.t * Flow.t =
    eval_block eval_instr_group Pl.Print.string_of_instr_group ctx block

  (* Invoke a relation *)

  and is_extern_rel (rel : Rel.t) : bool =
    match rel with Rel.Extern _ -> true | Rel.Defined _ -> false

  and invoke_rel ?(internal : bool = true) (ctx : Ctx.t) (id : id)
      (values_input : value list) : value list =
    try
      Hook.on_rel_enter id values_input;
      let values_output = invoke_rel' ~internal ctx id values_input in
      Hook.on_rel_exit id;
      values_output
    with Backtrace backtrace ->
      Hook.on_rel_exit id;
      back_nest id.at
        (fun () -> F.asprintf "relation %s failed" id.it)
        backtrace

  and invoke_rel' ~(internal : bool) (ctx : Ctx.t) (id : id)
      (values_input : value list) : value list =
    let rel = Ctx.find_rel ctx id in
    let invoke_rel'' () =
      match rel with
      | Rel.Extern (nottyp, inputs) ->
          invoke_extern_rel ctx nottyp inputs id values_input
      | Rel.Defined (_, exps_input, instr, elseblock_opt) ->
          invoke_defined_rel ctx id exps_input instr elseblock_opt values_input
    in
    if !cache_enabled && not (is_extern_rel rel) then (
      let cache_result = CCache.find !rel_cache (id.it, values_input) in
      match cache_result with
      | Some values_output -> values_output
      | None ->
          let checkpoint_before = Interface.checkpoint () in
          let extern_checkpoint_before = Extern.checkpoint () in
          let values_output = invoke_rel'' () in
          let checkpoint_after = Interface.checkpoint () in
          let extern_checkpoint_after = Extern.checkpoint () in
          if
            (not (Interface.seff checkpoint_before checkpoint_after))
            && not
                 (Extern.seff extern_checkpoint_before extern_checkpoint_after)
          then CCache.add !rel_cache (id.it, values_input) values_output;
          values_output)
    else (
      if not internal then check_rel_inputs ctx id values_input;
      invoke_rel'' ())

  and invoke_extern_rel (ctx : Ctx.t) (nottyp : nottyp) (inputs : Hints.Input.t)
      (id : id) (values_input : value list) : value list =
    let values_output =
      match Extern.eval_extern_rel id.it values_input with
      | Pass values_output -> values_output
      | Fail (at, msg) -> back_err at msg
    in
    check_rel_outputs ctx id nottyp inputs values_output;
    List.iteri
      (fun idx_arg value_input ->
        List.iter
          (fun value_output ->
            Hook.on_value_dependency value_output value_input
              (Dep.Edges.Rel (id, idx_arg)))
          values_output)
      values_input;
    values_output

  and invoke_defined_rel (ctx : Ctx.t) (id : id) (exps_input : exp list)
      (block : block_dispatch) (elseblock_opt : block_dispatch option)
      (values_input : value list) : value list =
    let ctx_local = Ctx.localize_rule ctx id values_input in
    let ctx_local = assign_exps ctx_local exps_input values_input in
    let flow =
      try
        let _, flow = eval_block_dispatch ctx_local block in
        flow
      with Backtrace (Unmatch traces) -> Flow.Cont traces
    in
    let _, flow =
      match (flow, elseblock_opt) with
      | Flow.Cont _, Some _ ->
          eval_elseblock_opt eval_instr_dispatch
            Pl.Print.string_of_instr_dispatch ctx_local elseblock_opt
      | _ -> (ctx_local, flow)
    in
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
    | Tailcall_func _ | Tailcall_rel _ -> assert false

  (* Invoke a function *)

  and is_extern_func (func : Func.t) : bool =
    match func with Func.Extern _ -> true | _ -> false

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
                | Type.Typdef.Defined ([], { it = Il.PlainT typ; _ }) ->
                    TIdMap.add tid typ theta
                | _ -> theta)
              tdenv_local TIdMap.empty
          in
          List.map (Type.Subst.subst_typ theta) targs
    in
    let values_input = eval_args ctx args in
    invoke_func_with_values ctx id targs values_input

  and invoke_func_with_values ?(internal : bool = true) (ctx : Ctx.t) (id : id)
      (targs : targ list) (values_input : value list) : value =
    try
      Hook.on_func_enter id values_input;
      let cursor, func = Ctx.find_func ctx id in
      let anon = cursor = Ctx.Local in
      let invoke_func_with_values' () =
        match func with
        | Func.Extern (tparams, _, typ) ->
            invoke_extern_func ctx id tparams targs values_input typ
        | Func.Builtin (tparams, _, typ) ->
            invoke_builtin_func ctx id tparams targs values_input typ
        | Func.Table (params, _, tablerows) ->
            invoke_table_func ctx id params tablerows values_input
        | Func.Defined (tparams, params, _, instr, elseblock_opt) ->
            invoke_defined_func ctx id tparams params instr elseblock_opt targs
              values_input
      in
      let value_output =
        if
          !cache_enabled && (not anon)
          && (not (is_extern_func func))
          && not (is_high_order_func values_input)
        then (
          let cache_result = CCache.find !func_cache (id.it, values_input) in
          match cache_result with
          | Some value_output -> value_output
          | None ->
              let checkpoint_before = Interface.checkpoint () in
              let extern_checkpoint_before = Extern.checkpoint () in
              let value_output = invoke_func_with_values' () in
              let checkpoint_after = Interface.checkpoint () in
              let extern_checkpoint_after = Extern.checkpoint () in
              if
                (not (Interface.seff checkpoint_before checkpoint_after))
                && not
                     (Extern.seff extern_checkpoint_before
                        extern_checkpoint_after)
              then CCache.add !func_cache (id.it, values_input) value_output;
              value_output)
        else (
          if not internal then check_func_inputs ctx id targs values_input;
          invoke_func_with_values' ())
      in
      Hook.on_func_exit id;
      value_output
    with Backtrace backtrace ->
      Hook.on_func_exit id;
      back_nest id.at
        (fun () -> F.asprintf "function %s failed" id.it)
        backtrace

  and invoke_extern_func (ctx : Ctx.t) (id : id) (tparams : tparam list)
      (targs : targ list) (values_input : value list) (typ_output : typ) : value
      =
    let value_output =
      match Extern.eval_extern_func id.it [] values_input with
      | Pass value_output -> value_output
      | Fail (at, msg) -> back_err at msg
    in
    check_func_output ctx id tparams typ_output targs value_output;
    List.iteri
      (fun idx_arg value_input ->
        Hook.on_value_dependency value_output value_input
          (Dep.Edges.Func (id, idx_arg)))
      values_input;
    value_output

  and invoke_builtin_func (ctx : Ctx.t) (id : id) (tparams : tparam list)
      (targs : targ list) (values_input : value list) (typ_output : typ) : value
      =
    let value_output =
      try
        Interface.call_builtin
          (fun value -> Hook.on_value value)
          id targs values_input
      with Builtin.Error.BuiltinError (at, msg) -> back_unmatch at msg
    in
    check_func_output ctx id tparams typ_output targs value_output;
    List.iteri
      (fun idx_arg value_input ->
        Hook.on_value_dependency value_output value_input
          (Dep.Edges.Func (id, idx_arg)))
      values_input;
    value_output

  and invoke_table_func (ctx : Ctx.t) (id : id) (params : param list)
      (tablerows : tablerow list) (values_input : value list) : value =
    let ctx_local = Ctx.localize_func ctx id values_input TDEnv.empty in
    let ctx_local = assign_params ctx ctx_local params values_input in
    let block = List.concat_map (fun (_, _, block) -> block) tablerows in
    let _, flow = eval_block_group ctx_local block in
    match flow with
    | Ret value_output ->
        List.iteri
          (fun idx_arg value_input ->
            Hook.on_value_dependency value_output value_input
              (Dep.Edges.Func (id, idx_arg)))
          values_input;
        value_output
    | _ -> back_err id.at "table did not return a value"

  and invoke_defined_func (ctx : Ctx.t) (id : id) (tparams : tparam list)
      (params : param list) (block : block_group)
      (elseblock_opt : block_group option) (targs : targ list)
      (values_input : value list) : value =
    let tdenv_local =
      check
        (List.length targs = List.length tparams)
        id.at "arity mismatch in type arguments";
      List.fold_left2
        (fun tdenv_local tparam targ ->
          let td = Type.Typdef.Defined ([], Il.PlainT targ $ targ.at) in
          TDEnv.add tparam td tdenv_local)
        TDEnv.empty tparams targs
    in
    let ctx_local = Ctx.localize_func ctx id values_input tdenv_local in
    let ctx_local = assign_params ctx ctx_local params values_input in
    let flow =
      try
        let _, flow = eval_block_group ctx_local block in
        flow
      with Backtrace (Unmatch traces) -> Flow.Cont traces
    in
    let _, flow =
      match (flow, elseblock_opt) with
      | Flow.Cont _, Some _ ->
          eval_elseblock_opt eval_instr_group Pl.Print.string_of_instr_group
            ctx_local elseblock_opt
      | _ -> (ctx_local, flow)
    in
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
    | Tailcall_func _ | Tailcall_rel _ -> assert false

  (* Entry points for evaluation *)

  let clear () : unit =
    CCache.empty !func_cache;
    CCache.empty !rel_cache;
    Hashtbl.clear sub_cache

  let do_eval_rel (relname : string) (values_input : value list) : value list =
    try
      let ctx = Ctx.empty () in
      let values_ouput =
        invoke_rel ~internal:false ctx (relname $ no_region) values_input
      in
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
        invoke_func_with_values ~internal:false ctx (funcname $ no_region) targs
          values_input
      in
      value_output
    with Backtrace backtrace ->
      let failtraces = back_failtraces backtrace in
      let msg = Util.Attempt.string_of_failtraces_short failtraces in
      error no_region msg

  let eval_program (relname : string) (includes_p4 : string list)
      (filename_p4 : string) : Run.program_result =
    clear ();
    try
      let value_program =
        match Interface.parse_program includes_p4 [ filename_p4 ] with
        | Pass value_program -> value_program
        | Fail (`Syntax (at, msg)) -> raise (P4.Error.ParseError (at, msg))
      in
      Hook.on_program value_program;
      let values_output = do_eval_rel relname [ value_program ] in
      Run.Pass values_output
    with
    | P4.Error.ParseError (at, msg) -> Run.Fail (`Syntax (at, msg))
    | Interp_common.Error.InterpError (at, msg) | Run.ExternError (at, msg) ->
        Run.Fail (`Runtime (at, msg))

  let eval_rel (relname : string) (values_input : value list) : Run.rel_result =
    clear ();
    try
      let values_output = do_eval_rel relname values_input in
      Run.Pass values_output
    with
    | Interp_common.Error.InterpError (at, msg) | Run.ExternError (at, msg) ->
      Run.Fail (at, msg)

  let eval_func (funcname : string) (targs : targ list)
      (values_input : value list) : Run.func_result =
    clear ();
    try
      let value_output = do_eval_func funcname targs values_input in
      Run.Pass value_output
    with
    | Interp_common.Error.InterpError (at, msg) | Run.ExternError (at, msg) ->
      Run.Fail (at, msg)

  (* Initialization *)

  let init ~(cache : bool) ~(det : bool) ~guard:_ (spec : spec) :
      (unit, Run.error) result =
    if cache then Cache.cache_on () else Cache.cache_off ();
    try Ok (Ctx.init ~det spec)
    with Interp_common.Error.InterpError (at, msg) -> Error { Run.at; msg }
end
