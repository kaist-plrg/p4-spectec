open Domain
module Mixfix = Domain.Mixfix
open Lib
open Lang
open Xl
open Il
open Al
module Type = Runtime.Type
module Typ = Type.Typ
module CCache = Runtime.Dynamic.Caches.CallCache
open Runtime.Dynamic_Al
open Envs
module Run = Runtime.Dynamic_Runner.Signature
module Dep = Runtime.Testgen_neg.Dep
module Hook = Inst.Hook
open Error
open Backtrack
open Nondet
module F = Format
open Util.Source

(* Cache *)

(* Interpreter *)

module Make (Interface : Run.INTERFACE) (Extern : Run.EXTERN) () :
  Run.INTERP_AL = struct
  (* Build context and caches *)

  module Ctx = Ctx.Make ()

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

  let check_guard = ref true

  let check_rel_inputs (ctx : Ctx.t) (id_rel : id) (values_input : value list) :
      unit =
    if not !check_guard then ()
    else
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
    if not !check_guard then ()
    else
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
    if not !check_guard then ()
    else
      let tparams, typs_params, _ = Ctx.find_func_signature ctx id_func in
      let ctx_local = Ctx.localize ctx in
      check
        (List.length targs = List.length tparams)
        id_func.at
        (F.sprintf "arity mismatch in type arguments of %s" id_func.it);
      let ctx_local =
        List.fold_left2
          (fun ctx_local tparam targ ->
            let td = Type.Typdef.Defined ([], PlainT targ $ targ.at) in
            Ctx.add_typdef ctx_local tparam td)
          ctx_local tparams targs
      in
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
    if not !check_guard then ()
    else
      let theta = TIdMap.of_lists tparams targs in
      let typ_output = Type.Subst.subst_typ theta typ_output in
      check
        (Value.Match.sub sub_cache (Ctx.find_typdef_opt ctx)
           (Ctx.find_func_signature ctx)
           typ_output value_output)
        id_func.at
        (F.sprintf
           "return value of function %s does not match the expected type"
           id_func.it)

  (* Helper for checking if an expression is a simple iteration of a variable *)

  let rec is_iter_var_exp (exp : exp) : Var.t option =
    match exp.it with
    | VarE id_exp -> Some (id_exp, [])
    | IterE (exp_inner, iterexp) -> (
        match is_iter_var_exp exp_inner with
        | Some (id_var, iters_var) -> (
            match iterexp with
            | iter, [ var ] ->
                let id_iter, _, iters_iter = var in
                if Id.eq id_var id_iter && iters_var = iters_iter then
                  Some (id_var, iters_var @ [ iter ])
                else None
            | _ -> None)
        | None -> None)
    | _ -> None

  (* Assignments *)

  (* Assigning a value to an expression *)

  let rec assign_exp (ctx : Ctx.t) (exp : exp) (value : value) : Ctx.t =
    let typ_value = value.note.typ $ exp.at in
    match (exp.it, value.it) with
    | VarE id, _ -> assign_var_exp ctx id value
    | TupleE exps, TupleV values -> assign_tuple_exp ctx exps values
    | CaseE notexp, CaseV valuecase -> assign_case_exp ctx notexp valuecase
    | StrE expfields, StructV valuefields ->
        assign_str_exp ctx expfields valuefields
    | OptE exp_opt, OptV value_opt -> assign_opt_exp ctx exp_opt value_opt
    | ListE exps, ListV values -> assign_list_exp ctx exps values
    | ConsE (exp_h, exp_t), ListV values_inner ->
        assign_cons_exp typ_value ctx exp_h exp_t values_inner
    | IterE (exp_inner, iterexp), _ ->
        assign_iter_exp (exp.note $ exp.at) ctx exp_inner iterexp value
    | _ ->
        error exp.at
          (F.asprintf "match failed %s <- %s"
             (Il.Print.string_of_exp exp)
             (Il.Print.string_of_value ~short:true value))

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

  and assign_var_exp (ctx : Ctx.t) (id : id) (value : value) : Ctx.t =
    Ctx.add_value ctx (id, []) value

  and assign_tuple_exp (ctx : Ctx.t) (exps : exp list) (values : value list) :
      Ctx.t =
    assign_exps ctx exps values

  and assign_case_exp (ctx : Ctx.t) (notexp : notexp) (valuecase : valuecase) :
      Ctx.t =
    assign_exps ctx (Mixfix.args notexp) (Mixfix.args valuecase)

  and assign_str_exp (ctx : Ctx.t) (expfields : (atom * exp) list)
      (valuefields : (atom * value) list) : Ctx.t =
    let exps = List.map snd expfields in
    let values = List.map snd valuefields in
    assign_exps ctx exps values

  and assign_opt_exp (ctx : Ctx.t) (exp_opt : exp option)
      (value_opt : value option) : Ctx.t =
    match (exp_opt, value_opt) with
    | Some exp, Some value -> assign_exp ctx exp value
    | None, None -> ctx
    | _ -> assert false

  and assign_list_exp (ctx : Ctx.t) (exps : exp list) (values : value list) :
      Ctx.t =
    assign_exps ctx exps values

  and assign_cons_exp (typ_value : typ) (ctx : Ctx.t) (exp_h : exp)
      (exp_t : exp) (values : value list) : Ctx.t =
    let value_h = List.hd values in
    let value_t = Value.Make.list typ_value (List.tl values) in
    let ctx = assign_exp ctx exp_h value_h in
    assign_exp ctx exp_t value_t

  and assign_iter_exp_opt (ctx : Ctx.t) (exp : exp) (vars : var list)
      (value : value) : Ctx.t =
    match Value.Get.opt value with
    | Some inner_value ->
        let ctx = assign_exp ctx exp inner_value in
        List.fold_left
          (fun ctx (id, typ, iters) ->
            let typ = Typ.Make.iterate typ (iters @ [ Opt ]) in
            let inner_value = Ctx.find_value ctx (id, iters) in
            let value_sub = Value.Make.opt typ (Some inner_value) in
            Ctx.add_value ctx (id, iters @ [ Opt ]) value_sub)
          ctx vars
    | None ->
        List.fold_left
          (fun ctx (id, typ, iters) ->
            let typ = Typ.Make.iterate typ (iters @ [ Opt ]) in
            let value_sub = Value.Make.opt typ None in
            Ctx.add_value ctx (id, iters @ [ Opt ]) value_sub)
          ctx vars

  and assign_iter_exp_list (ctx : Ctx.t) (exp : exp) (vars : var list)
      (value : value) : Ctx.t =
    let values = Value.Get.list value in
    let ctx_sub = { ctx with local = { ctx.local with venv = VEnv.empty } } in
    let ctxs = List.map (assign_exp ctx_sub exp) values in
    List.fold_left
      (fun ctx (id, typ, iters) ->
        let typ = Typ.Make.iterate typ (iters @ [ List ]) in
        let values =
          List.map (fun ctx -> Ctx.find_value ctx (id, iters)) ctxs
        in
        let value_sub = Value.Make.list typ values in
        Ctx.add_value ctx (id, iters @ [ List ]) value_sub)
      ctx vars

  and assign_iter_exp (typ_exp : typ) (ctx : Ctx.t) (exp : exp)
      (iterexp : iterexp) (value : value) : Ctx.t =
    match
      is_iter_var_exp (IterE (exp, iterexp) $$ (typ_exp.at, typ_exp.it))
    with
    | Some (id_var, iters_var) -> Ctx.add_value ctx (id_var, iters_var) value
    | None -> (
        let iter, vars = iterexp in
        match iter with
        | Opt -> assign_iter_exp_opt ctx exp vars value
        | List -> assign_iter_exp_list ctx exp vars value)

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
        let _, func = Ctx.find_func ctx_caller id_f in
        Ctx.add_func ctx_callee id func
    | _ ->
        error id.at
          (F.asprintf "cannot assign a value %s to a definition %s"
             (Il.Print.string_of_value ~short:true value)
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

  let rec eval_exp (ctx : Ctx.t) (exp : exp) : value backtrack =
    let typ_note = exp.note $ exp.at in
    match exp.it with
    | BoolE b -> eval_bool_exp typ_note b
    | NumE n -> eval_num_exp typ_note n
    | TextE s -> eval_text_exp typ_note s
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

  and eval_exps (ctx : Ctx.t) (exps : exp list) : value list backtrack =
    match exps with
    | [] -> Ok []
    | exp_h :: exps_t ->
        let* value_h = eval_exp ctx exp_h in
        let* values_t = eval_exps ctx exps_t in
        Ok (value_h :: values_t)

  (* Boolean expression evaluation *)

  and eval_bool_exp (_typ_note : typ) (b : bool) : value backtrack =
    let value_res = Value.Make.bool b in
    Ok value_res

  (* Numeric expression evaluation *)

  and eval_num_exp (_typ_note : typ) (n : Num.t) : value backtrack =
    let value_res = Value.Make.num n in
    Ok value_res

  (* Text expression evaluation *)

  and eval_text_exp (_typ_note : typ) (s : string) : value backtrack =
    let value_res = Value.Make.text s in
    Ok value_res

  (* Variable expression evaluation *)

  and eval_var_exp (_typ_note : typ) (ctx : Ctx.t) (id : id) : value backtrack =
    let value_res = Ctx.find_value ctx (id, []) in
    Ok value_res

  (* Unary expression evaluation *)

  and eval_un_bool (unop : Bool.unop) (value : value) : value =
    match unop with
    | `NotOp -> value |> Value.Get.bool |> not |> Value.Make.bool

  and eval_un_num (unop : Num.unop) (value : value) : value =
    value |> Value.Get.num |> Num.un unop |> Value.Make.num

  and eval_un_exp (_typ_note : typ) (ctx : Ctx.t) (unop : unop) (_optyp : optyp)
      (exp : exp) : value backtrack =
    let* value = eval_exp ctx exp in
    let value_res =
      match unop with
      | #Bool.unop as unop -> eval_un_bool unop value
      | #Num.unop as unop -> eval_un_num unop value
    in
    Ok value_res

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
      (_optyp : optyp) (exp_l : exp) (exp_r : exp) : value backtrack =
    let* value_l = eval_exp ctx exp_l in
    let* value_r = eval_exp ctx exp_r in
    let value_res =
      match binop with
      | #Bool.binop as binop -> eval_bin_bool binop value_l value_r
      | #Num.binop as binop -> eval_bin_num binop value_l value_r
    in
    Ok value_res

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
      (_optyp : optyp) (exp_l : exp) (exp_r : exp) : value backtrack =
    let* value_l = eval_exp ctx exp_l in
    let* value_r = eval_exp ctx exp_r in
    let value_res =
      match cmpop with
      | #Bool.cmpop as cmpop -> eval_cmp_bool cmpop value_l value_r
      | #Num.cmpop as cmpop -> eval_cmp_num cmpop value_l value_r
    in
    Ok value_res

  (* Upcast expression evaluation *)

  and upcast (ctx : Ctx.t) (typ : typ) (value : value) : value =
    match typ.it with
    | NumT `IntT -> (
        match value.it with
        | NumV (`Nat n) -> Value.Make.int n
        | NumV (`Int _) -> value
        | _ -> assert false)
    | VarT (tid, targs) -> (
        let tparams, deftyp = Ctx.find_defined_typdef ctx tid in
        let theta = TIdMap.of_lists tparams targs in
        match deftyp.it with
        | PlainT typ ->
            let typ = Type.Subst.subst_typ theta typ in
            upcast ctx typ value
        | _ -> value)
    | TupleT typs -> (
        match value.it with
        | TupleV values ->
            let values =
              List.fold_left2
                (fun values typ value ->
                  let value = upcast ctx typ value in
                  values @ [ value ])
                [] typs values
            in
            Value.Make.tuple typ values
        | _ -> assert false)
    | IterT (typ, Opt) -> (
        match value.it with
        | OptV value_opt ->
            let value_opt = Option.map (upcast ctx typ) value_opt in
            Value.Make.opt typ value_opt
        | _ -> assert false)
    | IterT (typ, List) -> (
        match value.it with
        | ListV values ->
            let values = List.map (upcast ctx typ) values in
            Value.Make.list typ values
        | _ -> assert false)
    | _ -> value

  and eval_upcast_exp (_typ_note : typ) (ctx : Ctx.t) (typ : typ) (exp : exp) :
      value backtrack =
    let* value = eval_exp ctx exp in
    let value_res = upcast ctx typ value in
    Ok value_res

  (* Downcast expression evaluation *)

  and downcast (ctx : Ctx.t) (typ : typ) (value : value) : value =
    match typ.it with
    | NumT `NatT -> (
        match value.it with
        | NumV (`Nat _) -> value
        | NumV (`Int i) when Bigint.(i >= zero) -> Value.Make.nat i
        | _ -> assert false)
    | VarT (tid, targs) -> (
        let tparams, deftyp = Ctx.find_defined_typdef ctx tid in
        let theta = TIdMap.of_lists tparams targs in
        match deftyp.it with
        | PlainT typ ->
            let typ = Type.Subst.subst_typ theta typ in
            downcast ctx typ value
        | _ -> value)
    | TupleT typs -> (
        match value.it with
        | TupleV values ->
            let values =
              List.fold_left2
                (fun values typ value ->
                  let value = downcast ctx typ value in
                  values @ [ value ])
                [] typs values
            in
            Value.Make.tuple typ values
        | _ -> assert false)
    | IterT (typ, Opt) -> (
        match value.it with
        | OptV value_opt ->
            let value_opt = Option.map (downcast ctx typ) value_opt in
            Value.Make.opt typ value_opt
        | _ -> assert false)
    | IterT (typ, List) -> (
        match value.it with
        | ListV values ->
            let values = List.map (downcast ctx typ) values in
            Value.Make.list typ values
        | _ -> assert false)
    | _ -> value

  and eval_downcast_exp (_typ_note : typ) (ctx : Ctx.t) (typ : typ) (exp : exp)
      : value backtrack =
    let* value = eval_exp ctx exp in
    let value_res = downcast ctx typ value in
    Ok value_res

  (* Subtype check expression evaluation *)

  and eval_sub_exp (_typ_note : typ) (ctx : Ctx.t) (exp : exp) (_typ : typ)
      (subcheck : subcheck) : value backtrack =
    let* value = eval_exp ctx exp in
    let sub =
      Value.Match.check sub_cache (Ctx.find_typdef_opt ctx)
        (Ctx.find_func_signature ctx)
        subcheck value
    in
    let value_res = Value.Make.bool sub in
    Ok value_res

  (* Pattern match check expression evaluation *)

  and eval_match_exp (_typ_note : typ) (ctx : Ctx.t) (exp : exp)
      (pattern : pattern) : value backtrack =
    let* value = eval_exp ctx exp in
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
    Ok value_res

  (* Tuple expression evaluation *)

  and eval_tuple_exp (typ_note : typ) (ctx : Ctx.t) (exps : exp list) :
      value backtrack =
    let* values = eval_exps ctx exps in
    let value_res = Value.Make.tuple typ_note values in
    Ok value_res

  (* Case expression evaluation *)

  and eval_case_exp (typ_note : typ) (ctx : Ctx.t) (notexp : notexp) :
      value backtrack =
    let mixop, exps = Mixfix.split notexp in
    let* values = eval_exps ctx exps in
    let value_res = Value.Make.case typ_note (Mixfix.fill mixop values) in
    Ok value_res

  (* Struct expression evaluation *)

  and eval_str_exp (typ_note : typ) (ctx : Ctx.t) (fields : (atom * exp) list) :
      value backtrack =
    let atoms, exps = List.split fields in
    let* values = eval_exps ctx exps in
    let valuefields = List.combine atoms values in
    let value_res = Value.Make.str typ_note valuefields in
    Ok value_res

  (* Option expression evaluation *)

  and eval_opt_exp (typ_note : typ) (ctx : Ctx.t) (exp_opt : exp option) :
      value backtrack =
    let* value_opt =
      match exp_opt with
      | Some exp ->
          let* value = eval_exp ctx exp in
          Ok (Some value)
      | None -> Ok None
    in
    let value_res = Value.Make.opt typ_note value_opt in
    Ok value_res

  (* List expression evaluation *)

  and eval_list_exp (typ_note : typ) (ctx : Ctx.t) (exps : exp list) :
      value backtrack =
    let* values = eval_exps ctx exps in
    let value_res = Value.Make.list typ_note values in
    Ok value_res

  (* Cons expression evaluation *)

  and eval_cons_exp (typ_note : typ) (ctx : Ctx.t) (exp_h : exp) (exp_t : exp) :
      value backtrack =
    let* value_h = eval_exp ctx exp_h in
    let* value_t = eval_exp ctx exp_t in
    let values_t = Value.Get.list value_t in
    let value_res = Value.Make.list typ_note (value_h :: values_t) in
    Ok value_res

  (* Concatenation expression evaluation *)

  and eval_cat_exp (typ_note : typ) (ctx : Ctx.t) (exp_l : exp) (exp_r : exp) :
      value backtrack =
    let* value_l = eval_exp ctx exp_l in
    let* value_r = eval_exp ctx exp_r in
    let value_res =
      match (value_l.it, value_r.it) with
      | TextV s_l, TextV s_r -> Value.Make.text (s_l ^ s_r)
      | ListV values_l, ListV values_r ->
          Value.Make.list typ_note (values_l @ values_r)
      | _ ->
          error
            (over_region [ exp_l.at; exp_r.at ])
            "concatenation expects either two texts or two lists"
    in
    Ok value_res

  (* Membership expression evaluation *)

  and eval_mem_exp (_typ_note : typ) (ctx : Ctx.t) (exp_e : exp) (exp_s : exp) :
      value backtrack =
    let* value_e = eval_exp ctx exp_e in
    let* value_s = eval_exp ctx exp_s in
    let values_s = Value.Get.list value_s in
    let mem = List.exists (Value.eq value_e) values_s in
    let value_res = Value.Make.bool mem in
    Ok value_res

  (* Length expression evaluation *)

  and eval_len_exp (_typ_note : typ) (ctx : Ctx.t) (exp : exp) : value backtrack
      =
    let* value = eval_exp ctx exp in
    let len =
      match value.it with
      | TextV s -> s |> String.length |> Bigint.of_int
      | ListV values -> values |> List.length |> Bigint.of_int
      | _ ->
          error exp.at
            (F.asprintf
               "length operation expects either a text or a list, but got %s"
               (Il.Print.string_of_value ~short:true value))
    in
    let value_res = Value.Make.nat len in
    Ok value_res

  (* Dot expression evaluation *)

  and eval_dot_exp (_typ_note : typ) (ctx : Ctx.t) (exp_b : exp) (atom : atom) :
      value backtrack =
    let* value_b = eval_exp ctx exp_b in
    let valuefields = Value.Get.str value_b in
    let value_res =
      valuefields
      |> List.find (fun (atom_field, _) -> Atom.eq atom_field.it atom.it)
      |> snd
    in
    Ok value_res

  (* Index expression evaluation *)

  and eval_idx_exp (_typ_note : typ) (ctx : Ctx.t) (exp_b : exp) (exp_i : exp) :
      value backtrack =
    let* value_b = eval_exp ctx exp_b in
    let* value_i = eval_exp ctx exp_i in
    let idx = value_i |> Value.Get.num |> Num.to_int |> Bigint.to_int_exn in
    let value_res =
      match value_b.it with
      | TextV s when idx < 0 || idx >= String.length s ->
          error exp_i.at
            (F.asprintf "index %d out of bounds [0, %d)" idx (String.length s))
      | TextV s ->
          let s = String.get s idx |> String.make 1 in
          Value.Make.text s
      | ListV values when idx < 0 || idx >= List.length values ->
          error exp_i.at
            (F.asprintf "index %d out of bounds [0, %d)" idx
               (List.length values))
      | ListV values -> List.nth values idx
      | _ ->
          error exp_b.at
            (F.asprintf "indexing expects either a text or a list, but got %s"
               (Il.Print.string_of_value ~short:true value_b))
    in
    Ok value_res

  (* Slice expression evaluation *)

  and eval_slice_exp (typ_note : typ) (ctx : Ctx.t) (exp_b : exp) (exp_i : exp)
      (exp_n : exp) : value backtrack =
    let* value_b = eval_exp ctx exp_b in
    let* value_i = eval_exp ctx exp_i in
    let idx_l = value_i |> Value.Get.num |> Num.to_int |> Bigint.to_int_exn in
    let* value_n = eval_exp ctx exp_n in
    let idx_n = value_n |> Value.Get.num |> Num.to_int |> Bigint.to_int_exn in
    let idx_h = idx_l + idx_n in
    let value_res =
      match value_b.it with
      | TextV s when idx_l < 0 || idx_h > String.length s ->
          error exp_i.at
            (F.asprintf "slice [%d, %d) out of bounds [0, %d)" idx_l idx_h
               (String.length s))
      | TextV s ->
          let s_slice = String.sub s idx_l (idx_h - idx_l) in
          Value.Make.text s_slice
      | ListV values when idx_l < 0 || idx_h > List.length values ->
          error exp_n.at
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
          Value.Make.list typ_note values_slice
      | _ ->
          error exp_b.at
            (F.asprintf "slicing expects either a text or a list, but got %s"
               (Il.Print.string_of_value ~short:true value_b))
    in
    Ok value_res

  (* Update expression evaluation *)

  and eval_access_path (ctx : Ctx.t) (value_b : value) (path : path) :
      value backtrack =
    match path.it with
    | RootP -> Ok value_b
    | IdxP (path, exp_i) ->
        let* value = eval_access_path ctx value_b path in
        let* value_i = eval_exp ctx exp_i in
        let idx = value_i |> Value.Get.num |> Num.to_int |> Bigint.to_int_exn in
        let value_res =
          match value.it with
          | TextV s when idx < 0 || idx >= String.length s ->
              error exp_i.at
                (F.asprintf "index %d out of bounds [0, %d)" idx
                   (String.length s))
          | TextV s ->
              let s = String.get s idx |> String.make 1 in
              Value.Make.text s
          | ListV values when idx < 0 || idx >= List.length values ->
              error exp_i.at
                (F.asprintf "index %d out of bounds [0, %d)" idx
                   (List.length values))
          | ListV values -> List.nth values idx
          | _ ->
              error path.at
                (F.asprintf
                   "indexing expects either a text or a list, but got %s"
                   (Il.Print.string_of_value ~short:true value))
        in
        Ok value_res
    | SliceP (path, exp_i, exp_n) ->
        let typ = path.note $ path.at in
        let* value = eval_access_path ctx value_b path in
        let* value_i = eval_exp ctx exp_i in
        let idx_l =
          value_i |> Value.Get.num |> Num.to_int |> Bigint.to_int_exn
        in
        let* value_n = eval_exp ctx exp_n in
        let idx_n =
          value_n |> Value.Get.num |> Num.to_int |> Bigint.to_int_exn
        in
        let idx_h = idx_l + idx_n in
        let value_res =
          match value.it with
          | TextV s when idx_l < 0 || idx_h > String.length s ->
              error exp_n.at
                (F.asprintf "slice [%d, %d) out of bounds [0, %d)" idx_l idx_h
                   (String.length s))
          | TextV s ->
              let s_slice = String.sub s idx_l (idx_h - idx_l) in
              Value.Make.text s_slice
          | ListV values when idx_l < 0 || idx_h > List.length values ->
              error exp_n.at
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
              Value.Make.list typ values_slice
          | _ ->
              error path.at
                (F.asprintf
                   "slicing expects either a text or a list, but got %s"
                   (Il.Print.string_of_value ~short:true value))
        in
        Ok value_res
    | DotP (path, atom) ->
        let* value = eval_access_path ctx value_b path in
        let valuefields = value |> Value.Get.str in
        let value_res =
          valuefields
          |> List.find (fun (atom_field, _) -> Atom.eq atom_field.it atom.it)
          |> snd
        in
        Ok value_res

  and eval_update_path (ctx : Ctx.t) (value_b : value) (path : path)
      (value_upd : value) : value backtrack =
    match path.it with
    | RootP -> Ok value_upd
    | IdxP (path, exp_i) -> (
        let typ = path.note $ path.at in
        let* value = eval_access_path ctx value_b path in
        let* value_i = eval_exp ctx exp_i in
        let idx_target =
          value_i |> Value.Get.num |> Num.to_int |> Bigint.to_int_exn
        in
        match value.it with
        | TextV s when idx_target < 0 || idx_target >= String.length s ->
            error exp_i.at
              (F.asprintf "index %d out of bounds [0, %d)" idx_target
                 (String.length s))
        | TextV s ->
            let s_n = Value.Get.text value_upd in
            if String.length s_n <> 1 then
              error exp_i.at
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
              eval_update_path ctx value_b path value
        | ListV values when idx_target < 0 || idx_target >= List.length values
          ->
            error exp_i.at
              (F.asprintf "index %d out of bounds [0, %d)" idx_target
                 (List.length values))
        | ListV values ->
            let values_updated =
              List.mapi
                (fun idx value -> if idx = idx_target then value_upd else value)
                values
            in
            let value = Value.Make.list typ values_updated in
            eval_update_path ctx value_b path value
        | _ ->
            error path.at
              (F.asprintf "indexing expects either a text or a list, but got %s"
                 (Sl.Print.string_of_value ~short:true value)))
    | SliceP (path, exp_i, exp_n) -> (
        let typ = path.note $ path.at in
        let* value = eval_access_path ctx value_b path in
        let* value_i = eval_exp ctx exp_i in
        let idx_l =
          value_i |> Value.Get.num |> Num.to_int |> Bigint.to_int_exn
        in
        let* value_n = eval_exp ctx exp_n in
        let idx_n =
          value_n |> Value.Get.num |> Num.to_int |> Bigint.to_int_exn
        in
        let idx_h = idx_l + idx_n in
        match value.it with
        | TextV s when idx_l < 0 || idx_h > String.length s ->
            error exp_n.at
              (F.asprintf "slice [%d, %d) out of bounds [0, %d)" idx_l idx_h
                 (String.length s))
        | TextV s ->
            let s_upd = Value.Get.text value_upd in
            if String.length s_upd <> idx_n then
              error exp_n.at
                (F.asprintf
                   "updating a slice of length %d requires a text of length \
                    %d, but got %s"
                   idx_n (String.length s_upd)
                   (Il.Print.string_of_value ~short:true value_upd))
            else
              let s_upd =
                String.sub s 0 idx_l ^ s_upd
                ^ String.sub s idx_h (String.length s - idx_h)
              in
              let value = Value.Make.text s_upd in
              eval_update_path ctx value_b path value
        | ListV values when idx_l < 0 || idx_h > List.length values ->
            error exp_n.at
              (F.asprintf "slice [%d, %d) out of bounds [0, %d)" idx_l idx_h
                 (List.length values))
        | ListV values ->
            let values_upd = Value.Get.list value_upd in
            if List.length values_upd <> idx_n then
              error exp_n.at
                (F.asprintf
                   "updating a slice of length %d requires a list of length \
                    %d, but got %s"
                   idx_n (List.length values_upd)
                   (Il.Print.string_of_value ~short:true value_upd))
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
              eval_update_path ctx value_b path value
        | _ ->
            error path.at
              (F.asprintf "slicing expects either a text or a list, but got %s"
                 (Il.Print.string_of_value ~short:true value)))
    | DotP (path, atom) ->
        let typ = path.note $ path.at in
        let* value = eval_access_path ctx value_b path in
        let valuefields = value |> Value.Get.str in
        let valuefields =
          List.map
            (fun (atom_f, value_f) ->
              if Atom.eq atom_f.it atom.it then (atom_f, value_upd)
              else (atom_f, value_f))
            valuefields
        in
        let value = Value.Make.str typ valuefields in
        eval_update_path ctx value_b path value

  and eval_upd_exp (_typ_note : typ) (ctx : Ctx.t) (exp_b : exp) (path : path)
      (exp_f : exp) : value backtrack =
    let* value_b = eval_exp ctx exp_b in
    let* value_f = eval_exp ctx exp_f in
    eval_update_path ctx value_b path value_f

  (* Function call expression evaluation *)

  and eval_call_exp (_typ_note : typ) (ctx : Ctx.t) (id : id)
      (targs : targ list) (args : arg list) : value backtrack =
    let targs =
      match targs with
      | [] -> []
      | targs ->
          let theta =
            TDEnv.fold
              (fun tid typdef theta ->
                match typdef with
                | Type.Typdef.Defined ([], { it = Il.PlainT typ; _ }) ->
                    TIdMap.add tid typ theta
                | _ -> theta)
              ctx.local.tdenv TIdMap.empty
          in
          List.map (Type.Subst.subst_typ theta) targs
    in
    let* values_args = eval_args ctx args in
    invoke_func ctx id targs values_args

  (* Iterated expression evaluation *)

  and eval_iter_exp_opt (typ_note : typ) (ctx : Ctx.t) (exp : exp)
      (vars : var list) : value backtrack =
    let* ctx_sub_opt = Ctx.sub_opt ctx vars in
    match ctx_sub_opt with
    | Some ctx_sub ->
        let* value = eval_exp ctx_sub exp in
        let value_res = Value.Make.opt typ_note (Some value) in
        Ok value_res
    | None ->
        let value_res = Value.Make.opt typ_note None in
        Ok value_res

  and eval_iter_exp_list (typ_note : typ) (ctx : Ctx.t) (exp : exp)
      (vars : var list) : value backtrack =
    let* ctxs_sub = Ctx.sub_list ctx vars in
    let* values_rev =
      List.fold_left
        (fun values_rev ctx_sub ->
          let* values_rev = values_rev in
          let* value = eval_exp ctx_sub exp in
          Ok (value :: values_rev))
        (Ok []) ctxs_sub
    in
    let values = List.rev values_rev in
    let value_res = Value.Make.list typ_note values in
    Ok value_res

  and eval_iter_exp (typ_note : typ) (ctx : Ctx.t) (exp : exp)
      (iterexp : iterexp) : value backtrack =
    match
      is_iter_var_exp (IterE (exp, iterexp) $$ (typ_note.at, typ_note.it))
    with
    | Some var -> Ok (Ctx.find_value ctx var)
    | None -> (
        let iter, vars = iterexp in
        match iter with
        | Opt -> eval_iter_exp_opt typ_note ctx exp vars
        | List -> eval_iter_exp_list typ_note ctx exp vars)

  (* Argument evaluation *)

  and eval_arg (ctx : Ctx.t) (arg : arg) : value backtrack =
    match arg.it with
    | ExpA exp -> eval_exp ctx exp
    | DefA id ->
        let tparams, typs, typ = Ctx.find_func_signature ctx id in
        let value_res = Value.Make.func id tparams typs typ in
        Ok value_res

  and eval_args (ctx : Ctx.t) (args : arg list) : value list backtrack =
    List.fold_left
      (fun values arg ->
        let* values = values in
        let* value = eval_arg ctx arg in
        Ok (values @ [ value ]))
      (Ok []) args

  (* Premise evaluation *)

  and eval_prem (ctx : Ctx.t) (prem : prem) : Ctx.t backtrack =
    eval_prem' ctx prem

  and eval_prem' (ctx : Ctx.t) (prem : prem) : Ctx.t backtrack =
    match prem.it with
    | RulePr (id, notexp, inputs) -> eval_rule_prem ctx id notexp inputs
    | IfPr exp_cond -> eval_if_prem ctx exp_cond
    | IfHoldPr (id, notexp) -> eval_if_hold_prem ctx id notexp
    | IfNotHoldPr (id, notexp) -> eval_if_not_hold_prem ctx id notexp
    | LetPr (exp_l, exp_r) -> eval_let_prem ctx exp_l exp_r
    | IterPr (prem, iterprem) -> eval_iter_prem ctx prem iterprem
    | DebugPr exp -> eval_debug_prem ctx exp

  and eval_prems (ctx : Ctx.t) (prems : prem list) : Ctx.t backtrack =
    List.fold_left
      (fun ctx prem ->
        let* ctx = ctx in
        eval_prem ctx prem)
      (Ok ctx) prems

  (* Rule premise evaluation *)

  and eval_rule_prem (ctx : Ctx.t) (id : id) (notexp : notexp)
      (inputs : Hints.Input.t) : Ctx.t backtrack =
    let exps = Mixfix.args notexp in
    let exps_input, exps_output = Hints.Input.split inputs exps in
    let* values_input = eval_exps ctx exps_input in
    let* values_output = invoke_rel ctx id values_input in
    let ctx = assign_exps ctx exps_output values_output in
    Ok ctx

  (* If premise evaluation *)

  and eval_if_prem (ctx : Ctx.t) (exp_cond : exp) : Ctx.t backtrack =
    let* value_cond = eval_exp ctx exp_cond in
    let cond = Value.Get.bool value_cond in
    if cond then Ok ctx
    else
      back_unmatch exp_cond.at
        (F.asprintf "condition %s was not met"
           (Il.Print.string_of_exp exp_cond))

  (* If-hold premise evaluation *)

  and eval_if_hold_prem (ctx : Ctx.t) (id : id) (notexp : notexp) :
      Ctx.t backtrack =
    let exps_input = Mixfix.args notexp in
    let* values_input = eval_exps ctx exps_input in
    match invoke_rel ctx id values_input with
    | Ok _ -> Ok ctx
    | Err _ as backtrack -> backtrack
    | Unmatch _ as backtrack ->
        backtrack
        |> back_nest id.at (fun () ->
               F.asprintf "condition hold %s was not met" id.it)

  (* If-not-hold premise evaluation *)

  and eval_if_not_hold_prem (ctx : Ctx.t) (id : id) (notexp : notexp) :
      Ctx.t backtrack =
    let exps_input = Mixfix.args notexp in
    let* values_input = eval_exps ctx exps_input in
    match invoke_rel ctx id values_input with
    | Ok _ ->
        back_unmatch id.at
          (F.asprintf "condition not-hold %s was not met" id.it)
    | Err _ as backtrack -> backtrack
    | Unmatch _ -> Ok ctx

  (* Let premise evaluation *)

  and eval_let_prem (ctx : Ctx.t) (exp_l : exp) (exp_r : exp) : Ctx.t backtrack
      =
    let* value = eval_exp ctx exp_r in
    let ctx = assign_exp ctx exp_l value in
    Ok ctx

  (* Iterated premise evaluation *)

  and eval_iter_prem_opt (ctx : Ctx.t) (prem : prem) (vars_bound : var list)
      (vars_bind : var list) : Ctx.t backtrack =
    (* Create a subcontext for the bound variable *)
    let* ctx_sub_opt = Ctx.sub_opt ctx vars_bound in
    match ctx_sub_opt with
    (* If the bound variable supposed to guide the iteration is already empty,
       then the binding variables are also empty *)
    | None ->
        let ctx =
          List.fold_left
            (fun ctx (id_binding, typ_binding, iters_binding) ->
              let typ =
                Typ.Make.iterate typ_binding (iters_binding @ [ Opt ])
              in
              let value_binding = Value.Make.opt typ None in
              Ctx.add_value ctx
                (id_binding, iters_binding @ [ Opt ])
                value_binding)
            ctx vars_bind
        in
        Ok ctx
    (* Otherwise, evaluate the premise for the bound values *)
    | Some ctx_sub ->
        let* ctx_sub = eval_prem ctx_sub prem in
        let ctx =
          List.fold_left
            (fun ctx (id_binding, typ_binding, iters_binding) ->
              let typ =
                Typ.Make.iterate typ_binding (iters_binding @ [ Opt ])
              in
              let value_binding =
                Ctx.find_value ctx_sub (id_binding, iters_binding)
              in
              let value_binding = Value.Make.opt typ (Some value_binding) in
              Ctx.add_value ctx
                (id_binding, iters_binding @ [ Opt ])
                value_binding)
            ctx vars_bind
        in
        Ok ctx

  and eval_iter_prem_list (ctx : Ctx.t) (prem : prem) (vars_bound : var list)
      (vars_bind : var list) : Ctx.t backtrack =
    (* Create a subcontext for each batch of bound values *)
    let* ctxs_sub = Ctx.sub_list ctx vars_bound in
    let* ctx, values_binding =
      match ctxs_sub with
      (* If the bound variable supposed to guide the iteration is already empty,
         then the binding variables are also empty *)
      | [] ->
          let values_binding =
            List.init (List.length vars_bind) (fun _ -> [])
          in
          Ok (ctx, values_binding)
      (* Otherwise, evaluate the premise for each batch of bound values,
         and collect the resulting binding batches *)
      | _ ->
          let* ctx, values_binding_batch =
            List.fold_left
              (fun ctx_values_binding_batch ctx_sub ->
                let* ctx, values_binding_batch = ctx_values_binding_batch in
                let* ctx_sub = eval_prem ctx_sub prem in
                let value_binding_batch =
                  List.map
                    (fun (id_binding, _typ_binding, iters_binding) ->
                      Ctx.find_value ctx_sub (id_binding, iters_binding))
                    vars_bind
                in
                let values_binding_batch =
                  values_binding_batch @ [ value_binding_batch ]
                in
                Ok (ctx, values_binding_batch))
              (Ok (ctx, []))
              ctxs_sub
          in
          let* values_binding = values_binding_batch |> Ctx.transpose in
          Ok (ctx, values_binding)
    in
    (* Finally, bind the resulting binding batches *)
    let ctx =
      List.fold_left2
        (fun ctx (id_binding, typ_binding, iters_binding) values_binding ->
          let typ = Typ.Make.iterate typ_binding (iters_binding @ [ List ]) in
          let value_binding = Value.Make.list typ values_binding in
          Ctx.add_value ctx (id_binding, iters_binding @ [ List ]) value_binding)
        ctx vars_bind values_binding
    in
    Ok ctx

  and eval_iter_prem (ctx : Ctx.t) (prem : prem) (iterprem : iterprem) :
      Ctx.t backtrack =
    let iter, vars_bound, vars_bind = iterprem in
    match iter with
    | Opt -> eval_iter_prem_opt ctx prem vars_bound vars_bind
    | List -> eval_iter_prem_list ctx prem vars_bound vars_bind

  (* Debug premise evaluation *)

  and eval_debug_prem (ctx : Ctx.t) (exp : exp) : Ctx.t backtrack =
    let* value = eval_exp ctx exp in
    string_of_region exp.at ^ ": " ^ Il.Print.string_of_exp exp |> print_endline;
    let region = string_of_region value.at in
    (if region = "" then "" else region ^ ": ") ^ Il.Print.string_of_value value
    |> print_endline;
    Ok ctx

  (* Invoke a relation *)

  and is_extern_rel (rel : Rel.t) : bool =
    match rel with Rel.Extern _ -> true | Rel.Defined _ -> false

  and match_rule (ctx : Ctx.t) (at : region) (rulematch : rulematch)
      (values_input : value list) : Ctx.t * prem list =
    let _, exps_input, prems_input = rulematch in
    check
      (List.length exps_input = List.length values_input)
      at "arity mismatch in rule";
    let ctx = assign_exps ctx exps_input values_input in
    (ctx, prems_input)

  and invoke_rel ?(internal : bool = true) (ctx : Ctx.t) (id : id)
      (values_input : value list) : value list backtrack =
    Hook.on_rel_enter id values_input;
    let result =
      let rel = Ctx.find_rel ctx id in
      let dispatch () =
        match rel with
        | Rel.Extern (nottyp, inputs) ->
            invoke_extern_rel ctx id nottyp inputs values_input
        | Rel.Defined (_, _, rulegroups, elsegroup_opt) ->
            invoke_defined_rel ctx id rulegroups elsegroup_opt values_input
      in
      if !cache_enabled && not (is_extern_rel rel) then (
        let cache_result = CCache.find !rel_cache (id.it, values_input) in
        match cache_result with
        | Some values_output -> Ok values_output
        | None ->
            let checkpoint_before = Interface.checkpoint () in
            let extern_checkpoint_before = Extern.checkpoint () in
            let* values_output = dispatch () in
            let checkpoint_after = Interface.checkpoint () in
            let extern_checkpoint_after = Extern.checkpoint () in
            (* Cache if neither the interface nor the extern created a side-effect *)
            if
              (not (Interface.seff checkpoint_before checkpoint_after))
              && not
                   (Extern.seff extern_checkpoint_before extern_checkpoint_after)
            then CCache.add !rel_cache (id.it, values_input) values_output;
            Ok values_output)
      else (
        if not internal then check_rel_inputs ctx id values_input;
        dispatch ())
    in
    Hook.on_rel_exit id;
    result
    |> back_nest id.at (fun () ->
           F.asprintf "invocation of relation %s failed" id.it)

  and invoke_extern_rel (ctx : Ctx.t) (id : id) (nottyp : nottyp)
      (inputs : Hints.Input.t) (values_input : value list) :
      value list backtrack =
    let* values_output =
      match Extern.eval_extern_rel id.it values_input with
      | Pass values -> Ok values
      | Fail (at, msg) -> back_err at msg
    in
    check_rel_outputs ctx id nottyp inputs values_output;
    Ok values_output

  and invoke_defined_rel (ctx : Ctx.t) (id : id) (rulegroups : rulegroup list)
      (elsegroup_opt : elsegroup option) (values_input : value list) :
      value list backtrack =
    (* Backtrack a rule path *)
    let do_backtrack_rulepath_inner (ctx : Ctx.t) (_id_rulegroup : id)
        (rulematch : rulematch) (id_rulepath : id) (prems : prem list)
        (exps_output : exp list) : unit -> value list backtrack =
     fun () ->
      (* Create a subtrace for the rule path *)
      let ctx_local = Ctx.localize ctx in
      (* Try matching the rule *)
      let ctx_local, prems_input =
        match_rule ctx_local id_rulepath.at rulematch values_input
      in
      (* Evaluate the premises *)
      let* ctx_local = eval_prems ctx_local (prems_input @ prems) in
      (* Evaluate the output expressions *)
      let* values_output = eval_exps ctx_local exps_output in
      Ok values_output
    in
    let do_backtrack_rulepath (ctx : Ctx.t) (id_rulegroup : id)
        (rulematch : rulematch) (rulepath : rulepath) :
        unit -> value list backtrack =
     fun () ->
      let id_rulepath, prems, exps_output = rulepath in
      do_backtrack_rulepath_inner ctx id_rulegroup rulematch id_rulepath prems
        exps_output ()
      |> back_nest id.at (fun () ->
             F.asprintf "application of rule %s/%s/%s failed" id.it
               id_rulegroup.it id_rulepath.it)
    in
    (* Backtrack a rule group *)
    let do_backtrack_rulegroup (ctx : Ctx.t) (rulegroup : rulegroup) :
        (unit -> value list backtrack) list =
      let id_rulegroup, rulematch, rulepaths = rulegroup.it in
      rulepaths |> List.map (do_backtrack_rulepath ctx id_rulegroup rulematch)
    in
    (* Backtrack an else group *)
    let do_backtrack_elsegroup (ctx : Ctx.t) (elsegroup : elsegroup) :
        value list backtrack =
      let id_rulegroup, rulematch, rulepath = elsegroup.it in
      (do_backtrack_rulepath ctx id_rulegroup rulematch rulepath) ()
    in
    let ids_path =
      rulegroups
      |> List.concat_map (fun rulegroup ->
             let id_rulegroup, _, rulepaths = rulegroup.it in
             rulepaths
             |> List.map (fun rulepath ->
                    let id_rulepath, _, _ = rulepath in
                    F.asprintf "%s/%s" id_rulegroup.it id_rulepath.it))
    in
    let backtracks_path =
      rulegroups |> List.concat_map (do_backtrack_rulegroup ctx)
    in
    let backtrack_det =
      if !Ctx.is_det then backtracks_path |> choose_deterministic ids_path
      else backtracks_path |> choose_sequential |> as_det
    in
    match backtrack_det with
    | Ok_det values_output -> Ok values_output
    | Err_det failtraces -> Err failtraces
    | Unmatch_det failtraces -> (
        match elsegroup_opt with
        | Some elsegroup -> do_backtrack_elsegroup ctx elsegroup
        | None -> Unmatch failtraces)
    | Nondet_det (id_path_a, id_path_b) ->
        back_err id.at
          (F.asprintf "non-deterministic application of relation %s: %s, %s"
             id.it id_path_a id_path_b)

  (* Invoke a function *)

  and is_extern_func (func : Func.t) : bool =
    match func with Func.Extern _ -> true | _ -> false

  and is_high_order_func (values_input : value list) : bool =
    List.exists
      (fun value_input ->
        match value_input.it with Il.FuncV _ -> true | _ -> false)
      values_input

  and invoke_func ?(internal : bool = true) (ctx : Ctx.t) (id : id)
      (targs : targ list) (values_input : value list) : value backtrack =
    Hook.on_func_enter id values_input;
    (* Find the function *)
    let cursor, func = Ctx.find_func ctx id in
    let anon = cursor = Ctx.Local in
    let result =
      if
        !cache_enabled && (not anon)
        && (not (is_extern_func func))
        && not (is_high_order_func values_input)
      then (
        let cache_result = CCache.find !func_cache (id.it, values_input) in
        match cache_result with
        | Some value_output -> Ok value_output
        | None ->
            let checkpoint_before = Interface.checkpoint () in
            let extern_checkpoint_before = Extern.checkpoint () in
            let* value_output =
              invoke_func_body ctx id func targs values_input
            in
            let checkpoint_after = Interface.checkpoint () in
            let extern_checkpoint_after = Extern.checkpoint () in
            (* Cache if neither the interface nor the extern created a side-effect *)
            if
              (not (Interface.seff checkpoint_before checkpoint_after))
              && not
                   (Extern.seff extern_checkpoint_before extern_checkpoint_after)
            then CCache.add !func_cache (id.it, values_input) value_output;
            Ok value_output)
      else (
        if not internal then check_func_inputs ctx id targs values_input;
        invoke_func_body ctx id func targs values_input)
    in
    Hook.on_func_exit id;
    result
    |> back_nest id.at (fun () ->
           F.asprintf "invocation of function %s%s failed"
             (Il.Print.string_of_defid id)
             (Il.Print.string_of_targs targs))

  and invoke_func_body (ctx : Ctx.t) (id : id) (func : Func.t)
      (targs : targ list) (values_input : value list) : value backtrack =
    match func with
    | Func.Extern (tparams, _, typ) ->
        invoke_extern_func ctx id tparams targs values_input typ
    | Func.Builtin (tparams, _, typ) ->
        invoke_builtin_func ctx id tparams targs values_input typ
    | Func.Table (_, _, tablerows) ->
        invoke_table_func ctx id tablerows values_input
    | Func.Defined (tparams, _, _, clauses, elseclause_opt) ->
        invoke_defined_func ctx id tparams clauses elseclause_opt targs
          values_input

  and invoke_extern_func (ctx : Ctx.t) (id : id) (tparams : tparam list)
      (targs : targ list) (values_input : value list) (typ_output : typ) :
      value backtrack =
    let* value_output =
      match Extern.eval_extern_func id.it [] values_input with
      | Pass value -> Ok value
      | Fail (at, msg) -> back_err at msg
    in
    check_func_output ctx id tparams typ_output targs value_output;
    Ok value_output

  and invoke_builtin_func (ctx : Ctx.t) (id : id) (tparams : tparam list)
      (targs : targ list) (values_input : value list) (typ_output : typ) :
      value backtrack =
    try
      let value_output =
        Interface.call_builtin (fun _ -> ()) id targs values_input
      in
      check_func_output ctx id tparams typ_output targs value_output;
      Ok value_output
    with Builtin.Error.BuiltinError (at, msg) -> back_unmatch at msg

  and match_tablerow (ctx_caller : Ctx.t) (ctx_callee : Ctx.t)
      (tablerow : tablerow) (values_input : value list) :
      Ctx.t * arg list * prem list * exp =
    let _, args_input, exp_output, prems = tablerow.it in
    check
      (List.length args_input = List.length values_input)
      tablerow.at "arity mismatch while matching table row";
    let ctx = assign_args ctx_caller ctx_callee args_input values_input in
    (ctx, args_input, prems, exp_output)

  and invoke_table_func (ctx : Ctx.t) (id : id) (tablerows : tablerow list)
      (values_input : value list) : value backtrack =
    let backtrack_tablerow' (ctx_local : Ctx.t) (prems : prem list)
        (exp_output : exp) : value backtrack =
      let* ctx_local = eval_prems ctx_local prems in
      let* value_output = eval_exp ctx_local exp_output in
      Ok value_output
    in
    tablerows
    |> List.mapi (fun _idx_row tablerow ->
           let backtrack_tablerow () : value backtrack =
             (* Create a subtrace for the table row *)
             let ctx_local = Ctx.localize ctx in
             (* Try to match the table row *)
             let ctx_local, args_input, prems, exp_output =
               match_tablerow ctx ctx_local tablerow values_input
             in
             (* Try evaluating the row *)
             backtrack_tablerow' ctx_local prems exp_output
             |> back_nest id.at (fun () ->
                    F.asprintf "application of table row %s%s failed" id.it
                      (Il.Print.string_of_args args_input))
           in
           backtrack_tablerow)
    |> choose_sequential

  and match_clause (ctx_caller : Ctx.t) (ctx_callee : Ctx.t) (clause : clause)
      (values_input : value list) : Ctx.t * arg list * prem list * exp =
    let args_input, exp_output, prems = clause.it in
    check
      (List.length args_input = List.length values_input)
      clause.at "arity mismatch while matching clause";
    let ctx = assign_args ctx_caller ctx_callee args_input values_input in
    (ctx, args_input, prems, exp_output)

  and invoke_defined_func (ctx : Ctx.t) (id : id) (tparams : tparam list)
      (clauses : clause list) (elseclause_opt : elseclause option)
      (targs : targ list) (values_input : value list) : value backtrack =
    (* Backtrack a clause *)
    let do_backtrack_clause_inner (ctx : Ctx.t) (_idx_clause : int)
        (clause : clause) : unit -> value backtrack =
     fun () ->
      (* Create a subtrace for the clause *)
      let ctx_local = Ctx.localize ctx in
      (* Add type arguments to the context *)
      check
        (List.length targs = List.length tparams)
        id.at "arity mismatch in type arguments";
      let ctx_local =
        List.fold_left2
          (fun ctx_local tparam targ ->
            let td = Type.Typdef.Defined ([], PlainT targ $ targ.at) in
            Ctx.add_typdef ctx_local tparam td)
          ctx_local tparams targs
      in
      (* Try matching the clause *)
      let ctx_local, _args_input, prems, exp_output =
        match_clause ctx ctx_local clause values_input
      in
      (* Try evaluating the clause *)
      let* ctx_local = eval_prems ctx_local prems in
      (* Evaluate the output expression *)
      let* value_output = eval_exp ctx_local exp_output in
      Ok value_output
    in
    let do_backtrack_clause (ctx : Ctx.t) (idx_clause : int) (clause : clause) :
        unit -> value backtrack =
     fun () ->
      do_backtrack_clause_inner ctx idx_clause clause ()
      |> back_nest id.at (fun () ->
             F.asprintf "application of clause %s%s failed" id.it
               (Il.Print.string_of_args
                  (let args_input, _, _ = clause.it in
                   args_input)))
    in
    let idxs_clause = clauses |> List.mapi (fun idx _ -> idx) in
    let backtracks_clause =
      clauses
      |> List.mapi (fun idx clause -> do_backtrack_clause ctx idx clause)
    in
    let backtrack_det =
      if !Ctx.is_det then backtracks_clause |> choose_deterministic idxs_clause
      else backtracks_clause |> choose_sequential |> as_det
    in
    match backtrack_det with
    | Ok_det value_output -> Ok value_output
    | Err_det failtraces -> Err failtraces
    | Unmatch_det failtraces -> (
        match elseclause_opt with
        | Some elseclause -> do_backtrack_clause ctx (-1) elseclause ()
        | None -> Unmatch failtraces)
    | Nondet_det (idx_clause_a, idx_clause_b) ->
        back_err id.at
          (F.asprintf "non-deterministic application of function %s: %d, %d"
             id.it idx_clause_a idx_clause_b)

  (* Entry points for evaluation *)

  let clear () : unit =
    CCache.empty !func_cache;
    CCache.empty !rel_cache;
    Hashtbl.clear sub_cache

  let do_eval_rel (relname : string) (values_input : value list) :
      value list backtrack =
    let ctx = Ctx.empty in
    invoke_rel ~internal:false ctx (relname $ no_region) values_input

  let do_eval_func (funcname : string) (targs : targ list)
      (values_input : value list) : value backtrack =
    let ctx = Ctx.empty in
    invoke_func ~internal:false ctx (funcname $ no_region) targs values_input

  let eval_program (relname : string) (includes_p4 : string list)
      (path_p4 : string) : Run.program_result =
    clear ();
    try
      let parse_result = Interface.parse_program includes_p4 [ path_p4 ] in
      match parse_result with
      | Pass value_program ->
          Hook.on_program value_program;
          let+ values_output = do_eval_rel relname [ value_program ] in
          (Run.Pass values_output : Run.program_result)
      | Fail (`Syntax (at, msg)) -> Run.Fail (`Syntax (at, msg))
    with
    | P4.Error.ParseError (at, msg) -> Run.Fail (`Syntax (at, msg))
    | Interp_common.Error.InterpError (at, msg) | Run.ExternError (at, msg) ->
        Run.Fail (`Runtime (at, msg))

  let eval_rel (relname : string) (values_input : value list) : Run.rel_result =
    clear ();
    try
      let+ values_output = do_eval_rel relname values_input in
      (Run.Pass values_output : Run.rel_result)
    with
    | Interp_common.Error.InterpError (at, msg) | Run.ExternError (at, msg) ->
      Run.Fail (at, msg)

  let eval_func (funcname : string) (targs : targ list)
      (values_input : value list) : Run.func_result =
    clear ();
    try
      let+ value_output = do_eval_func funcname targs values_input in
      (Run.Pass value_output : Run.func_result)
    with
    | Interp_common.Error.InterpError (at, msg) | Run.ExternError (at, msg) ->
      Run.Fail (at, msg)

  (* Initialization *)

  let init ~(cache : bool) ~(det : bool) ~(guard : bool) (spec : spec) :
      (unit, Run.error) result =
    if cache then Cache.cache_on () else Cache.cache_off ();
    check_guard := guard;
    try Ok (Ctx.init ~det spec)
    with Interp_common.Error.InterpError (at, msg) -> Error { Run.at; msg }
end
