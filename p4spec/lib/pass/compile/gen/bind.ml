open Domain.Lib
open Lang
open Sl
open Runtime.Dynamic_OCaml
open Error
open Util.Source

(* Binder *)

module Binder = struct
  type t = Ml.expr -> Ml.expr

  let nop : t = fun expr_ml -> expr_ml

  let make_let (pat_ml : Ml.pat) (expr_ml : Ml.expr) : t =
   fun (expr_body_ml : Ml.expr) -> Ml.LetE (pat_ml, expr_ml, expr_body_ml)

  let make_match (expr_scrut_ml : Ml.expr) (pat_then_ml : Ml.pat) : t =
   fun (expr_body_ml : Ml.expr) ->
    let arm_then_ml = (pat_then_ml, expr_body_ml) in
    let arm_else_ml =
      let pat_else_ml = Ml.WildP in
      let expr_else_ml = Common.raise_unmatch "binding pattern failed" in
      (pat_else_ml, expr_else_ml)
    in
    Ml.MatchE (expr_scrut_ml, [ arm_then_ml; arm_else_ml ])

  let make_option_map (expr_map_ml : Ml.expr) (id_iter_ml : Ml.id) : t =
   fun (expr_body_ml : Ml.expr) ->
    let expr_fun_ml = Ml.FunE ([ Ml.VarP id_iter_ml ], expr_body_ml) in
    Ml.AppE (Ml.VarE "Option.map", [ expr_fun_ml; expr_map_ml ])

  let make_list_map (expr_map_ml : Ml.expr) (id_iter_ml : Ml.id) : t =
   fun (expr_body_ml : Ml.expr) ->
    let expr_fun_ml = Ml.FunE ([ Ml.VarP id_iter_ml ], expr_body_ml) in
    Ml.AppE (Ml.VarE "List.map", [ expr_fun_ml; expr_map_ml ])

  let connect (binders : t list) : t =
    List.fold_right
      (fun binder binder_acc (expr : Ml.expr) -> expr |> binder_acc |> binder)
      binders
      (fun expr -> expr)

  let apply (binder : t) (expr : Ml.expr) : Ml.expr = binder expr
end

(* Binding *)

type binding = Var.t * Ml.id

(* Helpers *)

let compile_blk_header (ctx : Ctx.t) : Ctx.t = Ctx.push ctx

let compile_blk_footer (ctx : Ctx.t) (binder : Binder.t)
    (bindings : binding list) : Ctx.t * Ml.expr =
  let ids_bind_ml = List.map (fun (_, id_bind_ml) -> id_bind_ml) bindings in
  let expr_bind_ml =
    let exprs_bind_ml =
      List.map (fun id_bind_ml -> Ml.VarE id_bind_ml) ids_bind_ml
    in
    let expr_bind_ml = Ml.TupleE exprs_bind_ml in
    Binder.apply binder expr_bind_ml
  in
  let ctx = Ctx.pop ctx in
  (ctx, expr_bind_ml)

(* Binding *)

let rec compile_binding (ctx : Ctx.t) (expr_stub_ml : Ml.expr) (exp : exp) :
    Ctx.t * binding list * Binder.t =
  match exp.it with
  | VarE id -> compile_var_binding ctx expr_stub_ml id
  | TupleE exps -> compile_tuple_binding ctx expr_stub_ml exps
  | CaseE notexp -> compile_case_binding ctx expr_stub_ml notexp
  | StrE expfields -> compile_str_binding ctx expr_stub_ml expfields
  | OptE exp_opt -> compile_opt_binding ctx expr_stub_ml exp_opt
  | ListE exps -> compile_list_binding ctx expr_stub_ml exps
  | ConsE (exp_h, exp_t) -> compile_cons_binding ctx expr_stub_ml exp_h exp_t
  | IterE (exp, iterexp) -> compile_iter_binding ctx expr_stub_ml exp iterexp
  | _ ->
      error exp.at
        (Format.asprintf "unsupported binding expression: %s"
           (Sl.Print.string_of_exp exp))

and compile_bindings (ctx : Ctx.t) (exprs_stub_ml : Ml.expr list)
    (exps : exp list) : Ctx.t * binding list * Binder.t =
  match (exprs_stub_ml, exps) with
  | [], [] -> (ctx, [], fun expr -> expr)
  | expr_stub_h_ml :: exprs_stub_t_ml, exp_h :: exps_t ->
      let ctx, bindings_h, binder_h =
        compile_binding ctx expr_stub_h_ml exp_h
      in
      let ctx, bindings_t, binder_t =
        compile_bindings ctx exprs_stub_t_ml exps_t
      in
      let bindings = bindings_h @ bindings_t in
      let binder = Binder.connect [ binder_h; binder_t ] in
      (ctx, bindings, binder)
  | _, [] | [], _ -> assert false

(* Variable binding: [x] on stub [expr]

   Compiled to no-op *)

and compile_var_binding (ctx : Ctx.t) (expr_stub_ml : Ml.expr) (id : id) :
    Ctx.t * binding list * Binder.t =
  let id_ml = Names.var_of_id id in
  let binding = ((id, []), id_ml) in
  let binder = Binder.make_let (Ml.VarP id_ml) expr_stub_ml in
  (ctx, [ binding ], binder)

(* Tuple binding: [(exp_a, exp_b, ..., exp_z)] on stub [expr]

   Compiled to
    [
      let (..._a, ..., ..._z) =
        let (expr_a, expr_b, ..., expr_z) = expr in
        let ..._a = ... compiled exp_a on expr_a ... in
        ...
        let ..._z = ... compiled exp_z on expr_z ... in
        (..._a, ..., ..._z)
      in
    ] *)

and compile_tuple_binding (ctx : Ctx.t) (expr_stub_ml : Ml.expr)
    (exps : exp list) : Ctx.t * binding list * Binder.t =
  (* Create nested block *)
  let ctx_inner = compile_blk_header ctx in
  (* Create stub expressions for tuple elements *)
  let ctx_inner, ids_stub_ml =
    exps
    |> List.mapi (fun idx _ -> "tuple__" ^ string_of_int idx)
    |> List.fold_left
         (fun (ctx_inner, ids_ml) id_ml ->
           let ctx_inner, id_ml = Ctx.fresh ctx_inner id_ml in
           (ctx_inner, ids_ml @ [ id_ml ]))
         (ctx_inner, [])
  in
  let exprs_stub_ml =
    List.map (fun id_stub_ml -> Ml.VarE id_stub_ml) ids_stub_ml
  in
  (* Create [let (expr_a, ..., expr_z) = expr_stub in ...] *)
  let binder_tuple =
    let pats_ml = List.map (fun id_stub_ml -> Ml.VarP id_stub_ml) ids_stub_ml in
    let pat_ml = Ml.TupleP pats_ml in
    Binder.make_let pat_ml expr_stub_ml
  in
  (* Create binders for tuple elements *)
  let ctx_inner, bindings, binder_elems =
    compile_bindings ctx_inner exprs_stub_ml exps
  in
  (* Connect binders *)
  let binder = Binder.connect [ binder_tuple; binder_elems ] in
  (* Finish nested block with bindings *)
  let ctx, expr_bind_ml = compile_blk_footer ctx_inner binder bindings in
  (* Create [let (..._a, ..., ..._z) = ... in ...] *)
  let binder =
    let ids_bind_ml = List.map (fun (_, id_bind_ml) -> id_bind_ml) bindings in
    let pats_ml = List.map (fun id_bind_ml -> Ml.VarP id_bind_ml) ids_bind_ml in
    let pat_ml = Ml.TupleP pats_ml in
    Binder.make_let pat_ml expr_bind_ml
  in
  (ctx, bindings, binder)

(* Case binding: [mixop (exp_a, exp_b, ..., exp_z)] on stub [expr] *)

and compile_case_binding (_ctx : Ctx.t) (_expr_stub_ml : Ml.expr)
    (_notexp : notexp) : Ctx.t * binding list * Binder.t =
  error no_region "case binding is not supported"

(* Struct binding *)

and compile_str_binding (_ctx : Ctx.t) (_expr_stub_ml : Ml.expr)
    (_expfields : (atom * exp) list) : Ctx.t * binding list * Binder.t =
  error no_region "struct binding is not supported"

(* Option binding: [?exp] on stub [expr]

   If exp is Some exp_inner,
    [
      let ... =
        match expr with
        | Some expr_inner ->
            ... compiled exp_inner on expr_inner ...
        | None -> raise Unmatch
      in
    ]

   If exp is None,
    [
      let ... =
        match expr with
        | Some _ -> raise Unmatch
        | None -> ...
      in
    ] *)

and compile_opt_binding (ctx : Ctx.t) (expr_stub_ml : Ml.expr)
    (exp_opt : exp option) : Ctx.t * binding list * Binder.t =
  (* Create nested block *)
  let ctx_inner = compile_blk_header ctx in
  let ctx, bindings, expr_bind_ml =
    match exp_opt with
    | Some exp ->
        (* Create stub expression for option element *)
        let ctx_inner, id_stub_ml =
          let id_stub_ml = "opt__" in
          Ctx.fresh ctx_inner id_stub_ml
        in
        let expr_stub_ml = Ml.VarE id_stub_ml in
        (* Create [match expr with Some expr_inner -> ...] *)
        let binder_some =
          let pat_then_ml = Ml.OptP (Some (Ml.VarP id_stub_ml)) in
          Binder.make_match expr_stub_ml pat_then_ml
        in
        (* Create binder for option element *)
        let ctx_inner, bindings, binder_then =
          compile_binding ctx_inner expr_stub_ml exp
        in
        (* Connect binders *)
        let binder = Binder.connect [ binder_some; binder_then ] in
        (* Finish nested block with bindings *)
        let ctx, expr_bind_ml = compile_blk_footer ctx_inner binder bindings in
        (ctx, bindings, expr_bind_ml)
    | None ->
        (* Create [match expr with None -> ...] *)
        let binder_none =
          let pat_then_ml = Ml.OptP None in
          Binder.make_match expr_stub_ml pat_then_ml
        in
        (* Finish nested block with bindings *)
        let ctx, expr_bind_ml = compile_blk_footer ctx_inner binder_none [] in
        (ctx, [], expr_bind_ml)
  in
  (* Create [let (..._a, ..., ..._z) = ... in ...] *)
  let binder =
    let ids_bind_ml = List.map (fun (_, id_bind_ml) -> id_bind_ml) bindings in
    let pats_ml = List.map (fun id_bind_ml -> Ml.VarP id_bind_ml) ids_bind_ml in
    let pat_ml = Ml.TupleP pats_ml in
    Binder.make_let pat_ml expr_bind_ml
  in
  (ctx, bindings, binder)

(* List binding: [[exp_a; exp_b; ...; exp_z]] on stub [expr]

   Compiled to
    [
      let (..._a, ..., ..._z) =
        match expr with
        | [ expr_a; ...; expr_z ] ->
            let ..._a = ... compiled exp_a on expr_a ... in
            ...
            let ..._z = ... compiled exp_z on expr_z ... in
            (..._a, ..., ..._z)
        | _ -> raise Unmatch
      in
    ] *)

and compile_list_binding (_ctx : Ctx.t) (expr_stub_ml : Ml.expr)
    (exps : exp list) : Ctx.t * binding list * Binder.t =
  (* Create nested block *)
  let ctx_inner = compile_blk_header _ctx in
  (* Create stub expressions for list elements *)
  let ctx_inner, ids_stub_ml =
    exps
    |> List.mapi (fun idx _ -> "list__" ^ string_of_int idx)
    |> List.fold_left
         (fun (ctx_inner, ids_ml) id_ml ->
           let ctx_inner, id_ml = Ctx.fresh ctx_inner id_ml in
           (ctx_inner, ids_ml @ [ id_ml ]))
         (ctx_inner, [])
  in
  let exprs_stub_ml =
    List.map (fun id_stub_ml -> Ml.VarE id_stub_ml) ids_stub_ml
  in
  (* Create [match expr with [ expr_a; ...; expr_z ] -> ...] *)
  let binder_list =
    let pats_ml = List.map (fun id_stub_ml -> Ml.VarP id_stub_ml) ids_stub_ml in
    let pat_ml = Ml.ListP pats_ml in
    Binder.make_let pat_ml expr_stub_ml
  in
  (* Create binders for list elements *)
  let ctx_inner, bindings, binder_elems =
    compile_bindings ctx_inner exprs_stub_ml exps
  in
  (* Connect binders *)
  let binder = Binder.connect [ binder_list; binder_elems ] in
  (* Finish nested block with bindings *)
  let ctx, expr_bind_ml = compile_blk_footer ctx_inner binder bindings in
  (* Create [let (..._a, ..., ..._z) = ... in ...] *)
  let binder =
    let ids_bind_ml = List.map (fun (_, id_bind_ml) -> id_bind_ml) bindings in
    let pats_ml = List.map (fun id_bind_ml -> Ml.VarP id_bind_ml) ids_bind_ml in
    let pat_ml = Ml.TupleP pats_ml in
    Binder.make_let pat_ml expr_bind_ml
  in
  (ctx, bindings, binder)

(* Cons binding: [exp_h :: exp_t] on stub [expr]

   Compiled to
    [
      let (..._ah, ..., ..._at, ...) =
        match expr with
        | expr_h :: expr_t ->
            let ..._ah, ... = ... compiled exp_h on expr_h ... in
            let ..._at, ... = ... compiled exp_t on expr_t ... in
            (..._ah, ..., ..._at, ...)
        | _ -> raise Unmatch
      in
    ] *)

and compile_cons_binding (ctx : Ctx.t) (expr_stub_ml : Ml.expr) (exp_h : exp)
    (exp_t : exp) : Ctx.t * binding list * Binder.t =
  (* Create nested block *)
  let ctx_inner = compile_blk_header ctx in
  (* Create stub expressions for head and tail *)
  let ctx_inner, id_h_stub_ml =
    let id_h_stub_ml = "cons__h" in
    Ctx.fresh ctx_inner id_h_stub_ml
  in
  let ctx_inner, id_t_stub_ml =
    let id_t_stub_ml = "cons__t" in
    Ctx.fresh ctx_inner id_t_stub_ml
  in
  let expr_h_stub_ml = Ml.VarE id_h_stub_ml in
  let expr_t_stub_ml = Ml.VarE id_t_stub_ml in
  (* Create [match expr with expr_h :: expr_t -> ...] *)
  let binder_cons =
    let pat_then_ml = Ml.ConsP (Ml.VarP id_h_stub_ml, Ml.VarP id_t_stub_ml) in
    Binder.make_match expr_stub_ml pat_then_ml
  in
  (* Create binders for head and tail *)
  let ctx_inner, bindings, binder_elems =
    compile_bindings ctx_inner
      [ expr_h_stub_ml; expr_t_stub_ml ]
      [ exp_h; exp_t ]
  in
  (* Connect binders *)
  let binder = Binder.connect [ binder_cons; binder_elems ] in
  (* Finish nested block with bindings *)
  let ctx, expr_bind_ml = compile_blk_footer ctx_inner binder bindings in
  (* Create [let (..._ah, ..., ..._at, ...) = ... in ...] *)
  let binder =
    let ids_bind_ml = List.map (fun (_, id_bind_ml) -> id_bind_ml) bindings in
    let pats_ml = List.map (fun id_bind_ml -> Ml.VarP id_bind_ml) ids_bind_ml in
    let pat_ml = Ml.TupleP pats_ml in
    Binder.make_let pat_ml expr_bind_ml
  in
  (ctx, bindings, binder)

(* Iter binding *)

and is_iter_var_exp (exp : exp) : Var.t option =
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

(* Iter option binding: [exp?] on stub [expr]

   Compiled to
    [
      let ..._a, ..._b, ..., ..._z =
        Option.map
          (fun expr_stub_inner ->
            ... compiled exp_inner on expr_stub_inner ...)
          expr
        |> Option.splitN
      in
    ]
    where N = number of bindings from compiling exp_inner on expr_inner *)

and compile_iter_opt_binding (ctx : Ctx.t) (expr_stub_ml : Ml.expr) (exp : exp)
    : Ctx.t * binding list * Binder.t =
  (* Create nested block *)
  let ctx_inner = compile_blk_header ctx in
  (* Create stub expression for option element *)
  let ctx_inner, id_stub_iter_ml =
    let id_stub_iter_ml = "iter__opt" in
    Ctx.fresh ctx_inner id_stub_iter_ml
  in
  let expr_stub_iter_ml = Ml.VarE id_stub_iter_ml in
  (* Create [Option.map (fun expr_stub_inner -> ...) expr] *)
  let binder_map = Binder.make_option_map expr_stub_ml id_stub_iter_ml in
  (* Create binders for option element *)
  let ctx_inner, bindings, binder_elem =
    compile_binding ctx_inner expr_stub_iter_ml exp
  in
  (* Connect binders *)
  let binder = Binder.connect [ binder_map; binder_elem ] in
  (* Finish map block with bindings *)
  let ctx, expr_bind_ml = compile_blk_footer ctx_inner binder bindings in
  (* Create [Option.splitN ...] *)
  let ctx, expr_split_ml =
    let arity = List.length bindings in
    let ctx = Ctx.add_opt_arity ctx arity in
    let id_split_ml = "Option.split" ^ string_of_int arity in
    let expr_split_ml = Ml.AppE (Ml.VarE id_split_ml, [ expr_bind_ml ]) in
    (ctx, expr_split_ml)
  in
  (* Create [let (..._ah, ..., ..._at, ...) = ... in ...] *)
  let binder =
    let ids_bind_ml =
      List.map (fun (_, id_bind_ml) -> id_bind_ml ^ "__quest") bindings
    in
    let pats_ml = List.map (fun id_bind_ml -> Ml.VarP id_bind_ml) ids_bind_ml in
    let pat_ml = Ml.TupleP pats_ml in
    Binder.make_let pat_ml expr_split_ml
  in
  (ctx, bindings, binder)

(* Iter list binding: [exp*] on stub [expr]

   Compiled to
    [
      let ..._a, ..._b, ..., ..._z =
        List.map
          (fun expr_inner ->
            ... compiled exp_inner on expr_inner ...)
          expr
        |> List.splitN
      in
    ]
    where N = number of bindings from compiling exp_inner on expr_inner *)

and compile_iter_list_binding (ctx : Ctx.t) (expr_stub_ml : Ml.expr) (exp : exp)
    : Ctx.t * binding list * Binder.t =
  (* Create nested block *)
  let ctx_inner = compile_blk_header ctx in
  (* Create stub expression for list element *)
  let ctx_inner, id_stub_iter_ml =
    let id_stub_iter_ml = "iter__list" in
    Ctx.fresh ctx_inner id_stub_iter_ml
  in
  let expr_stub_iter_ml = Ml.VarE id_stub_iter_ml in
  (* Create [List.map (fun expr_stub_inner -> ...) expr] *)
  let binder_map = Binder.make_list_map expr_stub_ml id_stub_iter_ml in
  (* Create binders for list element *)
  let ctx_inner, bindings, binder_elem =
    compile_binding ctx_inner expr_stub_iter_ml exp
  in
  (* Connect binders *)
  let binder = Binder.connect [ binder_map; binder_elem ] in
  (* Finish map block with bindings *)
  let ctx, expr_bind_ml = compile_blk_footer ctx_inner binder bindings in
  (* Create [List.splitN ...] *)
  let ctx, expr_split_ml =
    let arity = List.length bindings in
    let ctx = Ctx.add_list_arity ctx arity in
    let id_split_ml = "List.split" ^ string_of_int arity in
    let expr_split_ml = Ml.AppE (Ml.VarE id_split_ml, [ expr_bind_ml ]) in
    (ctx, expr_split_ml)
  in
  (* Create [let (..._a, ..., ..._z) = ... in ...] *)
  let binder =
    let ids_bind_ml =
      List.map (fun (_, id_bind_ml) -> id_bind_ml ^ "__star") bindings
    in
    let pats_ml = List.map (fun id_bind_ml -> Ml.VarP id_bind_ml) ids_bind_ml in
    let pat_ml = Ml.TupleP pats_ml in
    Binder.make_let pat_ml expr_split_ml
  in
  (ctx, bindings, binder)

and compile_iter_binding (ctx : Ctx.t) (expr_stub_ml : Ml.expr) (exp : exp)
    (iterexp : iterexp) : Ctx.t * binding list * Binder.t =
  match is_iter_var_exp exp with
  | Some var ->
      let id_ml = Names.var_of_var var in
      let binding = (var, id_ml) in
      let binder = Binder.make_let (Ml.VarP id_ml) expr_stub_ml in
      (ctx, [ binding ], binder)
  | None -> (
      let iter, _ = iterexp in
      match iter with
      | Opt -> compile_iter_opt_binding ctx expr_stub_ml exp
      | List -> compile_iter_list_binding ctx expr_stub_ml exp)

(* Entry point *)

let compile (ctx : Ctx.t) (expr_stub_ml : Ml.expr) (exp : exp) :
    Ctx.t * Binder.t =
  let ctx, bindings, binder = compile_binding ctx expr_stub_ml exp in
  let vars, ids_ml = List.split bindings in
  let ctx = Ctx.add_bindings ctx vars ids_ml in
  (ctx, binder)
