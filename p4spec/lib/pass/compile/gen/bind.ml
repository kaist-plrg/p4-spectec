open Domain
open Lang
open Sl
module Var = Runtime.Dynamic.Var
open Runtime_compile
open Error
open Util.Source

(* Helpers *)

let compile_blk_footer (ctx_inner : Ctx.t) (ctx_outer : Ctx.t) (chain : Chain.t)
    (bindings : Binding.t list) : Ctx.t * Ml.expr =
  let ids_bind_ml = List.map (fun (_, id_bind_ml) -> id_bind_ml) bindings in
  let expr_bind_ml =
    let exprs_bind_ml =
      List.map (fun id_bind_ml -> Ml.VarE id_bind_ml) ids_bind_ml
    in
    let expr_bind_ml = Ml.TupleE exprs_bind_ml in
    Chain.apply chain expr_bind_ml
  in
  let ctx = Ctx.promote_preamble ctx_inner ctx_outer in
  (ctx, expr_bind_ml)

(* Binding *)

let rec compile_binding (ctx : Ctx.t) (expr_stub_ml : Ml.expr) (exp : exp) :
    Ctx.t * Binding.t list * Chain.t =
  let typ_exp = exp.note $ exp.at in
  match exp.it with
  | VarE id -> compile_var_binding ctx expr_stub_ml id
  | TupleE exps -> compile_tuple_binding ctx expr_stub_ml exps
  | CaseE notexp -> compile_case_binding ctx typ_exp expr_stub_ml notexp
  | StrE expfields -> compile_str_binding ctx expr_stub_ml expfields
  | OptE exp_opt -> compile_opt_binding ctx expr_stub_ml exp_opt
  | ListE exps -> compile_list_binding ctx expr_stub_ml exps
  | ConsE (exp_h, exp_t) -> compile_cons_binding ctx expr_stub_ml exp_h exp_t
  | IterE (exp, iterexp) ->
      compile_iter_binding ctx typ_exp expr_stub_ml exp iterexp
  | _ ->
      error exp.at
        (Format.asprintf "unsupported binding expression: %s"
           (Sl.Print.string_of_exp exp))

and compile_bindings (ctx : Ctx.t) (exprs_stub_ml : Ml.expr list)
    (exps : exp list) : Ctx.t * Binding.t list * Chain.t =
  match (exprs_stub_ml, exps) with
  | [], [] -> (ctx, [], fun expr -> expr)
  | expr_stub_h_ml :: exprs_stub_t_ml, exp_h :: exps_t ->
      let ctx, bindings_h, chain_h = compile_binding ctx expr_stub_h_ml exp_h in
      let ctx, bindings_t, chain_t =
        compile_bindings ctx exprs_stub_t_ml exps_t
      in
      let bindings = bindings_h @ bindings_t in
      let chain = Chain.connect [ chain_h; chain_t ] in
      (ctx, bindings, chain)
  | _, [] | [], _ -> assert false

(* Variable binding: [x] on stub [expr]

   Compiled to no-op *)

and compile_var_binding (ctx : Ctx.t) (expr_stub_ml : Ml.expr) (id : id) :
    Ctx.t * Binding.t list * Chain.t =
  let id_ml = Names.var_of_id id in
  let binding = ((id, []), id_ml) in
  let chain = Chain.make_let (Ml.VarP id_ml) expr_stub_ml in
  (ctx, [ binding ], chain)

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
    (exps : exp list) : Ctx.t * Binding.t list * Chain.t =
  let ctx_outer = ctx in
  (* Create stub expressions for tuple elements *)
  let ctx, ids_stub_ml = Stub.OCaml.vars ctx "tup__" (List.length exps) in
  let exprs_stub_ml =
    List.map (fun id_stub_ml -> Ml.VarE id_stub_ml) ids_stub_ml
  in
  (* Create [let (expr_a, ..., expr_z) = expr_stub in ...] *)
  let chain_tuple =
    let pats_ml = List.map (fun id_stub_ml -> Ml.VarP id_stub_ml) ids_stub_ml in
    let pat_ml = Ml.TupleP pats_ml in
    Chain.make_let pat_ml expr_stub_ml
  in
  (* Create chains for tuple elements *)
  let ctx, bindings, chain_elems = compile_bindings ctx exprs_stub_ml exps in
  (* Connect chains *)
  let chain = Chain.connect [ chain_tuple; chain_elems ] in
  (* Finish nested block with bindings *)
  let ctx, expr_bind_ml = compile_blk_footer ctx ctx_outer chain bindings in
  (* Create [let (..._a, ..., ..._z) = ... in ...] *)
  let chain =
    let ids_bind_ml = List.map (fun (_, id_bind_ml) -> id_bind_ml) bindings in
    let pats_ml = List.map (fun id_bind_ml -> Ml.VarP id_bind_ml) ids_bind_ml in
    let pat_ml = Ml.TupleP pats_ml in
    Chain.make_let pat_ml expr_bind_ml
  in
  (ctx, bindings, chain)

(* Case binding: [mixop (exp_a, exp_b, ..., exp_z)] on stub [expr]

   Compiled to
    [
      let (..._a, ..., ..._z) =
        match expr with
        | `CTOR (expr_a, expr_b, ..., expr_z) ->
            let ..._a = ... compiled exp_a on expr_a ... in
            let ..._b = ... compiled exp_b on expr_b ... in
            ...
            let ..._z = ... compiled exp_z on expr_z ... in
            (..._a, ..., ..._z)
        | _ -> raise Unmatch
      in
    ] *)

and compile_case_binding (ctx : Ctx.t) (typ_exp : typ) (expr_stub_ml : Ml.expr)
    (notexp : notexp) : Ctx.t * Binding.t list * Chain.t =
  let mixop, exps = Mixfix.split notexp in
  let ctx_outer = ctx in
  (* Create stub expressions for case elements *)
  let ctx, ids_stub_ml = Stub.OCaml.vars ctx "pyld__" (List.length exps) in
  let exprs_stub_ml =
    List.map (fun id_stub_ml -> Ml.VarE id_stub_ml) ids_stub_ml
  in
  (* Create [match expr with `CTOR (expr_a, expr_b, ..., expr_z) -> ...] *)
  let chain_match =
    let ctor_ml = Ctx.find_ctor ctx typ_exp mixop in
    let pats_ml = List.map (fun id_stub_ml -> Ml.VarP id_stub_ml) ids_stub_ml in
    let pat_ml = Ml.VariantP (`Poly (ctor_ml, pats_ml)) in
    Chain.make_match expr_stub_ml pat_ml
  in
  (* Create chains for case elements *)
  let ctx, bindings, chain_elems = compile_bindings ctx exprs_stub_ml exps in
  (* Connect chains *)
  let chain = Chain.connect [ chain_match; chain_elems ] in
  (* Finish nested block with bindings *)
  let ctx, expr_bind_ml = compile_blk_footer ctx ctx_outer chain bindings in
  (* Create [let (..._a, ..., ..._z) = ... in ...] *)
  let chain =
    let ids_bind_ml = List.map (fun (_, id_bind_ml) -> id_bind_ml) bindings in
    let pats_ml = List.map (fun id_bind_ml -> Ml.VarP id_bind_ml) ids_bind_ml in
    let pat_ml = Ml.TupleP pats_ml in
    Chain.make_let pat_ml expr_bind_ml
  in
  (ctx, bindings, chain)

(* Struct binding *)

and compile_str_binding (_ctx : Ctx.t) (_expr_stub_ml : Ml.expr)
    (_expfields : (atom * exp) list) : Ctx.t * Binding.t list * Chain.t =
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
    (exp_opt : exp option) : Ctx.t * Binding.t list * Chain.t =
  let ctx_outer = ctx in
  let ctx, bindings, expr_bind_ml =
    match exp_opt with
    | Some exp ->
        (* Create stub expression for option element *)
        let ctx, id_stub_ml = Stub.OCaml.var ctx "elem_opt__" in
        let expr_stub_ml = Ml.VarE id_stub_ml in
        (* Create [match expr with Some expr_inner -> ...] *)
        let chain_some =
          let pat_then_ml = Ml.OptP (Some (Ml.VarP id_stub_ml)) in
          Chain.make_match expr_stub_ml pat_then_ml
        in
        (* Create chain for option element *)
        let ctx, bindings, chain_then = compile_binding ctx expr_stub_ml exp in
        (* Connect chains *)
        let chain = Chain.connect [ chain_some; chain_then ] in
        (* Finish nested block with bindings *)
        let ctx, expr_bind_ml =
          compile_blk_footer ctx ctx_outer chain bindings
        in
        (ctx, bindings, expr_bind_ml)
    | None ->
        (* Create [match expr with None -> ...] *)
        let chain_none =
          let pat_then_ml = Ml.OptP None in
          Chain.make_match expr_stub_ml pat_then_ml
        in
        (* Finish nested block with bindings *)
        let ctx, expr_bind_ml =
          compile_blk_footer ctx ctx_outer chain_none []
        in
        (ctx, [], expr_bind_ml)
  in
  (* Create [let (..._a, ..., ..._z) = ... in ...] *)
  let chain =
    let ids_bind_ml = List.map (fun (_, id_bind_ml) -> id_bind_ml) bindings in
    let pats_ml = List.map (fun id_bind_ml -> Ml.VarP id_bind_ml) ids_bind_ml in
    let pat_ml = Ml.TupleP pats_ml in
    Chain.make_let pat_ml expr_bind_ml
  in
  (ctx, bindings, chain)

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

and compile_list_binding (ctx : Ctx.t) (expr_stub_ml : Ml.expr)
    (exps : exp list) : Ctx.t * Binding.t list * Chain.t =
  let ctx_outer = ctx in
  (* Create stub expressions for list elements *)
  let ctx, ids_stub_ml = Stub.OCaml.vars ctx "elem_list__" (List.length exps) in
  let exprs_stub_ml =
    List.map (fun id_stub_ml -> Ml.VarE id_stub_ml) ids_stub_ml
  in
  (* Create [match expr with [ expr_a; ...; expr_z ] -> ...] *)
  let chain_list =
    let pats_ml = List.map (fun id_stub_ml -> Ml.VarP id_stub_ml) ids_stub_ml in
    let pat_ml = Ml.ListP pats_ml in
    Chain.make_let pat_ml expr_stub_ml
  in
  (* Create chains for list elements *)
  let ctx, bindings, chain_elems = compile_bindings ctx exprs_stub_ml exps in
  (* Connect chains *)
  let chain = Chain.connect [ chain_list; chain_elems ] in
  (* Finish nested block with bindings *)
  let ctx, expr_bind_ml = compile_blk_footer ctx ctx_outer chain bindings in
  (* Create [let (..._a, ..., ..._z) = ... in ...] *)
  let chain =
    let ids_bind_ml = List.map (fun (_, id_bind_ml) -> id_bind_ml) bindings in
    let pats_ml = List.map (fun id_bind_ml -> Ml.VarP id_bind_ml) ids_bind_ml in
    let pat_ml = Ml.TupleP pats_ml in
    Chain.make_let pat_ml expr_bind_ml
  in
  (ctx, bindings, chain)

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
    (exp_t : exp) : Ctx.t * Binding.t list * Chain.t =
  let ctx_outer = ctx in
  (* Create stub expressions for head and tail *)
  let ctx, id_h_stub_ml = Stub.OCaml.var ctx "h__" in
  let ctx, id_t_stub_ml = Stub.OCaml.var ctx "t__" in
  let expr_h_stub_ml = Ml.VarE id_h_stub_ml in
  let expr_t_stub_ml = Ml.VarE id_t_stub_ml in
  (* Create [match expr with expr_h :: expr_t -> ...] *)
  let chain_cons =
    let pat_then_ml = Ml.ConsP (Ml.VarP id_h_stub_ml, Ml.VarP id_t_stub_ml) in
    Chain.make_match expr_stub_ml pat_then_ml
  in
  (* Create chains for head and tail *)
  let ctx, bindings, chain_elems =
    compile_bindings ctx [ expr_h_stub_ml; expr_t_stub_ml ] [ exp_h; exp_t ]
  in
  (* Connect chains *)
  let chain = Chain.connect [ chain_cons; chain_elems ] in
  (* Finish nested block with bindings *)
  let ctx, expr_bind_ml = compile_blk_footer ctx ctx_outer chain bindings in
  (* Create [let (..._ah, ..., ..._at, ...) = ... in ...] *)
  let chain =
    let ids_bind_ml = List.map (fun (_, id_bind_ml) -> id_bind_ml) bindings in
    let pats_ml = List.map (fun id_bind_ml -> Ml.VarP id_bind_ml) ids_bind_ml in
    let pat_ml = Ml.TupleP pats_ml in
    Chain.make_let pat_ml expr_bind_ml
  in
  (ctx, bindings, chain)

(* Iter binding *)

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
    : Ctx.t * Binding.t list * Chain.t =
  let ctx_outer = ctx in
  (* Create stub expression for option element *)
  let ctx, id_stub_iter_ml = Stub.OCaml.var ctx "elem_opt__" in
  let expr_stub_iter_ml = Ml.VarE id_stub_iter_ml in
  (* Create [Option.map (fun expr_stub_inner -> ...) expr] *)
  let chain_map = Chain.make_option_map expr_stub_ml id_stub_iter_ml in
  (* Create chains for option element *)
  let ctx, bindings, chain_elem = compile_binding ctx expr_stub_iter_ml exp in
  (* Connect chains *)
  let chain = Chain.connect [ chain_map; chain_elem ] in
  (* Finish map block with bindings *)
  let ctx, expr_bind_ml = compile_blk_footer ctx ctx_outer chain bindings in
  (* Create [Option.splitN ...] *)
  let ctx, expr_split_ml =
    let arity = List.length bindings in
    let ctx = Ctx.add_opt_arity ctx arity in
    let id_split_ml = "Option.split" ^ string_of_int arity in
    let expr_split_ml = Ml.AppE (Ml.VarE id_split_ml, [ expr_bind_ml ]) in
    (ctx, expr_split_ml)
  in
  (* Create [let (..._ah, ..., ..._at, ...) = ... in ...] *)
  let chain =
    let ids_bind_ml =
      List.map (fun (_, id_bind_ml) -> id_bind_ml ^ "__quest") bindings
    in
    let pats_ml = List.map (fun id_bind_ml -> Ml.VarP id_bind_ml) ids_bind_ml in
    let pat_ml = Ml.TupleP pats_ml in
    Chain.make_let pat_ml expr_split_ml
  in
  (* Lift bindings *)
  let bindings_lift =
    List.map
      (fun ((id, iters), _) ->
        let id_ml = Names.var_of_var (id, iters @ [ Il.Opt ]) in
        ((id, iters @ [ Il.Opt ]), id_ml))
      bindings
  in
  (ctx, bindings_lift, chain)

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
    : Ctx.t * Binding.t list * Chain.t =
  let ctx_outer = ctx in
  (* Create stub expression for list element *)
  let ctx, id_stub_iter_ml = Stub.OCaml.var ctx "elem_list__" in
  let expr_stub_iter_ml = Ml.VarE id_stub_iter_ml in
  (* Create [List.map (fun expr_stub_inner -> ...) expr] *)
  let chain_map = Chain.make_list_map expr_stub_ml id_stub_iter_ml in
  (* Create chains for list element *)
  let ctx, bindings, chain_elem = compile_binding ctx expr_stub_iter_ml exp in
  (* Connect chains *)
  let chain = Chain.connect [ chain_map; chain_elem ] in
  (* Finish map block with bindings *)
  let ctx, expr_bind_ml = compile_blk_footer ctx ctx_outer chain bindings in
  (* Create [List.splitN ...] *)
  let ctx, expr_split_ml =
    let arity = List.length bindings in
    let ctx = Ctx.add_list_arity ctx arity in
    let id_split_ml = "List.split" ^ string_of_int arity in
    let expr_split_ml = Ml.AppE (Ml.VarE id_split_ml, [ expr_bind_ml ]) in
    (ctx, expr_split_ml)
  in
  (* Create [let (..._a, ..., ..._z) = ... in ...] *)
  let chain =
    let ids_bind_ml =
      List.map (fun (_, id_bind_ml) -> id_bind_ml ^ "__star") bindings
    in
    let pats_ml = List.map (fun id_bind_ml -> Ml.VarP id_bind_ml) ids_bind_ml in
    let pat_ml = Ml.TupleP pats_ml in
    Chain.make_let pat_ml expr_split_ml
  in
  (* Lift bindings *)
  let bindings_lift =
    List.map
      (fun ((id, iters), _) ->
        let id_ml = Names.var_of_var (id, iters @ [ Il.List ]) in
        ((id, iters @ [ Il.List ]), id_ml))
      bindings
  in
  (ctx, bindings_lift, chain)

and compile_iter_binding (ctx : Ctx.t) (typ_exp : typ) (expr_stub_ml : Ml.expr)
    (exp : exp) (iterexp : iterexp) : Ctx.t * Binding.t list * Chain.t =
  match
    Common.is_iter_var_exp (Il.IterE (exp, iterexp) $$ (typ_exp.at, typ_exp.it))
  with
  | Some var ->
      let id_ml = Names.var_of_var var in
      let binding = (var, id_ml) in
      let chain = Chain.make_let (Ml.VarP id_ml) expr_stub_ml in
      (ctx, [ binding ], chain)
  | None -> (
      let iter, _ = iterexp in
      match iter with
      | Opt -> compile_iter_opt_binding ctx expr_stub_ml exp
      | List -> compile_iter_list_binding ctx expr_stub_ml exp)

(* Entry point *)

let compile (ctx : Ctx.t) (expr_stub_ml : Ml.expr) (exp : exp) : Ctx.t * Chain.t
    =
  let ctx, bindings, chain = compile_binding ctx expr_stub_ml exp in
  let vars, ids_ml = List.split bindings in
  let ctx = Ctx.add_bindings ctx vars ids_ml in
  (ctx, chain)
