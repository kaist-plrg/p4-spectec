open Lang
open Sl

(* Note-wrapping shared by expression construction (gen/ast/exp.ml) and cons-tail
   re-wrapping (gen/bind.ml); kept gen-level so [Bind] need not depend on [Ast] *)

(* Hash of a generic composite's [body__], mirroring the converter's per-shape
   fold: [hash_list]/[hash_opt] over the element hash, tuple folds seed*31+.. *)

let compile_wrap_hash ~(tparams : string list) (ctx : Ctx.t) (typ : typ) :
    Ml.expr =
  match typ.it with
  | Il.IterT (typ_elem, Il.List) ->
      let conv = Interface.Converter.resolve ctx tparams typ_elem in
      Ml.AppE (Ml.VarE "hash_list", [ conv.hash; Ml.VarE "body__" ])
  | Il.IterT (typ_elem, Il.Opt) ->
      let conv = Interface.Converter.resolve ctx tparams typ_elem in
      Ml.AppE (Ml.VarE "hash_opt", [ conv.hash; Ml.VarE "body__" ])
  | Il.TupleT typs ->
      let convs = List.map (Interface.Converter.resolve ctx tparams) typs in
      let vars_h = List.mapi (fun i _ -> "h" ^ string_of_int i) typs in
      let exprs_hash_ml =
        List.map2
          (fun (conv : Interface.Converter.t) var ->
            Ml.AppE (conv.hash, [ Ml.VarE var ]))
          convs vars_h
      in
      let expr_combine_ml =
        List.fold_left
          (fun acc_ml expr_ml ->
            Ml.BinopE ("+", Ml.BinopE ("*", acc_ml, Ml.LitE "31"), expr_ml))
          (Ml.LitE (string_of_int (List.length typs)))
          exprs_hash_ml
      in
      Ml.LetE
        ( Ml.TupleP (List.map (fun var -> Ml.VarP var) vars_h),
          Ml.VarE "body__",
          expr_combine_ml )
  | _ -> assert false

(* Wrap a raw body through the target type's smart constructor: a parametric
   named type calls the polymorphic [mk_<base>] with a [typ]/[hash] dictionary
   per argument, a generic composite (no mono [mk]) wraps via [Converter.wrap_note]
   with a folded hash, a ground type calls its monomorphic [mk_<name>] *)

let compile_mk_wrap ~(tparams : string list) (ctx : Ctx.t) (typ_exp : typ)
    (expr_body_ml : Ml.expr) : Ml.expr =
  let typ_expand = Ctx.expand_typ ctx typ_exp in
  match typ_expand.it with
  | Il.VarT (id, (_ :: _ as targs)) ->
      (* parametric: the polymorphic [mk_<base>] with a typ/hash dictionary per
         argument, resolved to a mono function for a ground arg *)
      let id_mk_ml = "mk_" ^ Names.var_of_id id in
      let exprs_dict_ml =
        List.concat_map
          (fun targ ->
            let expr_typ_ml = Interface.Dynamic_gen.make_typ_expr ~tparams targ in
            let conv = Interface.Converter.resolve ctx tparams targ in
            [ expr_typ_ml; conv.hash ])
          targs
      in
      Ml.AppE (Ml.VarE id_mk_ml, exprs_dict_ml @ [ expr_body_ml ])
  | Il.TupleT _ | Il.IterT _ ->
      (* composites have no reliable mono [mk] (nested/generic ones are never
         collected), so stamp a note directly via the element converters *)
      let expr_hash_ml = compile_wrap_hash ~tparams ctx typ_expand in
      Interface.Converter.wrap_note tparams typ_expand expr_body_ml expr_hash_ml
  | _ ->
      let id_mk_ml = "mk_" ^ Interface.Naming.name typ_expand in
      Ml.AppE (Ml.VarE id_mk_ml, [ expr_body_ml ])
