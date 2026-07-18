open Lang
module Typ = Runtime.Type
module Typdef = Typ.Typdef
open Util.Source

(* Booleans *)

let compile_bool_typ = Ml.AppE (Ml.LitE "Value.Make.bool", [ Ml.VarE "x" ])

(* Numbers *)

let compile_num_typ = function
  | `NatT -> Ml.AppE (Ml.LitE "Value.Make.nat", [ Ml.VarE "x" ])
  | `IntT -> Ml.AppE (Ml.LitE "Value.Make.int", [ Ml.VarE "x" ])

(* Texts *)

let compile_text_typ = Ml.AppE (Ml.LitE "Value.Make.text", [ Ml.VarE "x" ])

(* Variable types *)

(* Structs *)

let compile_field_atom (s : string) : Ml.expr =
  Common.make_phrase (Dynamic_gen.make_atom_string (Domain.Atom.Atom s))

let compile_struct_typ (typ_ref : string) (typfields : Sl.typfield list) :
    Ml.expr =
  let field_exprs_ml =
    List.map
      (fun (atom, typ) ->
        let atom_str = Names.Ctor.atom atom in
        let field_id = Names.field atom in
        let expr_atom_ml = compile_field_atom atom_str in
        let expr_field_ml = Ml.FieldE (Ml.VarE "x", field_id) in
        let expr_marshal_ml =
          Ml.AppE (Ml.VarE ("marshal_" ^ Naming.name typ), [ expr_field_ml ])
        in
        Ml.TupleE [ expr_atom_ml; expr_marshal_ml ])
      typfields
  in
  Ml.AppE
    (Ml.LitE "Value.Make.str", [ Ml.VarE typ_ref; Ml.ListE field_exprs_ml ])

(* Variants *)

let compile_case_value (expr_mixop_ml : Ml.expr) (expr_payload_ml : Ml.expr)
    (expr_typ_ml : Ml.expr) : Ml.expr =
  Ml.AppE
    ( Ml.LitE "Value.Make.case",
      [
        expr_typ_ml;
        Ml.AppE (Ml.LitE "Mixfix.fill", [ expr_mixop_ml; expr_payload_ml ]);
      ] )

let compile_variant_typ (pool : Constpool.t) (typ_ref : string)
    (ctors : (Domain.Mixop.t * Ml.ctor * Sl.typ list) list) :
    Constpool.t * Ml.expr =
  let pool, arms_ml =
    List.fold_left_map
      (fun pool (mixop, ctor_ml, payload_typs) ->
        let pvars =
          List.mapi (fun i _ -> "p_" ^ string_of_int i) payload_typs
        in
        let pat_ml =
          Ml.VariantP (`Poly (ctor_ml, List.map (fun var -> Ml.VarP var) pvars))
        in
        let marshal_calls_ml =
          List.map2
            (fun typ pvar ->
              Ml.AppE (Ml.VarE ("marshal_" ^ Naming.name typ), [ Ml.VarE pvar ]))
            payload_typs pvars
        in
        let pool, mo_ref = Constpool.intern_mixop pool mixop in
        let expr_case_ml =
          compile_case_value (Ml.VarE mo_ref) (Ml.ListE marshal_calls_ml)
            (Ml.VarE typ_ref)
        in
        (pool, (pat_ml, expr_case_ml)))
      pool ctors
  in
  (pool, Ml.MatchE (Ml.VarE "x", arms_ml))

let compile_var_typ (ctx : Ctx.t) (pool : Constpool.t) (typ_ref : string)
    (id : Sl.id) (targs : Sl.targ list) : Constpool.t * Ml.expr =
  match Ctx.find_typdef ctx id with
  | Typdef.Param | Typdef.Defining _ -> (pool, Ml.UnitE)
  | Typdef.Defined (tparams, deftyp) -> (
      let theta = Domain.Lib.TIdMap.of_lists tparams targs in
      match deftyp.it with
      | Il.PlainT typ_alias ->
          let typ_alias = Typ.Subst.subst_typ theta typ_alias in
          let name_alias = "marshal_" ^ Naming.name typ_alias in
          (pool, Ml.AppE (Ml.VarE name_alias, [ Ml.VarE "x" ]))
      | Il.StructT typfields ->
          let typfields =
            List.map
              (fun (atom, typ) ->
                let typ = Typ.Subst.subst_typ theta typ in
                (atom, typ))
              typfields
          in
          (pool, compile_struct_typ typ_ref typfields)
      | Il.VariantT _ ->
          let ctors = Ctx.find_ctors_full ctx id in
          let ctors =
            List.map
              (fun (mixop, ctor_ml, typs) ->
                let typs = Typ.Subst.subst_typs theta typs in
                (mixop, ctor_ml, typs))
              ctors
          in
          compile_variant_typ pool typ_ref ctors)
  | Typdef.Extern ->
      ( pool,
        Ml.AppE (Ml.LitE "Value.Make.extern", [ Ml.VarE typ_ref; Ml.VarE "x" ])
      )

(* Tuples *)

let compile_tuple_typ (typ_ref : string) (typs : Sl.typ list) : Ml.expr =
  let vars = List.mapi (fun i _ -> "x" ^ string_of_int i) typs in
  let marshal_calls_ml =
    List.map2
      (fun typ var ->
        Ml.AppE (Ml.VarE ("marshal_" ^ Naming.name typ), [ Ml.VarE var ]))
      typs vars
  in
  let pat_vars_ml = Ml.TupleP (List.map (fun var -> Ml.VarP var) vars) in
  let expr_tuple_ml =
    Ml.AppE
      ( Ml.LitE "Value.Make.tuple",
        [ Ml.VarE typ_ref; Ml.ListE marshal_calls_ml ] )
  in
  Ml.LetE (pat_vars_ml, Ml.VarE "x", expr_tuple_ml)

(* Iterations *)

let compile_iter_opt_typ (typ_ref : string) (typ : Sl.typ) : Ml.expr =
  let expr_map_ml =
    Ml.AppE
      ( Ml.LitE "Option.map",
        [ Ml.VarE ("marshal_" ^ Naming.name typ); Ml.VarE "x" ] )
  in
  Ml.AppE (Ml.LitE "Value.Make.opt", [ Ml.VarE typ_ref; expr_map_ml ])

let compile_iter_list_typ (typ_ref : string) (typ : Sl.typ) : Ml.expr =
  let expr_map_ml =
    Ml.AppE
      ( Ml.LitE "List.map",
        [ Ml.VarE ("marshal_" ^ Naming.name typ); Ml.VarE "x" ] )
  in
  Ml.AppE (Ml.LitE "Value.Make.list", [ Ml.VarE typ_ref; expr_map_ml ])

let compile_iter_typ (typ_ref : string) (typ : Sl.typ) (iter : Sl.iter) :
    Ml.expr =
  match iter with
  | Opt -> compile_iter_opt_typ typ_ref typ
  | List -> compile_iter_list_typ typ_ref typ

(* Entry point *)

let compile_body_typ (ctx : Ctx.t) (pool : Constpool.t) (typ : Sl.typ) :
    Constpool.t * Ml.expr =
  match typ.it with
  | Il.BoolT -> (pool, compile_bool_typ)
  | Il.NumT numtyp -> (pool, compile_num_typ numtyp)
  | Il.TextT -> (pool, compile_text_typ)
  | Il.VarT (id, targs) ->
      let pool, typ_ref =
        Constpool.intern_typ pool (Naming.name typ)
          (Dynamic_gen.make_typ_expr typ)
      in
      compile_var_typ ctx pool typ_ref id targs
  | Il.TupleT typs ->
      let pool, typ_ref =
        Constpool.intern_typ pool (Naming.name typ)
          (Dynamic_gen.make_typ_expr typ)
      in
      (pool, compile_tuple_typ typ_ref typs)
  | Il.IterT (typ_inner, iter) ->
      let pool, typ_ref =
        Constpool.intern_typ pool (Naming.name typ)
          (Dynamic_gen.make_typ_expr typ)
      in
      (pool, compile_iter_typ typ_ref typ_inner iter)
  | Il.FuncT _ -> (pool, Ml.UnitE)

let compile (ctx : Ctx.t) (pool : Constpool.t) (typ : Sl.typ) :
    Constpool.t * Ml.funcdef =
  let name = "marshal_" ^ Naming.name typ in
  let typ_param_ml = Type.compile_typ ~tparams:[] typ in
  let param_ml = ("x", Some typ_param_ml) in
  let typ_ret_ml = Some (Ml.NameT "Value.t") in
  let pool, expr_body_ml = compile_body_typ ctx pool typ in
  (pool, (name, [], [ param_ml ], typ_ret_ml, expr_body_ml))
