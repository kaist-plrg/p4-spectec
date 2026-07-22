open Domain
open Lang
open Util.Source

(* Whether [typ] still depends on an unresolved type parameter in [tparams] *)

let rec is_generic (tparams : string list) (typ : Sl.typ) : bool =
  match typ.it with
  | Il.BoolT | Il.NumT _ | Il.TextT -> false
  | Il.VarT (id, targs) ->
      List.mem id.it tparams || List.exists (is_generic tparams) targs
  | Il.TupleT typs -> List.exists (is_generic tparams) typs
  | Il.IterT (typ, _) -> is_generic tparams typ
  | Il.FuncT _ -> false

(* Typs *)

let rec compile_typ ~(tparams : string list) (typ : Sl.typ) : Ml.typ =
  match typ.it with
  | Il.BoolT -> compile_bool_typ
  | Il.NumT _ -> compile_num_typ
  | Il.TextT -> compile_text_typ
  | Il.VarT (id, targs) -> compile_var_typ ~tparams id targs
  | Il.TupleT typs -> compile_tuple_typ ~tparams typs
  | Il.IterT (typ, iter) -> compile_iter_typ ~tparams typ iter
  | Il.FuncT _ -> compile_func_typ

and compile_typs ~(tparams : string list) (typs : Sl.typ list) : Ml.typ list =
  List.map (compile_typ ~tparams) typs

(* Boolean types *)

and compile_bool_typ : Ml.typ = Ml.BoolT

(* Number types *)

and compile_num_typ : Ml.typ = Ml.BigintT

(* Text types *)

and compile_text_typ : Ml.typ = Ml.StringT

(* Variable types *)

and compile_var_typ ~(tparams : string list) (id : Sl.id) (targs : Sl.targ list)
    : Ml.typ =
  match targs with
  | [] when List.mem id.it tparams ->
      let id_ml = Names.tvar id in
      Ml.VarT id_ml
  | [] when not (List.mem id.it tparams) ->
      let id_ml = Names.var_of_id id in
      Ml.NameT id_ml
  | _ ->
      let id_ml = Names.var_of_id id in
      let targs_ml = compile_typs ~tparams targs in
      Ml.AppT (id_ml, targs_ml)

(* [(body, vnote) note_phrase]: a composite carries a note like a named type *)

and wrap_body_typ (typ_body_ml : Ml.typ) : Ml.typ =
  Ml.AppT ("note_phrase", [ typ_body_ml; Ml.NameT "Il.vnote" ])

(* Tuple types *)

and compile_tuple_typ ~(tparams : string list) (typs : Sl.typ list) : Ml.typ =
  wrap_body_typ (Ml.TupleT (compile_typs ~tparams typs))

(* Iter types *)

and compile_iter_typ ~(tparams : string list) (typ : Sl.typ) (iter : Sl.iter) :
    Ml.typ =
  let typ_ml = compile_typ ~tparams typ in
  match iter with
  | Il.Opt -> wrap_body_typ (Ml.AppT ("option", [ typ_ml ]))
  | Il.List -> wrap_body_typ (Ml.AppT ("list", [ typ_ml ]))

(* Func types *)

and compile_func_typ : Ml.typ = Ml.UnitT

(* Deftyps *)

let rec compile_deftyp ~(tparams : string list) (ctx : Ctx.t) (id : Sl.id)
    (deftyp : Sl.deftyp) : Ctx.t * Ml.deftyp =
  match deftyp.it with
  | Il.PlainT typ ->
      let deftyp_ml = compile_alias_deftyp ~tparams typ in
      (ctx, deftyp_ml)
  | Il.StructT typfields ->
      let deftyp_ml = compile_struct_deftyp ~tparams typfields in
      (ctx, deftyp_ml)
  | Il.VariantT typcases -> compile_variant_deftyp ~tparams ctx id typcases

(* Alias deftyps *)

and compile_alias_deftyp ~(tparams : string list) (typ : Sl.typ) : Ml.deftyp =
  let typ_ml = compile_typ ~tparams typ in
  Ml.AliasTD typ_ml

(* Struct deftyps *)

and compile_typfield ~(tparams : string list) (typfield : Sl.typfield) :
    Ml.typfield =
  let atom, typ = typfield in
  let field_ml = Names.field atom in
  let typ_ml = compile_typ ~tparams typ in
  (field_ml, typ_ml)

and compile_typfields ~(tparams : string list) (typfields : Sl.typfield list) :
    Ml.typfield list =
  List.map (compile_typfield ~tparams) typfields

and compile_struct_deftyp ~(tparams : string list)
    (typfields : Sl.typfield list) : Ml.deftyp =
  let typfields_ml = compile_typfields ~tparams typfields in
  Ml.RecordTD typfields_ml

(* Variant deftyps *)

and is_unique_variant (typcases_all : Sl.typcase list) (id : Sl.id) =
  let count =
    typcases_all
    |> List.filter (fun (_, typorigin, _) ->
           let id_sub, _ = typorigin.it in
           id_sub.it = id.it)
    |> List.length
  in
  count = 1

and compile_typcase ~(tparams : string list) (typcases_all : Sl.typcase list)
    (typcase : Sl.typcase) : Ml.typcase =
  let nottyp, typorigin, _ = typcase in
  let id_origin, _ = typorigin.it in
  let ctor_ml =
    if is_unique_variant typcases_all id_origin then
      String.capitalize_ascii id_origin.it
    else Names.ctor nottyp
  in
  let typs_arg = Domain.Mixfix.args nottyp.it in
  let typs_ml = compile_typs ~tparams typs_arg in
  (ctor_ml, typs_ml)

and dedup_typcases_ml (typcases_ml : Ml.typcase list) : Ml.typcase list =
  let ctor_seen = Hashtbl.create 16 in
  List.map
    (fun (ctor_ml, typs_ml) ->
      let count = try Hashtbl.find ctor_seen ctor_ml with Not_found -> 0 in
      Hashtbl.replace ctor_seen ctor_ml (count + 1);
      let ctor_ml =
        if count = 0 then ctor_ml else ctor_ml ^ "_" ^ string_of_int count
      in
      (ctor_ml, typs_ml))
    typcases_ml

and compile_typcases ~(tparams : string list) (typcases : Sl.typcase list) :
    Ml.typcase list =
  typcases |> List.map (compile_typcase ~tparams typcases) |> dedup_typcases_ml

and compile_variant_deftyp ~(tparams : string list) (ctx : Ctx.t) (id : Sl.id)
    (typcases : Sl.typcase list) : Ctx.t * Ml.deftyp =
  let typcases_ml = compile_typcases ~tparams typcases in
  let deftyp_ml = Ml.VariantTD typcases_ml in
  let ctx =
    List.combine typcases typcases_ml
    |> List.fold_left
         (fun ctx (typcase, typcase_ml) ->
           let nottyp, _, _ = typcase in
           let mixop, _ = Mixfix.split nottyp.it in
           let case = (id, mixop) in
           let ctor, _ = typcase_ml in
           let typs_arg = Domain.Mixfix.args nottyp.it in
           Ctx.add_ctor ctx case (ctor, typs_arg))
         ctx
  in
  (ctx, deftyp_ml)

(* Defs *)

(* Wrap a variant/record named type as [foo = (foo_body, vnote) note_phrase],
   emitting the raw body type first; aliases and externs stay unwrapped *)

let wrap_typdefs (tparams_ml : Ml.tparam list) (id : Sl.id)
    (deftyp_ml : Ml.deftyp) : Ml.typdef list =
  let id_ml = Names.var_of_id id in
  match deftyp_ml with
  | Ml.AliasTD _ -> [ (tparams_ml, id_ml, deftyp_ml) ]
  | Ml.RecordTD _ | Ml.VariantTD _ ->
      let body_id_ml = Names.body_of_id id in
      let typ_body_ml =
        match tparams_ml with
        | [] -> Ml.NameT body_id_ml
        | _ -> Ml.AppT (body_id_ml, List.map (fun t -> Ml.VarT t) tparams_ml)
      in
      let wrapper_ml =
        Ml.AliasTD
          (Ml.AppT ("note_phrase", [ typ_body_ml; Ml.NameT "Il.vnote" ]))
      in
      [ (tparams_ml, body_id_ml, deftyp_ml); (tparams_ml, id_ml, wrapper_ml) ]

let compile_def (ctx : Ctx.t) (def : Sl.def) : Ctx.t * Ml.typdef list =
  match def.it with
  | Sl.TypD (id, tparams, deftyp, _) ->
      let tparams_ml = List.map Names.var_of_id tparams in
      let ctx, deftyp_ml =
        let tparams = List.map it tparams in
        compile_deftyp ~tparams ctx id deftyp
      in
      (ctx, wrap_typdefs tparams_ml id deftyp_ml)
  | Sl.ExternTypD (id, _) ->
      let id_ml = Names.var_of_id id in
      let deftyp_ml = Ml.AliasTD (Ml.NameT "Yojson.Safe.t") in
      (ctx, [ ([], id_ml, deftyp_ml) ])
  | _ -> (ctx, [])

let compile_defs (ctx : Ctx.t) (defs : Sl.def list) : Ctx.t * Ml.typdef list =
  List.fold_left
    (fun (ctx, defs_ml) def ->
      let ctx, typdefs_ml = compile_def ctx def in
      (ctx, defs_ml @ typdefs_ml))
    (ctx, []) defs

(* Spec *)

let compile_spec (ctx : Ctx.t) (spec : Sl.spec) : Ctx.t * Ml.typdef list =
  compile_defs ctx spec

(* SCC-aware compilation: each group becomes one Ml.TypeRec *)
let compile_spec_scc (ctx : Ctx.t) (groups : Sl.def list list) :
    Ctx.t * Ml.typdef list list =
  List.fold_left
    (fun (ctx, acc) group ->
      let ctx, typdefs_ml = compile_defs ctx group in
      (ctx, acc @ [ typdefs_ml ]))
    (ctx, []) groups
