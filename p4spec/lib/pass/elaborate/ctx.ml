open Domain.Lib
open Lang
open El
open Runtime.Type
open Runtime.Static
open Envs
open Error
open Util.Source

(* Error *)

let error_undef ?code (at : region) (kind : string) (id : string) =
  error ?code at (Format.asprintf "%s `%s` is undefined" kind id)

let error_dup ?code ?(related = []) (at : region) (kind : string) (id : string)
    =
  error ?code ~related at
    (Format.asprintf "%s `%s` was already defined" kind id)

(* Global counter for unique identifiers *)

let tick = ref 0
let refresh () = tick := 0

let fresh () =
  let id = !tick in
  tick := !tick + 1;
  id

(* Context *)

type t = {
  (* Set of free ids, for unique id insertion *)
  frees : IdSet.t;
  (* Map from variable ids to dimensions *)
  venv : VEnv.t;
  (* Map from syntax ids to type definitions *)
  tdenv : TDEnv.t;
  (* Map from meta-variable ids to types *)
  menv : MEnv.t;
  (* Map from relation ids to relations *)
  renv : REnv.t;
  (* Map from function ids to functions *)
  fenv : FEnv.t;
}

(* Constructors *)

let empty : t =
  {
    frees = IdSet.empty;
    venv = VEnv.empty;
    tdenv = TDEnv.empty;
    menv = MEnv.empty;
    renv = REnv.empty;
    fenv = FEnv.empty;
  }

let init () : t =
  let menv =
    MEnv.empty
    |> MEnv.add ("bool" $ no_region) (Il.BoolT $ no_region)
    |> MEnv.add ("nat" $ no_region) (Il.NumT `NatT $ no_region)
    |> MEnv.add ("int" $ no_region) (Il.NumT `IntT $ no_region)
    |> MEnv.add ("text" $ no_region) (Il.TextT $ no_region)
  in
  { empty with menv }

(* Finders *)

let region_of_duplicate (env : 'a IdMap.t) (id : Id.t) : region option =
  match IdMap.find_first_opt (fun k -> Id.compare k id >= 0) env with
  | Some (k, _) when Id.compare k id = 0 -> Some k.at
  | _ -> None

let region_of_duplicate_metavar (ctx : t) (tid : TId.t) : region option =
  region_of_duplicate ctx.menv tid

let region_of_duplicate_typdef (ctx : t) (tid : TId.t) : region option =
  region_of_duplicate ctx.tdenv tid

let region_of_duplicate_rel (ctx : t) (rid : RId.t) : region option =
  region_of_duplicate ctx.renv rid

let region_of_duplicate_dec (ctx : t) (fid : FId.t) : region option =
  region_of_duplicate ctx.fenv fid

let related_of_duplicate (at_opt : region option) : (region * string) list =
  match at_opt with
  | Some at -> [ (at, "originally defined here") ]
  | None -> []

let renew_region (env : 'a IdMap.t) (id : Id.t) (value : 'a) : 'a IdMap.t =
  match IdMap.find_first_opt (fun key -> Id.compare key id >= 0) env with
  | Some (key, _) when Id.compare key id = 0 -> IdMap.add key value env
  | _ ->
      (* Every call site has already found [id] in [env]. *)
      assert false

(* Finders for type definitions *)

let find_typdef_opt (ctx : t) (tid : TId.t) : Typdef.t option =
  TDEnv.find_opt tid ctx.tdenv

let find_typdef (ctx : t) (tid : TId.t) : Typdef.t =
  match find_typdef_opt ctx tid with
  | Some td -> td
  | None -> error_undef ~code:Type_undefined tid.at "type" tid.it

let bound_typdef (ctx : t) (tid : TId.t) : bool =
  find_typdef_opt ctx tid |> Option.is_some

(* Finders for meta-variables *)

let find_metavar_opt (ctx : t) (tid : TId.t) : Typ.t option =
  MEnv.find_opt tid ctx.menv

let bound_metavar (ctx : t) (tid : TId.t) : bool =
  find_metavar_opt ctx tid |> Option.is_some

(* Finders for rules *)

let find_defined_rel_opt (ctx : t) (rid : RId.t) :
    (Il.nottyp * int list * Il.rulegroup list * Il.elsegroup option) option =
  let rel_opt = REnv.find_opt rid ctx.renv in
  Option.bind rel_opt (function
    | Rel.Defined (nottyp_il, inputs, rulegroups, elsegroup_opt) ->
        Some (nottyp_il, inputs, rulegroups, elsegroup_opt)
    | Rel.Extern _ -> None)

let find_defined_rel (ctx : t) (rid : RId.t) :
    Il.nottyp * int list * Il.rulegroup list * Il.elsegroup option =
  match REnv.find_opt rid ctx.renv with
  | Some (Rel.Defined (nottyp_il, inputs, rulegroups, elsegroup_opt)) ->
      (nottyp_il, inputs, rulegroups, elsegroup_opt)
  | Some (Rel.Extern _) ->
      let region_of_duplicate =
        match region_of_duplicate_rel ctx rid with
        | Some at -> at
        | None ->
            (* The preceding match found [rid] in the same [REnv]. *)
            assert false
      in
      error ~code:Extern_relation_rules rid.at
        (Format.asprintf "extern relation `%s` does not allow rules" rid.it)
        ~related:[ (region_of_duplicate, "extern relation declared here") ]
  | None -> error_undef ~code:Rule_relation_undefined rid.at "relation" rid.it

let bound_defined_rel (ctx : t) (rid : RId.t) : bool =
  find_defined_rel_opt ctx rid |> Option.is_some

let find_rel_signature_opt (ctx : t) (rid : RId.t) :
    (Il.nottyp * int list) option =
  REnv.find_opt rid ctx.renv
  |> Option.map (function
         | Rel.Extern (nottyp_il, inputs) | Rel.Defined (nottyp_il, inputs, _, _)
         -> (nottyp_il, inputs))

let find_rel_signature (ctx : t) (rid : RId.t) : Il.nottyp * int list =
  match find_rel_signature_opt ctx rid with
  | Some (nottyp_il, inputs) -> (nottyp_il, inputs)
  | None -> error_undef ~code:Relation_undefined rid.at "relation" rid.it

let bound_rel (ctx : t) (rid : RId.t) : bool =
  find_rel_signature_opt ctx rid |> Option.is_some

let rulegroup_region_of_duplicate (ctx : t) (rid : RId.t) (rulegroupid : Id.t) :
    region option =
  match find_defined_rel_opt ctx rid with
  | Some (_, _, rulegroups, elsegroup_opt) -> (
      let regular_at_opt =
        List.find_map
          (fun rulegroup ->
            let id, _ = rulegroup.it in
            if Id.eq rulegroupid id then Some id.at else None)
          rulegroups
      in
      match regular_at_opt with
      | Some _ -> regular_at_opt
      | None ->
          Option.bind elsegroup_opt (fun elsegroup ->
              let id, _ = elsegroup.it in
              if Id.eq rulegroupid id then Some id.at else None))
  | None -> None

let check_rulegroup_fresh (ctx : t) (rid : RId.t) (rulegroupid : Id.t) : unit =
  match rulegroup_region_of_duplicate ctx rid rulegroupid with
  | Some region_of_duplicate ->
      error_dup ~code:Rule_group_redefined rulegroupid.at "rule group"
        rulegroupid.it
        ~related:[ (region_of_duplicate, "originally defined here") ]
  | None -> ()

(* Finders for definitions *)

let find_table_func_opt (ctx : t) (fid : FId.t) :
    (Il.param list * Il.typ * Il.tablerow list) option =
  let func_opt = FEnv.find_opt fid ctx.fenv in
  Option.bind func_opt (function
    | Func.Table (params_il, typ_il, tablerows_il) ->
        Some (params_il, typ_il, tablerows_il)
    | Func.Defined _ | Func.Extern _ | Func.Builtin _ -> None)

let find_table_func (ctx : t) (fid : FId.t) :
    Il.param list * Il.typ * Il.tablerow list =
  match FEnv.find_opt fid ctx.fenv with
  | Some (Func.Table (params_il, typ_il, tablerows_il)) ->
      (params_il, typ_il, tablerows_il)
  | Some (Func.Defined _ | Func.Extern _ | Func.Builtin _) ->
      error ~code:Table_function_required fid.at
        (Format.asprintf "non-table function `%s` does not allow table rows"
           fid.it)
        ~related:
          (match region_of_duplicate_dec ctx fid with
          | Some at -> [ (at, "function declared here") ]
          | None -> [])
  | None ->
      error_undef ~code:Table_function_undefined fid.at "table function" fid.it

let find_defined_func_opt (ctx : t) (fid : FId.t) :
    (Il.tparam list
    * Il.param list
    * Il.typ
    * Il.clause list
    * Il.clause option)
    option =
  let func_opt = FEnv.find_opt fid ctx.fenv in
  Option.bind func_opt (function
    | Func.Defined (tparams_il, params_il, typ_il, clauses_il, elseclause_il_opt)
      ->
        Some (tparams_il, params_il, typ_il, clauses_il, elseclause_il_opt)
    | Func.Table _ | Func.Extern _ | Func.Builtin _ -> None)

let find_defined_func (ctx : t) (fid : FId.t) :
    Il.tparam list * Il.param list * Il.typ * Il.clause list * Il.clause option
    =
  match find_defined_func_opt ctx fid with
  | Some (tparams_il, params_il, typ_il, clauses_il, elseclause_il_opt) ->
      (tparams_il, params_il, typ_il, clauses_il, elseclause_il_opt)
  | None ->
      error ~code:Function_declaration_required fid.at
        (Format.asprintf
           "a definition of function `%s` requires a preceding matching `dec` \
            declaration"
           fid.it)

let find_func_signature_opt (ctx : t) (fid : FId.t) :
    (Il.tparam list * Il.param list * Il.typ) option =
  FEnv.find_opt fid ctx.fenv
  |> Option.map (function
       | Func.Extern (tparams_il, params_il, typ_il)
       | Func.Builtin (tparams_il, params_il, typ_il)
       | Func.Defined (tparams_il, params_il, typ_il, _, _) ->
           (tparams_il, params_il, typ_il)
       | Func.Table (params_il, typ_il, _) -> ([], params_il, typ_il))

let find_func_signature (ctx : t) (fid : FId.t) :
    Il.tparam list * Il.param list * Il.typ =
  match find_func_signature_opt ctx fid with
  | Some (tparams_il, params_il, typ_il) -> (tparams_il, params_il, typ_il)
  | None -> error_undef ~code:Function_undefined fid.at "function" fid.it

let bound_func (ctx : t) (fid : FId.t) : bool =
  find_func_signature_opt ctx fid |> Option.is_some

(* Adders *)

(* Adders for free variables *)

let add_free (ctx : t) (id : Id.t) : t =
  let frees = IdSet.add id ctx.frees in
  { ctx with frees }

let add_frees (ctx : t) (ids : IdSet.t) : t =
  ids |> IdSet.elements |> List.fold_left (fun ctx id -> add_free ctx id) ctx

(* Adders for meta-variables *)

let add_metavar (ctx : t) (tid : TId.t) (typ : Typ.t) : t =
  if bound_metavar ctx tid then
    error_dup ~code:Meta_variable_redefined
      ~related:(related_of_duplicate (region_of_duplicate_metavar ctx tid))
      tid.at "meta-variable" tid.it;
  let menv = MEnv.add tid typ ctx.menv in
  { ctx with menv }

(* Adders for type definitions *)

let add_typdef (ctx : t) (tid : TId.t) (td : Typdef.t) : t =
  if bound_typdef ctx tid then
    error ~code:Type_redeclared
      ~related:
        (match region_of_duplicate_typdef ctx tid with
        | Some at -> [ (at, "first declared here") ]
        | None -> [])
      tid.at
      (Format.asprintf "type `%s` was already declared" tid.it);
  let tdenv = TDEnv.add tid td ctx.tdenv in
  { ctx with tdenv }

let add_tparam (ctx : t) (tparam : tparam) : t =
  let ctx = add_typdef ctx tparam Typdef.Param in
  add_metavar ctx tparam (Il.VarT (tparam, []) $ tparam.at)

let add_tparams (ctx : t) (tparams : tparam list) : t =
  List.fold_left add_tparam ctx tparams

(* Adders for rules *)

let add_extern_rel (ctx : t) (rid : RId.t) (nottyp_il : Il.nottyp)
    (inputs : int list) : t =
  if bound_rel ctx rid then
    error_dup ~code:Extern_relation_redefined
      ~related:(related_of_duplicate (region_of_duplicate_rel ctx rid))
      rid.at "relation" rid.it;
  let rel = Rel.Extern (nottyp_il, inputs) in
  let renv = REnv.add rid rel ctx.renv in
  { ctx with renv }

let add_defined_rel (ctx : t) (rid : RId.t) (nottyp_il : Il.nottyp)
    (inputs : int list) : t =
  if bound_rel ctx rid then
    error_dup ~code:Relation_redefined
      ~related:(related_of_duplicate (region_of_duplicate_rel ctx rid))
      rid.at "relation" rid.it;
  let rel = Rel.Defined (nottyp_il, inputs, [], None) in
  let renv = REnv.add rid rel ctx.renv in
  { ctx with renv }

let add_defined_rulegroup (ctx : t) (rid : RId.t) (rulegroup_il : Il.rulegroup)
    : t =
  (* [elab_rulegroup] has already found [rid] as a defined relation. *)
  assert (bound_defined_rel ctx rid);
  let rulegroupid, _ = rulegroup_il.it in
  check_rulegroup_fresh ctx rid rulegroupid;
  let nottyp_il, inputs, rulegroups_il, elsegroup_il_opt =
    find_defined_rel ctx rid
  in
  let rulegroups_il = rulegroups_il @ [ rulegroup_il ] in
  let rel = Rel.Defined (nottyp_il, inputs, rulegroups_il, elsegroup_il_opt) in
  let renv = renew_region ctx.renv rid rel in
  { ctx with renv }

let add_defined_elsegroup (ctx : t) (rid : RId.t) (elsegroup_il : Il.elsegroup)
    : t =
  (* [elab_rulegroup] has already found [rid] as a defined relation. *)
  assert (bound_defined_rel ctx rid);
  let rulegroupid, _ = elsegroup_il.it in
  check_rulegroup_fresh ctx rid rulegroupid;
  let nottyp_il, inputs, rulegroups_il, elsegroup_il_opt =
    find_defined_rel ctx rid
  in
  match elsegroup_il_opt with
  | Some elsegroup_il_prior ->
      error ~code:Relation_otherwise_redefined elsegroup_il.at
        (Format.asprintf
           "an `otherwise` rule was already defined for relation `%s`" rid.it)
        ~related:
          [ (elsegroup_il_prior.at, "previous `otherwise` rule defined here") ]
  | None ->
      let elsegroup_il_opt = Some elsegroup_il in
      let rel =
        Rel.Defined (nottyp_il, inputs, rulegroups_il, elsegroup_il_opt)
      in
      let renv = renew_region ctx.renv rid rel in
      { ctx with renv }

(* Adders for definitions *)

let add_extern_func_dec (ctx : t) (fid : FId.t) (tparams : tparam list)
    (params_il : Il.param list) (typ_il : Il.typ) : t =
  if bound_func ctx fid then
    error_dup ~code:Extern_function_redefined
      ~related:(related_of_duplicate (region_of_duplicate_dec ctx fid))
      fid.at "extern function" fid.it;
  let func = Func.Extern (tparams, params_il, typ_il) in
  let fenv = FEnv.add fid func ctx.fenv in
  { ctx with fenv }

let add_builtin_func_dec (ctx : t) (fid : FId.t) (tparams : tparam list)
    (params_il : Il.param list) (typ_il : Il.typ) : t =
  if bound_func ctx fid then
    error_dup ~code:Builtin_function_redefined
      ~related:(related_of_duplicate (region_of_duplicate_dec ctx fid))
      fid.at "builtin function" fid.it;
  let func = Func.Builtin (tparams, params_il, typ_il) in
  let fenv = FEnv.add fid func ctx.fenv in
  { ctx with fenv }

let add_table_func_dec (ctx : t) (fid : FId.t) (params_il : Il.param list)
    (typ_il : Il.typ) : t =
  if bound_func ctx fid then
    error_dup ~code:Table_function_redefined
      ~related:(related_of_duplicate (region_of_duplicate_dec ctx fid))
      fid.at "table function" fid.it;
  let func = Func.Table (params_il, typ_il, []) in
  let fenv = FEnv.add fid func ctx.fenv in
  { ctx with fenv }

let add_defined_func_dec (ctx : t) (fid : FId.t) (tparams_il : Il.tparam list)
    (params_il : Il.param list) (typ_il : Il.typ) : t =
  if bound_func ctx fid then
    error_dup ~code:Function_redefined
      ~related:(related_of_duplicate (region_of_duplicate_dec ctx fid))
      fid.at "function" fid.it;
  let func = Func.Defined (tparams_il, params_il, typ_il, [], None) in
  let fenv = FEnv.add fid func ctx.fenv in
  { ctx with fenv }

let add_table_func_tablerows (ctx : t) (fid : FId.t)
    (tablerows : Il.tablerow list) : t =
  (* [elab_table_def_def] has already found [fid] as a table. *)
  assert (find_table_func_opt ctx fid |> Option.is_some);
  let params, plaintyp, tablerows_found = find_table_func ctx fid in
  if List.length tablerows_found > 0 then
    error ~code:Table_rows_redefined
      ~related:[ ((List.hd tablerows_found).at, "first row definition here") ]
      fid.at
      (Format.asprintf "table rows for function `%s` were already defined"
         fid.it);
  let func = Func.Table (params, plaintyp, tablerows) in
  let fenv = renew_region ctx.fenv fid func in
  { ctx with fenv }

let add_defined_func_clause (ctx : t) (fid : FId.t) (clause : Il.clause) : t =
  (* [elab_clause] has already found [fid] as a defined function. *)
  assert (find_defined_func_opt ctx fid |> Option.is_some);
  let tparams, params, plaintyp, clauses, elseclause_opt =
    find_defined_func ctx fid
  in
  let func =
    Func.Defined
      (tparams, params, plaintyp, clauses @ [ clause ], elseclause_opt)
  in
  let fenv = renew_region ctx.fenv fid func in
  { ctx with fenv }

let add_defined_func_elseclause (ctx : t) (fid : FId.t)
    (elseclause : Il.elseclause) : t =
  (* [elab_clause] has already found [fid] as a defined function. *)
  assert (find_defined_func_opt ctx fid |> Option.is_some);
  let tparams, params, plaintyp, clauses, elseclause_opt =
    find_defined_func ctx fid
  in
  match elseclause_opt with
  | Some elseclause_prior ->
      error_dup ~code:Function_otherwise_redefined elseclause.at
        "an `otherwise` clause for function" fid.it
        ~related:
          [ (elseclause_prior.at, "previous `otherwise` clause defined here") ]
  | None ->
      let elseclause_opt = Some elseclause in
      let func =
        Func.Defined (tparams, params, plaintyp, clauses, elseclause_opt)
      in
      let fenv = renew_region ctx.fenv fid func in
      { ctx with fenv }

(* Updaters *)

let update_typdef (ctx : t) (tid : TId.t) (td : Typdef.t) : t =
  (* [elab_typ_def] has already added or found [tid] in [tdenv]. *)
  assert (bound_typdef ctx tid);
  let tdenv = TDEnv.add tid td ctx.tdenv in
  { ctx with tdenv }
