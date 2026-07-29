open Domain.Lib
open Lang
open El
open Runtime.Type
open Runtime.Static
open Envs
open Error
open Util.Source

(* Error *)

let error_undef (at : region) (kind : string) (id : string) =
  error at (Format.asprintf "%s `%s` is undefined" kind id)

let error_dup (at : region) (kind : string) (id : string) =
  error at (Format.asprintf "%s `%s` was already defined" kind id)

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
  (* Map from table ids to tables *)
  tenv : TEnv.t;
  (* Map from table group ids to groups *)
  genv : Table.group IdMap.t;
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
    tenv = TEnv.empty;
    genv = IdMap.empty;
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

(* Finders for type definitions *)

let find_typdef_opt (ctx : t) (tid : TId.t) : Typdef.t option =
  TDEnv.find_opt tid ctx.tdenv

let find_typdef (ctx : t) (tid : TId.t) : Typdef.t =
  match find_typdef_opt ctx tid with
  | Some td -> td
  | None -> error_undef tid.at "type" tid.it

let bound_typdef (ctx : t) (tid : TId.t) : bool =
  find_typdef_opt ctx tid |> Option.is_some

(* Finders for meta-variables *)

let find_metavar_opt (ctx : t) (tid : TId.t) : Typ.t option =
  MEnv.find_opt tid ctx.menv

let find_metavar (ctx : t) (tid : TId.t) : Typ.t =
  match find_metavar_opt ctx tid with
  | Some typ -> typ
  | None -> error_undef tid.at "meta-variable" tid.it

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
  match find_defined_rel_opt ctx rid with
  | Some (nottyp_il, inputs, rulegroups, elsegroup_opt) ->
      (nottyp_il, inputs, rulegroups, elsegroup_opt)
  | None -> error_undef rid.at "defined relation" rid.it

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
  | None -> error_undef rid.at "relation" rid.it

let bound_rel (ctx : t) (rid : RId.t) : bool =
  find_rel_signature_opt ctx rid |> Option.is_some

let bound_rulegroup (ctx : t) (rid : RId.t) (rulegroupid : Id.t) : bool =
  match find_defined_rel_opt ctx rid with
  | Some (_, _, rulegroups, elsegroup_opt) ->
      let rulegroupids =
        List.map
          (fun rulegroup ->
            let id, _ = rulegroup.it in
            id)
          rulegroups
        @
        match elsegroup_opt with
        | Some elsegroup ->
            let id, _ = elsegroup.it in
            [ id ]
        | None -> []
      in
      List.exists (Id.eq rulegroupid) rulegroupids
  | None -> false

(* Finders for definitions *)

let find_table_opt (ctx : t) (fid : FId.t) :
    (Il.param list * Il.typ * Il.tablerow list) option =
  TEnv.find_opt fid ctx.tenv

let find_table (ctx : t) (fid : FId.t) :
    Il.param list * Il.typ * Il.tablerow list =
  match find_table_opt ctx fid with
  | Some (params_il, typ_il, tablerows_il) -> (params_il, typ_il, tablerows_il)
  | None -> error_undef fid.at "table function" fid.it

let bound_table (ctx : t) (fid : FId.t) : bool =
  find_table_opt ctx fid |> Option.is_some

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
    | Func.Extern _ | Func.Builtin _ -> None)

let find_defined_func (ctx : t) (fid : FId.t) :
    Il.tparam list * Il.param list * Il.typ * Il.clause list * Il.clause option
    =
  match find_defined_func_opt ctx fid with
  | Some (tparams_il, params_il, typ_il, clauses_il, elseclause_il_opt) ->
      (tparams_il, params_il, typ_il, clauses_il, elseclause_il_opt)
  | None -> error_undef fid.at "defined function" fid.it

let bound_defined_func (ctx : t) (fid : FId.t) : bool =
  find_defined_func_opt ctx fid |> Option.is_some

let find_func_signature_opt (ctx : t) (fid : FId.t) :
    (Il.tparam list * Il.param list * Il.typ) option =
  FEnv.find_opt fid ctx.fenv
  |> Option.map (function
       | Func.Extern (tparams_il, params_il, typ_il)
       | Func.Builtin (tparams_il, params_il, typ_il)
       | Func.Defined (tparams_il, params_il, typ_il, _, _) ->
           (tparams_il, params_il, typ_il))

(* a table resolves to its monomorphic signature (empty tparams) *)
let find_table_signature_opt (ctx : t) (fid : FId.t) :
    (Il.tparam list * Il.param list * Il.typ) option =
  find_table_opt ctx fid
  |> Option.map (fun (params_il, typ_il, _) -> ([], params_il, typ_il))

let find_func_signature (ctx : t) (fid : FId.t) :
    Il.tparam list * Il.param list * Il.typ =
  match find_func_signature_opt ctx fid with
  | Some (tparams_il, params_il, typ_il) -> (tparams_il, params_il, typ_il)
  | None -> error_undef fid.at "function" fid.it

let find_table_signature (ctx : t) (fid : FId.t) :
    Il.tparam list * Il.param list * Il.typ =
  match find_table_signature_opt ctx fid with
  | Some (tparams_il, params_il, typ_il) -> (tparams_il, params_il, typ_il)
  | None -> error_undef fid.at "table" fid.it

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
  if bound_metavar ctx tid then error_dup tid.at "meta-variable" tid.it;
  let menv = MEnv.add tid typ ctx.menv in
  { ctx with menv }

(* Adders for type definitions *)

let add_typdef (ctx : t) (tid : TId.t) (td : Typdef.t) : t =
  if bound_typdef ctx tid then error_dup tid.at "type" tid.it;
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
  if bound_rel ctx rid then error_dup rid.at "relation" rid.it;
  let rel = Rel.Extern (nottyp_il, inputs) in
  let renv = REnv.add rid rel ctx.renv in
  { ctx with renv }

let add_defined_rel (ctx : t) (rid : RId.t) (nottyp_il : Il.nottyp)
    (inputs : int list) : t =
  if bound_rel ctx rid then error_dup rid.at "relation" rid.it;
  let rel = Rel.Defined (nottyp_il, inputs, [], None) in
  let renv = REnv.add rid rel ctx.renv in
  { ctx with renv }

let add_defined_rulegroup (ctx : t) (rid : RId.t) (rulegroup_il : Il.rulegroup)
    : t =
  if not (bound_defined_rel ctx rid) then error_undef rid.at "relation" rid.it;
  let rulegroupid, _ = rulegroup_il.it in
  if bound_rulegroup ctx rid rulegroupid then
    error_dup rulegroupid.at "rulegroup" rulegroupid.it;
  let nottyp_il, inputs, rulegroups_il, elsegroup_il_opt =
    find_defined_rel ctx rid
  in
  let rulegroups_il = rulegroups_il @ [ rulegroup_il ] in
  let rel = Rel.Defined (nottyp_il, inputs, rulegroups_il, elsegroup_il_opt) in
  let renv = REnv.add rid rel ctx.renv in
  { ctx with renv }

let add_defined_elsegroup (ctx : t) (rid : RId.t) (elsegroup_il : Il.elsegroup)
    : t =
  if not (bound_defined_rel ctx rid) then error_undef rid.at "relation" rid.it;
  let rulegroupid, _ = elsegroup_il.it in
  if bound_rulegroup ctx rid rulegroupid then
    error_dup rulegroupid.at "rulegroup" rulegroupid.it;
  let nottyp_il, inputs, rulegroups_il, elsegroup_il_opt =
    find_defined_rel ctx rid
  in
  match elsegroup_il_opt with
  | Some _ -> error_dup elsegroup_il.at "otherwise group for relation" rid.it
  | None ->
      let elsegroup_il_opt = Some elsegroup_il in
      let rel =
        Rel.Defined (nottyp_il, inputs, rulegroups_il, elsegroup_il_opt)
      in
      let renv = REnv.add rid rel ctx.renv in
      { ctx with renv }

(* Adders for definitions *)

let add_extern_func_dec (ctx : t) (fid : FId.t) (tparams : tparam list)
    (params_il : Il.param list) (typ_il : Il.typ) : t =
  if bound_func ctx fid then error_dup fid.at "extern function" fid.it;
  let func = Func.Extern (tparams, params_il, typ_il) in
  let fenv = FEnv.add fid func ctx.fenv in
  { ctx with fenv }

let add_builtin_func_dec (ctx : t) (fid : FId.t) (tparams : tparam list)
    (params_il : Il.param list) (typ_il : Il.typ) : t =
  if bound_func ctx fid then error_dup fid.at "builtin function" fid.it;
  let func = Func.Builtin (tparams, params_il, typ_il) in
  let fenv = FEnv.add fid func ctx.fenv in
  { ctx with fenv }

let add_table_dec (ctx : t) (fid : FId.t) (params_il : Il.param list)
    (typ_il : Il.typ) : t =
  if bound_table ctx fid then error_dup fid.at "table function" fid.it;
  let table = (params_il, typ_il, []) in
  let tenv = TEnv.add fid table ctx.tenv in
  { ctx with tenv }

let add_defined_func_dec (ctx : t) (fid : FId.t) (tparams_il : Il.tparam list)
    (params_il : Il.param list) (typ_il : Il.typ) : t =
  if bound_func ctx fid then error_dup fid.at "function" fid.it;
  let func = Func.Defined (tparams_il, params_il, typ_il, [], None) in
  let fenv = FEnv.add fid func ctx.fenv in
  { ctx with fenv }

let add_table_rows (ctx : t) (fid : FId.t)
    (tablerows : Il.tablerow list) : t =
  if not (bound_table ctx fid) then
    error_undef (List.hd tablerows).at "table function" fid.it;
  let params, plaintyp, tablerows_found = find_table ctx fid in
  if List.length tablerows_found > 0 then
    error_dup (List.hd tablerows_found).at "table function" fid.it;
  let table = (params, plaintyp, tablerows) in
  let tenv = TEnv.add fid table ctx.tenv in
  { ctx with tenv }

(* Table groups *)

let find_group_opt (ctx : t) (gid : Id.t) : Table.group option =
  IdMap.find_opt gid ctx.genv

let find_group (ctx : t) (gid : Id.t) : Table.group =
  match find_group_opt ctx gid with
  | Some group -> group
  | None -> error_undef gid.at "table group" gid.it

let bound_group (ctx : t) (gid : Id.t) : bool =
  find_group_opt ctx gid |> Option.is_some

let add_group_dec (ctx : t) (gid : Id.t) (param_il : Il.param)
    (typ_il : Il.typ) (cols : (Id.t * Il.hint list) list) : t =
  if bound_group ctx gid then error_dup gid.at "table group" gid.it;
  let group : Table.group =
    { param = param_il; typ = typ_il; cols; rows = IdMap.empty }
  in
  let genv = IdMap.add gid group ctx.genv in
  { ctx with genv }

let add_group_col_rows (ctx : t) (gid : Id.t) (cid : Id.t)
    (tablerows : Il.tablerow list) : t =
  let group = find_group ctx gid in
  if not (List.exists (fun (c, _) -> Id.eq c cid) group.cols) then
    error_undef cid.at "table group column" cid.it;
  if IdMap.mem cid group.rows then error_dup cid.at "table group column" cid.it;
  let group = { group with rows = IdMap.add cid tablerows group.rows } in
  let genv = IdMap.add gid group ctx.genv in
  { ctx with genv }

let add_defined_func_clause (ctx : t) (fid : FId.t) (clause : Il.clause) : t =
  if not (bound_func ctx fid) then error_undef clause.at "function" fid.it;
  let tparams, params, plaintyp, clauses, elseclause_opt =
    find_defined_func ctx fid
  in
  let func =
    Func.Defined
      (tparams, params, plaintyp, clauses @ [ clause ], elseclause_opt)
  in
  let fenv = FEnv.add fid func ctx.fenv in
  { ctx with fenv }

let add_defined_func_elseclause (ctx : t) (fid : FId.t)
    (elseclause : Il.elseclause) : t =
  if not (bound_func ctx fid) then error_undef elseclause.at "function" fid.it;
  let tparams, params, plaintyp, clauses, elseclause_opt =
    find_defined_func ctx fid
  in
  match elseclause_opt with
  | Some _ -> error_dup elseclause.at "otherwise clause for function" fid.it
  | None ->
      let elseclause_opt = Some elseclause in
      let func =
        Func.Defined (tparams, params, plaintyp, clauses, elseclause_opt)
      in
      let fenv = FEnv.add fid func ctx.fenv in
      { ctx with fenv }

(* Updaters *)

let update_typdef (ctx : t) (tid : TId.t) (td : Typdef.t) : t =
  if not (bound_typdef ctx tid) then error_undef tid.at "type" tid.it;
  let tdenv = TDEnv.add tid td ctx.tdenv in
  { ctx with tdenv }
