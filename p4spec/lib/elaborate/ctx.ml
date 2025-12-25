open Domain.Lib
open Lang
open El
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
  menv : PTEnv.t;
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
    menv = PTEnv.empty;
    renv = REnv.empty;
    fenv = FEnv.empty;
  }

let init () : t =
  let menv =
    PTEnv.empty
    |> PTEnv.add ("bool" $ no_region) (BoolT $ no_region)
    |> PTEnv.add ("nat" $ no_region) (NumT `NatT $ no_region)
    |> PTEnv.add ("int" $ no_region) (NumT `IntT $ no_region)
    |> PTEnv.add ("text" $ no_region) (TextT $ no_region)
  in
  { empty with menv }

(* Finders *)

(* Finders for variables *)

let bound_var (ctx : t) (id : Id.t) : bool = VEnv.mem id ctx.venv

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

let find_metavar_opt (ctx : t) (tid : TId.t) : Plaintyp.t option =
  TDEnv.find_opt tid ctx.menv

let find_metavar (ctx : t) (tid : TId.t) : Plaintyp.t =
  match find_metavar_opt ctx tid with
  | Some plaintyp -> plaintyp
  | None -> error_undef tid.at "meta-variable" tid.it

let bound_metavar (ctx : t) (tid : TId.t) : bool =
  find_metavar_opt ctx tid |> Option.is_some

(* Finders for rules *)

let find_defined_rel_opt (ctx : t) (rid : RId.t) :
    (nottyp * Il.nottyp * int list * Il.rulegroup list) option =
  let rel_opt = REnv.find_opt rid ctx.renv in
  Option.bind rel_opt (function
    | Rel.Defined (nottyp, nottyp_il, inputs, rulegroups) ->
        Some (nottyp, nottyp_il, inputs, rulegroups)
    | Rel.Extern _ -> None)

let find_defined_rel (ctx : t) (rid : RId.t) :
    nottyp * Il.nottyp * int list * Il.rulegroup list =
  match find_defined_rel_opt ctx rid with
  | Some (nottyp, nottyp_il, inputs, rulegroups) ->
      (nottyp, nottyp_il, inputs, rulegroups)
  | None -> error_undef rid.at "defined relation" rid.it

let bound_defined_rel (ctx : t) (rid : RId.t) : bool =
  find_defined_rel_opt ctx rid |> Option.is_some

let find_rel_signature_opt (ctx : t) (rid : RId.t) :
    (nottyp * Il.nottyp * int list) option =
  REnv.find_opt rid ctx.renv
  |> Option.map (function
         | Rel.Extern (nottyp, nottyp_il, inputs)
         | Rel.Defined (nottyp, nottyp_il, inputs, _)
         -> (nottyp, nottyp_il, inputs))

let find_rel_signature (ctx : t) (rid : RId.t) : nottyp * Il.nottyp * int list =
  match find_rel_signature_opt ctx rid with
  | Some (nottyp, nottyp_il, inputs) -> (nottyp, nottyp_il, inputs)
  | None -> error_undef rid.at "relation" rid.it

let bound_rel (ctx : t) (rid : RId.t) : bool =
  find_rel_signature_opt ctx rid |> Option.is_some

let bound_rulegroup (ctx : t) (rid : RId.t) (rulegroupid : Id.t) : bool =
  match find_defined_rel_opt ctx rid with
  | Some (_, _, _, rulegroups) ->
      List.exists
        (fun rulegroup ->
          let id, _, _ = rulegroup.it in
          Id.eq id rulegroupid)
        rulegroups
  | None -> false

(* Finders for definitions *)

let find_table_func_opt (ctx : t) (fid : FId.t) :
    (param list * plaintyp * Il.tablerow list) option =
  let func_opt = FEnv.find_opt fid ctx.fenv in
  Option.bind func_opt (function
    | Func.Table (params, plaintyp, tablerows) ->
        Some (params, plaintyp, tablerows)
    | Func.Defined _ | Func.Extern _ | Func.Builtin _ -> None)

let find_table_func (ctx : t) (fid : FId.t) :
    param list * plaintyp * Il.tablerow list =
  match find_table_func_opt ctx fid with
  | Some (params, plaintyp, tablerows) -> (params, plaintyp, tablerows)
  | None -> error_undef fid.at "table function" fid.it

let find_defined_func_opt (ctx : t) (fid : FId.t) :
    (tparam list * param list * plaintyp * Il.clause list) option =
  let func_opt = FEnv.find_opt fid ctx.fenv in
  Option.bind func_opt (function
    | Func.Defined (tparams, params, plaintyp, clauses) ->
        Some (tparams, params, plaintyp, clauses)
    | Func.Table _ | Func.Extern _ | Func.Builtin _ -> None)

let find_defined_func (ctx : t) (fid : FId.t) :
    tparam list * param list * plaintyp * Il.clause list =
  match find_defined_func_opt ctx fid with
  | Some (tparams, params, plaintyp, clauses) ->
      (tparams, params, plaintyp, clauses)
  | None -> error_undef fid.at "plain function" fid.it

let bound_defined_func (ctx : t) (fid : FId.t) : bool =
  find_defined_func_opt ctx fid |> Option.is_some

let find_func_signature_opt (ctx : t) (fid : FId.t) :
    (tparam list * param list * plaintyp) option =
  FEnv.find_opt fid ctx.fenv
  |> Option.map (function
       | Func.Extern (tparams, params, plaintyp)
       | Func.Builtin (tparams, params, plaintyp)
       | Func.Defined (tparams, params, plaintyp, _) ->
           (tparams, params, plaintyp)
       | Func.Table (params, plaintyp, _) -> ([], params, plaintyp))

let find_func_signature (ctx : t) (fid : FId.t) :
    tparam list * param list * plaintyp =
  match find_func_signature_opt ctx fid with
  | Some (tparams, params, plaintyp) -> (tparams, params, plaintyp)
  | None -> error_undef fid.at "function" fid.it

let bound_func (ctx : t) (fid : FId.t) : bool =
  find_func_signature_opt ctx fid |> Option.is_some

(* Adders *)

(* Adders for free variables *)

let add_free (ctx : t) (id : Id.t) : t =
  let frees = IdSet.add id ctx.frees in
  { ctx with frees }

let add_frees (ctx : t) (ids : IdSet.t) : t =
  ids |> IdSet.elements |> List.fold_left (fun ctx id -> add_free ctx id) ctx

(* Adders for variables *)

let add_var (ctx : t) (var : Id.t * Typ.t) : t =
  let id, typ = var in
  if bound_var ctx id then error_dup id.at "variable" id.it;
  let venv = VEnv.add id typ ctx.venv in
  { ctx with venv }

let add_vars (ctx : t) (vars : (Id.t * Typ.t) list) : t =
  List.fold_left add_var ctx vars

(* Adders for meta-variables *)

let add_metavar (ctx : t) (tid : TId.t) (plaintyp : Plaintyp.t) : t =
  if bound_metavar ctx tid then error_dup tid.at "meta-variable" tid.it;
  let menv = PTEnv.add tid plaintyp ctx.menv in
  { ctx with menv }

(* Adders for type definitions *)

let add_typdef (ctx : t) (tid : TId.t) (td : Typdef.t) : t =
  if bound_typdef ctx tid then error_dup tid.at "type" tid.it;
  let tdenv = TDEnv.add tid td ctx.tdenv in
  { ctx with tdenv }

let add_tparam (ctx : t) (tparam : tparam) : t =
  let ctx = add_typdef ctx tparam Typdef.Param in
  add_metavar ctx tparam (VarT (tparam, []) $ tparam.at)

let add_tparams (ctx : t) (tparams : tparam list) : t =
  List.fold_left add_tparam ctx tparams

(* Adders for rules *)

let add_extern_rel (ctx : t) (rid : RId.t) (nottyp : nottyp)
    (nottyp_il : Il.nottyp) (inputs : int list) : t =
  if bound_rel ctx rid then error_dup rid.at "relation" rid.it;
  let rel = Rel.Extern (nottyp, nottyp_il, inputs) in
  let renv = REnv.add rid rel ctx.renv in
  { ctx with renv }

let add_defined_rel (ctx : t) (rid : RId.t) (nottyp : nottyp)
    (nottyp_il : Il.nottyp) (inputs : int list) : t =
  if bound_rel ctx rid then error_dup rid.at "relation" rid.it;
  let rel = Rel.Defined (nottyp, nottyp_il, inputs, []) in
  let renv = REnv.add rid rel ctx.renv in
  { ctx with renv }

let add_defined_rulegroup (ctx : t) (rid : RId.t) (rulegroup : Il.rulegroup) : t
    =
  if not (bound_defined_rel ctx rid) then error_undef rid.at "relation" rid.it;
  let rulegroupid, _, _ = rulegroup.it in
  if bound_rulegroup ctx rid rulegroupid then
    error_dup rulegroupid.at "rulegroup" rulegroupid.it;
  let nottyp, nottyp_il, inputs, rulegroups = find_defined_rel ctx rid in
  let rulegroups = rulegroups @ [ rulegroup ] in
  let rel = Rel.Defined (nottyp, nottyp_il, inputs, rulegroups) in
  let renv = REnv.add rid rel ctx.renv in
  { ctx with renv }

(* Adders for definitions *)

let add_extern_func_dec (ctx : t) (fid : FId.t) (tparams : tparam list)
    (params : param list) (plaintyp : plaintyp) : t =
  if bound_func ctx fid then error_dup fid.at "extern function" fid.it;
  let func = Func.Extern (tparams, params, plaintyp) in
  let fenv = FEnv.add fid func ctx.fenv in
  { ctx with fenv }

let add_builtin_func_dec (ctx : t) (fid : FId.t) (tparams : tparam list)
    (params : param list) (plaintyp : plaintyp) : t =
  if bound_func ctx fid then error_dup fid.at "builtin function" fid.it;
  let func = Func.Builtin (tparams, params, plaintyp) in
  let fenv = FEnv.add fid func ctx.fenv in
  { ctx with fenv }

let add_table_func_dec (ctx : t) (fid : FId.t) (params : param list)
    (plaintyp : plaintyp) : t =
  if bound_func ctx fid then error_dup fid.at "table function" fid.it;
  let func = Func.Table (params, plaintyp, []) in
  let fenv = FEnv.add fid func ctx.fenv in
  { ctx with fenv }

let add_defined_func_dec (ctx : t) (fid : FId.t) (tparams : tparam list)
    (params : param list) (plaintyp : plaintyp) : t =
  if bound_func ctx fid then error_dup fid.at "function" fid.it;
  let func = Func.Defined (tparams, params, plaintyp, []) in
  let fenv = FEnv.add fid func ctx.fenv in
  { ctx with fenv }

let add_table_func_tablerows (ctx : t) (fid : FId.t)
    (tablerows : Il.tablerow list) : t =
  if not (bound_func ctx fid) then
    error_undef (List.hd tablerows).at "table function" fid.it;
  let params, plaintyp, tablerows_found = find_table_func ctx fid in
  if List.length tablerows_found > 0 then
    error_dup (List.hd tablerows_found).at "table function" fid.it;
  let func = Func.Table (params, plaintyp, tablerows) in
  let fenv = FEnv.add fid func ctx.fenv in
  { ctx with fenv }

let add_defined_func_clause (ctx : t) (fid : FId.t) (clause : Il.clause) : t =
  if not (bound_func ctx fid) then error_undef clause.at "function" fid.it;
  let tparams, params, plaintyp, clauses = find_defined_func ctx fid in
  let func = Func.Defined (tparams, params, plaintyp, clauses @ [ clause ]) in
  let fenv = FEnv.add fid func ctx.fenv in
  { ctx with fenv }

(* Updaters *)

let update_typdef (ctx : t) (tid : TId.t) (td : Typdef.t) : t =
  if not (bound_typdef ctx tid) then error_undef tid.at "type" tid.it;
  let tdenv = TDEnv.add tid td ctx.tdenv in
  { ctx with tdenv }
