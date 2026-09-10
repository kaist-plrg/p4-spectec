open Domain.Lib
open Lang
open Il
open Al
module Typdef = Runtime.Type.Typdef
open Runtime.Dynamic_Al
open Envs
open Backtrack
open Util.Source

(* Backtracing *)

let back_undef (at : region) (kind : string) (id : string) =
  back_err at (Format.asprintf "%s `%s` is undefined" kind id)

let back_dup (at : region) (kind : string) (id : string) =
  back_err at (Format.asprintf "%s `%s` was already defined" kind id)

let error_dup (at : region) (kind : string) (id : string) : Diagnostic.t =
  Interp_common.Error.error at
    (Format.asprintf "%s `%s` was already defined" kind id)

module Make () = struct
  (* Cursor *)

  type cursor = Global | Local

  (* Mode *)

  let is_det : bool ref = ref false

  (* Context *)

  (* Global layer *)

  type global = {
    (* Map from syntax ids to type definitions *)
    tdtbl : TDTbl.t;
    (* Map from relation ids to relations *)
    rtbl : RTbl.t;
    (* Map from function ids to functions *)
    ftbl : FTbl.t;
  }

  (* Local layer *)

  type local = {
    (* Map from syntax ids to type definitions *)
    tdenv : TDEnv.t;
    (* Map from function ids to functions *)
    fenv : FEnv.t;
    (* Map from variables to values *)
    venv : VEnv.t;
  }

  type t = {
    (* Global layer *)
    global : global;
    (* Local layer *) local : local;
  }

  (* Global constructor *)

  let global : global =
    let tdtbl = TDTbl.create ~size:500 in
    let rtbl = RTbl.create ~size:500 in
    let ftbl = FTbl.create ~size:500 in
    { tdtbl; rtbl; ftbl }

  (* Adders for globals *)

  let add_typdef_global (tid : TId.t) (td : Typdef.t) :
      (unit, Diagnostic.t) result =
    if TDTbl.find_opt tid global.tdtbl |> Option.is_some then
      Error (error_dup tid.at "type" tid.it)
    else Ok (TDTbl.add tid td global.tdtbl)

  let add_rel_global (rid : RId.t) (rel : Rel.t) : (unit, Diagnostic.t) result =
    if RTbl.find_opt rid global.rtbl |> Option.is_some then
      Error (error_dup rid.at "relation" rid.it)
    else Ok (RTbl.add rid rel global.rtbl)

  let add_func_global (fid : FId.t) (func : Func.t) :
      (unit, Diagnostic.t) result =
    if FTbl.find_opt fid global.ftbl |> Option.is_some then
      Error (error_dup fid.at "function" fid.it)
    else Ok (FTbl.add fid func global.ftbl)

  (* Global initializer *)

  let load_def (def : def) : (unit, Diagnostic.t) result =
    match def.it with
    | ExternTypD (id, _) ->
        let td = Typdef.Extern in
        add_typdef_global id td
    | TypD (id, tparams, deftyp, _) ->
        let td = Typdef.Defined (tparams, deftyp) in
        add_typdef_global id td
    | VarD _ -> Ok ()
    | ExternRelD (id, nottyp, inputs, _) ->
        let rel = Rel.Extern (nottyp, inputs) in
        add_rel_global id rel
    | RelD (id, nottyp, input, rulegroups, elsegroup_opt, _) ->
        let rel = Rel.Defined (nottyp, input, rulegroups, elsegroup_opt) in
        add_rel_global id rel
    | ExternDecD (id, tparams, params, typ, _) ->
        let func = Func.Extern (tparams, params, typ) in
        add_func_global id func
    | BuiltinDecD (id, tparams, params, typ, _) ->
        let func = Func.Builtin (tparams, params, typ) in
        add_func_global id func
    | TableDecD (id, params, typ, tablerows, _) ->
        let func = Func.Table (params, typ, tablerows) in
        add_func_global id func
    | FuncDecD (id, tparams, params, typ, clauses, elseclause_opt, _) ->
        let func =
          Func.Defined (tparams, params, typ, clauses, elseclause_opt)
        in
        add_func_global id func

  let rec load_defs (defs : def list) : (unit, Diagnostic.t) result =
    match defs with
    | [] -> Ok ()
    | def_h :: defs_t -> (
        match load_def def_h with
        | Ok () -> load_defs defs_t
        | Error _ as error -> error)

  let init ~(det : bool) (spec : spec) : (unit, Diagnostic.t) result =
    is_det := det;
    load_defs spec

  (* Constructor *)

  let empty_local () : local =
    { tdenv = TDEnv.empty; fenv = FEnv.empty; venv = VEnv.empty }

  let empty : t = { global; local = empty_local () }

  (* Finders *)

  (* Finders for values *)

  let find_value_opt (ctx : t) (var : Var.t) : Value.t option =
    VEnv.find_opt var ctx.local.venv

  let find_value (ctx : t) (var : Var.t) : Value.t backtrack =
    match find_value_opt ctx var with
    | Some value -> Ok value
    | None ->
        let id, _ = var in
        back_undef id.at "value" (Var.to_string var)

  let rec find_values (ctx : t) (vars : Var.t list) : Value.t list backtrack =
    match vars with
    | [] -> Ok []
    | var_h :: vars_t ->
        let* value_h = find_value ctx var_h in
        let* values_t = find_values ctx vars_t in
        Ok (value_h :: values_t)

  let bound_value (ctx : t) (var : Var.t) : bool =
    find_value_opt ctx var |> Option.is_some

  (* Finders for type definitions *)

  let find_typdef_opt (ctx : t) (tid : TId.t) : Typdef.t option =
    match TDEnv.find_opt tid ctx.local.tdenv with
    | Some td -> Some td
    | None -> TDTbl.find_opt tid ctx.global.tdtbl

  let find_typdef (ctx : t) (tid : TId.t) : Typdef.t backtrack =
    match find_typdef_opt ctx tid with
    | Some td -> Ok td
    | None -> back_undef tid.at "type" tid.it

  let find_defined_typdef (ctx : t) (tid : TId.t) :
      (tparam list * deftyp) backtrack =
    let* td = find_typdef ctx tid in
    match td with
    | Param | Extern | Defining _ -> back_undef tid.at "defined type" tid.it
    | Defined (tparams, deftyp) -> Ok (tparams, deftyp)

  let bound_typdef (ctx : t) (tid : TId.t) : bool =
    find_typdef_opt ctx tid |> Option.is_some

  (* Finders for rules *)

  let find_rel_opt (ctx : t) (rid : RId.t) : Rel.t option =
    RTbl.find_opt rid ctx.global.rtbl

  let find_rel (ctx : t) (rid : RId.t) : Rel.t backtrack =
    match find_rel_opt ctx rid with
    | Some rel -> Ok rel
    | None -> back_undef rid.at "relation" rid.it

  let find_rel_signature_opt (ctx : t) (rid : RId.t) :
      (nottyp * Hints.Input.t) option =
    find_rel_opt ctx rid |> Option.map Rel.get_signature

  let find_rel_signature (ctx : t) (rid : RId.t) :
      (nottyp * Hints.Input.t) backtrack =
    match find_rel_signature_opt ctx rid with
    | Some (nottyp, inputs) -> Ok (nottyp, inputs)
    | None -> back_undef rid.at "relation" rid.it

  let bound_rel (ctx : t) (rid : RId.t) : bool =
    find_rel_opt ctx rid |> Option.is_some

  (* Finders for definitions *)

  let find_func_opt (ctx : t) (fid : FId.t) : (cursor * Func.t) option =
    match FEnv.find_opt fid ctx.local.fenv with
    | Some func -> Some (Local, func)
    | None ->
        FTbl.find_opt fid ctx.global.ftbl
        |> Option.map (fun func -> (Global, func))

  let find_func (ctx : t) (fid : FId.t) : (cursor * Func.t) backtrack =
    match find_func_opt ctx fid with
    | Some (cursor, func) -> Ok (cursor, func)
    | None -> back_undef fid.at "function" fid.it

  let find_func_signature_opt (ctx : t) (fid : FId.t) :
      (tparam list * typ list * typ) option =
    find_func_opt ctx fid
    |> Option.map (fun (_, func) -> Func.get_signature func)

  let find_func_signature (ctx : t) (fid : FId.t) :
      (tparam list * typ list * typ) backtrack =
    match find_func_signature_opt ctx fid with
    | Some (tparams, typs, typ) -> Ok (tparams, typs, typ)
    | None -> back_undef fid.at "function" fid.it

  let bound_func (ctx : t) (fid : FId.t) : bool =
    find_func_opt ctx fid |> Option.is_some

  (* Adders *)

  (* Adders for values *)

  let add_value (ctx : t) (var : Var.t) (value : Value.t) : t =
    let venv = VEnv.add var value ctx.local.venv in
    { ctx with local = { ctx.local with venv } }

  (* Adders for type definitions *)

  let add_typdef (ctx : t) (tid : TId.t) (td : Typdef.t) : t backtrack =
    if bound_typdef ctx tid then back_dup tid.at "type" tid.it
    else
      let tdenv = TDEnv.add tid td ctx.local.tdenv in
      Ok { ctx with local = { ctx.local with tdenv } }

  (* Adders for functions *)

  let add_func (ctx : t) (fid : FId.t) (func : Func.t) : t backtrack =
    if bound_func ctx fid then back_dup fid.at "function" fid.it
    else
      let fenv = FEnv.add fid func ctx.local.fenv in
      Ok { ctx with local = { ctx.local with fenv } }

  (* Constructors *)

  (* Constructing a local context *)

  let localize (ctx : t) : t =
    let local = empty_local () in
    { ctx with local }

  (* Constructing sub-contexts *)

  (* Transpose a matrix of values, as a list of value batches
     that are to be each fed into an iterated expression *)

  let transpose (value_matrix : value list list) : value list list backtrack =
    match value_matrix with
    | [] -> Ok []
    | row_h :: _ -> (
        let width = List.length row_h in
        let cols = Array.make width [] in
        try
          List.iter
            (fun row ->
              if List.length row <> width then
                raise
                  (Invalid_argument "cannot transpose a matrix of value batches");
              List.iteri (fun j v -> cols.(j) <- v :: cols.(j)) row)
            (List.rev value_matrix);
          Ok (Array.to_list cols)
        with Invalid_argument msg -> back_err no_region msg)

  let sub_opt (ctx : t) (vars : var list) : t option backtrack =
    (* First collect the values that are to be iterated over *)
    let* values =
      find_values ctx
        (List.map (fun (id, _typ, iters) -> (id, iters @ [ Opt ])) vars)
    in
    let values = List.map Value.Get.opt values in
    (* Iteration is valid when all variables agree on their optionality *)
    if List.for_all Option.is_some values then
      let values = List.map Option.get values in
      let ctx_sub =
        List.fold_left2
          (fun ctx_sub (id, _typ, iters) value ->
            add_value ctx_sub (id, iters) value)
          ctx vars values
      in
      Ok (Some ctx_sub)
    else if List.for_all Option.is_none values then Ok None
    else back_err no_region "mismatch in optionality of iterated variables"

  let sub_list (ctx : t) (vars : var list) : t list backtrack =
    (* First break the values that are to be iterated over,
       into a batch of values *)
    let* values =
      find_values ctx
        (List.map (fun (id, _typ, iters) -> (id, iters @ [ List ])) vars)
    in
    let* values_batch = values |> List.map Value.Get.list |> transpose in
    (* For each batch of values, create a sub-context *)
    let ctxs_sub =
      List.fold_left
        (fun ctxs_sub value_batch ->
          let ctx_sub =
            List.fold_left2
              (fun ctx_sub (id, _typ, iters) value ->
                add_value ctx_sub (id, iters) value)
              ctx vars value_batch
          in
          ctxs_sub @ [ ctx_sub ])
        [] values_batch
    in
    Ok ctxs_sub
end
