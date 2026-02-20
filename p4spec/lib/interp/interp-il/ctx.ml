open Domain.Lib
open Lang
open Il
open Runtime.Dynamic_Il
open Envs
open Error
open Backtrack
open Util.Source

(* Error *)

let error_undef (at : region) (kind : string) (id : string) =
  error at (Format.asprintf "%s `%s` is undefined" kind id)

let error_dup (at : region) (kind : string) (id : string) =
  error at (Format.asprintf "%s `%s` was already defined" kind id)

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

type t = { (* Global layer *)
           global : global; (* Local layer *) local : local }

(* Global constructor *)

let global : global =
  let tdtbl = TDTbl.create ~size:500 in
  let rtbl = RTbl.create ~size:500 in
  let ftbl = FTbl.create ~size:500 in
  { tdtbl; rtbl; ftbl }

(* Adders for globals *)

let add_typdef_global (tid : TId.t) (td : Typdef.t) : unit =
  if TDTbl.find_opt tid global.tdtbl |> Option.is_some then
    error_dup tid.at "type" tid.it;
  TDTbl.add tid td global.tdtbl

let add_rel_global (rid : RId.t) (rel : Rel.t) : unit =
  if RTbl.find_opt rid global.rtbl |> Option.is_some then
    error_dup rid.at "relation" rid.it;
  RTbl.add rid rel global.rtbl

let add_func_global (fid : FId.t) (func : Func.t) : unit =
  if FTbl.find_opt fid global.ftbl |> Option.is_some then
    error_dup fid.at "function" fid.it;
  FTbl.add fid func global.ftbl

(* Global initializer *)

let load_def (def : def) : unit =
  match def.it with
  | ExternTypD (id, _) ->
      let td = Typdef.Extern in
      add_typdef_global id td
  | TypD (id, tparams, deftyp, _) ->
      let td = Typdef.Defined (tparams, deftyp) in
      add_typdef_global id td
  | ExternRelD (id, _, _, _) ->
      let rel = Rel.Extern in
      add_rel_global id rel
  | RelD (id, _, _, rulegroups, elsegroup_opt, _) ->
      let rel = Rel.Defined (rulegroups, elsegroup_opt) in
      add_rel_global id rel
  | ExternDecD (id, _, _, _, _) ->
      let func = Func.Extern in
      add_func_global id func
  | BuiltinDecD (id, _, _, _, _) ->
      let func = Func.Builtin in
      add_func_global id func
  | TableDecD (id, params, _, tablerows, _) ->
      let func = Func.Table (params, tablerows) in
      add_func_global id func
  | FuncDecD (id, tparams, _, _, clauses, elseclause_opt, _) ->
      let func = Func.Defined (tparams, clauses, elseclause_opt) in
      add_func_global id func

let init ~(det : bool) (spec : spec) : unit =
  is_det := det;
  List.iter load_def spec

(* Constructor *)

let empty_local () : local =
  { tdenv = TDEnv.empty; fenv = FEnv.empty; venv = VEnv.empty }

let empty : t = { global; local = empty_local () }

(* Finders *)

(* Finders for values *)

let find_value_opt (ctx : t) (var : Var.t) : Value.t option =
  VEnv.find_opt var ctx.local.venv

let find_value (ctx : t) (var : Var.t) : Value.t =
  match find_value_opt ctx var with
  | Some value -> value
  | None ->
      let id, _ = var in
      error_undef id.at "value" (Var.to_string var)

let bound_value (ctx : t) (var : Var.t) : bool =
  find_value_opt ctx var |> Option.is_some

(* Finders for type definitions *)

let find_typdef_opt (ctx : t) (tid : TId.t) : Typdef.t option =
  match TDEnv.find_opt tid ctx.local.tdenv with
  | Some td -> Some td
  | None -> TDTbl.find_opt tid ctx.global.tdtbl

let find_typdef (ctx : t) (tid : TId.t) : Typdef.t =
  match find_typdef_opt ctx tid with
  | Some td -> td
  | None -> error_undef tid.at "type" tid.it

let find_defined_typdef (ctx : t) (tid : TId.t) : tparam list * deftyp =
  match find_typdef ctx tid with
  | Param | Extern -> error_undef tid.at "defined type" tid.it
  | Defined (tparams, deftyp) -> (tparams, deftyp)

let bound_typdef (ctx : t) (tid : TId.t) : bool =
  find_typdef_opt ctx tid |> Option.is_some

(* Finders for rules *)

let find_rel_opt (ctx : t) (rid : RId.t) : Rel.t option =
  RTbl.find_opt rid ctx.global.rtbl

let find_rel (ctx : t) (rid : RId.t) : Rel.t =
  match find_rel_opt ctx rid with
  | Some rel -> rel
  | None -> error_undef rid.at "relation" rid.it

let bound_rel (ctx : t) (rid : RId.t) : bool =
  find_rel_opt ctx rid |> Option.is_some

(* Finders for definitions *)

let find_func_opt (ctx : t) (fid : FId.t) : (cursor * Func.t) option =
  match FEnv.find_opt fid ctx.local.fenv with
  | Some func -> Some (Local, func)
  | None ->
      FTbl.find_opt fid ctx.global.ftbl
      |> Option.map (fun func -> (Global, func))

let find_func (ctx : t) (fid : FId.t) : cursor * Func.t =
  match find_func_opt ctx fid with
  | Some (cursor, func) -> (cursor, func)
  | None -> error_undef fid.at "function" fid.it

let bound_func (ctx : t) (fid : FId.t) : bool =
  find_func_opt ctx fid |> Option.is_some

(* Adders *)

(* Adders for values *)

let add_value (ctx : t) (var : Var.t) (value : Value.t) : t =
  let venv = VEnv.add var value ctx.local.venv in
  { ctx with local = { ctx.local with venv } }

(* Adders for type definitions *)

let add_typdef (ctx : t) (tid : TId.t) (td : Typdef.t) : t =
  if bound_typdef ctx tid then error_dup tid.at "type" tid.it;
  let tdenv = TDEnv.add tid td ctx.local.tdenv in
  { ctx with local = { ctx.local with tdenv } }

(* Adders for functions *)

let add_func (ctx : t) (fid : FId.t) (func : Func.t) : t =
  if bound_func ctx fid then error_dup fid.at "function" fid.it;
  let fenv = FEnv.add fid func ctx.local.fenv in
  { ctx with local = { ctx.local with fenv } }

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
  let values =
    List.map
      (fun (id, _typ, iters) ->
        find_value ctx (id, iters @ [ Opt ]) |> Value.get_opt)
      vars
  in
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
  let* values_batch =
    List.map
      (fun (id, _typ, iters) ->
        find_value ctx (id, iters @ [ List ]) |> Value.get_list)
      vars
    |> transpose
  in
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
