open Domain.Lib
open Lang
open Sl
open Runtime.Dynamic_Sl
open Envs
open Error
open Backtrace
open Util.Source

(* Error *)

let error_undef (at : region) (kind : string) (id : string) =
  error at (Format.asprintf "%s `%s` is undefined" kind id)

let back_undef (at : region) (kind : string) (id : string) =
  back_err at (Format.asprintf "%s `%s` is undefined" kind id)

let error_dup (at : region) (kind : string) (id : string) =
  error at (Format.asprintf "%s `%s` was already defined" kind id)

let back_dup (at : region) (kind : string) (id : string) =
  back_err at (Format.asprintf "%s `%s` was already defined" kind id)

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

type local =
  | Empty
  | Rel of {
      (* Relation name *)
      rid : RId.t;
      (* Input values *)
      values_input : value list;
      (* Map from variables to values *)
      venv : VEnv.t;
    }
  | Func of {
      (* Function name *)
      fid : FId.t;
      (* Input values *)
      values_input : value list;
      (* Map from syntax ids to type definitions *)
      tdenv : TDEnv.t;
      (* Map from function ids to functions *)
      fenv : FEnv.t;
      (* Map from variables to values *)
      venv : VEnv.t;
    }

type t = { global : global; local : local }

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
  | RelD (id, _, exps_match, block, elseblock_opt, _) ->
      let rel = Rel.Defined (exps_match, block, elseblock_opt) in
      add_rel_global id rel
  | ExternDecD (id, _, _, _, _) ->
      let func = Func.Extern in
      add_func_global id func
  | BuiltinDecD (id, _, _, _, _) ->
      let func = Func.Builtin in
      add_func_global id func
  | TableDecD (id, params, _typ, tablerows, _) ->
      let func = Func.Table (params, tablerows) in
      add_func_global id func
  | FuncDecD (id, tparams, params, _typ, block, elseblock_opt, _) ->
      let func = Func.Defined (tparams, params, block, elseblock_opt) in
      add_func_global id func

let init ~(det : bool) (spec : spec) : unit =
  is_det := det;
  List.iter load_def spec

(* Constructor *)

let empty () : t = { global; local = Empty }

(* Finders *)

(* Finders for input values *)

let find_values_input_opt (ctx : t) : Value.t list option =
  match ctx.local with
  | Empty -> None
  | Rel { values_input; _ } -> Some values_input
  | Func { values_input; _ } -> Some values_input

let find_values_input (ctx : t) : Value.t list =
  match find_values_input_opt ctx with
  | Some values_input -> values_input
  | None -> back_err no_region "cannot find input values in empty local context"

(* Finders for values *)

let find_value_opt (ctx : t) (var : Var.t) : Value.t option =
  match ctx.local with
  | Empty -> None
  | Rel { venv; _ } -> VEnv.find_opt var venv
  | Func { venv; _ } -> VEnv.find_opt var venv

let find_value (ctx : t) (var : Var.t) : Value.t =
  match find_value_opt ctx var with
  | Some value -> value
  | None ->
      let id, _ = var in
      back_undef id.at "value" (Var.to_string var)

let bound_value (ctx : t) (var : Var.t) : bool =
  find_value_opt ctx var |> Option.is_some

(* Finders for type definitions *)

let find_typdef_opt (ctx : t) (tid : TId.t) : Typdef.t option =
  let tdenv =
    match ctx.local with
    | Empty | Rel _ -> TDEnv.empty
    | Func { tdenv; _ } -> tdenv
  in
  match TDEnv.find_opt tid tdenv with
  | Some td -> Some td
  | None -> TDTbl.find_opt tid ctx.global.tdtbl

let find_typdef (ctx : t) (tid : TId.t) : Typdef.t =
  match find_typdef_opt ctx tid with
  | Some td -> td
  | None -> back_undef tid.at "type" tid.it

let find_defined_typdef (ctx : t) (tid : TId.t) : tparam list * deftyp =
  match find_typdef ctx tid with
  | Param | Extern -> back_undef tid.at "defined type" tid.it
  | Defined (tparams, deftyp) -> (tparams, deftyp)

let bound_typdef (ctx : t) (tid : TId.t) : bool =
  find_typdef_opt ctx tid |> Option.is_some

(* Finders for rules *)

let find_rel_opt (ctx : t) (rid : RId.t) : Rel.t option =
  RTbl.find_opt rid ctx.global.rtbl

let find_rel (ctx : t) (rid : RId.t) : Rel.t =
  match find_rel_opt ctx rid with
  | Some rel -> rel
  | None -> back_undef rid.at "relation" rid.it

let bound_rel (ctx : t) (rid : RId.t) : bool =
  find_rel_opt ctx rid |> Option.is_some

(* Finders for definitions *)

let find_func_opt (ctx : t) (fid : FId.t) : (cursor * Func.t) option =
  let fenv =
    match ctx.local with
    | Empty | Rel _ -> FEnv.empty
    | Func { fenv; _ } -> fenv
  in
  match FEnv.find_opt fid fenv with
  | Some func -> Some (Local, func)
  | None ->
      FTbl.find_opt fid ctx.global.ftbl
      |> Option.map (fun func -> (Global, func))

let find_func (ctx : t) (fid : FId.t) : cursor * Func.t =
  match find_func_opt ctx fid with
  | Some (cursor, func) -> (cursor, func)
  | None -> back_undef fid.at "function" fid.it

let bound_func (ctx : t) (fid : FId.t) : bool =
  find_func_opt ctx fid |> Option.is_some

(* Adders *)

(* Adders for values *)

let add_value (ctx : t) (var : Var.t) (value : Value.t) : t =
  match ctx.local with
  | Empty ->
      let id, _ = var in
      back_err id.at "cannot add value to empty local context"
  | Rel { rid; values_input; venv } ->
      let venv = VEnv.add var value venv in
      { ctx with local = Rel { rid; values_input; venv } }
  | Func { fid; values_input; tdenv; fenv; venv } ->
      let venv = VEnv.add var value venv in
      { ctx with local = Func { fid; values_input; tdenv; fenv; venv } }

(* Adders for type definitions *)

let add_typdef (ctx : t) (tid : TId.t) (td : Typdef.t) : t =
  if bound_typdef ctx tid then back_dup tid.at "type" tid.it;
  match ctx.local with
  | Empty -> back_err tid.at "cannot add type to empty local context"
  | Rel _ -> back_err tid.at "cannot add type to rule context"
  | Func { fid; values_input; tdenv; fenv; venv } ->
      let tdenv = TDEnv.add tid td tdenv in
      { ctx with local = Func { fid; values_input; tdenv; fenv; venv } }

(* Adders for functions *)

let add_func (ctx : t) (fid : FId.t) (func : Func.t) : t =
  if bound_func ctx fid then back_dup fid.at "function" fid.it;
  match ctx.local with
  | Empty -> back_err fid.at "cannot add function to empty local context"
  | Rel _ -> back_err fid.at "cannot add function to relation context"
  | Func { fid = fid_local; values_input; tdenv; fenv; venv } ->
      let fenv = FEnv.add fid func fenv in
      {
        ctx with
        local = Func { fid = fid_local; values_input; tdenv; fenv; venv };
      }

(* Constructors *)

(* Constructing a local context *)

let localize (ctx : t) : t = { ctx with local = Empty }

let localize_rule (ctx : t) (rid : RId.t) (values_input : value list) : t =
  let local = Rel { rid; values_input; venv = VEnv.empty } in
  { ctx with local }

let localize_func (ctx : t) (fid : FId.t) (values_input : value list)
    (tdenv : TDEnv.t) : t =
  let local =
    Func { fid; values_input; tdenv; fenv = FEnv.empty; venv = VEnv.empty }
  in
  { ctx with local }

let localize_clear (ctx : t) : t =
  match ctx.local with
  | Empty -> back_err no_region "cannot clear empty local context"
  | Rel { rid; values_input; _ } ->
      { ctx with local = Rel { rid; values_input; venv = VEnv.empty } }
  | Func { fid; values_input; tdenv; fenv; _ } ->
      {
        ctx with
        local = Func { fid; values_input; tdenv; fenv; venv = VEnv.empty };
      }

(* Constructing sub-contexts *)

(* Transpose a matrix of values, as a list of value batches
   that are to be each fed into an iterated expression *)

let transpose (value_matrix : value list list) : value list list =
  match value_matrix with
  | [] -> []
  | row_h :: _ ->
      let width = List.length row_h in
      let cols = Array.make width [] in
      List.iter
        (fun row ->
          check_back_err
            (List.length row = width)
            no_region "cannot transpose a matrix of value batches";
          List.iteri (fun j v -> cols.(j) <- v :: cols.(j)) row)
        (List.rev value_matrix);
      Array.to_list cols

let sub_opt (ctx : t) (vars : var list) : t option =
  (* First collect the values that are to be iterated over *)
  let values =
    List.map
      (fun (id, _typ, iters) ->
        find_value ctx (id, iters @ [ Il.Opt ]) |> Value.get_opt)
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
    Some ctx_sub
  else if List.for_all Option.is_none values then None
  else back_err no_region "mismatch in optionality of iterated variables"

let sub_list (ctx : t) (vars : var list) : t list =
  (* First break the values that are to be iterated over,
     into a batch of values *)
  let values_batch =
    List.map
      (fun (id, _typ, iters) ->
        find_value ctx (id, iters @ [ Il.List ]) |> Value.get_list)
      vars
    |> transpose
  in
  (* For each batch of values, create a sub-context *)
  List.map
    (fun value_batch ->
      List.fold_left2
        (fun ctx_sub (id, _typ, iters) value ->
          add_value ctx_sub (id, iters) value)
        ctx vars value_batch)
    values_batch
