open Domain.Lib
module InputHint = Runtime_static.Rel.InputHint
open Runtime_dynamic
open Runtime_dynamic_sl
open Envs
module Dep = Runtime_testgen.Dep
module SCov = Runtime_testgen.Cov.Single
open Sl.Ast
open Error
open Util.Source

(* Error *)

let error_undef (at : region) (kind : string) (id : string) =
  error at (Format.asprintf "%s `%s` is undefined" kind id)

let error_dup (at : region) (kind : string) (id : string) =
  error at (Format.asprintf "%s `%s` was already defined" kind id)

(* Cursor *)

type cursor = Global | Local

(* Context *)

(* Testing and coverage layer

   The interpreter relies on the fact that both graph and cover
   are mutable, so that they can be updated in place.
   Their references are copied when constructing sub-contexts,
   thus sharing the same graph and cover across contexts. *)

type coverage = SCov.Cover.t ref
type vdg = { graph : Dep.Graph.t; vid_program : vid }
type testing = EndToEnd of [ `On of vdg | `Off of vdg ] | Partial

(* Global layer *)

type global = {
  (* Map from syntax ids to type definitions *)
  tdenv : TDEnv.t;
  (* Map from relation ids to relations *)
  renv : REnv.t;
  (* Map from function ids to functions *)
  fenv : FEnv.t;
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

type t = {
  (* Testing and coverage layers *)
  coverage : coverage;
  testing : testing;
  (* Global layer *)
  global : global;
  (* Local layer *)
  local : local;
}

(* Cover *)

let cover (ctx : t) (hit : bool) (pid : pid) (vid : vid) : unit =
  if hit then ctx.coverage := SCov.hit !(ctx.coverage) pid
  else ctx.coverage := SCov.miss !(ctx.coverage) pid vid

(* Value dependencies *)

let deriving (ctx : t) : bool =
  match ctx.testing with EndToEnd (`On _) -> true | _ -> false

let add_node ?(taint = false) (ctx : t) (value : value) : unit =
  match ctx.testing with
  | EndToEnd (`On { graph; _ }) -> Dep.Graph.add_node ~taint graph value
  | _ -> ()

let add_edge (ctx : t) (value_from : value) (value_to : value)
    (label : Dep.Edges.label) : unit =
  match ctx.testing with
  | EndToEnd (`On { graph; _ }) ->
      Dep.Graph.add_edge graph value_from value_to label
  | _ -> ()

(* Finders *)

(* Finders for input values *)

let find_values_input_opt (cursor : cursor) (ctx : t) : Value.t list option =
  match cursor with
  | Global -> None
  | Local -> (
      match ctx.local with
      | Empty -> None
      | Rel { values_input; _ } -> Some values_input
      | Func { values_input; _ } -> Some values_input)

let find_values_input (cursor : cursor) (ctx : t) : Value.t list =
  match find_values_input_opt cursor ctx with
  | Some values_input -> values_input
  | None ->
      error no_region
        "cannot find input values in global context or empty local context"

(* Finders for values *)

let find_value_opt (cursor : cursor) (ctx : t) (var : Var.t) : Value.t option =
  match cursor with
  | Global -> None
  | Local -> (
      match ctx.local with
      | Empty -> None
      | Rel { venv; _ } -> VEnv.find_opt var venv
      | Func { venv; _ } -> VEnv.find_opt var venv)

let find_value (cursor : cursor) (ctx : t) (var : Var.t) : Value.t =
  match find_value_opt cursor ctx var with
  | Some value -> value
  | None ->
      let id, _ = var in
      error_undef id.at "value" (Var.to_string var)

let bound_value (cursor : cursor) (ctx : t) (var : Var.t) : bool =
  find_value_opt cursor ctx var |> Option.is_some

(* Finders for type definitions *)

let rec find_typdef_opt (cursor : cursor) (ctx : t) (tid : TId.t) :
    Typdef.t option =
  match cursor with
  | Global -> TDEnv.find_opt tid ctx.global.tdenv
  | Local -> (
      let tdenv =
        match ctx.local with
        | Empty | Rel _ -> TDEnv.empty
        | Func { tdenv; _ } -> tdenv
      in
      match TDEnv.find_opt tid tdenv with
      | Some td -> Some td
      | None -> find_typdef_opt Global ctx tid)

let find_typdef (cursor : cursor) (ctx : t) (tid : TId.t) : Typdef.t =
  match find_typdef_opt cursor ctx tid with
  | Some td -> td
  | None -> error_undef tid.at "type" tid.it

let find_defined_typdef (cursor : cursor) (ctx : t) (tid : TId.t) :
    tparam list * deftyp =
  match find_typdef cursor ctx tid with
  | Extern -> error_undef tid.at "defined type" tid.it
  | Defined (tparams, deftyp) -> (tparams, deftyp)

let bound_typdef (cursor : cursor) (ctx : t) (tid : TId.t) : bool =
  find_typdef_opt cursor ctx tid |> Option.is_some

(* Finders for rules *)

let find_rel_opt (_cursor : cursor) (ctx : t) (rid : RId.t) : Rel.t option =
  REnv.find_opt rid ctx.global.renv

let find_rel (cursor : cursor) (ctx : t) (rid : RId.t) : Rel.t =
  match find_rel_opt cursor ctx rid with
  | Some rel -> rel
  | None -> error_undef rid.at "relation" rid.it

let find_rel_inputs (cursor : cursor) (ctx : t) (rid : RId.t) : InputHint.t =
  let rel = find_rel cursor ctx rid in
  match rel with Rel.Extern inputs | Rel.Defined (inputs, _, _) -> inputs

let bound_rel (cursor : cursor) (ctx : t) (rid : RId.t) : bool =
  find_rel_opt cursor ctx rid |> Option.is_some

(* Finders for definitions *)

let rec find_func_opt (cursor : cursor) (ctx : t) (fid : FId.t) : Func.t option
    =
  match cursor with
  | Global -> FEnv.find_opt fid ctx.global.fenv
  | Local -> (
      let fenv =
        match ctx.local with
        | Empty | Rel _ -> FEnv.empty
        | Func { fenv; _ } -> fenv
      in
      match FEnv.find_opt fid fenv with
      | Some func -> Some func
      | None -> find_func_opt Global ctx fid)

let find_func (cursor : cursor) (ctx : t) (fid : FId.t) : Func.t =
  match find_func_opt cursor ctx fid with
  | Some func -> func
  | None -> error_undef fid.at "function" fid.it

let bound_func (cursor : cursor) (ctx : t) (fid : FId.t) : bool =
  find_func_opt cursor ctx fid |> Option.is_some

(* Adders *)

(* Adders for values *)

let add_value (cursor : cursor) (ctx : t) (var : Var.t) (value : Value.t) : t =
  (if cursor = Global then
     let id, _ = var in
     error id.at "cannot add value to global context");
  (* (if bound_value cursor ctx var then *)
  (*    let id, _ = var in *)
  (*    error_dup id.at "value" (Var.to_string var)); *)
  match ctx.local with
  | Empty ->
      let id, _ = var in
      error id.at "cannot add value to empty local context"
  | Rel { rid; values_input; venv } ->
      let venv = VEnv.add var value venv in
      { ctx with local = Rel { rid; values_input; venv } }
  | Func { fid; values_input; tdenv; fenv; venv } ->
      let venv = VEnv.add var value venv in
      { ctx with local = Func { fid; values_input; tdenv; fenv; venv } }

(* Adders for type definitions *)

let add_typdef (cursor : cursor) (ctx : t) (tid : TId.t) (td : Typdef.t) : t =
  if bound_typdef cursor ctx tid then error_dup tid.at "type" tid.it;
  match cursor with
  | Global ->
      let tdenv = TDEnv.add tid td ctx.global.tdenv in
      { ctx with global = { ctx.global with tdenv } }
  | Local -> (
      match ctx.local with
      | Empty -> error tid.at "cannot add type to empty local context"
      | Rel _ -> error tid.at "cannot add type to rule context"
      | Func { fid; values_input; tdenv; fenv; venv } ->
          let tdenv = TDEnv.add tid td tdenv in
          { ctx with local = Func { fid; values_input; tdenv; fenv; venv } })

(* Adders for relations *)

let add_rel (cursor : cursor) (ctx : t) (rid : RId.t) (rel : Rel.t) : t =
  if cursor = Local then error rid.at "cannot add relation to local context";
  if bound_rel cursor ctx rid then error_dup rid.at "relation" rid.it;
  let renv = REnv.add rid rel ctx.global.renv in
  { ctx with global = { ctx.global with renv } }

(* Adders for functions *)

let add_func (cursor : cursor) (ctx : t) (fid : FId.t) (func : Func.t) : t =
  if bound_func cursor ctx fid then error_dup fid.at "function" fid.it;
  match cursor with
  | Global ->
      let fenv = FEnv.add fid func ctx.global.fenv in
      { ctx with global = { ctx.global with fenv } }
  | Local -> (
      match ctx.local with
      | Empty -> error fid.at "cannot add function to empty local context"
      | Rel _ -> error fid.at "cannot add function to relation context"
      | Func { fid = fid_local; values_input; tdenv; fenv; venv } ->
          let fenv = FEnv.add fid func fenv in
          {
            ctx with
            local = Func { fid = fid_local; values_input; tdenv; fenv; venv };
          })

(* Constructors *)

(* Constructing an empty context *)

let empty_global () : global =
  { tdenv = TDEnv.empty; renv = REnv.empty; fenv = FEnv.empty }

let empty_local () : local = Empty

let empty_end_to_end ~(derive : bool) (vdg : vdg) (cover : SCov.Cover.t ref) : t
    =
  let coverage = cover in
  let testing = if derive then EndToEnd (`On vdg) else EndToEnd (`Off vdg) in
  let global = empty_global () in
  let local = empty_local () in
  { coverage; testing; global; local }

let empty_partial (cover : SCov.Cover.t ref) : t =
  let coverage = cover in
  let testing = Partial in
  let global = empty_global () in
  let local = empty_local () in
  { coverage; testing; global; local }

(* Constructing a local context *)

let localize (ctx : t) : t =
  let local = empty_local () in
  { ctx with local }

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
  | Empty -> error no_region "cannot clear empty local context"
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
  | rows ->
      let width = List.length (List.hd rows) in
      check
        (List.for_all (fun row -> List.length row = width) rows)
        no_region "cannot transpose a matrix of value batches";
      List.fold_right
        (List.map2 (fun element row -> element :: row))
        rows
        (List.init width (fun _ -> []))

let sub_opt (ctx : t) (vars : var list) : t option =
  (* First collect the values that are to be iterated over *)
  let values =
    List.map
      (fun (id, _typ, iters) ->
        find_value Local ctx (id, iters @ [ Il.Ast.Opt ]) |> Value.get_opt)
      vars
  in
  (* Iteration is valid when all variables agree on their optionality *)
  if List.for_all Option.is_some values then
    let values = List.map Option.get values in
    let ctx_sub =
      List.fold_left2
        (fun ctx_sub (id, _typ, iters) value ->
          add_value Local ctx_sub (id, iters) value)
        ctx vars values
    in
    Some ctx_sub
  else if List.for_all Option.is_none values then None
  else error no_region "mismatch in optionality of iterated variables"

let sub_list (ctx : t) (vars : var list) : t list =
  (* First break the values that are to be iterated over,
     into a batch of values *)
  let values_batch =
    List.map
      (fun (id, _typ, iters) ->
        find_value Local ctx (id, iters @ [ Il.Ast.List ]) |> Value.get_list)
      vars
    |> transpose
  in
  (* For each batch of values, create a sub-context *)
  List.map
    (fun value_batch ->
      List.fold_left2
        (fun ctx_sub (id, _typ, iters) value ->
          add_value Local ctx_sub (id, iters) value)
        ctx vars value_batch)
    values_batch
