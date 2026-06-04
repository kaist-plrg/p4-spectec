open Domain
open Lib
open Lang
module Typ = Runtime.Type.Typ
module Typdef = Runtime.Type.Typdef
module Var = Runtime.Dynamic.Var
open Compile_runtime
open Envs
open Error

(* Preamble *)

type preamble = { opts : int list; lists : int list }

(* Variables *)

type vars = { bounds : Bounds.t; bindings : Bindings.t }

(* Context *)

type t = {
  preamble : preamble;
  typdefs : Typdefs.t;
  ctors : Ctors.t;
  vars : vars;
}

(* Initialization *)

let load_def (typdefs : Typdefs.t) (def : Sl.def) : Typdefs.t =
  match def.it with
  | ExternTypD (id, _) ->
      let td = Typdef.Extern in
      Typdefs.add id td typdefs
  | TypD (id, tparams, deftyp, _) ->
      let td = Typdef.Defined (tparams, deftyp) in
      Typdefs.add id td typdefs
  | _ -> typdefs

let load_defs (typdefs : Typdefs.t) (defs : Sl.def list) : Typdefs.t =
  List.fold_left load_def typdefs defs

let init (spec : Sl.spec) : t =
  let typdefs = load_defs Typdefs.empty spec in
  {
    preamble = { opts = []; lists = [] };
    typdefs;
    ctors = Ctors.empty;
    vars = { bounds = [ IdSet.empty ]; bindings = Bindings.empty };
  }

(* Preamble setters *)

let add_opt_arity (ctx : t) (n : int) : t =
  let preamble = ctx.preamble in
  if List.mem n preamble.opts then ctx
  else
    let preamble = { preamble with opts = n :: preamble.opts } in
    { ctx with preamble }

let add_list_arity (ctx : t) (n : int) : t =
  let preamble = ctx.preamble in
  if List.mem n preamble.lists then ctx
  else
    let preamble = { preamble with lists = n :: preamble.lists } in
    { ctx with preamble }

(* Block setters *)

let push (ctx : t) : t =
  { ctx with vars = { ctx.vars with bounds = Bounds.push ctx.vars.bounds } }

let pop (ctx : t) : t =
  { ctx with vars = { ctx.vars with bounds = Bounds.pop ctx.vars.bounds } }

(* Fresh *)

let fresh (ctx : t) (id_ml : Ml.id) : t * Ml.id =
  let bounds, id_ml = Bounds.fresh ctx.vars.bounds id_ml in
  let ctx = { ctx with vars = { ctx.vars with bounds } } in
  (ctx, id_ml)

(* Adders *)

let add_ctor (ctx : t) (case : Case.t) (ctor : Ctor.t) : t =
  let ctors = Ctors.add case ctor ctx.ctors in
  { ctx with ctors }

let add_binding (ctx : t) (var : Var.t) (id_ml : Ml.id) : t =
  let bindings = Bindings.add var id_ml ctx.vars.bindings in
  { ctx with vars = { ctx.vars with bindings } }

let add_bindings (ctx : t) (vars : Var.t list) (ids_ml : Ml.id list) : t =
  List.fold_left2 add_binding ctx vars ids_ml

(* Finders *)

let find_ctor (ctx : t) (typ : Typ.t) (mixop : Mixop.t) : Ml.ctor =
  let find_typdef_opt (tid : TId.t) = Typdefs.find_opt tid ctx.typdefs in
  let typ = Runtime.Type.Expand.expand_typ find_typdef_opt typ in
  match typ.it with
  | VarT (id, _) ->
      let case = (id, mixop) in
      Ctors.find case ctx.ctors
  | _ ->
      error typ.at
        (Format.asprintf "%s is not a variant type"
           (Sl.Print.string_of_typ typ))
