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
  rels : Rels.t;
  vars : vars;
}

(* Adders *)

let add_typdef (ctx : t) (tid : TId.t) (typdef : Typdef.t) : t =
  let typdefs = Typdefs.add tid typdef ctx.typdefs in
  { ctx with typdefs }

let add_ctor (ctx : t) (case : Case.t) (ctor : Ctor.t) : t =
  let ctors = Ctors.add case ctor ctx.ctors in
  { ctx with ctors }

let add_binding (ctx : t) (var : Var.t) (id_ml : Ml.id) : t =
  let bindings = Bindings.add var id_ml ctx.vars.bindings in
  { ctx with vars = { ctx.vars with bindings } }

let add_bindings (ctx : t) (vars : Var.t list) (ids_ml : Ml.id list) : t =
  List.fold_left2 add_binding ctx vars ids_ml

let add_rel (ctx : t) (rid : RId.t) (input : Input.t) : t =
  let rels = Rels.add rid input ctx.rels in
  { ctx with rels }

(* Removers *)

let remove_binding (ctx : t) (var : Var.t) : t =
  let bindings = Bindings.remove var ctx.vars.bindings in
  { ctx with vars = { ctx.vars with bindings } }

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

let find_binding (ctx : t) (var : Var.t) : Ml.id =
  Bindings.find var ctx.vars.bindings

let find_rel (ctx : t) (rid : RId.t) : Input.t = Rels.find rid ctx.rels

(* Initialization *)

let load_def (ctx : t) (def : Sl.def) : t =
  match def.it with
  | ExternTypD (id, _) ->
      let td = Typdef.Extern in
      add_typdef ctx id td
  | TypD (id, tparams, deftyp, _) ->
      let td = Typdef.Defined (tparams, deftyp) in
      add_typdef ctx id td
  | ExternRelD (id, rel_signature, _, _) | RelD (id, rel_signature, _, _, _, _)
    ->
      let _, inputs = rel_signature in
      add_rel ctx id inputs
  | _ -> ctx

let load_defs (ctx : t) (defs : Sl.def list) : t =
  List.fold_left load_def ctx defs

let init (spec : Sl.spec) : t =
  let ctx =
    {
      preamble = { opts = []; lists = [] };
      typdefs = Typdefs.empty;
      ctors = Ctors.empty;
      rels = Rels.empty;
      vars = { bounds = [ IdSet.empty ]; bindings = Bindings.empty };
    }
  in
  load_defs ctx spec

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
