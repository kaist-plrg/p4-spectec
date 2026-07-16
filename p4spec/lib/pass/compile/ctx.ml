open Domain
open Lib
open Lang
module Typ = Runtime.Type.Typ
module Typdef = Runtime.Type.Typdef
module Var = Runtime.Dynamic.Var
open Runtime_compile
open Envs
open Error
open Util.Source

(* Preamble *)

type lib = {
  splits : int list; (* arities for splitN *)
  combines : int list; (* arities for combineN *)
  folds : (int * int) list; (* (n_in, n_out) arities for fused fold_N_M *)
  foralls : int list; (* arities for fused for_all_N *)
}

type preamble = { opts : lib; lists : lib }

(* Context *)

type t = {
  preamble : preamble;
  typdefs : Typdefs.t;
  ctors : Ctors.t;
  rels : Rels.t;
  bindings : Bindings.t;
  poly_sigs : (string * Il.tparam list) list;
}

(* Adders *)

let add_typdef (ctx : t) (tid : TId.t) (typdef : Typdef.t) : t =
  { ctx with typdefs = Typdefs.add tid typdef ctx.typdefs }

let add_ctor (ctx : t) (case : Case.t) (ctor : Ctor.t) : t =
  { ctx with ctors = Ctors.add case ctor ctx.ctors }

let add_binding (ctx : t) (var : Var.t) (id_ml : Ml.id) : t =
  { ctx with bindings = Bindings.add var id_ml ctx.bindings }

let add_bindings (ctx : t) (vars : Var.t list) (ids_ml : Ml.id list) : t =
  List.fold_left2 add_binding ctx vars ids_ml

let add_rel (ctx : t) (rid : RId.t) (input : Input.t) : t =
  { ctx with rels = Rels.add rid input ctx.rels }

(* Finders *)

let find_ctor (ctx : t) (typ : Typ.t) (mixop : Mixop.t) : Ml.ctor =
  let find_typdef_opt (tid : TId.t) = Typdefs.find_opt tid ctx.typdefs in
  let typ = Runtime.Type.Expand.expand_typ find_typdef_opt typ in
  match typ.it with
  | VarT (id, _) ->
      let case = (id, mixop) in
      let ctor_ml, _ = Ctors.find case ctx.ctors in
      ctor_ml
  | _ ->
      error typ.at
        (Format.asprintf "%s is not a variant type"
           (Sl.Print.string_of_typ typ))

let find_typdef_opt (ctx : t) (id : TId.t) : Typdef.t option =
  Typdefs.find_opt id ctx.typdefs

let find_typdef (ctx : t) (id : TId.t) : Typdef.t =
  match find_typdef_opt ctx id with
  | Some typdef -> typdef
  | None ->
      error no_region (Format.asprintf "%s is not defined" (TId.to_string id))

let fold_typdefs (f : TId.t -> Typdef.t -> 'a -> 'a) (ctx : t) (init : 'a) : 'a
    =
  Typdefs.fold f ctx.typdefs init

let find_ctors (ctx : t) (id : TId.t) : (Ml.ctor * Il.typ list) list =
  let find_typdef_opt (tid : TId.t) = Typdefs.find_opt tid ctx.typdefs in
  let typ =
    Runtime.Type.Expand.expand_typ find_typdef_opt (Il.VarT (id, []) $ no_region)
  in
  match typ.it with
  | Il.VarT (id', _) ->
      Ctors.fold
        (fun (tid, _) ctor acc -> if Id.eq tid id' then ctor :: acc else acc)
        ctx.ctors []
  | _ -> []

let find_ctors_full (ctx : t) (id : TId.t) :
    (Mixop.t * Ml.ctor * Il.typ list) list =
  Ctors.fold
    (fun (tid, mixop) (ctor_ml, payload_typs) acc ->
      if Id.eq tid id then (mixop, ctor_ml, payload_typs) :: acc else acc)
    ctx.ctors []

let find_binding (ctx : t) (var : Var.t) : Ml.id =
  match Bindings.find_opt var ctx.bindings with
  | Some id_ml -> id_ml
  | None ->
      error no_region (Format.asprintf "%s is not bound" (Var.to_string var))

let find_rel (ctx : t) (rid : RId.t) : Input.t = Rels.find rid ctx.rels

let find_poly_tparams (ctx : t) (name : string) : Il.tparam list option =
  List.assoc_opt name ctx.poly_sigs

(* Initialization *)

let empty_lib = { splits = []; combines = []; folds = []; foralls = [] }
let empty_preamble = { opts = empty_lib; lists = empty_lib }

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

(* Definitions still generic after monomorphization: name -> its own tparams. *)

let poly_sig_of_def (def : Sl.def) : (string * Il.tparam list) option =
  match def.it with
  | FuncDecD (id, tparams, _, _, _, _, _) when tparams <> [] ->
      Some (id.it, tparams)
  | BuiltinDecD (id, tparams, _, _, _) when tparams <> [] ->
      Some (id.it, tparams)
  | ExternDecD (id, tparams, _, _, _) when tparams <> [] -> Some (id.it, tparams)
  | _ -> None

let init (spec : Sl.spec) : t =
  let ctx =
    {
      preamble = empty_preamble;
      typdefs = Typdefs.empty;
      ctors = Ctors.empty;
      rels = Rels.empty;
      bindings = Bindings.empty;
      poly_sigs = List.filter_map poly_sig_of_def spec;
    }
  in
  load_defs ctx spec

(* Preamble setters *)

let update_opts (ctx : t) (f : lib -> lib) : t =
  { ctx with preamble = { ctx.preamble with opts = f ctx.preamble.opts } }

let update_lists (ctx : t) (f : lib -> lib) : t =
  { ctx with preamble = { ctx.preamble with lists = f ctx.preamble.lists } }

let add_split (n : int) (lib : lib) : lib =
  if List.mem n lib.splits then lib else { lib with splits = n :: lib.splits }

let add_combine (n : int) (lib : lib) : lib =
  if List.mem n lib.combines then lib
  else { lib with combines = n :: lib.combines }

let add_fold (nm : int * int) (lib : lib) : lib =
  if List.mem nm lib.folds then lib else { lib with folds = nm :: lib.folds }

let add_forall (n : int) (lib : lib) : lib =
  if List.mem n lib.foralls then lib
  else { lib with foralls = n :: lib.foralls }

let add_opt_split (ctx : t) (n : int) : t = update_opts ctx (add_split n)
let add_opt_combine (ctx : t) (n : int) : t = update_opts ctx (add_combine n)
let add_opt_fold (ctx : t) (nm : int * int) : t = update_opts ctx (add_fold nm)
let add_opt_forall (ctx : t) (n : int) : t = update_opts ctx (add_forall n)
let add_list_split (ctx : t) (n : int) : t = update_lists ctx (add_split n)
let add_list_combine (ctx : t) (n : int) : t = update_lists ctx (add_combine n)

let add_list_fold (ctx : t) (nm : int * int) : t =
  update_lists ctx (add_fold nm)

let add_list_forall (ctx : t) (n : int) : t = update_lists ctx (add_forall n)

(* Scope *)

let promote_preamble (ctx_inner : t) (ctx_outer : t) : t =
  { ctx_outer with preamble = ctx_inner.preamble }

(* Fresh: generate a unique OCaml id *)

let fresh (ctx : t) (id_ml : Ml.id) : Ml.id =
  let is_used id =
    Bindings.fold
      (fun _ id_bound used -> used || id_bound = id)
      ctx.bindings false
  in
  let rec gen id = if is_used id then gen (id ^ "_") else id in
  gen id_ml
