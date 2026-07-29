open Domain.Lib
module Mixfix = Domain.Mixfix
open Lang
open Sl
module Typ = Runtime.Type.Typ
module Typdef = Runtime.Type.Typdef
open Runtime.Prose.Envs
open Error
open Util.Source

(* Error *)

let error_undef (at : region) (kind : string) (id : string) =
  error at (Format.asprintf "%s `%s` is undefined" kind id)

let error_dup (at : region) (kind : string) (id : string) =
  error at (Format.asprintf "%s `%s` was already defined" kind id)

(* Context *)

type namespace = Rel of Id.t | Func of Id.t | Empty
type branch = If | ElseIf | Else | Check | Empty

type t = {
  (* Enclosing namespace *)
  namespace : namespace;
  (* Branching style *)
  branch : branch;
  (* Used identifiers *)
  frees : IdSet.t;
  (* Prose hints *)
  henv : HEnv.t;
  (* Meta-variables *)
  menv : MEnv.t;
  (* Type definitions *)
  tdenv : TDEnv.t;
}

let empty : t =
  {
    branch = Empty;
    namespace = Empty;
    frees = IdSet.empty;
    henv = HEnv.empty;
    menv = MEnv.empty;
    tdenv = TDEnv.empty;
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

(* Namespace *)

let enter_rel (ctx : t) (id_rel : Id.t) : t =
  { ctx with namespace = Rel id_rel }

let enter_func (ctx : t) (id_func : Id.t) : t =
  { ctx with namespace = Func id_func }

let get_namespace (ctx : t) : Id.t =
  match ctx.namespace with Rel id | Func id -> id | Empty -> assert false

(* Branching context *)

let set_branch (ctx : t) (branch : branch) : t = { ctx with branch }

(* Free identifiers *)

let set_free (ctx : t) (frees : IdSet.t) : t = { ctx with frees }

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

(* Finders for hints *)

let find_hint_alter (ctx : t) (hid : string) (key : HEnv.key) :
    Hints.Alter.t option =
  HEnv.find_alter ctx.henv (hid $ no_region) key

let find_hint_fields (ctx : t) (hid : string) (key : HEnv.key) :
    Hints.Fields.t option =
  HEnv.find_fields ctx.henv (hid $ no_region) key

let find_hint_prose (ctx : t) (key : HEnv.key) : Hints.Alter.t option =
  find_hint_alter ctx "prose" key

let find_hint_prose_in (ctx : t) (key : HEnv.key) : Hints.Alter.t option =
  find_hint_alter ctx "prose_in" key

let find_hint_prose_out (ctx : t) (key : HEnv.key) : Hints.Alter.t option =
  find_hint_alter ctx "prose_out" key

let find_hint_prose_true (ctx : t) (key : HEnv.key) : Hints.Alter.t option =
  find_hint_alter ctx "prose_true" key

let find_hint_prose_false (ctx : t) (key : HEnv.key) : Hints.Alter.t option =
  find_hint_alter ctx "prose_false" key

let find_hint_prose_fields (ctx : t) (key : HEnv.key) : Hints.Fields.t option =
  find_hint_fields ctx "prose_fields" key

(* Adders *)

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

let add_tparam (ctx : t) (tid : TId.t) : t =
  let td = Typdef.Param in
  add_typdef ctx tid td

let add_tparams (ctx : t) (tids : TId.t list) : t =
  List.fold_left add_tparam ctx tids

(* Adders for hints *)

let add_hint_alter (ctx : t) (hid : HId.t) (key : HEnv.key)
    (hint_alter : Hints.Alter.t) : t =
  let henv = HEnv.add_alter ctx.henv hid key hint_alter in
  { ctx with henv }

let add_hint_fields (ctx : t) (hid : HId.t) (key : HEnv.key)
    (hint_fields : Hints.Fields.t) : t =
  let henv = HEnv.add_fields ctx.henv hid key hint_fields in
  { ctx with henv }

(* Validation *)

let validate_hint_alter (at : region) (hint_alter : Hints.Alter.t)
    (items : 'a list) : unit =
  match Hints.Alter.validate hint_alter items with
  | Ok () -> ()
  | Error msg -> error at msg

let validate_hint_fields (at : region) (hint_fields : Hints.Fields.t)
    (arity : int) : unit =
  match Hints.Fields.validate hint_fields arity with
  | Ok () -> ()
  | Error msg -> error at msg

(* Unrolling types *)

let unroll_typ (ctx : t) (typ : Sl.typ) : Sl.typ = TDEnv.unroll ctx.tdenv typ

(* Constructor *)

let load_hints (ctx : t) (key : HEnv.key) (hints : El.hint list) : t =
  List.fold_left
    (fun ctx El.{ hintid; hintexp } ->
      match hintid.it with
      (* Alter hints *)
      | "prose" | "prose_in" | "prose_out" | "prose_true" | "prose_false" -> (
          let hint_alter_opt = Hints.Alter.init hintexp in
          match hint_alter_opt with
          | Some hint_alter -> add_hint_alter ctx hintid key hint_alter
          | None ->
              error hintexp.at
                (Format.asprintf "invalid hint expression %s for hint %s"
                   (El.Print.string_of_exp hintexp)
                   hintid.it))
      (* Field hints *)
      | "prose_fields" -> (
          let hint_fields_opt = Hints.Fields.init hintexp in
          match hint_fields_opt with
          | Some hint_fields -> add_hint_fields ctx hintid key hint_fields
          | None ->
              error hintexp.at
                (Format.asprintf "invalid hint expression %s for hint %s"
                   (El.Print.string_of_exp hintexp)
                   hintid.it))
      | _ -> ctx)
    ctx hints

let load_typcases (ctx : t) (tid : TId.t) (typcases : typcase list) : t =
  List.fold_left
    (fun ctx (nottyp, _, hints) ->
      let mixop = Mixfix.to_mixop nottyp.it in
      let cid = (tid, mixop) in
      load_hints ctx (`Typ cid) hints)
    ctx typcases

let load_def (ctx : t) (def : def) : t =
  match def.it with
  | ExternTypD (id, _) ->
      let typ = Typ.Make.var id [] in
      let ctx = add_metavar ctx id typ in
      let td = Typdef.Extern in
      add_typdef ctx id td
  | TypD (id, tparams, deftyp, _) -> (
      let ctx =
        if tparams = [] then
          let typ = Typ.Make.var id [] in
          add_metavar ctx id typ
        else ctx
      in
      let td = Typdef.Defined (tparams, deftyp) in
      let ctx = add_typdef ctx id td in
      match deftyp.it with
      | VariantT typcases -> load_typcases ctx id typcases
      | _ -> ctx)
  | VarD (id, typ, _) -> add_metavar ctx id typ
  | ExternRelD (id, _, _, hints) | RelD (id, _, _, _, _, hints) ->
      load_hints ctx (`Rel id) hints
  | ExternDecD (id, _, _, _, hints)
  | BuiltinDecD (id, _, _, _, hints)
  | FuncDecD (id, _, _, _, _, _, hints)
  | TableDecD (id, _, _, _, hints) ->
      load_hints ctx (`Func id) hints

let load_spec (ctx : t) (spec : Sl.spec) : t = List.fold_left load_def ctx spec
