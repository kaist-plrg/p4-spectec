open Domain.Lib
open Lang
module InputHint = Runtime.Static.Rel.InputHint
module Typdef = Runtime.Dynamic_Sl.Typdef
module Hint = Runtime.Prose.Hints.Hint
open Runtime.Prose.Envs
open Util.Source

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
  (* Input hints *)
  ienv : IEnv.t;
  (* Type definitions *)
  tdenv : TDEnv.t;
}

(* Constructor *)

let load_hints (key : HEnv.key) (henv : HEnv.t) (hints : El.hint list) : HEnv.t
    =
  List.fold_left
    (fun henv El.{ hintid; hintexp } ->
      match hintid.it with
      | "prose" | "prose_in" | "prose_out" | "prose_true" | "prose_false"
      | "prose_fields" ->
          HEnv.add hintid key hintexp henv
      | _ -> henv)
    henv hints

let load_typcases (tid : TId.t) (henv : HEnv.t) (typcases : Sl.typcase list) :
    HEnv.t =
  List.fold_left
    (fun henv (nottyp, hints) ->
      let mixop, _ = nottyp.it in
      load_hints (`Typ (tid, mixop)) henv hints)
    henv typcases

let load_defs (henv : HEnv.t) (ienv : IEnv.t) (tdenv : TDEnv.t) (def : Sl.def) :
    HEnv.t * IEnv.t * TDEnv.t =
  match def.it with
  | ExternTypD (tid, _) ->
      let td = Typdef.Extern in
      let tdenv = TDEnv.add tid td tdenv in
      (henv, ienv, tdenv)
  | TypD (tid, tparams, deftyp, _) ->
      let henv =
        match deftyp.it with
        | VariantT typcases -> load_typcases tid henv typcases
        | _ -> henv
      in
      let td = Typdef.Defined (tparams, deftyp) in
      let tdenv = TDEnv.add tid td tdenv in
      (henv, ienv, tdenv)
  | ExternRelD (rid, (_, inputs), _, hints)
  | RelD (rid, (_, inputs), _, _, hints) ->
      let henv = load_hints (`Rel rid) henv hints in
      let ienv = IEnv.add rid inputs ienv in
      (henv, ienv, tdenv)
  | ExternDecD (fid, _, _, _, hints)
  | BuiltinDecD (fid, _, _, _, hints)
  | TableDecD (fid, _, _, _, hints)
  | FuncDecD (fid, _, _, _, _, hints) ->
      let henv = load_hints (`Func fid) henv hints in
      (henv, ienv, tdenv)

let load_spec (spec : Sl.spec) : HEnv.t * IEnv.t * TDEnv.t =
  List.fold_left
    (fun (henv, ienv, tdenv) def -> load_defs henv ienv tdenv def)
    (HEnv.empty, IEnv.empty, TDEnv.empty)
    spec

let init (spec_sl : Sl.spec) : t =
  let henv, ienv, tdenv = load_spec spec_sl in
  { branch = Empty; namespace = Empty; frees = IdSet.empty; henv; ienv; tdenv }

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

let find_inputs (ctx : t) (id_rel : Id.t) : InputHint.t =
  IEnv.find_opt id_rel ctx.ienv |> Option.value ~default:[]

let find_hint (ctx : t) (hid : string) (key : HEnv.key) : Hint.t option =
  HEnv.find (hid $ no_region) key ctx.henv

let find_hint_prose (ctx : t) (key : HEnv.key) : Hint.t option =
  find_hint ctx "prose" key

let find_hint_prose_in (ctx : t) (key : HEnv.key) : Hint.t option =
  find_hint ctx "prose_in" key

let find_hint_prose_out (ctx : t) (key : HEnv.key) : Hint.t option =
  find_hint ctx "prose_out" key

let find_hint_prose_true (ctx : t) (key : HEnv.key) : Hint.t option =
  find_hint ctx "prose_true" key

let find_hint_prose_false (ctx : t) (key : HEnv.key) : Hint.t option =
  find_hint ctx "prose_false" key

let find_hint_prose_fields (ctx : t) (key : HEnv.key) : Hint.t option =
  find_hint ctx "prose_fields" key

(* Unrolling types *)

let rec unroll_typ (ctx : t) (typ : Sl.typ) : Sl.typ =
  match typ.it with
  | VarT (tid, _) -> (
      let td = TDEnv.find tid ctx.tdenv in
      match td with
      | Extern -> typ
      | Defined (_, deftyp) -> (
          match deftyp.it with PlainT typ -> unroll_typ ctx typ | _ -> typ))
  | _ -> typ
