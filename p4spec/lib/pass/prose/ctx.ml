open Domain.Lib
open Lang
module Typdef = Runtime.Dynamic_Sl.Typdef
open Runtime.Prose.Envs
open Error
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
  (* Type definitions *)
  tdenv : TDEnv.t;
}

(* Constructor *)

let load_hints (key : HEnv.key) (henv : HEnv.t) (hints : El.hint list) : HEnv.t
    =
  List.fold_left
    (fun henv El.{ hintid; hintexp } ->
      match hintid.it with
      (* Alter hints *)
      | "prose" | "prose_in" | "prose_out" | "prose_true" | "prose_false" -> (
          let hint_alter_opt = Hints.Alter.init hintexp in
          match hint_alter_opt with
          | Some hint_alter -> HEnv.add_alter henv hintid key hint_alter
          | None ->
              error hintexp.at
                (Format.asprintf "invalid hint expression %s for hint %s"
                   (El.Print.string_of_exp hintexp)
                   hintid.it))
      (* Field hints *)
      | "prose_fields" -> (
          let hint_fields_opt = Hints.Fields.init hintexp in
          match hint_fields_opt with
          | Some hint_fields -> HEnv.add_fields henv hintid key hint_fields
          | None ->
              error hintexp.at
                (Format.asprintf "invalid hint expression %s for hint %s"
                   (El.Print.string_of_exp hintexp)
                   hintid.it))
      | _ -> henv)
    henv hints

let load_typcases (tid : TId.t) (henv : HEnv.t) (typcases : Sl.typcase list) :
    HEnv.t =
  List.fold_left
    (fun henv (nottyp, hints) ->
      let mixop, _ = nottyp.it in
      let cid = (tid, mixop) in
      load_hints (`Typ cid) henv hints)
    henv typcases

let load_defs (henv : HEnv.t) (tdenv : TDEnv.t) (def : Sl.def) :
    HEnv.t * TDEnv.t =
  match def.it with
  | ExternTypD (tid, _) ->
      let td = Typdef.Extern in
      let tdenv = TDEnv.add tid td tdenv in
      (henv, tdenv)
  | TypD (tid, tparams, deftyp, _) ->
      let henv =
        match deftyp.it with
        | VariantT typcases -> load_typcases tid henv typcases
        | _ -> henv
      in
      let td = Typdef.Defined (tparams, deftyp) in
      let tdenv = TDEnv.add tid td tdenv in
      (henv, tdenv)
  | ExternRelD (rid, _, _, hints) | RelD (rid, _, _, _, _, hints) ->
      let henv = load_hints (`Rel rid) henv hints in
      (henv, tdenv)
  | ExternDecD (fid, _, _, _, hints)
  | BuiltinDecD (fid, _, _, _, hints)
  | TableDecD (fid, _, _, _, hints)
  | FuncDecD (fid, _, _, _, _, _, hints) ->
      let henv = load_hints (`Func fid) henv hints in
      (henv, tdenv)

let load_spec (spec : Sl.spec) : HEnv.t * TDEnv.t =
  List.fold_left
    (fun (henv, tdenv) def -> load_defs henv tdenv def)
    (HEnv.empty, TDEnv.empty) spec

let init (spec_sl : Sl.spec) : t =
  let henv, tdenv = load_spec spec_sl in
  { branch = Empty; namespace = Empty; frees = IdSet.empty; henv; tdenv }

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

(* Adders *)

let add_tparam (ctx : t) (tid : TId.t) : t =
  let td = Typdef.Param in
  let tdenv = TDEnv.add tid td ctx.tdenv in
  { ctx with tdenv }

let add_tparams (ctx : t) (tids : TId.t list) : t =
  List.fold_left add_tparam ctx tids

(* Finders *)

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
