open Domain.Lib
module Mixfix = Domain.Mixfix
open Lang
open Sl
module Typ = Runtime.Type.Typ
module Typdef = Runtime.Type.Typdef
open Runtime.Prose.Envs
open Error
open Util.Source

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

let bound_typdef (ctx : t) (tid : TId.t) : bool =
  find_typdef_opt ctx tid |> Option.is_some

(* Finders for meta-variables *)

let find_metavar_opt (ctx : t) (tid : TId.t) : Typ.t option =
  MEnv.find_opt tid ctx.menv

let bound_metavar (ctx : t) (tid : TId.t) : bool =
  find_metavar_opt ctx tid |> Option.is_some

(* Finders for hints *)

let hint_value (hint : 'a HEnv.located_hint option) =
  Option.map HEnv.hint_value hint

let map_hint f (hint : 'a HEnv.located_hint option) =
  Option.map (HEnv.map_hint f) hint

let find_hint_alter (ctx : t) (hid : string) (key : HEnv.key) :
    Hints.Alter.t HEnv.located_hint option =
  HEnv.find_alter ctx.henv (hid $ no_region) key

let find_hint_fields (ctx : t) (hid : string) (key : HEnv.key) :
    Hints.Fields.t HEnv.located_hint option =
  HEnv.find_fields ctx.henv (hid $ no_region) key

let find_hint_prose (ctx : t) (key : HEnv.key) :
    Hints.Alter.t HEnv.located_hint option =
  find_hint_alter ctx "prose" key

let find_hint_prose_in (ctx : t) (key : HEnv.key) :
    Hints.Alter.t HEnv.located_hint option =
  find_hint_alter ctx "prose_in" key

let find_hint_prose_out (ctx : t) (key : HEnv.key) :
    Hints.Alter.t HEnv.located_hint option =
  find_hint_alter ctx "prose_out" key

let find_hint_prose_true (ctx : t) (key : HEnv.key) :
    Hints.Alter.t HEnv.located_hint option =
  find_hint_alter ctx "prose_true" key

let find_hint_prose_false (ctx : t) (key : HEnv.key) :
    Hints.Alter.t HEnv.located_hint option =
  find_hint_alter ctx "prose_false" key

let find_hint_prose_fields (ctx : t) (key : HEnv.key) :
    Hints.Fields.t HEnv.located_hint option =
  find_hint_fields ctx "prose_fields" key

(* Adders *)

(* Adders for meta-variables *)

let add_metavar (ctx : t) (tid : TId.t) (typ : Typ.t) : t =
  (* Elaboration rejects duplicate meta-variable definitions. *)
  assert (not (bound_metavar ctx tid));
  let menv = MEnv.add tid typ ctx.menv in
  { ctx with menv }

(* Adders for type definitions *)

let add_typdef (ctx : t) (tid : TId.t) (td : Typdef.t) : t =
  (* Elaboration rejects duplicate global type definitions. *)
  assert (not (bound_typdef ctx tid));
  let tdenv = TDEnv.add tid td ctx.tdenv in
  { ctx with tdenv }

let add_tparam (ctx : t) (tid : TId.t) : t =
  (* Type parameters shadow global types within their declaration. *)
  let tdenv = TDEnv.add tid Typdef.Param ctx.tdenv in
  { ctx with tdenv }

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

let validate_hint_alter (declaration : HId.t) (hint_alter : Hints.Alter.t)
    (arity : int) : unit =
  match Hints.Alter.validate hint_alter arity with
  | Ok () -> ()
  | Error { at; placeholder; index; arity } ->
      let noun = if arity = 1 then "value" else "values" in
      let verb = if arity = 1 then "is" else "are" in
      error ~code:Hint_placeholder_out_of_bounds at
        (Format.asprintf
           "hint `%s` placeholder `%s` selects index %d, but only %d %s %s \
            available"
           declaration.it placeholder index arity noun verb)

let validate_hint_fields (at_hint : region) (declaration : HId.t)
    (hint_fields : Hints.Fields.t) (arity : int) : unit =
  match Hints.Fields.validate hint_fields arity with
  | Ok () -> ()
  | Error { expected; actual } ->
      let at =
        let rec find_extra index = function
          | [] -> at_hint
          | at :: _ when index = 0 -> at
          | _ :: rest -> find_extra (index - 1) rest
        in
        let ats_fields = List.map at hint_fields in
        if actual > expected then find_extra expected ats_fields
        else
          match List.rev ats_fields with
          | at :: _ -> region_after at
          | [] -> at_hint
      in
      let name_noun = if actual = 1 then "field name" else "field names" in
      let field_noun = if expected = 1 then "field" else "fields" in
      error ~code:Hint_fields_arity_mismatch at
        (Format.asprintf "hint `%s` has %d %s, but the syntax case has %d %s"
           declaration.it actual name_noun expected field_noun)

(* Unrolling types *)

let unroll_typ (ctx : t) (typ : Sl.typ) : Sl.typ = TDEnv.unroll ctx.tdenv typ

(* Constructor *)

let load_hints (ctx : t) (key : HEnv.key) (hints : El.hint list) : t =
  List.fold_left
    (fun ctx El.{ hintid; hintexp; at } ->
      match hintid.it with
      (* Alter hints *)
      | "prose" | "prose_in" | "prose_out" | "prose_true" | "prose_false" ->
          Hints.Alter.init hintexp |> add_hint_alter ctx hintid key
      (* Field hints *)
      | "prose_fields" -> (
          let hint_fields_opt = Hints.Fields.init hintexp in
          match hint_fields_opt with
          | Some hint_fields ->
              (match key with
              | `Typ (_, mixop) ->
                  validate_hint_fields at hintid hint_fields
                    (Mixfix.arity mixop)
              | `Func _ | `Rel _ -> ());
              add_hint_fields ctx hintid key hint_fields
          | None ->
              error ~code:Hint_fields_text_expected hintexp.at
                (Format.asprintf
                   "hint `prose_fields` field names must be text literals, but \
                    got %s"
                   (Diagnostic.quote (El.Print.string_of_exp hintexp))))
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
  | TableDecD (id, _, _, _, hints)
  | FuncDecD (id, _, _, _, _, _, hints) ->
      load_hints ctx (`Func id) hints

let load_spec (ctx : t) (spec : Sl.spec) : t = List.fold_left load_def ctx spec
