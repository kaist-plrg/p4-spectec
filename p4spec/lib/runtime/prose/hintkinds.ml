open Lang
open Domain
open Lib

(* Hint kinds *)

module Kind = struct
  type t = Alter of Hints.Alter.t | Fields of Hints.Fields.t

  let to_string = function
    | Alter hint_alter -> Hints.Alter.to_string hint_alter
    | Fields hint_fields -> Hints.Fields.to_string hint_fields
end

(* Hints associated with type cases *)

module TypHints = MakeCaseIdEnv (Kind)

(* Hints associated with relation ids *)

module RelHints = MakeRIdEnv (Kind)

(* Hints associated with function ids *)

module FuncHints = MakeFIdEnv (Kind)

(* Collection of hints for a single hint id *)

type t = { typs : TypHints.t; funcs : FuncHints.t; rels : RelHints.t }

let empty =
  { typs = TypHints.empty; funcs = FuncHints.empty; rels = RelHints.empty }

(* Adders *)

let add_typ (cid : CaseId.t) (kind : Kind.t) (kinds : t) : t =
  { kinds with typs = TypHints.add cid kind kinds.typs }

let add_func (fid : FId.t) (kind : Kind.t) (kinds : t) : t =
  { kinds with funcs = FuncHints.add fid kind kinds.funcs }

let add_rel (rid : RId.t) (kind : Kind.t) (kinds : t) : t =
  { kinds with rels = RelHints.add rid kind kinds.rels }

(* Finders *)

let find_typ (cid : CaseId.t) (kinds : t) : Kind.t option =
  TypHints.find_opt cid kinds.typs

let find_func (fid : FId.t) (kinds : t) : Kind.t option =
  FuncHints.find_opt fid kinds.funcs

let find_rel (rid : RId.t) (kinds : t) : Kind.t option =
  RelHints.find_opt rid kinds.rels
