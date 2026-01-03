open Lang
open Domain
open Lib

(* Hints associated with type cases *)

module TypHintMap = MakeCIdEnv (Hints.Hint)

(* Hints associated with relation ids *)

module RelHintMap = MakeRIdEnv (Hints.Hint)

(* Hints associated with function ids *)

module FuncHintMap = MakeFIdEnv (Hints.Hint)

(* Collection of hints for a single hint id *)

type t = { typs : TypHintMap.t; funcs : FuncHintMap.t; rels : RelHintMap.t }

let empty =
  {
    typs = TypHintMap.empty;
    funcs = FuncHintMap.empty;
    rels = RelHintMap.empty;
  }

(* Adders *)

let add_typ (tid : TId.t) (mixop : Mixop.t) (hint : Hints.Hint.t) (kinds : t) :
    t =
  { kinds with typs = TypHintMap.add (tid, mixop) hint kinds.typs }

let add_func (fid : FId.t) (hint : Hints.Hint.t) (kinds : t) : t =
  { kinds with funcs = FuncHintMap.add fid hint kinds.funcs }

let add_rel (rid : RId.t) (hint : Hints.Hint.t) (kinds : t) : t =
  { kinds with rels = RelHintMap.add rid hint kinds.rels }

(* Finders *)

let find_typ (tid : TId.t) (mixop : Mixop.t) (kinds : t) : Hints.Hint.t option =
  TypHintMap.find_opt (tid, mixop) kinds.typs

let find_func (fid : FId.t) (kinds : t) : Hints.Hint.t option =
  FuncHintMap.find_opt fid kinds.funcs

let find_rel (rid : RId.t) (kinds : t) : Hints.Hint.t option =
  RelHintMap.find_opt rid kinds.rels
