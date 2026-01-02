open Domain
open Lib
open Lang

(* Hints *)

module Hint = struct
  type t = El.exp

  let to_string = El.Print.string_of_exp
end

(* Hints associated with type cases *)

module TypHintMap = MakeCIdEnv (Hint)

(* Hints associated with relation ids *)

module RelHintMap = MakeRIdEnv (Hint)

(* Hints associated with function ids *)

module FuncHintMap = MakeFIdEnv (Hint)

(* Collection of hints for a single hint id *)

type t = { typs : TypHintMap.t; funcs : FuncHintMap.t; rels : RelHintMap.t }

let empty =
  {
    typs = TypHintMap.empty;
    funcs = FuncHintMap.empty;
    rels = RelHintMap.empty;
  }

(* Adders *)

let add_typ (tid : TId.t) (mixop : Mixop.t) (hint : Hint.t) (hints : t) : t =
  { hints with typs = TypHintMap.add (tid, mixop) hint hints.typs }

let add_func (fid : FId.t) (hint : Hint.t) (hints : t) : t =
  { hints with funcs = FuncHintMap.add fid hint hints.funcs }

let add_rel (rid : RId.t) (hint : Hint.t) (hints : t) : t =
  { hints with rels = RelHintMap.add rid hint hints.rels }

(* Finders *)

let find_typ (tid : TId.t) (mixop : Mixop.t) (hints : t) : Hint.t option =
  TypHintMap.find_opt (tid, mixop) hints.typs

let find_func (fid : FId.t) (hints : t) : Hint.t option =
  FuncHintMap.find_opt fid hints.funcs

let find_rel (rid : RId.t) (hints : t) : Hint.t option =
  RelHintMap.find_opt rid hints.rels
