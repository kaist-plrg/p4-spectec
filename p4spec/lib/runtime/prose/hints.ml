open Domain.Lib
open Lang
open Xl

(* Hints *)

module Hint = struct
  type t = El.exp

  let to_string = El.Print.string_of_exp
end

(* Hints associated with type cases *)

module TypCase = struct
  type t = TId.t * Mixop.t

  let compare (tid_a, mixop_a) (tid_b, mixop_b) =
    let c = TId.compare tid_a tid_b in
    if c <> 0 then c else Mixop.compare mixop_a mixop_b
end

module TypCaseMap = struct
  include Map.Make (TypCase)
end

module TypHintMap = struct
  include TypCaseMap

  type t = Hint.t TypCaseMap.t

  let to_string (m : t) : string =
    let bindings = TypCaseMap.bindings m in
    let binding_to_string ((tid, mixop), exp) =
      Printf.sprintf "(%s, %s) -> %s" (TId.to_string tid)
        (Mixop.string_of_mixop mixop)
        (Hint.to_string exp)
    in
    String.concat "\n" (List.map binding_to_string bindings)
end

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
