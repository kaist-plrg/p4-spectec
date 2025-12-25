open Domain.Lib
open Lang
open Xl

module HintExp = struct
  type t = El.exp

  let to_string = El.Print.string_of_exp
end

module TypCase = struct
  type t = TId.t * Mixop.t

  let compare (tid1, mixop1) (tid2, mixop2) =
    let c = TId.compare tid1 tid2 in
    if c <> 0 then c else Mixop.compare mixop1 mixop2
end

module TypCaseMap = struct
  include Map.Make (TypCase)
end

module TypHintMap = struct
  include TypCaseMap

  type t = HintExp.t TypCaseMap.t

  let to_string (m : t) : string =
    let bindings = TypCaseMap.bindings m in
    let binding_to_string ((tid, mixop), exp) =
      Printf.sprintf "(%s, %s) -> %s" (TId.to_string tid)
        (Mixop.string_of_mixop mixop)
        (HintExp.to_string exp)
    in
    String.concat "\n" (List.map binding_to_string bindings)
end

module RelHintMap = MakeRIdEnv (HintExp)
module FuncHintMap = MakeFIdEnv (HintExp)

(* Collection of hints for single hintid *)
type t = { typs : TypHintMap.t; funcs : FuncHintMap.t; rels : RelHintMap.t }

let add_typ (tid : TId.t) (mixop : Mixop.t) (exp : El.exp) (h : t) : t =
  { h with typs = TypHintMap.add (tid, mixop) exp h.typs }

let add_func (fid : FId.t) (exp : El.exp) (h : t) : t =
  { h with funcs = FuncHintMap.add fid exp h.funcs }

let add_rel (rid : RId.t) (exp : El.exp) (h : t) : t =
  { h with rels = RelHintMap.add rid exp h.rels }

let get_typ (tid : TId.t) (mixop : Mixop.t) (h : t) : El.exp option =
  TypHintMap.find_opt (tid, mixop) h.typs

let get_func (fid : FId.t) (h : t) : El.exp option =
  FuncHintMap.find_opt fid h.funcs

let get_rel (rid : RId.t) (h : t) : El.exp option =
  RelHintMap.find_opt rid h.rels

let empty =
  {
    typs = TypHintMap.empty;
    funcs = FuncHintMap.empty;
    rels = RelHintMap.empty;
  }

let to_string (h : t) : string =
  let typs_str = TypHintMap.to_string h.typs in
  let funcs_str = FuncHintMap.to_string h.funcs in
  let rels_str = RelHintMap.to_string h.rels in
  Printf.sprintf "Types:\n%s\nFunctions:\n%s\nRelations:\n%s" typs_str funcs_str
    rels_str
