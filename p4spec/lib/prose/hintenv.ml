open Domain.Lib

module HintExp = struct
  type t = El.Ast.exp

  let to_string = El.Print.string_of_exp
end

module RelHintMap = MakeRIdEnv (HintExp)
module FuncHintMap = MakeFIdEnv (HintExp)

(* Collection of hints for single hintid *)
type t = {
  funcs: FuncHintMap.t;
  rels: RelHintMap.t;
}

let add_func (fid : FId.t) (exp : El.Ast.exp) (h : t) : t =
  { h with funcs = FuncHintMap.add fid exp h.funcs }

let add_rel (rid : RId.t) (exp : El.Ast.exp) (h : t) : t =
  { h with rels = RelHintMap.add rid exp h.rels }

let get_func (fid : FId.t) (h : t) : El.Ast.exp option =
  FuncHintMap.find_opt fid h.funcs

let get_rel (rid : RId.t) (h : t) : El.Ast.exp option =
  RelHintMap.find_opt rid h.rels

let empty = { funcs = FuncHintMap.empty; rels = RelHintMap.empty }
