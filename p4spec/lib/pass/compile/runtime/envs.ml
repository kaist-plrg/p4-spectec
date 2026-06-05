open Domain.Lib

(* Environments *)

module Typdefs = Runtime.Type.Envs.TDEnv

(* Constructor environment *)

module Ctors = struct
  module CaseMap = Map.Make (Case)
  include CaseMap

  type t = Ctor.t CaseMap.t
end

(* Binding environment mapping SpecTec vars to OCaml vars *)

module Bindings = Runtime.Dynamic.Envs.MakeVarEnv (struct
  type t = Ml.id

  let to_string = Fun.id
end)

(* Relation environment *)

module Rels = MakeRIdEnv (Input)
