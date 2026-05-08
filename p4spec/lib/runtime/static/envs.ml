open Domain.Lib
open Lang

(* Environments *)

(* Identifier type and dimension environment *)

module VEnv = MakeIdEnv (Typdim)

(* Meta-variable environment *)

module MEnv = MakeIdEnv (Type.Typ)

(* Type definition environment *)

module TDEnv = Type.Envs.TDEnv

(* Relation environment *)

module REnv = MakeRIdEnv (Rel)
module REnv2 = MakeRIdEnv (Rel2)
module IHEnv = MakeHIdEnv (Hints.Input)

(* Definition environment *)

module FEnv = MakeFIdEnv (Func)
module FEnv2 = MakeFIdEnv (Func2)
