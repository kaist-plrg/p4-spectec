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
module IHEnv = MakeHIdEnv (Hints.Input)

(* Definition environment *)

module FEnv = MakeFIdEnv (Func)
