open Lang
open Domain.Lib

(* Environments *)

(* Value environment *)

module VEnv = Dynamic.Envs.VEnv

(* Type definition environment *)

module TDEnv = Dynamic.Envs.TDEnv

(* Relation environment *)

module REnv = MakeRIdEnv (Rel)
module IHEnv = MakeHIdEnv (Hints.Input)

(* Definition environment *)

module FEnv = MakeFIdEnv (Func)
