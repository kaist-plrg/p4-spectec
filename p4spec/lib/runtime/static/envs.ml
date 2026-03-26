open Lang
open Domain.Lib

(* Environments *)

(* Identifier type and dimension environment *)

module VEnv = MakeIdEnv (Typdim)

(* Meta-variable environment *)

module MEnv = MakeIdEnv (Typ)

(* Type definition environment *)

module TDEnv = MakeTIdEnv (Typdef)

(* Relation environment *)

module REnv = MakeRIdEnv (Rel)
module IHEnv = MakeHIdEnv (Hints.Input)

(* Definition environment *)

module FEnv = MakeFIdEnv (Func)
