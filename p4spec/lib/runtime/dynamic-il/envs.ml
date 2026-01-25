open Domain.Lib

(* Environments *)

(* Value environment *)

module VEnv = Dynamic.Envs.VEnv

(* Type definition environment *)

module TDEnv = Dynamic.Envs.TDEnv
module TDTbl = Dynamic.Envs.TDTbl

(* Relation environment *)

module REnv = MakeRIdEnv (Rel)
module RTbl = MakeRIdTbl (Rel)

(* Definition environment *)

module FEnv = MakeFIdEnv (Func)
module FTbl = MakeFIdTbl (Func)
