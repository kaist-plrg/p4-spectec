open Domain.Lib

(* Type definition environment *)

module TDEnv = Dynamic.Envs.TDEnv

(* Mixop family environment *)

module MixopEnv = MakeIdEnv (Mixops)
