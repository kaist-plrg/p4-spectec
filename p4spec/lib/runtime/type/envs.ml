open Domain.Lib

(* Type definition environment *)

module TDEnv = MakeTIdEnv (Typdef)
module TDTbl = MakeTIdTbl (Typdef)
