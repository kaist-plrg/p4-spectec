open Lang
open Sl

(* Signatures for control flow *)

type t = Cont | Res of value list | Ret of value
