open Lang
open Sl

(* Signatures for control flow *)

type otherwise = bool
type t = Cont | Res of otherwise * value list | Ret of otherwise * value
