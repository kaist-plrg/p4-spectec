open Lang
open Sl
open Backtrace

(* Signatures for control flow *)

type t = Cont of trace list | Res of value list | Ret of value
