open Lang
open Sl
open Backtrace

(* Signatures for control flow *)

type t =
  | Cont of trace list
  | Res of value list
  | Ret of value
  | Tailcall_func of id * targ list * value list
  | Tailcall_rel of id * value list
