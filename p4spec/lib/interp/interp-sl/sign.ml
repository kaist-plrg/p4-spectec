open Lang
open Sl
open Util.Source

exception Nondeterminism of region

type otherwise = bool
type t = Cont | Res of otherwise * value list | Ret of otherwise * value
