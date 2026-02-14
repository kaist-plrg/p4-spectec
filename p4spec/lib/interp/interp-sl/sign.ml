open Lang
open Sl

type otherwise = bool
type t = Cont | Res of otherwise * value list | Ret of otherwise * value
