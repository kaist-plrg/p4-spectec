open Util.Source

(* LaTeX renderer errors *)

let error (at : region) (msg : string) : 'a = Util.Error.error_latex at msg
let error_no_region (msg : string) : 'a = error no_region msg

(* Error checks *)

let check (condition : bool) (at : region) (msg : string) : unit =
  if not condition then error at msg

let check_no_region (condition : bool) (msg : string) : unit =
  check condition no_region msg
