module Run = Runtime.Dynamic_Runner.Signature
open Util.Source

(* Failures reported by the p4spectec entry points *)

type t =
  | PassError of Pass.error
  | RunError of Run.error
  | CommandError of string

let to_region_msg = function
  | PassError e -> Pass.to_region_msg e
  | RunError e -> Run.to_region_msg e
  | CommandError msg -> (no_region, msg)

let to_string (e : t) : string =
  let at, msg = to_region_msg e in
  Util.Error.string_of_error at msg

let source_of = function
  | PassError _ -> "pass"
  | RunError _ -> "interp"
  | CommandError _ -> "command"

let to_diagnostics (e : t) : Diagnostic.Report.t =
  let at, msg = to_region_msg e in
  Diagnostic.Report.singleton (Diagnostic.error ~source:(source_of e) at msg)
