open Lang
open Il
open Util.Source

(* Errors *)

type error = { at : region; msg : string }

let to_region_msg { at; msg } = (at, msg)

(* Entry point *)

let algo_spec (spec : spec) : (Al.spec, error) result =
  try
    Ok (spec |> Binding.Analyze.analyze_spec |> Sidecondition.Guard.insert_spec)
  with Error.AlgoError (at, msg) -> Error { at; msg }
