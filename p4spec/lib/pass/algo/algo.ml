open Lang
open Il

(* Entry point *)

type error = Diagnostic.t

let algo_spec (spec : spec) : (Al.spec, error) result =
  try
    Ok (spec |> Binding.Analyze.analyze_spec |> Sidecondition.Guard.insert_spec)
  with Error.AlgoError d -> Error d
