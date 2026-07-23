open Lang
open Il

(* Entry point *)

let algo_spec (spec : spec) : Al.spec =
  spec |> Binding.Analyze.analyze_spec |> Sidecondition.Guard.insert_spec
