open Lang

type error = Diagnostic.t

(* Entry point *)

let elab_spec (spec : El.spec) : (Il.spec, error) result =
  try Ok (Elab.elab_spec spec) with Error.ElabError d -> Error d
