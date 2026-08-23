open Lang

type error = Diagnostic.t

(* Entry point *)

let struct_spec ~(final : bool) (spec : Al.spec) : (Sl.spec, error) result =
  try Ok (Struct.struct_spec ~final spec) with Error.StructError d -> Error d
