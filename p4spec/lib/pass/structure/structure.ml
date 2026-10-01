open Lang

(* Entry point *)

type error = Diagnostic.t

let struct_spec ~(final : bool) (spec : Al.spec) : (Sl.spec, error) result =
  Ok (Struct.struct_spec ~final spec)
