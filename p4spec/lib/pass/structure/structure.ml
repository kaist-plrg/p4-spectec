open Lang

type error = Diagnostic.t

(* Entry point *)

let struct_spec ~(final : bool) (spec : Al.spec) : (Sl.spec, error) result =
  Ok (Struct.struct_spec ~final spec)
