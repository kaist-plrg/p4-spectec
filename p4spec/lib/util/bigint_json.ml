(* json conversion helpers for bignum librarie's bigint *)

let to_yojson (num : Bigint.t) : Yojson.Safe.t = `String (Bigint.to_string num)

let of_yojson : Yojson.Safe.t -> (Bigint.t, string) result = function
  | `String n -> (
      try Ok (Bigint.of_string n)
      with _ -> Error (Format.sprintf "Error while converting %s to Bigint" n))
  | `Int n -> Ok (Bigint.of_int n)
  | json ->
      Error (Format.sprintf "Invalid Bigint: %s" (Yojson.Safe.to_string json))
