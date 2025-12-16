(* JSON conversion helpers for bignum bigint *)

let bigint_to_yojson (num : Bigint.t) : Yojson.Safe.t =
  `String (Bigint.to_string num)

let bigint_of_yojson : Yojson.Safe.t -> (Bigint.t, string) result = function
  | `String n -> (
      try Ok (Bigint.of_string n)
      with _ -> Error (Format.sprintf "error while converting %s to Bigint" n))
  | `Int n -> Ok (Bigint.of_int n)
  | json ->
      Error (Format.sprintf "invalid Bigint: %s" (Yojson.Safe.to_string json))

(* JSON conversion helpers for arrays *)

let array_to_yojson (e_to_yojson : 'a -> Yojson.Safe.t) (arr : 'a Array.t) :
    Yojson.Safe.t =
  `List (Array.to_list (Array.map e_to_yojson arr))

let array_of_yojson (e_of_yojson : Yojson.Safe.t -> ('a, string) result)
    (json : Yojson.Safe.t) : ('a Array.t, string) result =
  match json with
  | `List lst ->
      let rec aux acc = function
        | [] -> Ok (Array.of_list (List.rev acc))
        | x :: xs -> (
            match e_of_yojson x with
            | Ok v -> aux (v :: acc) xs
            | Error e -> Error e)
      in
      aux [] lst
  | _ -> Error "expected a JSON list"
