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

(* JSON conversion helpers for key-value maps *)

module Map = struct
  module Make (V : sig
    type t [@@deriving yojson]
  end) =
  struct
    module M = Map.Make (Int)
    include M

    type t = V.t M.t

    let to_yojson (t : t) : Yojson.Safe.t =
      let kvs =
        bindings t |> List.map (fun (k, v) -> (string_of_int k, V.to_yojson v))
      in
      `Assoc kvs

    let of_yojson (j : Yojson.Safe.t) : (t, string) result =
      match j with
      | `Assoc kvs ->
          let rec aux acc = function
            | [] -> Ok acc
            | (ks, vj) :: tl -> (
                match int_of_string_opt ks with
                | None -> Error ("Key is not an int: " ^ ks)
                | Some k -> (
                    match V.of_yojson vj with
                    | Error e -> Error ("Value error at key " ^ ks ^ ": " ^ e)
                    | Ok v -> aux (M.add k v acc) tl))
          in
          aux M.empty kvs
      | _ -> Error "Expected JSON object"
  end
end
