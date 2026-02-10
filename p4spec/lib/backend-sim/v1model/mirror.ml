(* Mirror table *)

module Table = struct
  module M = Map.Make (Int)
  include M

  type t = int M.t

  let to_yojson (tbl : t) : Yojson.Safe.t =
    let kvs =
      bindings tbl |> List.map (fun (k, v) -> (string_of_int k, `Int v))
    in
    `Assoc kvs

  let of_yojson : Yojson.Safe.t -> (t, string) result = function
    | `Assoc kvs ->
        let rec aux acc = function
          | [] -> Result.map (fun s -> s |> List.to_seq |> of_seq) acc
          | (k, v) :: tl ->
              Result.bind acc (fun acc' ->
                  match v with
                  | `Int iv -> aux (Ok ((int_of_string k, iv) :: acc')) tl
                  | json ->
                      Error
                        (Format.sprintf "Invalid value for MirrorTable: %s"
                           (Yojson.Safe.to_string json)))
        in
        aux (Ok []) kvs
    | json ->
        Error
          (Format.sprintf "Invalid MirrorTable: %s"
             (Yojson.Safe.to_string json))
end
