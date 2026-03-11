module Json = Util.Json

(* Mirror table *)

(* Clone session id -> multicast group id *)
module Table = Json.Map.Make (struct
  type t = int [@@deriving yojson]
end)
