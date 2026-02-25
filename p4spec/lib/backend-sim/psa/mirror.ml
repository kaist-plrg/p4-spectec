module Json = Util.Json

(* Mirror table *)

module Table = Json.Map.Make (struct
  type t = int [@@deriving yojson]
end)
