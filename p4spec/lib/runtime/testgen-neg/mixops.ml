open Domain
open Lib

module Family = Set.Make (struct
  type t = MixIdSet.t

  let compare = compare
end)

type t = Family.t

let to_string t =
  t |> Family.elements
  |> List.concat_map MixIdSet.elements
  |> List.map Mixop.string_of_mixop
  |> String.concat ", "
