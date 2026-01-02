open Domain
open Lib

module Family = Set.Make (struct
  type t = MIdSet.t

  let compare = compare
end)

type t = Family.t

let to_string t =
  t |> Family.elements
  |> List.concat_map MIdSet.elements
  |> List.map Mixop.string_of_mixop
  |> String.concat ", "
