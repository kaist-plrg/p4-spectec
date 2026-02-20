open Lang
open El
open Domain.Lib

module Column = struct
  type t = Il.tablerow list

  let to_string t =
    String.concat "\n" (List.map (fun row -> Il.Print.string_of_tablerow row) t)
end

module Columns = MakeFIdEnv (Column)

module Group = struct
  type t = param * plaintyp * Columns.t

  let to_string _ = "table group"
end
