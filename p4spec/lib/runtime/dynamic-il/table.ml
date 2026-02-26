open Lang.Il
open Domain.Lib

module Func = struct
  type t = param * tablerow list

  let to_string _ = "table column"
end

module Funcs = MakeFIdEnv (Func)

module Group = struct
  type t = Funcs.t

  let to_string _ = "table group"
end
