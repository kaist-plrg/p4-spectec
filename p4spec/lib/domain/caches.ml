(* Cache entry using mixops *)

module MixopEntry = struct
  type t = Mixop.t

  let default : t = Mixop.Arg
  let equal = Mixop.eq

  let hash (m : Mixop.t) : int =
    Hashtbl.hash (Mixop.string_of_mixop m) land 0x7FFFFFFF
end

module MixopCache = Cache.Make.Make (MixopEntry)
