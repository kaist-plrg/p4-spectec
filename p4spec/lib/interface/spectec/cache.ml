module Value = Runtime.Value
open Util.Source

(* Cache for boot and unbooting *)

module Tbl = Hashtbl.Make (struct
  type t = Value.t

  let equal = Value.eq
  let hash (value : Value.t) = value.note.vhash
end)
