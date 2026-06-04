open Domain
open Lib

(* Constructor identifier *)

type t = Id.t * Mixop.t

let compare (id_a, mixop_a) (id_b, mixop_b) =
  let c = Id.compare id_a id_b in
  if c <> 0 then c else Mixop.compare mixop_a mixop_b
