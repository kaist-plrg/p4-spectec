module Typ = Runtime.Type.Typ
module Value = Runtime.Value
open Util.Source

type t = {
  queue : Scheduler.t;
  mirrortable : Mirror.Table.t;
  multicast : Multicast.State.t;
}
[@@deriving yojson]

(* Constructors *)

let empty =
  {
    queue = Scheduler.empty;
    mirrortable = Mirror.Table.empty;
    multicast = Multicast.State.empty;
  }

(* Value conversion *)

let to_value (t : t) =
  t |> to_yojson
  |> Value.Make.extern (Typ.Make.var ("archState" $ no_region) [])

let of_value (v : Value.t) = v |> Value.Get.extern |> of_yojson |> Result.get_ok

(* Queue and mirror table setters *)

let with_queue (queue : Scheduler.t) (t : t) = { t with queue }

let with_mirrortable (mirrortable : Mirror.Table.t) (t : t) =
  { t with mirrortable }

let with_multicast (multicast : Multicast.State.t) (t : t) =
  { t with multicast }
