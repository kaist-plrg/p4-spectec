module Typ = Runtime.Type.Typ
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

(* Value conversion (functorized over [V]; the rest of the module is
   representation-independent plain data, so callers keep using the top-level
   [Arch] for setters/constructors and an [Arch.Make (V)] instance for these
   two conversions). *)

module Make (V : Valrep.SAFE) = struct
  let to_value (t : t) =
    t |> to_yojson |> V.Make.extern (Typ.Make.var ("archState" $ no_region) [])

  let of_value (v : V.t) = v |> V.Get.extern |> of_yojson |> Result.get_ok
end

(* Queue and mirror table setters *)

let with_queue (queue : Scheduler.t) (t : t) = { t with queue }

let with_mirrortable (mirrortable : Mirror.Table.t) (t : t) =
  { t with mirrortable }

let with_multicast (multicast : Multicast.State.t) (t : t) =
  { t with multicast }
