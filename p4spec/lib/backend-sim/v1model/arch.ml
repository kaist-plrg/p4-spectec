module Typ = Runtime.Type.Typ
open Util.Source

type t = {
  queue : Scheduler.t;
  mirrortable : Mirror.Table.t;
  multicast : Multicast.State.t;
  action : Packet.action;
}
[@@deriving yojson]

(* Constructors *)

let empty =
  {
    queue = Scheduler.empty;
    mirrortable = Mirror.Table.empty;
    multicast = Multicast.State.empty;
    action = Packet.empty_action;
  }

let reset (t : t) = { t with action = Packet.empty_action }

(* Value conversion (functorized over [V]; the rest of the module is
   representation-independent plain data, so callers keep using the top-level
   [Arch] for setters/constructors and an [Arch.Make (V)] instance for these
   two conversions). *)

module Make (V : Val.VAL) = struct
  let to_value (t : t) =
    let typ = Typ.Make.var ("archState" $ no_region) [] in
    t |> to_yojson |> V.Make.extern typ

  let of_value (v : V.t) =
    v |> V.Get.extern |> of_yojson |> Result.get_ok
end

(* Queue and mirror table setters *)

let with_queue (queue : Scheduler.t) (t : t) = { t with queue }

let with_mirrortable (mirrortable : Mirror.Table.t) (t : t) =
  { t with mirrortable }

let with_multicast (multicast : Multicast.State.t) (t : t) =
  { t with multicast }

(* Clone setters *)

let with_clone_opt (clone_opt : Packet.CloneInfo.t option) (t : t) : t =
  { t with action = { t.action with clone_opt } }

let with_clone (clone : Packet.CloneInfo.t) = with_clone_opt (Some clone)

(* Resubmit setters *)

let with_resubmit_opt (resubmit_opt : Packet.ResubmitInfo.t option) (t : t) : t
    =
  { t with action = { t.action with resubmit_opt } }

let with_resubmit (resubmit : Packet.ResubmitInfo.t) =
  with_resubmit_opt (Some resubmit)

(* Recirculate setters *)

let with_recirculate_opt (recirculate_opt : Packet.RecirculateInfo.t option)
    (t : t) : t =
  { t with action = { t.action with recirculate_opt } }

let with_recirculate (recirculate : Packet.RecirculateInfo.t) =
  with_recirculate_opt (Some recirculate)
