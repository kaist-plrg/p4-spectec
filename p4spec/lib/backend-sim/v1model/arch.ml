open Interface.Wrap
open Interface.Unwrap
module Value = Runtime.Sim.Value

type t = {
  queue : Scheduler.t;
  mirrortable : Mirror.Table.t;
  clone_opt : Packet.CloneInfo.t option;
  resubmit_opt : Packet.ResubmitInfo.t option;
}
[@@deriving yojson]

(* Constructors *)

let empty =
  {
    queue = Scheduler.empty;
    mirrortable = Mirror.Table.empty;
    resubmit_opt = None;
    clone_opt = None;
  }

let reset (t : t) = { t with resubmit_opt = None; clone_opt = None }

(* Value conversion *)

let to_value (t : t) = t |> to_yojson |> wrap_extern_v "archState"
let of_value (v : Value.t) = v |> unwrap_extern_v |> of_yojson |> Result.get_ok

(* Queue and mirror table setters *)

let with_queue (queue : Scheduler.t) (t : t) = { t with queue }

let with_mirrortable (mirrortable : Mirror.Table.t) (t : t) =
  { t with mirrortable }

(* Resubmit setters *)

let with_resubmit_opt (resubmit_opt : Packet.ResubmitInfo.t option) (t : t) : t
    =
  { t with resubmit_opt }

let with_resubmit (resubmit : Packet.ResubmitInfo.t) =
  with_resubmit_opt (Some resubmit)

(* Clone setters *)

let with_clone_opt (clone_opt : Packet.CloneInfo.t option) (t : t) : t =
  { t with clone_opt }

let with_clone (clone : Packet.CloneInfo.t) = with_clone_opt (Some clone)
