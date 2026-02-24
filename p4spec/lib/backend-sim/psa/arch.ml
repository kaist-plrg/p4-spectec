open Interface.Wrap
open Interface.Unwrap
module Value = Runtime.Sim.Value

type t = { queue : Scheduler.t; multicast : Multicast.State.t }
[@@deriving yojson]

(* Constructors *)

let empty = { queue = Scheduler.empty; multicast = Multicast.State.empty }

(* Value conversion *)

let to_value (t : t) = t |> to_yojson |> wrap_extern_v "archState"
let of_value (v : Value.t) = v |> unwrap_extern_v |> of_yojson |> Result.get_ok

(* Queue and mirror table setters *)

let with_queue (queue : Scheduler.t) (t : t) = { t with queue }

let with_multicast (multicast : Multicast.State.t) (t : t) =
  { t with multicast }
