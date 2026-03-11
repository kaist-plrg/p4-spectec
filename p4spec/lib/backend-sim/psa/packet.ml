module Value = Runtime.Sim.Value

(* Processing context per packet *)

type entrypoint = Ingress | Egress [@@deriving yojson]

type t = {
  (* Evaluation context *)
  value_ctx : Value.t;
  (* Packet input *)
  packet_in : Core.Object.PacketIn.t;
  (* Which pipeline the packet should begin processing *)
  entrypoint : entrypoint;
}
[@@deriving yojson]
