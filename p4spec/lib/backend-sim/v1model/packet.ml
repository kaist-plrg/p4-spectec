module Value = Runtime.Value

(* The data types below are cold serialized state (yojson, [Value.t]-based); only
   the value<->info conversions touch spec values, so they are functorized over
   [V] (see [Make] at the bottom). *)

(* Packet clones *)

module CloneInfo = struct
  type clone_type = I2E | E2E [@@deriving yojson]
  type t = clone_type * int * int [@@deriving yojson]
end

(* Packet resubmissions *)

module ResubmitInfo = struct
  type t = int [@@deriving yojson]
end

(* Packet recirculations *)

module RecirculateInfo = struct
  type t = int [@@deriving yojson]
end

(* Actions on a packet *)

type action = {
  clone_opt : CloneInfo.t option;
  resubmit_opt : ResubmitInfo.t option;
  recirculate_opt : RecirculateInfo.t option;
}
[@@deriving yojson]

let empty_action =
  { clone_opt = None; resubmit_opt = None; recirculate_opt = None }

(* Processing context per packet *)

type entrypoint = Ingress | Egress [@@deriving yojson]

type t = {
  (* Evaluation context *)
  value_ctx : Value.t;
  (* Packet input *)
  packet_in : Core.Object.PacketIn.t;
  (* Which block the packet should begin processing
     AFTER running Parser + Verify block *)
  entrypoint : entrypoint;
}
[@@deriving yojson]

(* The [of_value]/[to_value] conversions pack/unpack spec values, so they are
   functorized over [V]; callers instantiate [Packet.Make (V)] and pass their own
   [vt] directly instead of round-tripping through [V.to_value]/[V.of_value]. *)

module Make (V : Valrep.VAL) = struct
  module Pack = Spec.Pack.Make (V)
  module Unpack = Spec.Unpack.Make (V)
  open Pack
  open Unpack

  module CloneInfo = struct
    let of_value (value_clone_type, value_session, value_index) : CloneInfo.t =
      let clone_type =
        match unpack_p4_enum value_clone_type |> snd with
        | "I2E" -> CloneInfo.I2E
        | "E2E" -> CloneInfo.E2E
        | name ->
            failwith ("Invalid enum value \"" ^ name ^ "\". Expected I2E or E2E")
      in
      let session =
        unpack_p4_fixedBit value_session |> snd |> Bigint.to_int_exn
      in
      let index = unpack_p4_fixedBit value_index |> snd |> Bigint.to_int_exn in
      (clone_type, session, index)

    let to_value ((clone_type, session, index) : CloneInfo.t) =
      let value_clone_type =
        match clone_type with
        | CloneInfo.I2E -> pack_p4_enum "CloneType" "I2E"
        | CloneInfo.E2E -> pack_p4_enum "CloneType" "E2E"
      in
      let value_session =
        pack_p4_fixedBit (Bigint.of_int 32) (Bigint.of_int session)
      in
      let value_index =
        pack_p4_fixedBit (Bigint.of_int 8) (Bigint.of_int index)
      in
      (value_clone_type, value_session, value_index)
  end

  module ResubmitInfo = struct
    let of_value value_index : ResubmitInfo.t =
      unpack_p4_fixedBit value_index |> snd |> Bigint.to_int_exn

    let to_value index =
      pack_p4_fixedBit (Bigint.of_int 8) (Bigint.of_int index)
  end

  module RecirculateInfo = struct
    let of_value value_index : RecirculateInfo.t =
      unpack_p4_fixedBit value_index |> snd |> Bigint.to_int_exn

    let to_value index =
      pack_p4_fixedBit (Bigint.of_int 8) (Bigint.of_int index)
  end
end
