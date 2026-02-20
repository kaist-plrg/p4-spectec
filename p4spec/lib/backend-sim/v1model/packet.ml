open Interface.Pack
open Interface.Unpack
module Value = Runtime.Sim.Value

(* Packet clones *)

module CloneInfo = struct
  type clone_type = I2E | E2E [@@deriving yojson]
  type t = clone_type * int * int [@@deriving yojson]

  let of_value (value_clone_type, value_session, value_index) =
    let clone_type =
      match unpack_p4_enum value_clone_type |> snd with
      | "I2E" -> I2E
      | "E2E" -> E2E
      | name ->
          failwith ("Invalid enum value \"" ^ name ^ "\". Expected I2E or E2E")
    in
    let session =
      unpack_p4_fixedBit value_session |> snd |> Bigint.to_int_exn
    in
    let index = unpack_p4_fixedBit value_index |> snd |> Bigint.to_int_exn in
    (clone_type, session, index)

  let to_value (clone_type, session, index) =
    let value_clone_type =
      match clone_type with
      | I2E -> pack_p4_enum "CloneType" "I2E"
      | E2E -> pack_p4_enum "CloneType" "E2E"
    in
    let value_session =
      pack_p4_fixedBit (Bigint.of_int 32) (Bigint.of_int session)
    in
    let value_index =
      pack_p4_fixedBit (Bigint.of_int 8) (Bigint.of_int index)
    in
    (value_clone_type, value_session, value_index)
end

(* Packet resubmissions *)

module ResubmitInfo = struct
  type t = int [@@deriving yojson]

  let of_value value_index : t =
    let index = unpack_p4_fixedBit value_index |> snd |> Bigint.to_int_exn in
    index

  let to_value index =
    let value_index =
      pack_p4_fixedBit (Bigint.of_int 8) (Bigint.of_int index)
    in
    value_index
end

(* Packet recirculations *)

module RecirculateInfo = struct
  type t = int [@@deriving yojson]

  let of_value value_index : t =
    let index = unpack_p4_fixedBit value_index |> snd |> Bigint.to_int_exn in
    index

  let to_value index =
    let value_index =
      pack_p4_fixedBit (Bigint.of_int 8) (Bigint.of_int index)
    in
    value_index
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
