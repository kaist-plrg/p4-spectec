open Interface.Pack
open Interface.Unpack
open Interface.Wrap
open Interface.Unwrap
module Value = Runtime.Sim.Value

module CloneInfo = struct
  type clone_type = I2E | E2E
  type t = clone_type * int * int
  type v = Value.t * Value.t * Value.t [@@deriving yojson]

  let to_t (value_clone_type, value_session, value_index) =
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

  let to_v (clone_type, session, index) =
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

module ResubmitInfo = struct
  type t = int

  (* bit<8> field_list index *)
  type v = Value.t [@@deriving yojson]

  let to_t (value_index : v) : t =
    let index = unpack_p4_fixedBit value_index |> snd |> Bigint.to_int_exn in
    index

  let to_v (index : t) : v =
    let value_index =
      pack_p4_fixedBit (Bigint.of_int 8) (Bigint.of_int index)
    in
    value_index
end

type t = {
  clone_opt : CloneInfo.v option;
  resubmit_opt : ResubmitInfo.v option;
}
[@@deriving yojson]

let empty = { resubmit_opt = None; clone_opt = None }

let with_clone_opt (clone_opt : CloneInfo.v option) (t : t) : t =
  { t with clone_opt }

let with_clone (clone : CloneInfo.v) = with_clone_opt (Some clone)

let with_resubmit_opt (resubmit_opt : ResubmitInfo.v option) (t : t) : t =
  { t with resubmit_opt }

let with_resubmit (resubmit : ResubmitInfo.v) =
  with_resubmit_opt (Some resubmit)

let to_value (t : t) = t |> to_yojson |> wrap_extern_v "archState"
let of_value (v : Value.t) = v |> unwrap_extern_v |> of_yojson |> Result.get_ok
