module Value = Runtime_dynamic.Value
open Interface.Wrap
open Interface.Unwrap
open Interface.Unpack

(* Extern objects *)

(* Counter *)

module Counter = struct
  (* Type *)

  type t =
    | Packets of
        (Bigint.t
        [@to_yojson Util.Json.bigint_to_yojson]
        [@of_yojson Util.Json.bigint_of_yojson])
        list
    | Bytes of
        (Bigint.t
        [@to_yojson Util.Json.bigint_to_yojson]
        [@of_yojson Util.Json.bigint_of_yojson])
        list
    | PacketsAndBytes of
        ((Bigint.t
         [@to_yojson Util.Json.bigint_to_yojson]
         [@of_yojson Util.Json.bigint_of_yojson])
        * (Bigint.t
          [@to_yojson Util.Json.bigint_to_yojson]
          [@of_yojson Util.Json.bigint_of_yojson]))
        list
  [@@deriving yojson]

  let pp fmt (_ctr : t) = Format.fprintf fmt "counter"

  (* A counter object is created by calling its constructor.  This
     creates an array of counter states, with the number of counter
     states specified by the size parameter.  The array indices are
     in the range [0, size-1].

     You must provide a choice of whether to maintain only a packet
     count (CounterType.packets), only a byte count
     (CounterType.bytes), or both (CounterType.packets_and_bytes).

     Counters can be updated from your P4 program, but can only be
     read from the control plane.  If you need something that can be
     both read and written from the P4 program, consider using a
     register.

     counter(bit<32> size, CounterType type); *)
  let init (_value_type_args : Value.t) (value_args : Value.t) : t =
    let values_arg = unwrap_list_v value_args in
    let value_size, value_type =
      match values_arg with
      | [ value_size; value_type ] -> (value_size, value_type)
      | _ -> assert false
    in
    let _, size = unpack_p4_fixedBit value_size in
    let size = Bigint.to_int_exn size in
    let id_enum, id_type = unpack_p4_enum value_type in
    match (id_enum, id_type) with
    | "CounterType", "packets" ->
        Packets (List.init size (fun _ -> Bigint.zero))
    | "CounterType", "bytes" -> Bytes (List.init size (fun _ -> Bigint.zero))
    | "CounterType", "packets_and_bytes" ->
        PacketsAndBytes (List.init size (fun _ -> (Bigint.zero, Bigint.zero)))
    | _ -> assert false

  (* count() causes the counter state with the specified index to be
      read, modified, and written back, atomically relative to the
      processing of other packets, updating the packet count, byte
      count, or both, depending upon the CounterType of the counter
      instance used when it was constructed.

      @param index The index of the counter state in the array to be
                   updated, normally a value in the range [0,
                   size-1].  If index >= size, no counter state will be
                   updated.

     void count(in bit<32> index); *)
  let count (value_ctx : Value.t) (value_sto : Value.t)
      (packet_in : Core.Object.PacketIn.t) (counter : t) :
      t * Value.t * Value.t * Value.t =
    (* Get "index" *)
    let value_index = Spec.Func.find_var_e_local value_ctx "index" in
    let _, index = unpack_p4_fixedBit value_index in
    let index_target = Bigint.to_int_exn index in
    (* Update counter *)
    let counter =
      match counter with
      | Packets counts ->
          let counts =
            List.mapi
              (fun index count ->
                if index = index_target then Bigint.(count + one) else count)
              counts
          in
          Packets counts
      | Bytes counts ->
          let len = packet_in.len |> Bigint.of_int in
          let counts =
            List.mapi
              (fun index count ->
                if index = index_target then Bigint.(count + len) else count)
              counts
          in
          Bytes counts
      | PacketsAndBytes counts ->
          let len = packet_in.len |> Bigint.of_int in
          let counts =
            List.mapi
              (fun index (count_packets, count_bytes) ->
                if index = index_target then
                  (Bigint.(count_packets + one), Bigint.(count_bytes + len))
                else (count_packets, count_bytes))
              counts
          in
          PacketsAndBytes counts
    in
    (* Create call result *)
    let value_callResult =
      let value_eps = wrap_opt_v "value" None in
      [ Term "RETURN"; NT value_eps ] #@ "returnResult"
    in
    (counter, value_ctx, value_sto, value_callResult)
end
