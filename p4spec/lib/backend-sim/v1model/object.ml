module Value = Runtime.Sim.Value
open Interface.Pack
open Interface.Wrap
open Interface.Unwrap
open Interface.Unpack
open Error

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
      | _ ->
          error_no_region
            (Format.asprintf
               "counter constructor expects 2 arguments, but %d were given"
               (List.length values_arg))
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
    | _ ->
        error_no_region
          (Format.asprintf "invalid CounterType enum value: %s.%s" id_enum
             id_type)

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

  let count (value_ctx : Value.t) (value_arch : Value.t)
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
    (counter, value_ctx, value_arch, value_callResult)
end

(* Register *)

module Register = struct
  (* Type *)

  (* type t = { typ : Il.Ast.value; values : Il.Ast.value list } [@@deriving yojson] *)
  type t = { typ : Value.t; values : Value.t list } [@@deriving yojson]

  let pp fmt (_reg : t) = Format.fprintf fmt "Register"

  (* A register object is created by calling its constructor.  This
     creates an array of 'size' identical elements, each with type
     T.  The array indices are in the range [0, size-1].  For
     example, this constructor call:

         register<bit<32>>(512) my_reg;

     allocates storage for 512 values, each with type bit<32>.

     register(bit<32> size); *)

  let init (value_type_args : Value.t) (value_args : Value.t) : t =
    let values_type_arg = unwrap_list_v value_type_args in
    let value_type =
      match values_type_arg with
      | [ value_type ] -> value_type
      | _ ->
          error_no_region
            (Format.asprintf
               "register constructor expects 1 type argument, but %d were given"
               (List.length values_type_arg))
    in
    let values_arg = unwrap_list_v value_args in
    let value_size =
      match values_arg with
      | [ value_size ] -> value_size
      | _ ->
          error_no_region
            (Format.asprintf
               "register constructor expects 1 argument, but %d were given"
               (List.length values_arg))
    in
    let value_default = Spec.Func.default value_type in
    let _, size = unpack_p4_fixedBit value_size in
    let size = Bigint.to_int_exn size in
    let values = List.init size (fun _ -> value_default) in
    { typ = value_type; values }

  (* read() reads the state of the register array stored at the
     specified index, and returns it as the value written to the
     result parameter.

     @param index The index of the register array element to be
                  read, normally a value in the range [0, size-1].
     @param result Only types T that are bit<W> are currently
                  supported.  When index is in range, the value of
                  result becomes the value read from the register
                  array element.  When index >= size, the final
                  value of result is not specified, and should be
                  ignored by the caller.

     void read(out T result, in bit<32> index); *)

  let read (value_ctx : Value.t) (value_arch : Value.t) (reg : t) :
      t * Value.t * Value.t * Value.t =
    let value_index_target = Spec.Func.find_var_e_local value_ctx "index" in
    let _, index_target = unpack_p4_fixedBit value_index_target in
    let index_target = Bigint.to_int_exn index_target in
    let value =
      if index_target < List.length reg.values then
        List.nth reg.values index_target
      else Spec.Func.default reg.typ
    in
    let value_ctx =
      Spec.Rel.lvalue_write_var_local value_ctx value_arch "result" value
    in
    let value_callResult =
      let value_eps = wrap_opt_v "value" None in
      [ Term "RETURN"; NT value_eps ] #@ "returnResult"
    in
    (reg, value_ctx, value_arch, value_callResult)

  (* write() writes the state of the register array at the specified
     index, with the value provided by the value parameter.

     If you wish to perform a read() followed later by a write() to
     the same register array element, and you wish the
     read-modify-write sequence to be atomic relative to other
     processed packets, then there may be parallel implementations
     of the v1model architecture for which you must execute them in
     a P4_16 block annotated with an @atomic annotation.  See the
     P4_16 language specification description of the @atomic
     annotation for more details.

     @param index The index of the register array element to be
                  written, normally a value in the range [0,
                  size-1].  If index >= size, no register state will
                  be updated.
     @param value Only types T that are bit<W> are currently
                  supported.  When index is in range, this
                  parameter's value is written into the register
                  array element specified by index.
     void write(in bit<32> index, in T value); *)

  let write (value_ctx : Value.t) (value_arch : Value.t) (reg : t) :
      t * Value.t * Value.t * Value.t =
    let value_index_target = Spec.Func.find_var_e_local value_ctx "index" in
    let _, index_target = unpack_p4_fixedBit value_index_target in
    let index_target = Bigint.to_int_exn index_target in
    let value_target = Spec.Func.find_var_e_local value_ctx "value" in
    let values =
      List.mapi
        (fun idx value -> if idx = index_target then value_target else value)
        reg.values
    in
    let reg = { reg with values } in
    let value_callResult =
      let value_eps = wrap_opt_v "value" None in
      [ Term "RETURN"; NT value_eps ] #@ "returnResult"
    in
    (reg, value_ctx, value_arch, value_callResult)
end

(* Direct counter *)

module DirectCounter = struct
  (* Type *)

  type t =
    | Packets of
        (Bigint.t
        [@to_yojson Util.Json.bigint_to_yojson]
        [@of_yojson Util.Json.bigint_of_yojson])
    | Bytes of
        (Bigint.t
        [@to_yojson Util.Json.bigint_to_yojson]
        [@of_yojson Util.Json.bigint_of_yojson])
    | PacketsAndBytes of
        ((Bigint.t
         [@to_yojson Util.Json.bigint_to_yojson]
         [@of_yojson Util.Json.bigint_of_yojson])
        * (Bigint.t
          [@to_yojson Util.Json.bigint_to_yojson]
          [@of_yojson Util.Json.bigint_of_yojson]))
  [@@deriving yojson]

  let pp fmt (_ctr : t) = Format.fprintf fmt "direct counter"

  (* A direct_counter object is created by calling its constructor.
     You must provide a choice of whether to maintain only a packet
     count (CounterType.packets), only a byte count
     (CounterType.bytes), or both (CounterType.packets_and_bytes).
     After constructing the object, you can associate it with at
     most one table, by adding the following table property to the
     definition of that table:

         counters = <object_name>;

     Counters can be updated from your P4 program, but can only be
     read from the control plane.  If you need something that can be
     both read and written from the P4 program, consider using a
     register.

     direct_counter(CounterType type); *)

  let init (_value_type_args : Value.t) (value_args : Value.t) : t =
    let values_arg = unwrap_list_v value_args in
    let value_type =
      match values_arg with
      | [ value_type ] -> value_type
      | _ ->
          error_no_region
            (Format.asprintf
               "direct_counter constructor expects 1 argument, but %d were \
                given"
               (List.length values_arg))
    in
    let id_enum, id_type = unpack_p4_enum value_type in
    match (id_enum, id_type) with
    | "CounterType", "packets" -> Packets Bigint.zero
    | "CounterType", "bytes" -> Bytes Bigint.zero
    | "CounterType", "packets_and_bytes" ->
        PacketsAndBytes (Bigint.zero, Bigint.zero)
    | _ ->
        error_no_region
          (Format.asprintf "invalid CounterType enum value: %s.%s" id_enum
             id_type)

  (* The count() method is actually unnecessary in the v1model
     architecture.  This is because after a direct_counter object
     has been associated with a table as described in the
     documentation for the direct_counter constructor, every time
     the table is applied and a table entry is matched, the counter
     state associated with the matching entry is read, modified, and
     written back, atomically relative to the processing of other
     packets, regardless of whether the count() method is called in
     the body of that action.

     void count(); *)

  let count (value_ctx : Value.t) (value_arch : Value.t)
      (packet_in : Core.Object.PacketIn.t) (counter : t) :
      t * Value.t * Value.t * Value.t =
    (* Update counter *)
    let counter =
      match counter with
      | Packets counts -> Packets Bigint.(counts + one)
      | Bytes counts ->
          let len = packet_in.len |> Bigint.of_int in
          Bytes Bigint.(counts + len)
      | PacketsAndBytes (counts_packets, count_bytes) ->
          let len = packet_in.len |> Bigint.of_int in
          PacketsAndBytes
            (Bigint.(counts_packets + one), Bigint.(count_bytes + len))
    in
    (* Create call result *)
    let value_callResult =
      let value_eps = wrap_opt_v "value" None in
      [ Term "RETURN"; NT value_eps ] #@ "returnResult"
    in
    (counter, value_ctx, value_arch, value_callResult)
end

(* Direct meter *)

module DirectMeter = struct
  (* Type *)

  type t =
    | Packets of
        (Bigint.t
        [@to_yojson Util.Json.bigint_to_yojson]
        [@of_yojson Util.Json.bigint_of_yojson])
    | Bytes of
        (Bigint.t
        [@to_yojson Util.Json.bigint_to_yojson]
        [@of_yojson Util.Json.bigint_of_yojson])
  [@@deriving yojson]

  let pp fmt (_ctr : t) = Format.fprintf fmt "direct meter"

  (* A direct_meter object is created by calling its constructor.
     You must provide a choice of whether to meter based on the
     number of packets, regardless of their size
     (MeterType.packets), or based upon the number of bytes the
     packets contain (MeterType.bytes).  After constructing the
     object, you can associate it with at most one table, by adding
     the following table property to the definition of that table:

         meters = <object_name>;

     direct_meter(MeterType type); *)

  let init (_value_type_args : Value.t) (value_args : Value.t) : t =
    let values_arg = unwrap_list_v value_args in
    let value_type =
      match values_arg with
      | [ value_type ] -> value_type
      | _ ->
          error_no_region
            (Format.asprintf
               "direct_counter constructor expects 1 argument, but %d were \
                given"
               (List.length values_arg))
    in
    let id_enum, id_type = unpack_p4_enum value_type in
    match (id_enum, id_type) with
    | "MeterType", "packets" -> Packets Bigint.zero
    | "MeterType", "bytes" -> Bytes Bigint.zero
    | _ ->
        error_no_region
          (Format.asprintf "invalid CounterType enum value: %s.%s" id_enum
             id_type)

  (* After a direct_meter object has been associated with a table as
     described in the documentation for the direct_meter
     constructor, every time the table is applied and a table entry
     is matched, the meter state associated with the matching entry
     is read, modified, and written back, atomically relative to the
     processing of other packets, regardless of whether the read()
     method is called in the body of that action.

     read() may only be called within an action executed as a result
     of matching a table entry, of a table that has a direct_meter
     associated with it.  Calling read() causes an integer encoding
     of one of the colors green, yellow, or red to be written to the
     result out parameter.

     @param result Type T must be bit<W> with W >= 2.  The value of
                  result will be assigned 0 for color GREEN, 1 for
                  color YELLOW, and 2 for color RED (see RFC 2697
                  and RFC 2698 for the meaning of these colors).

     void read(out T result); *)

  let read (value_ctx : Value.t) (value_sto : Value.t)
      (_packet_in : Core.Object.PacketIn.t) (meter : t) :
      t * Value.t * Value.t * Value.t =
    (* Get "T" *)
    let value_typ = Spec.Func.find_type_e_local value_ctx "T" in
    (* Get size of "T" *)
    let size =
      Spec.Func.subst_type_e_local value_ctx value_typ
      |> Spec.Func.sizeof_maxSizeInBits'
    in
    (* NOTE: returning GREEN for now *)
    let value = pack_p4_fixedBit size Bigint.zero in
    let value_ctx =
      Spec.Rel.lvalue_write_var_local value_ctx value_sto "result" value
    in
    (* Create call result *)
    let value_callResult =
      let value_eps = wrap_opt_v "value" None in
      [ Term "RETURN"; NT value_eps ] #@ "returnResult"
    in
    (meter, value_ctx, value_sto, value_callResult)
end
