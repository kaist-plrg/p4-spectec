module Typ = Runtime.Type.Typ
module Value = Runtime.Value
open Error
open Util.Source

module Make
    (V : Runtime.Valrep.SAFE)
    (Spec_Func : Spec.Func.S with type vt = V.t) =
struct
  module Pack = Spec.Pack.Make (V)
  module Unpack = Spec.Unpack.Make (V)
  module Hash = Hash.Make (V)
  open Pack
  open Unpack

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

    let pp fmt (_ctr : t) = Format.fprintf fmt "Counter"

    (* extern Counter<W, S>

       Indirect counter with n_counters independent counter values, where
       every counter value has a data plane size specified by type W.

       Counter(bit<32> n_counters, PSA_CounterType_t type); *)

    let init (_value_type_args : V.t) (value_args : V.t) : t =
      let values_arg = V.Get.list value_args in
      let value_size, value_type =
        match values_arg with
        | [ value_size; value_type ] -> (value_size, value_type)
        | _ ->
            error_no_region
              (Format.asprintf
                 "Counter constructor expects 2 arguments, but %d were given"
                 (List.length values_arg))
      in
      let _, size = unpack_p4_fixedBit value_size in
      let size = Bigint.to_int_exn size in
      let id_enum, id_type = unpack_p4_enum value_type in
      match (id_enum, id_type) with
      | "PSA_CounterType_t", "PACKETS" ->
          Packets (List.init size (fun _ -> Bigint.zero))
      | "PSA_CounterType_t", "BYTES" ->
          Bytes (List.init size (fun _ -> Bigint.zero))
      | "PSA_CounterType_t", "PACKETS_AND_BYTES" ->
          PacketsAndBytes (List.init size (fun _ -> (Bigint.zero, Bigint.zero)))
      | _ ->
          error_no_region
            (Format.asprintf "invalid PSA_CounterType_t enum value: %s.%s"
               id_enum id_type)

    (* void count(in S index); *)

    let count (value_ctx : V.t) (value_arch : V.t) (counter : t) :
        t * V.t * V.t * V.t =
      (* Get "index" *)
      let value_index = Spec_Func.find_var_e_local value_ctx "index" in
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
        | _ ->
            error_no_region
              "Only enum value PACKETS of PSA_CounterType_t is supported"
      in
      (* Create call result *)
      let value_callResult =
        let typ = Typ.Make.opt (Typ.Make.var ("value" $ no_region) []) in
        let value_eps = V.Make.opt typ None in
        V.Make.("RETURN value?" <| [ value_eps ] <<| Typs.return_result)
      in
      (counter, value_ctx, value_arch, value_callResult)
  end

  (* Meter *)

  module Meter = struct
    (* Type *)

    type color = RED | GREEN | YELLOW [@@deriving yojson]
    type t = Packets of color list | Bytes of color list [@@deriving yojson]

    let pp fmt (_meter : t) = Format.fprintf fmt "Meter"

    (* extern Meter<S>

       Indexed meter with n_meters independent meter states.

       Meter(bit<32> n_meters, PSA_MeterType_t type); *)

    let init (_value_type_args : V.t) (value_args : V.t) : t =
      let values_arg = V.Get.list value_args in
      let value_size, value_type =
        match values_arg with
        | [ value_size; value_type ] -> (value_size, value_type)
        | _ ->
            error_no_region
              (Format.asprintf
                 "Meter constructor expects 2 arguments, but %d were given"
                 (List.length values_arg))
      in
      let _, size = unpack_p4_fixedBit value_size in
      let size = Bigint.to_int_exn size in
      let id_enum, id_type = unpack_p4_enum value_type in
      match (id_enum, id_type) with
      | "PSA_MeterType_t", "PACKETS" ->
          Packets (List.init size (fun _ -> GREEN))
      | "PSA_MeterType_t", "BYTES" -> Bytes (List.init size (fun _ -> GREEN))
      | _ ->
          error_no_region
            (Format.asprintf "invalid PSA_MeterType_t enum value: %s.%s" id_enum
               id_type)

    (* Use this method call to perform a color aware meter update (see
       RFC 2698). The color of the packet before the method call was
       made is specified by the color parameter.

       PSA_MeterColor_t execute(in S index, in PSA_MeterColor_t color); *)

    let execute_color_aware (value_ctx : V.t) (value_arch : V.t)
        (meter : t) : t * V.t * V.t * V.t =
      (* NOTE: returning GREEN for now *)
      let value_color = pack_p4_enum "PSA_MeterColor_t" "GREEN" in
      let value_callResult =
        let typ = Typ.Make.opt (Typ.Make.var ("value" $ no_region) []) in
        let value_color_opt = V.Make.opt typ (Some value_color) in
        V.Make.("RETURN value?" <| [ value_color_opt ] <<| Typs.return_result)
      in
      (meter, value_ctx, value_arch, value_callResult)

    (* Use this method call to perform a color blind meter update (see
       RFC 2698).  It may be implemented via a call to execute(index,
       MeterColor_t.GREEN), which has the same behavior.

       PSA_MeterColor_t execute(in S index); *)

    let execute_color_blind (value_ctx : V.t) (value_arch : V.t)
        (meter : t) : t * V.t * V.t * V.t =
      (* NOTE: returning GREEN for now *)
      let value_color = pack_p4_enum "PSA_MeterColor_t" "GREEN" in
      let value_callResult =
        let typ = Typ.Make.opt (Typ.Make.var ("value" $ no_region) []) in
        let value_color_opt = V.Make.opt typ (Some value_color) in
        V.Make.("RETURN value?" <| [ value_color_opt ] <<| Typs.return_result)
      in
      (meter, value_ctx, value_arch, value_callResult)
  end

  (* Register *)

  module Register = struct
    (* Type *)

    type t = { typ : Value.t; values : Value.t list } [@@deriving yojson]

    let pp fmt (_reg : t) = Format.fprintf fmt "Register"

    (* extern Register<T, S>

       Instantiate an array of <size> registers. The initial value is
       undefined.
       Register(bit<32> size);

       Initialize an array of <size> registers and set their value to
       initial_value.

       Register(bit<32> size, T initial_value); *)

    let init (value_type_args : V.t) (value_args : V.t) : t =
      let values_type_arg = V.Get.list value_type_args in
      let value_type =
        match values_type_arg with
        | [ value_type; _value_type_size ] -> value_type
        | _ ->
            error_no_region
              (Format.asprintf
                 "Register constructor expects 2 type arguments, but %d were \
                  given"
                 (List.length values_type_arg))
      in
      let values_arg = V.Get.list value_args in
      let value_size, value_initial =
        match values_arg with
        | [ value_size ] -> (value_size, Spec_Func.default value_type)
        | [ value_size; value_initial ] -> (value_size, value_initial)
        | _ ->
            error_no_region
              (Format.asprintf
                 "Register constructor expects 1 or 2 arguments, but %d were \
                  given"
                 (List.length values_arg))
      in
      let _, size = unpack_p4_fixedBit value_size in
      let size = Bigint.to_int_exn size in
      let value_initial = V.marshal Typs.value value_initial in
      let values = List.init size (fun _ -> value_initial) in
      { typ = V.marshal Typs.type_ir value_type; values }

    (* T read(in S index); *)

    let read (value_ctx : V.t) (value_arch : V.t) (reg : t) :
        t * V.t * V.t * V.t =
      let value_index_target = Spec_Func.find_var_e_local value_ctx "index" in
      let _, index_target = unpack_p4_fixedBit value_index_target in
      let index_target = Bigint.to_int_exn index_target in
      let value =
        if index_target < List.length reg.values then
          V.unmarshal Typs.value (List.nth reg.values index_target)
        else Spec_Func.default (V.unmarshal Typs.type_ir reg.typ)
      in
      let value_callResult =
        let typ = Typ.Make.opt (Typ.Make.var ("value" $ no_region) []) in
        let value_opt = V.Make.opt typ (Some value) in
        V.Make.("RETURN value?" <| [ value_opt ] <<| Typs.return_result)
      in
      (reg, value_ctx, value_arch, value_callResult)

    (* void write (in S index, in T value); *)

    let write (value_ctx : V.t) (value_arch : V.t) (reg : t) :
        t * V.t * V.t * V.t =
      let value_index_target = Spec_Func.find_var_e_local value_ctx "index" in
      let _, index_target = unpack_p4_fixedBit value_index_target in
      let index_target = Bigint.to_int_exn index_target in
      let value_target =
        Spec_Func.find_var_e_local value_ctx "value" |> V.marshal Typs.value
      in
      let values =
        List.mapi
          (fun idx value -> if idx = index_target then value_target else value)
          reg.values
      in
      let reg = { reg with values } in
      let value_callResult =
        let typ = Typ.Make.opt (Typ.Make.var ("value" $ no_region) []) in
        let value_eps = V.Make.opt typ None in
        V.Make.("RETURN value?" <| [ value_eps ] <<| Typs.return_result)
      in
      (reg, value_ctx, value_arch, value_callResult)
  end

  (* Hash *)

  module HashExtern = struct
    (* Type *)

    type t = string [@@deriving yojson]

    let pp fmt (_hash : t) = Format.fprintf fmt "Hash"

    (* extern Hash<O>

       Hash(PSA_HashAlgorithm_t algo); *)

    let init (_value_type_args : V.t) (value_args : V.t) : t =
      let values_arg = V.Get.list value_args in
      let value_algo =
        match values_arg with
        | [ value_algo ] -> value_algo
        | _ ->
            error_no_region
              (Format.asprintf
                 "Hash constructor expects 1 argument, but %d were given"
                 (List.length values_arg))
      in
      match unpack_p4_enum value_algo with
      | "PSA_HashAlgorithm_t", "IDENTITY" -> "identity"
      | "PSA_HashAlgorithm_t", "CRC32" -> "crc32"
      | "PSA_HashAlgorithm_t", "CRC16" -> "crc16"
      | "PSA_HashAlgorithm_t", "ONES_COMPLEMENT16" -> "csum16"
      | "PSA_HashAlgorithm_t", algo -> algo
      | _ -> assert false

    (* Compute the hash for data.
       @param data The data over which to calculate the hash.
       @return The hash value.

       O get_hash<D>(in D data); *)

    let get_hash (value_ctx : V.t) (value_arch : V.t) (hash : t) :
        t * V.t * V.t * V.t =
      let values =
        Spec_Func.find_var_e_local value_ctx "data" |> unpack_p4_tuple
      in
      let result = Hash.compute_checksum hash values in
      let value_typ_O = Spec_Func.find_type_e_local value_ctx "O" in
      let value_result = pack_p4_arbitraryInt result in
      let value_result = Spec_Func.cast_op value_typ_O value_result in
      let value_callResult =
        let typ = Typ.Make.opt (Typ.Make.var ("value" $ no_region) []) in
        let value_result_opt = V.Make.opt typ (Some value_result) in
        V.Make.("RETURN value?" <| [ value_result_opt ] <<| Typs.return_result)
      in
      (hash, value_ctx, value_arch, value_callResult)

    (* Compute the hash for data, with modulo by max, then add base.
       @param base Minimum return value.
       @param data The data over which to calculate the hash.
       @param max The hash value is divided by max to get modulo.
              An implementation may limit the largest value supported,
              e.g. to a value like 32, or 256, and may also only
              support powers of 2 for this value.  P4 developers should
              limit their choice to such values if they wish to
              maximize portability.
       @return (base + (h % max)) where h is the hash value.

       O get_hash<T, D>(in T base, in D data, in T max); *)

    let get_hash_adjust (value_ctx : V.t) (value_arch : V.t) (hash : t)
        : t * V.t * V.t * V.t =
      let base =
        Spec_Func.find_var_e_local value_ctx "base" |> unpack_p4_fixedBit |> snd
      in
      let rmax =
        Spec_Func.find_var_e_local value_ctx "max" |> unpack_p4_fixedBit |> snd
      in
      let values =
        Spec_Func.find_var_e_local value_ctx "data" |> unpack_p4_tuple
      in
      let result = Hash.compute_checksum hash values in
      let result = Bigint.(base + (result % rmax)) in
      let value_typ_O = Spec_Func.find_type_e_local value_ctx "O" in
      let value_result = pack_p4_arbitraryInt result in
      let value_result = Spec_Func.cast_op value_typ_O value_result in
      let value_callResult =
        let value_result_opt =
          let typ = Typ.Make.opt (Typ.Make.var ("value" $ no_region) []) in
          V.Make.opt typ (Some value_result)
        in
        V.Make.("RETURN value?" <| [ value_result_opt ] <<| Typs.return_result)
      in
      (hash, value_ctx, value_arch, value_callResult)
  end

  (* InternetChecksum *)

  module InternetChecksum = struct
    (* Type *)

    type t =
      (Bigint.t
      [@to_yojson Util.Json.bigint_to_yojson]
      [@of_yojson Util.Json.bigint_of_yojson])
    [@@deriving yojson]

    let pp fmt (_ck : t) = Format.fprintf fmt "InternetChecksum"

    (* extern InternetChecksum

       Checksum based on `ONES_COMPLEMENT16` algorithm used in IPv4, TCP, and UDP.
       Supports incremental updating via `subtract` method.
       See IETF RFC 1624.

       InternetChecksum(); *)

    let init (_value_type_args : V.t) (_value_args : V.t) : t =
      Bigint.zero

    (* Reset internal state and prepare unit for computation. Every
       instance of an InternetChecksum object is automatically
       initialized as if clear() had been called on it, once for each
       time the parser or control it is instantiated within is
       executed.  All state maintained by it is independent per packet.

       void clear(); *)

    let clear (value_ctx : V.t) (value_arch : V.t) (_checksum : t) :
        t * V.t * V.t * V.t =
      let value_callResult =
        let typ = Typ.Make.opt (Typ.Make.var ("value" $ no_region) []) in
        let value_eps = V.Make.opt typ None in
        V.Make.("RETURN value?" <| [ value_eps ] <<| Typs.return_result)
      in
      (Bigint.zero, value_ctx, value_arch, value_callResult)

    (* Add data to checksum. data must be a multiple of 16 bits long.

       void add<T>(in T data); *)

    let add (value_ctx : V.t) (value_arch : V.t) (checksum : t) :
        t * V.t * V.t * V.t =
      let values =
        Spec_Func.find_var_e_local value_ctx "data" |> unpack_p4_tuple
      in
      let checksum =
        Hash.compute_checksum "csum16" ~value_init:checksum values
      in
      let checksum = Hash.bitwise_neg checksum (Bigint.of_int 16) in
      let value_callResult =
        let value_eps =
          let typ = Typ.Make.opt (Typ.Make.var ("value" $ no_region) []) in
          V.Make.opt typ None
        in
        V.Make.("RETURN value?" <| [ value_eps ] <<| Typs.return_result)
      in
      (checksum, value_ctx, value_arch, value_callResult)

    (* Subtract data from existing checksum. data must be a multiple of
       16 bits long.

       void subtract<T>(in T data); *)

    let subtract (value_ctx : V.t) (value_arch : V.t) (checksum : t) :
        t * V.t * V.t * V.t =
      let values =
        Spec_Func.find_var_e_local value_ctx "data" |> unpack_p4_tuple
      in
      let checksum =
        Hash.compute_checksum "csum16_sub" ~value_init:checksum values
      in
      let checksum = Hash.bitwise_neg checksum (Bigint.of_int 16) in
      let value_callResult =
        let value_eps =
          let typ = Typ.Make.opt (Typ.Make.var ("value" $ no_region) []) in
          V.Make.opt typ None
        in
        V.Make.("RETURN value?" <| [ value_eps ] <<| Typs.return_result)
      in
      (checksum, value_ctx, value_arch, value_callResult)

    (* Get checksum for data added (and not removed) since last clear

       bit<16> get(); *)

    let get (value_ctx : V.t) (value_arch : V.t) (checksum : t) :
        t * V.t * V.t * V.t =
      let checksum = Hash.bitwise_neg checksum (Bigint.of_int 16) in
      let value_checksum = pack_p4_fixedBit (Bigint.of_int 16) checksum in
      let value_callResult =
        let value_checksum_opt =
          let typ = Typ.Make.opt (Typ.Make.var ("value" $ no_region) []) in
          V.Make.opt typ (Some value_checksum)
        in
        V.Make.(
          "RETURN value?" <| [ value_checksum_opt ] <<| Typs.return_result)
      in
      (checksum, value_ctx, value_arch, value_callResult)

    (* Get current state of checksum computation.  The return value is
       only intended to be used for a future call to the set_state
       method.

       bit<16> get_state(); *)

    let get_state (value_ctx : V.t) (value_arch : V.t) (checksum : t) :
        t * V.t * V.t * V.t =
      let value_checksum = pack_p4_fixedBit (Bigint.of_int 16) checksum in
      let value_callResult =
        let value_checksum_opt =
          let typ = Typ.Make.opt (Typ.Make.var ("value" $ no_region) []) in
          V.Make.opt typ (Some value_checksum)
        in
        V.Make.(
          "RETURN value?" <| [ value_checksum_opt ] <<| Typs.return_result)
      in
      (checksum, value_ctx, value_arch, value_callResult)

    (* Restore the state of the InternetChecksum instance to one
       returned from an earlier call to the get_state method.  This
       state could have been returned from the same instance of the
       InternetChecksum extern, or a different one.

       void set_state(in bit<16> checksum_state); *)

    let set_state (value_ctx : V.t) (value_arch : V.t) (_checksum : t) :
        t * V.t * V.t * V.t =
      let checksum_state =
        Spec_Func.find_var_e_local value_ctx "checksum_state"
        |> unpack_p4_fixedBit |> snd
      in
      let value_callResult =
        let value_eps =
          let typ = Typ.Make.opt (Typ.Make.var ("value" $ no_region) []) in
          V.Make.opt typ None
        in
        V.Make.("RETURN value?" <| [ value_eps ] <<| Typs.return_result)
      in
      (checksum_state, value_ctx, value_arch, value_callResult)
  end
end
