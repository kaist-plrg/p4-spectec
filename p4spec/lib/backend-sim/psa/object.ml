module Value = Runtime.Sim.Value
open Interface.Wrap
open Interface.Unwrap
open Interface.Pack
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

  let pp fmt (_ctr : t) = Format.fprintf fmt "Counter"

  (* extern Counter<W, S>

     Indirect counter with n_counters independent counter values, where
     every counter value has a data plane size specified by type W.

     Counter(bit<32> n_counters, PSA_CounterType_t type); *)

  let init (_value_type_args : Value.t) (value_args : Value.t) : t =
    let values_arg = unwrap_list_v value_args in
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
          (Format.asprintf "invalid PSA_CounterType_t enum value: %s.%s" id_enum
             id_type)

  (* void count(in S index); *)

  let count (value_ctx : Value.t) (value_arch : Value.t) (counter : t) :
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
      | _ ->
          error_no_region
            "Only enum value PACKETS of PSA_CounterType_t is supported"
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

  (* extern Register<T, S>

     Instantiate an array of <size> registers. The initial value is
     undefined.
     Register(bit<32> size);

     Initialize an array of <size> registers and set their value to
     initial_value.

     Register(bit<32> size, T initial_value); *)

  let init (value_type_args : Value.t) (value_args : Value.t) : t =
    let values_type_arg = unwrap_list_v value_type_args in
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
    let values_arg = unwrap_list_v value_args in
    let value_size, value_initial =
      match values_arg with
      | [ value_size ] -> (value_size, Spec.Func.default value_type)
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
    let values = List.init size (fun _ -> value_initial) in
    { typ = value_type; values }

  (* T read(in S index); *)

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
    let value_callResult =
      let value_opt = wrap_opt_v "value" (Some value) in
      [ Term "RETURN"; NT value_opt ] #@ "returnResult"
    in
    (reg, value_ctx, value_arch, value_callResult)

  (* void write (in S index, in T value); *)

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

(* Hash *)

module Hash = struct
  (* Type *)

  type t = string [@@deriving yojson]

  let pp fmt (_hash : t) = Format.fprintf fmt "Hash"

  (* extern Hash<O>

     Hash(PSA_HashAlgorithm_t algo); *)

  let init (_value_type_args : Value.t) (value_args : Value.t) : t =
    let values_arg = unwrap_list_v value_args in
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
    | "PSA_HashAlgorithm_t", algo -> String.lowercase_ascii algo
    | _ -> assert false

  (* Compute the hash for data.
     @param data The data over which to calculate the hash.
     @return The hash value.

     O get_hash<D>(in D data); *)

  let get_hash (value_ctx : Value.t) (value_arch : Value.t) (hash : t) :
      t * Value.t * Value.t * Value.t =
    let values =
      Spec.Func.find_var_e_local value_ctx "data" |> unpack_p4_tuple
    in
    let result = Hash.compute_checksum hash values in
    let value_typ_O = Spec.Func.find_type_e_local value_ctx "O" in
    let value_result = pack_p4_arbitraryInt result in
    let value_result = Spec.Func.cast_op value_typ_O value_result in
    let value_callResult =
      let value_result_opt = wrap_opt_v "value" (Some value_result) in
      [ Term "RETURN"; NT value_result_opt ] #@ "returnResult"
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

  let get_hash_adjust (value_ctx : Value.t) (value_arch : Value.t) (hash : t) :
      t * Value.t * Value.t * Value.t =
    let base =
      Spec.Func.find_var_e_local value_ctx "base" |> unpack_p4_fixedBit |> snd
    in
    let rmax =
      Spec.Func.find_var_e_local value_ctx "max" |> unpack_p4_fixedBit |> snd
    in
    let values =
      Spec.Func.find_var_e_local value_ctx "data" |> unpack_p4_tuple
    in
    let result = Hash.compute_checksum hash values in
    let result = Bigint.(base + (result % rmax)) in
    let value_typ_O = Spec.Func.find_type_e_local value_ctx "O" in
    let value_result = pack_p4_arbitraryInt result in
    let value_result = Spec.Func.cast_op value_typ_O value_result in
    let value_callResult =
      let value_result_opt = wrap_opt_v "value" (Some value_result) in
      [ Term "RETURN"; NT value_result_opt ] #@ "returnResult"
    in
    (hash, value_ctx, value_arch, value_callResult)
end
