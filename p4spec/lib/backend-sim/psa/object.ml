module Value = Runtime.Sim.Value
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

  let count (value_ctx : Value.t) (value_sto : Value.t) (counter : t) :
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
    (counter, value_ctx, value_sto, value_callResult)
end
