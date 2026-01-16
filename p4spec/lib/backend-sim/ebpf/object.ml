module Value = Runtime.Sim.Value
open Interface.Wrap
open Interface.Unwrap
open Interface.Unpack
open Error

(* Extern objects *)

(* CounterArray *)

module CounterArray = struct
  (* Type *)

  type t = int list [@@deriving yojson]

  let pp fmt (_carr : t) = Format.fprintf fmt "counter array"

  (* A counter array is a dense or sparse array of unsigned 32-bit values, visible to the
     control-plane as an EBPF map (array or hash).
     Each counter is addressed by a 32-bit index.
     Counters can only be incremented by the data-plane, but they can be read or
     reset by the control-plane.

     Allocate an array of counters.
     @param max_index  Maximum counter index supported.
     @param sparse     The counter array is supposed to be sparse.

     CounterArray(bit<32> max_index, bool sparse); *)

  let init (_value_type_args : Value.t) (value_args : Value.t) : t =
    let values_arg = unwrap_list_v value_args in
    let value_max_index, value_sparse =
      match values_arg with
      | [ value_max_index; value_sparse ] -> (value_max_index, value_sparse)
      | _ ->
          error_no_region
            (Format.asprintf
               "counter array constructor expects 2 arguments, but %d were \
                given"
               (List.length values_arg))
    in
    let _, max_index = unpack_p4_fixedBit value_max_index in
    let max_index = Bigint.to_int_exn max_index in
    let _sparse = unpack_p4_bool value_sparse in
    List.init max_index (fun _ -> 0)

  (* Increment counter with specified index.

     void increment(in bit<32> index); *)

  let increment (value_ctx : Value.t) (value_sto : Value.t) (counter_array : t)
      : t * Value.t * Value.t * Value.t =
    (* Get "index" *)
    let value_index = Spec.Func.find_var_e_local value_ctx "index" in
    let _, index = unpack_p4_fixedBit value_index in
    let index_target = Bigint.to_int_exn index in
    (* Update counter *)
    let counter_array =
      List.mapi
        (fun idx count -> if idx = index_target then count + 1 else count)
        counter_array
    in
    (* Create call result *)
    let value_callResult =
      let value_eps = wrap_opt_v "value" None in
      [ Term "RETURN"; NT value_eps ] #@ "returnResult"
    in
    (counter_array, value_ctx, value_sto, value_callResult)

  (* Add value to counter with specified index.

     void add(in bit<32> index, in bit<32> value) *)

  let add (value_ctx : Value.t) (value_sto : Value.t) (counter_array : t) :
      t * Value.t * Value.t * Value.t =
    (* Get "index" *)
    let value_index = Spec.Func.find_var_e_local value_ctx "index" in
    let _, index = unpack_p4_fixedBit value_index in
    let index_target = Bigint.to_int_exn index in
    (* Get "value" *)
    let value_value = Spec.Func.find_var_e_local value_ctx "value" in
    let _, value = unpack_p4_fixedBit value_value in
    let value = Bigint.to_int_exn value in
    (* Update counter *)
    let counter_array =
      List.mapi
        (fun idx count -> if idx = index_target then count + value else count)
        counter_array
    in
    (* Create call result *)
    let value_callResult =
      let value_eps = wrap_opt_v "value" None in
      [ Term "RETURN"; NT value_eps ] #@ "returnResult"
    in
    (counter_array, value_ctx, value_sto, value_callResult)
end
