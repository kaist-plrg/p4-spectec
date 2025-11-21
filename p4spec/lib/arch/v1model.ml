open Interface.Wrap
open Interface.Unwrap
open Interface.Pack
open Interface.Unpack
open Interface.Flatten
module Value = Runtime_dynamic.Value
module IO = Runtime_simulator.Io
module Sim = Runtime_simulator.Simulator
open Error

module Make (Interp_IL : Sim.INTERP_IL) (Interp_SL : Sim.INTERP_SL) : Sim.ARCH =
struct
  (* Specification *)

  let spec : Sim.spec ref = ref Sim.Empty
  let init_spec (spec_ : Sim.spec) : unit = spec := spec_

  (* Call entry points *)

  let call_rel (relname : string) (values_input : Value.t list) : Value.t list =
    let result =
      match !spec with
      | IL spec_il -> Interp_IL.eval_rel spec_il relname values_input
      | SL spec_sl -> Interp_SL.eval_rel spec_sl relname values_input
      | Empty -> assert false
    in
    match result with
    | Pass (values_output, _) -> values_output
    | Fail (at, msg, _) -> error at msg

  let init_call_rel () = Spec.Rel.register call_rel

  let call_func (funcname : string) (typs_input : Sl.Ast.typ list)
      (values_input : Value.t list) : Value.t =
    let result =
      match !spec with
      | IL spec_il ->
          Interp_IL.eval_func spec_il funcname typs_input values_input
      | SL spec_sl ->
          Interp_SL.eval_func spec_sl funcname typs_input values_input
      | Empty -> assert false
    in
    match result with
    | Pass (value_output, _) -> value_output
    | Fail (at, msg, _) -> error at msg

  let init_call_func () = Spec.Func.register call_func

  (* Extern objects *)

  type extern =
    | PacketIn of Core.Object.PacketIn.t
    | PacketOut of Core.Object.PacketOut.t
  [@@deriving yojson]

  let get_extern (value_sto : Value.t) (value_oid : Value.t) : extern =
    Spec.Func.find_store_externState value_sto value_oid
    |> unwrap_extern_v |> extern_of_yojson |> Result.get_ok

  let get_packet_in (value_sto : Value.t) : Core.Object.PacketIn.t =
    let value_oid = wrap_list_v "id" [ wrap_text_v "packet_in" ] in
    match get_extern value_sto value_oid with
    | PacketIn packet_in -> packet_in
    | _ -> failwith "expected PacketIn extern"

  let get_packet_out (value_sto : Value.t) : Core.Object.PacketOut.t =
    let value_oid = wrap_list_v "id" [ wrap_text_v "packet_out" ] in
    match get_extern value_sto value_oid with
    | PacketOut packet_out -> packet_out
    | _ -> failwith "expected PacketOut extern"

  (* Extern functions *)

  (* Generate a random number in the range lo..hi, inclusive, and write
     it to the result parameter.  The value written to result is not
     specified if lo > hi.

     @param T          Must be a type bit<W>

     extern void random<T>(out T result, in T lo, in T hi); *)
  let _random (_value_ctx : Value.t) (_value_sto : Value.t) : Value.t * Value.t
      =
    failwith "extern function random is not implemented"

  (* Calling digest causes a message containing the values specified in
     the data parameter to be sent to the control plane software.  It is
     similar to sending a clone of the packet to the control plane
     software, except that it can be more efficient because the messages
     are typically smaller than packets, and many such small digest
     messages are typically coalesced together into a larger "batch"
     which the control plane software processes all at once.

     The value of the fields that are sent in the message to the control
     plane is the value they have at the time the digest call occurs,
     even if those field values are changed by later ingress control
     code.  See Note 3.

     Calling digest is only supported in the ingress control.  There is
     no way to undo its effects once it has been called.

     If the type T is a named struct, the name is used to generate the
     control plane API.

     The BMv2 implementation of the v1model architecture ignores the
     value of the receiver parameter.

     extern void digest<T>(in bit<32> receiver, in T data); *)
  let _digest (_value_ctx : Value.t) (_value_sto : Value.t) : Value.t * Value.t
      =
    failwith "extern function digest is not implemented"

  (* mark_to_drop(standard_metadata) is a primitive action that modifies
     standard_metadata.egress_spec to an implementation-specific special
     value that in some cases causes the packet to be dropped at the end
     of ingress or egress processing.  It also asssigs 0 to
     standard_metadata.mcast_grp.  Either of those metadata fields may
     be changed by executing later P4 code, after calling
     mark_to_drop(), and this can change the resulting behavior of the
     packet to do something other than drop.

     extern void mark_to_drop(inout standard_metadata_t standard_metadata); *)
  let _mark_to_drop (_value_ctx : Value.t) (_value_sto : Value.t) :
      Value.t * Value.t =
    failwith "extern function mark_to_drop is not implemented"

  (* Calculate a hash function of the value specified by the data
     parameter.  The value written to the out parameter named result
     will always be in the range [base, base+max-1] inclusive, if max >=
     1.  If max=0, the value written to result will always be base.

     Note that the types of all of the parameters may be the same as, or
     different from, each other, and thus their bit widths are allowed
     to be different.

     @param O          Must be a type bit<W>
     @param D          Must be a tuple type where all the fields are bit-fields
                       (type bit<W> or int<W>) or varbits.
     @param T          Must be a type bit<W>
     @param M          Must be a type bit<W>

     extern void hash<O, T, D, M>(out O result, in HashAlgorithm algo,
                                  in T base, in D data, in M max); *)
  let hash (value_ctx : Value.t) (value_sto : Value.t) :
      Value.t * Value.t * Value.t =
    let base =
      Spec.Func.find_var_e_local value_ctx "base" |> unpack_p4_fixedBit |> snd
    in
    let max =
      Spec.Func.find_var_e_local value_ctx "max" |> unpack_p4_fixedBit |> snd
    in
    let values =
      Spec.Func.find_var_e_local value_ctx "data" |> unpack_p4_sequence
    in
    let id_enum, id_enum_field =
      Spec.Func.find_var_e_local value_ctx "algo" |> unpack_p4_enum
    in
    let result =
      match (id_enum, id_enum_field) with
      | "HashAlgorithm", algo ->
          Hash.compute_checksum algo values |> Hash.adjust base max
      | _ -> assert false
    in
    let value_typ_O = Spec.Func.find_type_e_local value_ctx "O" in
    let value_result = pack_p4_arbitraryInt result in
    let result = Spec.Func.cast_op value_typ_O value_result in
    let value_ctx =
      Spec.Rel.lvalue_write_var_local value_ctx value_sto "result" result
    in
    let value_callResult =
      let value_eps = wrap_opt_v "value" None in
      [ Term "RETURN"; NT value_eps ] #@ "returnResult"
    in
    (value_ctx, value_sto, value_callResult)

  (* Verifies the checksum of the supplied data.  If this method detects
     that a checksum of the data is not correct, then the value of the
     standard_metadata checksum_error field will be equal to 1 when the
     packet begins ingress processing.

     Calling verify_checksum is only supported in the VerifyChecksum
     control.

     @param T          Must be a tuple type where all the tuple elements
                       are of type bit<W>, int<W>, or varbit<W>.  The
                       total length of the fields must be a multiple of
                       the output size.
     @param O          Checksum type; must be bit<X> type.
     @param condition  If 'false' the verification always succeeds.
     @param data       Data whose checksum is verified.
     @param checksum   Expected checksum of the data; note that it must
                       be a left-value.
     @param algo       Algorithm to use for checksum (not all algorithms
                       may be supported).  Must be a compile-time
                       constant.

     extern void verify_checksum<T, O>(in bool condition, in T data,
                                       in O checksum, HashAlgorithm algo);

     verify_checksum_with_payload is identical in all ways to
     verify_checksum, except that it includes the payload of the packet
     in the checksum calculation.  The payload is defined as "all bytes
     of the packet which were not parsed by the parser".

     Calling verify_checksum_with_payload is only supported in the
     VerifyChecksum control.

     extern void verify_checksum_with_payload<T, O>(in bool condition, in T data,
                                                    in O checksum, HashAlgorithm algo); *)

  let do_verify_checksum ~(payload : bool) (value_ctx : Value.t)
      (value_sto : Value.t) : Value.t * Value.t * Value.t =
    (* Get "data" in context *)
    let value_data = Spec.Func.find_var_e_local value_ctx "data" in
    let values = value_data |> unpack_p4_sequence in
    (* Get payload *)
    let values_payload =
      if payload then
        let packet_in = get_packet_in value_sto in
        let payload_bytes = Core.Object.PacketIn.payload_bytes packet_in in
        payload_bytes |> Array.to_list
        |> List.map (fun byte -> pack_p4_fixedBit (Bigint.of_int 8) byte)
      else []
    in
    (* Get "checksum" in context *)
    let value_checksum = Spec.Func.find_var_e_local value_ctx "checksum" in
    let checksum_expect = value_checksum |> unpack_p4_fixedBit |> snd in
    (* Get "algo" in context *)
    let value_algo = Spec.Func.find_var_e_local value_ctx "algo" in
    let id_enum, id_enum_field = value_algo |> unpack_p4_enum in
    (* Compute checksum *)
    let checksum_actual =
      match (id_enum, id_enum_field) with
      | "HashAlgorithm", algo ->
          Hash.compute_checksum algo (values @ values_payload)
      | _ -> assert false
    in
    let verified = Bigint.(checksum_expect = checksum_actual) in
    (* Update standard_metadata.checksum_error *)
    let value_ctx =
      if verified then value_ctx
      else
        let value_checksum_error =
          pack_p4_fixedBit (Bigint.of_int 1) (Bigint.of_int 1)
        in
        Spec.Rel.lvalue_write_dot_global value_ctx value_sto "standard_metadata"
          "checksum_error" value_checksum_error
    in
    let value_callResult =
      let value_eps = wrap_opt_v "value" None in
      [ Term "RETURN"; NT value_eps ] #@ "returnResult"
    in
    (value_ctx, value_sto, value_callResult)

  let verify_checksum (value_ctx : Value.t) (value_sto : Value.t) :
      Value.t * Value.t * Value.t =
    (* Get "condition" in context *)
    let value_condition = Spec.Func.find_var_e_local value_ctx "condition" in
    let condition = value_condition |> unpack_p4_bool in
    if condition then do_verify_checksum ~payload:false value_ctx value_sto
    else
      let value_callResult =
        let value_eps = wrap_opt_v "value" None in
        [ Term "RETURN"; NT value_eps ] #@ "returnResult"
      in
      (value_ctx, value_sto, value_callResult)

  let verify_checksum_with_payload (value_ctx : Value.t) (value_sto : Value.t) :
      Value.t * Value.t * Value.t =
    (* Get "condition" in context *)
    let value_condition = Spec.Func.find_var_e_local value_ctx "condition" in
    let condition = value_condition |> unpack_p4_bool in
    if condition then do_verify_checksum ~payload:true value_ctx value_sto
    else
      let value_callResult =
        let value_eps = wrap_opt_v "value" None in
        [ Term "RETURN"; NT value_eps ] #@ "returnResult"
      in
      (value_ctx, value_sto, value_callResult)

  (* Computes the checksum of the supplied data and writes it to the
     checksum parameter.

     Calling update_checksum is only supported in the ComputeChecksum
     control.

     @param T          Must be a tuple type where all the tuple elements
                       are of type bit<W>, int<W>, or varbit<W>.  The
                       total length of the fields must be a multiple of
                       the output size.
     @param O          Output type; must be bit<X> type.
     @param condition  If 'false' the checksum parameter is not changed
     @param data       Data whose checksum is computed.
     @param checksum   Checksum of the data.
     @param algo       Algorithm to use for checksum (not all algorithms
                       may be supported).  Must be a compile-time
                       constant.

     extern void update_checksum<T, O>(in bool condition, in T data,
                                       inout O checksum, HashAlgorithm algo);

     update_checksum_with_payload is identical in all ways to
     update_checksum, except that it includes the payload of the packet
     in the checksum calculation.  The payload is defined as "all bytes
     of the packet which were not parsed by the parser".

     Calling update_checksum_with_payload is only supported in the
     ComputeChecksum control.

     extern void update_checksum_with_payload<T, O>(in bool condition, in T data,
                                                    inout O checksum, HashAlgorithm algo); *)

  let do_update_checksum ~(payload : bool) (value_ctx : Value.t)
      (value_sto : Value.t) : Value.t * Value.t * Value.t =
    (* Get "data" in context *)
    let value_data = Spec.Func.find_var_e_local value_ctx "data" in
    let values = value_data |> unpack_p4_sequence in
    (* Get payload *)
    let values_payload =
      if payload then
        let packet_in = get_packet_in value_sto in
        let payload_bytes = Core.Object.PacketIn.payload_bytes packet_in in
        payload_bytes |> Array.to_list
        |> List.map (fun byte -> pack_p4_fixedBit (Bigint.of_int 8) byte)
      else []
    in
    (* Get "algo" in context *)
    let value_algo = Spec.Func.find_var_e_local value_ctx "algo" in
    let id_enum, id_enum_field = value_algo |> unpack_p4_enum in
    (* Compute checksum *)
    let checksum =
      match (id_enum, id_enum_field) with
      | "HashAlgorithm", algo ->
          Hash.compute_checksum algo (values @ values_payload)
      | _ -> assert false
    in
    (* Get "O" type in context *)
    let value_typ_O = Spec.Func.find_type_e_local value_ctx "O" in
    (* Cast "checksum" *)
    let value_checksum = pack_p4_arbitraryInt checksum in
    let value_checksum = Spec.Func.cast_op value_typ_O value_checksum in
    (* Write to "checksum" in context *)
    let value_ctx =
      Spec.Rel.lvalue_write_var_local value_ctx value_sto "checksum"
        value_checksum
    in
    (* Return void *)
    let value_callResult =
      let value_eps = wrap_opt_v "value" None in
      [ Term "RETURN"; NT value_eps ] #@ "returnResult"
    in
    (value_ctx, value_sto, value_callResult)

  let update_checksum (value_ctx : Value.t) (value_sto : Value.t) :
      Value.t * Value.t * Value.t =
    (* Get "condition" in context *)
    let condition =
      Spec.Func.find_var_e_local value_ctx "condition" |> unpack_p4_bool
    in
    if condition then do_update_checksum ~payload:false value_ctx value_sto
    else
      let value_callResult =
        let value_eps = wrap_opt_v "value" None in
        [ Term "RETURN"; NT value_eps ] #@ "returnResult"
      in
      (value_ctx, value_sto, value_callResult)

  let update_checksum_with_payload (value_ctx : Value.t) (value_sto : Value.t) :
      Value.t * Value.t * Value.t =
    (* Get "condition" in context *)
    let condition =
      Spec.Func.find_var_e_local value_ctx "condition" |> unpack_p4_bool
    in
    if condition then do_update_checksum ~payload:true value_ctx value_sto
    else
      let value_callResult =
        let value_eps = wrap_opt_v "value" None in
        [ Term "RETURN"; NT value_eps ] #@ "returnResult"
      in
      (value_ctx, value_sto, value_callResult)

  (* clone is in most ways identical to the clone_preserving_field_list
     operation, with the only difference being that it never preserves
     any user-defined metadata fields with the cloned packet.  It is
     equivalent to calling clone_preserving_field_list with the same
     type and session parameter values, with empty data.

     extern void clone(in CloneType type, in bit<32> session); *)
  let _clone (_value_ctx : Value.t) (_value_sto : Value.t) : Value.t * Value.t =
    failwith "extern function clone is not implemented"

  (* Calling resubmit_preserving_field_list during execution of the
     ingress control will cause the packet to be resubmitted, i.e. it
     will begin processing again with the parser, with the contents of
     the packet exactly as they were when it last began parsing.  The
     only difference is in the value of the standard_metadata
     instance_type field, and any user-defined metadata fields that the
     resubmit_preserving_field_list operation causes to be preserved.

     The user metadata fields that are tagged with @field_list(index) will
     be sent to the parser together with the packet.

     Calling resubmit_preserving_field_list is only supported in the
     ingress control.  There is no way to undo its effects once it has
     been called.  If resubmit_preserving_field_list is called multiple
     times during a single execution of the ingress control, only one
     packet is resubmitted, and only the user-defined metadata fields
     specified by the field list index from the last such call are
     preserved.  See the v1model architecture documentation (Note 1) for
     more details.

     For example, the user metadata fields can be annotated as follows:
     struct UM {
        @field_list(1)
        bit<32> x;
        @field_list(1, 2)
        bit<32> y;
        bit<32> z;
     }

     Calling resubmit_preserving_field_list(1) will resubmit the packet
     and preserve fields x and y of the user metadata.  Calling
     resubmit_preserving_field_list(2) will only preserve field y.

     extern void resubmit_preserving_field_list(bit<8> index); *)
  let _resubmit_preserving_field_list (_value_ctx : Value.t)
      (_value_sto : Value.t) : Value.t * Value.t =
    failwith "extern function resubmit_preserving_field_list is not implemented"

  (* Calling recirculate_preserving_field_list during execution of the
     egress control will cause the packet to be recirculated, i.e. it
     will begin processing again with the parser, with the contents of
     the packet as they are created by the deparser.  Recirculated
     packets can be distinguished from new packets in ingress processing
     by the value of the standard_metadata instance_type field.  The
     caller may request that some user-defined metadata fields be
     preserved with the recirculated packet.

     The user metadata fields that are tagged with @field_list(index) will be
     sent to the parser together with the packet.

     Calling recirculate_preserving_field_list is only supported in the
     egress control.  There is no way to undo its effects once it has
     been called.  If recirculate_preserving_field_list is called
     multiple times during a single execution of the egress control,
     only one packet is recirculated, and only the user-defined metadata
     fields specified by the field list index from the last such call
     are preserved.  See the v1model architecture documentation (Note 1)
     for more details.

     extern void recirculate_preserving_field_list(bit<8> index); *)
  let _recirculate_preserving_field_list (_value_ctx : Value.t)
      (_value_sto : Value.t) : Value.t * Value.t =
    failwith
      "extern function recirculate_preserving_field_list is not implemented"

  (* Calling clone_preserving_field_list during execution of the ingress
     or egress control will cause the packet to be cloned, sometimes
     also called mirroring, i.e. zero or more copies of the packet are
     made, and each will later begin egress processing as an independent
     packet from the original packet.  The original packet continues
     with its normal next steps independent of the clone(s).

     The session parameter is an integer identifying a clone session id
     (sometimes called a mirror session id).  The control plane software
     must configure each session you wish to use, or else no clones will
     be made using that session.  Typically this will involve the
     control plane software specifying one output port to which the
     cloned packet should be sent, or a list of (port, egress_rid) pairs
     to which a separate clone should be created for each, similar to
     multicast packets.

     Cloned packets can be distinguished from others by the value of the
     standard_metadata instance_type field.

     The user metadata fields that are tagged with @field_list(index) will be
     sent to the parser together with a clone of the packet.

     If clone_preserving_field_list is called during ingress processing,
     the first parameter must be CloneType.I2E.  If
     clone_preserving_field_list is called during egress processing, the
     first parameter must be CloneType.E2E.

     There is no way to undo its effects once it has been called.  If
     there are multiple calls to clone_preserving_field_list and/or
     clone during a single execution of the same ingress (or egress)
     control, only the last clone session and index are used.  See the
     v1model architecture documentation (Note 1) for more details.

     extern void clone_preserving_field_list(in CloneType type,
                                             in bit<32> session, bit<8> index); *)
  let _clone_preserving_field_list (_value_ctx : Value.t) (_value_sto : Value.t)
      : Value.t * Value.t =
    failwith "extern function clone_preserving_field_list is not implemented"

  let _truncate (_value_ctx : Value.t) (_value_sto : Value.t) :
      Value.t * Value.t =
    failwith "extern function truncate is not implemented"

  (* Calling assert when the argument is true has no effect, except any
     effect that might occur due to evaluation of the argument (but see
     below).  If the argument is false, the precise behavior is
     target-specific, but the intent is to record or log which assert
     statement failed, and optionally other information about the
     failure.

     For example, on the simple_switch target, executing an assert
     statement with a false argument causes a log message with the file
     name and line number of the assert statement to be printed, and
     then the simple_switch process exits.

     If you provide the --ndebug command line option to p4c when
     compiling, the compiled program behaves as if all assert statements
     were not present in the source code.

     We strongly recommend that you avoid using expressions as an
     argument to an assert call that can have side effects, e.g. an
     extern method or function call that has side effects.  p4c will
     allow you to do this with no warning given.  We recommend this
     because, if you follow this advice, your program will behave the
     same way when assert statements are removed.

     extern void assert(in bool check); *)
  let _assert_ (_value_ctx : Value.t) (_value_sto : Value.t) : Value.t * Value.t
      =
    failwith "extern function assert is not implemented"

  (* For the purposes of compiling and executing P4 programs on a target
     device, assert and assume are identical, including the use of the
     --ndebug p4c option to elide them.  See documentation for assert.

     The reason that assume exists as a separate function from assert is
     because they are expected to be used differently by formal
     verification tools.  For some formal tools, the goal is to try to
     find example packets and sets of installed table entries that cause
     an assert statement condition to be false.

     Suppose you run such a tool on your program, and the example packet
     given is an MPLS packet, i.e. hdr.ethernet.etherType == 0x8847.
     You look at the example, and indeed it does cause an assert
     condition to be false.  However, your plan is to deploy your P4
     program in a network in places where no MPLS packets can occur.
     You could add extra conditions to your P4 program to handle the
     processing of such a packet cleanly, without assertions failing,
     but you would prefer to tell the tool "such example packets are not
     applicable in my scenario -- never show them to me".  By adding a
     statement:

         assume(hdr.ethernet.etherType != 0x8847);

     at an appropriate place in your program, the formal tool should
     never show you such examples -- only ones that make all such assume
     conditions true.

     The reason that assume statements behave the same as assert
     statements when compiled to a target device is that if the
     condition ever evaluates to false when operating in a network, it
     is likely that your assumption was wrong, and should be reexamined.

     extern void assume(in bool check); *)
  let _assume (_value_ctx : Value.t) (_value_sto : Value.t) : Value.t * Value.t
      =
    failwith "extern function assume is not implemented"

  (* Log user defined messages
     Example: log_msg("User defined message");
     or log_msg("Value1 = {}, Value2 = {}",{value1, value2});

     extern void log_msg(string msg);
     extern void log_msg<T>(string msg, in T data); *)
  let _log_msg (_value_ctx : Value.t) (_value_sto : Value.t) : Value.t * Value.t
      =
    failwith "extern function log_msg is not implemented"

  (* Extern calls *)

  let eval_extern_init (_values_input : Value.t list) : Value.t =
    wrap_extern_v "externState" `Null

  let eval_extern_func_call (values_input : Value.t list) : Value.t list =
    let value_ctx, value_sto, value_name_func, value_names_param =
      match values_input with
      | [ value_ctx; value_sto; value_name_func; value_names_param ] ->
          (value_ctx, value_sto, value_name_func, value_names_param)
      | _ -> failwith "unexpected number of arguments to extern function call"
    in
    let name_func = unwrap_text_v value_name_func in
    let names_param =
      value_names_param |> unwrap_list_v |> List.map unwrap_text_v
    in
    let value_ctx, value_sto, value_callResult =
      match (name_func, names_param) with
      | "verify", [ "check"; "toSignal" ] ->
          Core.Func.verify value_ctx value_sto
      | "verify_checksum", [ "condition"; "data"; "checksum"; "algo" ] ->
          verify_checksum value_ctx value_sto
      | ( "verify_checksum_with_payload",
          [ "condition"; "data"; "checksum"; "algo" ] ) ->
          verify_checksum_with_payload value_ctx value_sto
      | "update_checksum", [ "condition"; "data"; "checksum"; "algo" ] ->
          update_checksum value_ctx value_sto
      | ( "update_checksum_with_payload",
          [ "condition"; "data"; "checksum"; "algo" ] ) ->
          update_checksum_with_payload value_ctx value_sto
      | "hash", [ "result"; "algo"; "base"; "data"; "max" ] ->
          hash value_ctx value_sto
      | _ ->
          failwith
            ("unsupported extern function call: " ^ name_func ^ "("
            ^ String.concat ", " names_param
            ^ ")")
    in
    [ value_ctx; value_sto; value_callResult ]

  let eval_extern_method_call (values_input : Value.t list) : Value.t list =
    let value_ctx, value_sto, value_oid, value_name_method, value_names_param =
      match values_input with
      | [
       value_ctx; value_sto; value_oid; value_name_method; value_names_param;
      ] ->
          (value_ctx, value_sto, value_oid, value_name_method, value_names_param)
      | _ -> failwith "unexpected number of arguments to extern method call"
    in
    let extern = get_extern value_sto value_oid in
    let name_method = unwrap_text_v value_name_method in
    let names_param =
      value_names_param |> unwrap_list_v |> List.map unwrap_text_v
    in
    let extern, value_ctx, value_sto, value_callResult =
      match (extern, name_method, names_param) with
      | PacketIn packet_in, "extract", [ "hdr" ] ->
          let packet_in, value_ctx, value_sto, value_callResult =
            Core.Object.PacketIn.extract value_ctx value_sto packet_in
          in
          let packet_in = PacketIn packet_in in
          (packet_in, value_ctx, value_sto, value_callResult)
      | ( PacketIn packet_in,
          "extract",
          [ "variableSizeHeader"; "variableFieldSizeInBits" ] ) ->
          let packet_in, value_ctx, value_sto, value_callResult =
            Core.Object.PacketIn.extract_varsize value_ctx value_sto packet_in
          in
          let packet_in = PacketIn packet_in in
          (packet_in, value_ctx, value_sto, value_callResult)
      | PacketIn packet_in, "lookahead", [] ->
          let packet_in, value_ctx, value_sto, value_callResult =
            Core.Object.PacketIn.lookahead value_ctx value_sto packet_in
          in
          let packet_in = PacketIn packet_in in
          (packet_in, value_ctx, value_sto, value_callResult)
      | PacketIn packet_in, "advance", [ "sizeInBits" ] ->
          let packet_in, value_ctx, value_sto, value_callResult =
            Core.Object.PacketIn.advance value_ctx value_sto packet_in
          in
          let packet_in = PacketIn packet_in in
          (packet_in, value_ctx, value_sto, value_callResult)
      | PacketOut packet_out, "emit", [ "hdr" ] ->
          let packet_out, value_ctx, value_sto, value_callResult =
            Core.Object.PacketOut.emit value_ctx value_sto packet_out
          in
          let packet_out = PacketOut packet_out in
          (packet_out, value_ctx, value_sto, value_callResult)
      | _ ->
          let oid =
            value_oid |> unwrap_list_v |> List.map unwrap_text_v
            |> String.concat "."
          in
          failwith
            ("unsupported extern method call: " ^ oid ^ "." ^ name_method ^ "("
            ^ String.concat ", " names_param
            ^ ")")
    in
    let value_extern =
      extern |> extern_to_yojson |> wrap_extern_v "externState"
    in
    let value_sto =
      Spec.Func.update_store_externState value_sto value_oid value_extern
    in
    [ value_ctx; value_sto; value_callResult ]

  (* Pipeline initializer *)

  let init_pipe (spec_ : Sim.spec) (includes_p4 : string list)
      (filename_p4 : string) : Value.t * Value.t =
    init_spec spec_;
    init_call_rel ();
    init_call_func ();
    let result =
      match !spec with
      | IL spec_il ->
          Interp_IL.eval_program spec_il "V1Model_init" includes_p4 filename_p4
      | SL spec_sl ->
          Interp_SL.eval_program ~derive:false spec_sl "V1Model_init"
            includes_p4 filename_p4
      | Empty -> assert false
    in
    match result with
    | Pass ([ value_ctx; value_sto ], _, _, _) -> (value_ctx, value_sto)
    | _ -> failwith "Unexpected return from V1Model_init"

  (* Pipeline driver *)

  let setup_rx (value_ctx : Value.t) (value_sto : Value.t) (rx : IO.rx) :
      Value.t * Value.t =
    let port_in, packet_in = rx in
    (* Setup packet_in extern *)
    let packet_in = PacketIn (Core.Object.PacketIn.init packet_in) in
    let packet_in_state = extern_to_yojson packet_in in
    let value_packet_in_state = wrap_extern_v "externState" packet_in_state in
    let value_ctx, value_sto =
      Spec.Rel.v1model_init_packet_in value_ctx value_sto value_packet_in_state
    in
    (* Setup packet_out extern *)
    let packet_out = PacketOut (Core.Object.PacketOut.init ()) in
    let packet_out_state = extern_to_yojson packet_out in
    let value_packet_out_state = wrap_extern_v "externState" packet_out_state in
    let value_ctx, value_sto =
      Spec.Rel.v1model_init_packet_out value_ctx value_sto
        value_packet_out_state
    in
    (* Setup global variables *)
    let value_ctx = Spec.Rel.v1model_init_globals value_ctx value_sto port_in in
    (value_ctx, value_sto)

  let drive_p (value_ctx : Value.t) (value_sto : Value.t) : Value.t * Value.t =
    let value_ctx, value_sto, value_parser_result =
      Spec.Rel.v1model_parser value_ctx value_sto
    in
    let value_ctx =
      match flatten_case_v_opt value_parser_result with
      | Some (_, [ [ "REJECT" ]; [] ], [ value_error ]) ->
          Spec.Rel.lvalue_write_dot_global value_ctx value_sto
            "standard_metadata" "parser_error" value_error
      | Some _ -> value_ctx
      | None -> assert false
    in
    (value_ctx, value_sto)

  let drive_vr (value_ctx : Value.t) (value_sto : Value.t) :
      Value.t * Value.t * Value.t =
    Spec.Rel.v1model_verify value_ctx value_sto

  let drive_ig (value_ctx : Value.t) (value_sto : Value.t) :
      Value.t * Value.t * Value.t =
    Spec.Rel.v1model_ingress value_ctx value_sto

  let drive_eg (value_ctx : Value.t) (value_sto : Value.t) :
      Value.t * Value.t * Value.t =
    Spec.Rel.v1model_egress value_ctx value_sto

  let drive_ck (value_ctx : Value.t) (value_sto : Value.t) :
      Value.t * Value.t * Value.t =
    Spec.Rel.v1model_check value_ctx value_sto

  let drive_dep (value_ctx : Value.t) (value_sto : Value.t) :
      Value.t * Value.t * Value.t =
    Spec.Rel.v1model_deparse value_ctx value_sto

  let resulting_port_packet (value_ctx : Value.t) (value_sto : Value.t) :
      IO.tx option =
    (* Get egress port *)
    let port =
      Spec.Rel.lvalue_read_dot_global value_ctx value_sto "standard_metadata"
        "egress_spec"
      |> unpack_p4_fixedBit |> snd |> Bigint.to_int_exn
    in
    (* Get output packet *)
    let header =
      get_packet_out value_sto |> Format.asprintf "%a" Core.Object.PacketOut.pp
    in
    let payload =
      get_packet_in value_sto
      |> Format.asprintf "%a" Core.Object.PacketIn.pp_payload
    in
    let packet = header ^ payload in
    (* Return port and packet *)
    let tx = (port, packet) in
    Some tx

  let drive_pipe_pre (value_ctx : Value.t) (value_sto : Value.t) (rx : IO.rx) :
      Value.t * Value.t * bool =
    (* Setup port and packet *)
    let value_ctx, value_sto = setup_rx value_ctx value_sto rx in
    (* Parser block *)
    let value_ctx, value_sto = drive_p value_ctx value_sto in
    (* Verify block *)
    let value_ctx, value_sto, _value_verify_result =
      drive_vr value_ctx value_sto
    in
    (* Ingress block *)
    let value_ctx, value_sto, _value_verify_result =
      drive_ig value_ctx value_sto
    in
    (* Check if packet should be dropped *)
    let drop =
      let value_egress_spec =
        Spec.Rel.lvalue_read_dot_global value_ctx value_sto "standard_metadata"
          "egress_spec"
      in
      let width_egress_spec, int_egress_spec =
        unpack_p4_fixedBit value_egress_spec
      in
      Bigint.(width_egress_spec = of_int 9 && int_egress_spec = of_int 511)
    in
    (value_ctx, value_sto, drop)

  let drive_pipe_post (value_ctx : Value.t) (value_sto : Value.t) :
      Value.t * Value.t * IO.tx option =
    (* Egress block *)
    let value_ctx, value_sto, _value_verify_result =
      drive_eg value_ctx value_sto
    in
    (* Check block *)
    let value_ctx, value_sto, _value_check_result =
      drive_ck value_ctx value_sto
    in
    (* Deparser block *)
    let value_ctx, value_sto, _value_deparse_result =
      drive_dep value_ctx value_sto
    in
    (* Get resulting port and packet *)
    let result_opt = resulting_port_packet value_ctx value_sto in
    (value_ctx, value_sto, result_opt)

  let drive_pipe (value_ctx : Value.t) (value_sto : Value.t) (rx : IO.rx) :
      Value.t * Value.t * IO.tx option =
    let value_ctx, value_sto, drop = drive_pipe_pre value_ctx value_sto rx in
    if drop then (value_ctx, value_sto, None)
    else drive_pipe_post value_ctx value_sto
end
