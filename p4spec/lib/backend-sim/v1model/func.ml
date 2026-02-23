module Value = Runtime.Sim.Value
open Interface.Wrap
open Interface.Pack
open Interface.Unpack
open Error

(* Generate a random number in the range lo..hi, inclusive, and write
   it to the result parameter.  The value written to result is not
   specified if lo > hi.

   @param T          Must be a type bit<W>

   extern void random<T>(out T result, in T lo, in T hi); *)
let _random (_value_ctx : Value.t) (_value_sto : Value.t) : Value.t * Value.t =
  error_no_region "extern function random is not implemented"

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
let digest (value_ctx : Value.t) (value_sto : Value.t) :
    Value.t * Value.t * Value.t =
  (* no-op *)
  let value_callResult =
    let value_eps = wrap_opt_v "value" None in
    [ Term "RETURN"; NT value_eps ] #@ "returnResult"
  in
  (value_ctx, value_sto, value_callResult)

(* mark_to_drop(standard_metadata) is a primitive action that modifies
   standard_metadata.egress_spec to an implementation-specific special
   value that in some cases causes the packet to be dropped at the end
   of ingress or egress processing. It also asssigs 0 to
   standard_metadata.mcast_grp. Either of those metadata fields may
   be changed by executing later P4 code, after calling
   mark_to_drop(), and this can change the resulting behavior of the
   packet to do something other than drop.

   extern void mark_to_drop(inout standard_metadata_t standard_metadata); *)
let mark_to_drop (value_ctx : Value.t) (value_sto : Value.t) :
    Value.t * Value.t * Value.t =
  let value_egress_spec =
    pack_p4_fixedBit (Bigint.of_int 9) (Bigint.of_int 511)
  in
  let value_ctx =
    Spec.Rel.lvalue_write_dot_local value_ctx value_sto "standard_metadata"
      "egress_spec" value_egress_spec
  in
  let value_mcast_grp = pack_p4_fixedBit (Bigint.of_int 16) (Bigint.of_int 0) in
  let value_ctx =
    Spec.Rel.lvalue_write_dot_local value_ctx value_sto "standard_metadata"
      "mcast_grp" value_mcast_grp
  in
  let value_callResult =
    let value_eps = wrap_opt_v "value" None in
    [ Term "RETURN"; NT value_eps ] #@ "returnResult"
  in
  (value_ctx, value_sto, value_callResult)

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
  let values = Spec.Func.find_var_e_local value_ctx "data" |> unpack_p4_tuple in
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

let do_verify_checksum ~(payload : Core.Object.PacketIn.t option)
    (value_ctx : Value.t) (value_sto : Value.t) : Value.t * Value.t * Value.t =
  (* Get "data" in context *)
  let value_data = Spec.Func.find_var_e_local value_ctx "data" in
  let values = value_data |> unpack_p4_tuple in
  (* Get payload *)
  let values_payload =
    match payload with
    | Some packet_in ->
        let payload_bytes = Core.Object.PacketIn.payload_bytes packet_in in
        payload_bytes |> Array.to_list
        |> List.map (fun byte -> pack_p4_fixedBit (Bigint.of_int 8) byte)
    | None -> []
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
  if condition then do_verify_checksum ~payload:None value_ctx value_sto
  else
    let value_callResult =
      let value_eps = wrap_opt_v "value" None in
      [ Term "RETURN"; NT value_eps ] #@ "returnResult"
    in
    (value_ctx, value_sto, value_callResult)

let verify_checksum_with_payload (value_ctx : Value.t) (value_sto : Value.t)
    (packet_in : Core.Object.PacketIn.t) : Value.t * Value.t * Value.t =
  (* Get "condition" in context *)
  let value_condition = Spec.Func.find_var_e_local value_ctx "condition" in
  let condition = value_condition |> unpack_p4_bool in
  if condition then
    do_verify_checksum ~payload:(Some packet_in) value_ctx value_sto
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

let do_update_checksum ~(payload : Core.Object.PacketIn.t option)
    (value_ctx : Value.t) (value_sto : Value.t) : Value.t * Value.t * Value.t =
  (* Get "data" in context *)
  let value_data = Spec.Func.find_var_e_local value_ctx "data" in
  let values = value_data |> unpack_p4_tuple in
  (* Get payload *)
  let values_payload =
    match payload with
    | Some packet_in ->
        let payload_bytes = Core.Object.PacketIn.payload_bytes packet_in in
        payload_bytes |> Array.to_list
        |> List.map (fun byte -> pack_p4_fixedBit (Bigint.of_int 8) byte)
    | None -> []
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
  if condition then do_update_checksum ~payload:None value_ctx value_sto
  else
    let value_callResult =
      let value_eps = wrap_opt_v "value" None in
      [ Term "RETURN"; NT value_eps ] #@ "returnResult"
    in
    (value_ctx, value_sto, value_callResult)

let update_checksum_with_payload (value_ctx : Value.t) (value_sto : Value.t)
    (packet_in : Core.Object.PacketIn.t) : Value.t * Value.t * Value.t =
  (* Get "condition" in context *)
  let condition =
    Spec.Func.find_var_e_local value_ctx "condition" |> unpack_p4_bool
  in
  if condition then
    do_update_checksum ~payload:(Some packet_in) value_ctx value_sto
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
  error_no_region "extern function clone is not implemented"

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
let resubmit_preserving_field_list (value_ctx : Value.t) (value_sto : Value.t) :
    Value.t * Value.t * Value.t =
  let value_index = Spec.Func.find_var_e_local value_ctx "index" in
  (* write resubmit index in arch state *)
  let value_arch_state =
    value_sto |> Spec.Func.find_archState_e |> Arch.of_value
    |> Arch.with_resubmit (Packet.ResubmitInfo.of_value value_index)
    |> Arch.to_value
  in
  let value_sto = Spec.Func.update_archState_e value_sto value_arch_state in
  let value_callResult =
    let value_eps = wrap_opt_v "value" None in
    [ Term "RETURN"; NT value_eps ] #@ "returnResult"
  in
  (value_ctx, value_sto, value_callResult)

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
let recirculate_preserving_field_list (value_ctx : Value.t)
    (value_arch : Value.t) : Value.t * Value.t * Value.t =
  let value_index = Spec.Func.find_var_e_local value_ctx "index" in
  (* write recirculate index in arch state *)
  let value_arch_state =
    value_arch |> Spec.Func.find_archState_e |> Arch.of_value
    |> Arch.with_recirculate (Packet.RecirculateInfo.of_value value_index)
    |> Arch.to_value
  in
  let value_arch = Spec.Func.update_archState_e value_arch value_arch_state in
  let value_callResult =
    let value_eps = wrap_opt_v "value" None in
    [ Term "RETURN"; NT value_eps ] #@ "returnResult"
  in
  (value_ctx, value_arch, value_callResult)

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
let clone_preserving_field_list (value_ctx : Value.t) (value_sto : Value.t) :
    Value.t * Value.t * Value.t =
  let arch_state = Spec.Func.find_archState_e value_sto |> Arch.of_value in
  let value_type = Spec.Func.find_var_e_local value_ctx "type" in
  let value_session = Spec.Func.find_var_e_local value_ctx "session" in
  let value_index = Spec.Func.find_var_e_local value_ctx "index" in
  let packet_clone =
    Packet.CloneInfo.of_value (value_type, value_session, value_index)
  in
  (* mark arch state with clone information *)
  let value_arch_state =
    arch_state |> Arch.with_clone packet_clone |> Arch.to_value
  in
  let value_sto = Spec.Func.update_archState_e value_sto value_arch_state in
  (* Return void *)
  let value_callResult =
    let value_eps = wrap_opt_v "value" None in
    [ Term "RETURN"; NT value_eps ] #@ "returnResult"
  in
  (value_ctx, value_sto, value_callResult)

let _truncate (_value_ctx : Value.t) (_value_sto : Value.t) : Value.t * Value.t
    =
  error_no_region "extern function truncate is not implemented"

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
let _assert_ (_value_ctx : Value.t) (_value_sto : Value.t) : Value.t * Value.t =
  error_no_region "extern function assert is not implemented"

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
let _assume (_value_ctx : Value.t) (_value_sto : Value.t) : Value.t * Value.t =
  error_no_region "extern function assume is not implemented"

(* Log user defined messages
   Example: log_msg("User defined message");
   or log_msg("Value1 = {}, Value2 = {}",{value1, value2});

   extern void log_msg(string msg);
   extern void log_msg<T>(string msg, in T data); *)

let log_msg (value_ctx : Value.t) (value_sto : Value.t) :
    Value.t * Value.t * Value.t =
  let msg = Spec.Func.find_var_e_local value_ctx "msg" |> unpack_p4_string in
  print_endline msg;
  (* Return void *)
  let value_callResult =
    let value_eps = wrap_opt_v "value" None in
    [ Term "RETURN"; NT value_eps ] #@ "returnResult"
  in
  (value_ctx, value_sto, value_callResult)

let format_braces (fmt : string) (args : Value.t list) =
  let n = String.length fmt in
  let buf = Buffer.create (n + 64) in
  let rec walk i args =
    if i >= n then
      match args with
      | [] -> Buffer.contents buf
      | _ -> error_no_region "too many arguments for format string in log_msg"
    else
      match fmt.[i] with
      | '{' when i + 1 < n && fmt.[i + 1] = '{' ->
          Buffer.add_char buf '{';
          walk (i + 2) args
      | '}' when i + 1 < n && fmt.[i + 1] = '}' ->
          Buffer.add_char buf '}';
          walk (i + 2) args
      | '{' when i + 1 < n && fmt.[i + 1] = '}' -> (
          match args with
          | a :: rest ->
              Buffer.add_string buf (Value.to_string a);
              walk (i + 2) rest
          | [] ->
              error_no_region
                "not enough arguments for format string in log_msg")
      | c ->
          Buffer.add_char buf c;
          walk (i + 1) args
  in
  walk 0 args

let log_msg_format (value_ctx : Value.t) (value_sto : Value.t) :
    Value.t * Value.t * Value.t =
  let msg = Spec.Func.find_var_e_local value_ctx "msg" |> unpack_p4_string in
  let data = Spec.Func.find_var_e_local value_ctx "data" |> unpack_p4_tuple in
  format_braces msg data |> print_endline;
  (* Return void *)
  let value_callResult =
    let value_eps = wrap_opt_v "value" None in
    [ Term "RETURN"; NT value_eps ] #@ "returnResult"
  in
  (value_ctx, value_sto, value_callResult)
