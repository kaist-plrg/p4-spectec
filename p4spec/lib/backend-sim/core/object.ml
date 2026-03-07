module Value = Runtime.Sim.Value
open Interface.Wrap
open Interface.Unwrap
open Interface.Pack
open Interface.Unpack

(* Bit manipulation *)

type bits = bool Array.t

let bits_to_yojson = Util.Json.array_to_yojson (fun b -> `Bool b)

let bits_of_yojson =
  Util.Json.array_of_yojson (function
    | `Bool b -> Ok b
    | _ -> Error "expected boolean")

let string_to_bits str =
  let char_to_bits c =
    let n =
      match c with
      | '0' .. '9' -> Char.code c - Char.code '0'
      | 'a' .. 'f' -> Char.code c - Char.code 'a' + 10
      | 'A' .. 'F' -> Char.code c - Char.code 'A' + 10
      | _ -> assert false
    in
    [ n land 8 <> 0; n land 4 <> 0; n land 2 <> 0; n land 1 <> 0 ]
  in
  str |> String.to_seq |> List.of_seq |> List.map char_to_bits |> List.flatten
  |> Array.of_list

let bits_to_string bits =
  let bits_to_int bits =
    List.fold_left (fun i bit -> (i lsl 1) + if bit then 1 else 0) 0 bits
  in
  let int_to_char i =
    if i < 10 then Char.chr (i + Char.code '0')
    else Char.chr (i - 10 + Char.code 'A')
  in
  let len = Array.length bits in
  let rec loop idx str =
    if idx >= len then str
    else
      let bits = Array.sub bits idx (min 4 (len - idx)) |> Array.to_list in
      let bits =
        if List.length bits < 4 then
          bits @ List.init (4 - List.length bits) (fun _ -> false)
        else bits
      in
      let c = bits |> bits_to_int |> int_to_char in
      loop (idx + 4) (str ^ String.make 1 c)
  in
  loop 0 ""

let bits_to_int_unsigned bits =
  Array.fold_left
    (fun i bit -> Bigint.((i lsl 1) + if bit then one else zero))
    Bigint.zero bits

let bits_to_int_signed bits =
  let ssig = bits.(0) in
  let int_unsigned = bits_to_int_unsigned bits in
  if ssig then
    let int_max =
      let len = Array.length bits - 1 in
      Bigint.(one lsl len)
    in
    Bigint.(int_unsigned - (int_max * (one + one)))
  else int_unsigned

let int_to_bits_unsigned value size =
  Array.init size (fun i -> Bigint.(value land (one lsl i) > zero))
  |> Array.to_list |> List.rev |> Array.of_list

let int_to_bits_signed value size =
  let mask = Bigint.((one lsl size) - one) in
  let value = Bigint.(value land mask) in
  int_to_bits_unsigned value size

(* Core extern objects *)

(* Input packet *)

module PacketIn = struct
  (* Type *)

  type t = { bits : bits; idx : int; len : int } [@@deriving yojson]

  let pp fmt (pkt : t) = Format.fprintf fmt "%s" (bits_to_string pkt.bits)

  let pp_payload fmt (pkt : t) =
    let bits = Array.sub pkt.bits pkt.idx (pkt.len - pkt.idx) in
    Format.fprintf fmt "%s" (bits_to_string bits)

  (* Initializer *)

  let init (pkt : string) : t =
    let bits = string_to_bits pkt in
    { bits; idx = 0; len = Array.length bits }

  (* Resetter *)

  let reset (pkt : t) : t = { pkt with idx = 0 }

  (* Parser *)

  let parse (pkt : t) (size : int) : t * bits =
    let bits = Array.sub pkt.bits pkt.idx size in
    let pkt = { pkt with idx = pkt.idx + size } in
    (pkt, bits)

  (* Payload *)

  let payload (pkt : t) : bits =
    let bits = Array.sub pkt.bits pkt.idx (pkt.len - pkt.idx) in
    bits

  let payload_bytes (pkt : t) : Bigint.t Array.t =
    let bits = payload pkt in
    let len = Array.length bits in
    let len = len / 8 in
    Array.init len (fun i -> Array.sub bits (i * 8) 8 |> bits_to_int_unsigned)

  (* Read a header from the packet into a fixed-sized header @hdr and advance the cursor.
     May trigger error PacketTooShort or StackOutOfBounds.
     @T must be a fixed-size header type

     void extract<T>(out T hdr); *)

  let extract (value_ctx : Value.t) (value_arch : Value.t) (pkt : t) :
      t * Value.t * Value.t * Value.t =
    (* Get "T" *)
    let value_typ = Spec.Func.find_type_e_local value_ctx "T" in
    (* Get size of "T" *)
    let size =
      Spec.Func.subst_type_e_local value_ctx value_typ
      |> Spec.Func.sizeof_maxSizeInBits' |> Bigint.to_int_exn
    in
    if pkt.idx + size > pkt.len then
      let value_callResult =
        let value_err =
          wrap_case_v
            [ Term "ERROR"; Term "."; NT (wrap_text_v "PacketTooShort") ]
          |> with_typ (wrap_var_t "errorValue")
        in
        [ Term "REJECT"; NT value_err ] #@ "rejectTransitionResult"
      in
      (pkt, value_ctx, value_arch, value_callResult)
    else
      (* Parse from packet *)
      let pkt, bits = parse pkt size in
      (* Find "hdr" in context *)
      let value_hdr = Spec.Func.find_var_e_local value_ctx "hdr" in
      (* Write bits to "hdr" *)
      let value_hdr = Spec.Func.write_value_from_bits value_hdr 0 bits in
      (* Update "hdr" in context *)
      let value_ctx =
        Spec.Rel.lvalue_write_var_local value_ctx value_arch "hdr" value_hdr
      in
      (* Create call result *)
      let value_callResult =
        let value_eps = wrap_opt_v "value" None in
        [ Term "RETURN"; NT value_eps ] #@ "returnResult"
      in
      (pkt, value_ctx, value_arch, value_callResult)

  (* Read bits from the packet into a variable-sized header @variableSizeHeader
     and advance the cursor.
     @T must be a header containing exactly 1 varbit field.
     May trigger errors PacketTooShort, StackOutOfBounds, or HeaderTooShort.

     void extract<T>(out T variableSizeHeader,
                      in bit<32> variableFieldSizeInBits); *)

  let extract_varsize (value_ctx : Value.t) (value_arch : Value.t) (pkt : t) :
      t * Value.t * Value.t * Value.t =
    (* Get "T" *)
    let value_typ = Spec.Func.find_type_e_local value_ctx "T" in
    (* Get size of "T" *)
    let value_typ_subst = Spec.Func.subst_type_e_local value_ctx value_typ in
    let size_min =
      Spec.Func.sizeof_minSizeInBits' value_typ_subst |> Bigint.to_int_exn
    in
    let size_max =
      Spec.Func.sizeof_maxSizeInBits' value_typ_subst |> Bigint.to_int_exn
    in
    (* Get "variableFieldSizeInBits" in context *)
    let value_variableFieldSizeInBits =
      Spec.Func.find_var_e_local value_ctx "variableFieldSizeInBits"
    in
    let alignment =
      Spec.Func.bitacc_op value_variableFieldSizeInBits
        (pack_p4_arbitraryInt (Bigint.of_int 2))
        (pack_p4_arbitraryInt (Bigint.of_int 0))
      |> unpack_p4_fixedBit |> snd |> Bigint.to_int_exn
    in
    let size_varsize =
      value_variableFieldSizeInBits |> unwrap_case_v |> snd |> fun values ->
      List.nth values 1 |> unwrap_num_v |> Bigint.to_int_exn
    in
    let size = size_min + size_varsize in
    if alignment <> 0 then
      let value_callResult =
        let value_err =
          wrap_case_v
            [ Term "ERROR"; Term "."; NT (wrap_text_v "ParserInvalidArgument") ]
          |> with_typ (wrap_var_t "errorValue")
        in
        [ Term "REJECT"; NT value_err ] #@ "rejectTransitionResult"
      in
      (pkt, value_ctx, value_arch, value_callResult)
    else if pkt.idx + size > pkt.len then
      let value_callResult =
        let value_err =
          wrap_case_v
            [ Term "ERROR"; Term "."; NT (wrap_text_v "PacketTooShort") ]
          |> with_typ (wrap_var_t "errorValue")
        in
        [ Term "REJECT"; NT value_err ] #@ "rejectTransitionResult"
      in
      (pkt, value_ctx, value_arch, value_callResult)
    else if size > size_max then
      let value_callResult =
        let value_err =
          wrap_case_v
            [ Term "ERROR"; Term "."; NT (wrap_text_v "HeaderTooShort") ]
          |> with_typ (wrap_var_t "errorValue")
        in
        [ Term "REJECT"; NT value_err ] #@ "rejectTransitionResult"
      in
      (pkt, value_ctx, value_arch, value_callResult)
    else
      (* Parse from packet *)
      let pkt, bits = parse pkt size in
      (* Get "variableSizeHeader" in context *)
      let value_variableSizeHeader =
        Spec.Func.find_var_e_local value_ctx "variableSizeHeader"
      in
      (* Write bits to "variableSizeHeader" *)
      let value_variableSizeHeader =
        Spec.Func.write_value_from_bits value_variableSizeHeader size_varsize
          bits
      in
      (* Update "variableSizeHeader" in context *)
      let value_ctx =
        Spec.Rel.lvalue_write_var_local value_ctx value_arch
          "variableSizeHeader" value_variableSizeHeader
      in
      (* Create call result *)
      let value_callResult =
        let value_eps = wrap_opt_v "value" None in
        [ Term "RETURN"; NT value_eps ] #@ "returnResult"
      in
      (pkt, value_ctx, value_arch, value_callResult)

  (* Read bits from the packet without advancing the cursor.
     @returns: the bits read from the packet.
     T may be an arbitrary fixed-size type.

     T lookahead<T>(); *)

  let lookahead (value_ctx : Value.t) (value_arch : Value.t) (pkt : t) :
      t * Value.t * Value.t * Value.t =
    (* Get "T" *)
    let value_typ = Spec.Func.find_type_e_local value_ctx "T" in
    (* Get size of "T" *)
    let size =
      Spec.Func.subst_type_e_local value_ctx value_typ
      |> Spec.Func.sizeof_maxSizeInBits' |> Bigint.to_int_exn
    in
    (* Create a dummy "hdr" *)
    let value_hdr = Spec.Func.default value_typ in
    if pkt.idx + size > pkt.len then
      let value_callResult =
        let value_err =
          wrap_case_v
            [ Term "ERROR"; Term "."; NT (wrap_text_v "PacketTooShort") ]
          |> with_typ (wrap_var_t "errorValue")
        in
        [ Term "REJECT"; NT value_err ] #@ "rejectTransitionResult"
      in
      (pkt, value_ctx, value_arch, value_callResult)
    else
      (* Parse from packet *)
      let _pkt, bits = parse pkt size in
      (* Write bits to "hdr" *)
      let value_hdr = Spec.Func.write_value_from_bits value_hdr 0 bits in
      (* Create call result *)
      let value_callResult =
        let value_hdr = wrap_opt_v "value" (Some value_hdr) in
        [ Term "RETURN"; NT value_hdr ] #@ "returnResult"
      in
      (pkt, value_ctx, value_arch, value_callResult)

  (* Advance the packet cursor by the specified number of bits.

     void advance(in bit<32> sizeInBits); *)

  let advance (value_ctx : Value.t) (value_arch : Value.t) (pkt : t) :
      t * Value.t * Value.t * Value.t =
    (* Get "sizeInBits" in context *)
    let value_sizeInBits = Spec.Func.find_var_e_local value_ctx "sizeInBits" in
    let size =
      value_sizeInBits |> unpack_p4_fixedBit |> snd |> Bigint.to_int_exn
    in
    if pkt.idx + size > pkt.len then
      let value_callResult =
        let value_err =
          wrap_case_v
            [ Term "ERROR"; Term "."; NT (wrap_text_v "PacketTooShort") ]
          |> with_typ (wrap_var_t "errorValue")
        in
        [ Term "REJECT"; NT value_err ] #@ "rejectTransitionResult"
      in
      (pkt, value_ctx, value_arch, value_callResult)
    else
      (* Advance cursor *)
      let pkt = { pkt with idx = pkt.idx + size } in
      let value_callResult =
        let value_eps = wrap_opt_v "value" None in
        [ Term "RETURN"; NT value_eps ] #@ "returnResult"
      in
      (pkt, value_ctx, value_arch, value_callResult)

  (* @return packet length in bytes.  This method may be unavailable on
     some target architectures.

     bit<32> length(); *)
  let length (value_ctx : Value.t) (value_arch : Value.t) (pkt : t) :
      t * Value.t * Value.t * Value.t =
    (* Get packet length in bytes *)
    let length = if pkt.len mod 8 = 0 then pkt.len / 8 else (pkt.len / 8) + 1 in
    let value_length =
      pack_p4_fixedBit (Bigint.of_int 32) (Bigint.of_int length)
    in
    let value_callResult =
      let value_length_opt = wrap_opt_v "value" (Some value_length) in
      [ Term "RETURN"; NT value_length_opt ] #@ "returnResult"
    in
    (pkt, value_ctx, value_arch, value_callResult)
end

(* Output packet *)

module PacketOut = struct
  (* Type *)

  type t = { bits : bits } [@@deriving yojson]

  let pp fmt pkt = Format.fprintf fmt "%s" (bits_to_string pkt.bits)

  (* Initializer *)

  let init () = { bits = Array.make 0 false }

  (* Write @hdr into the output packet, advancing cursor.
     @T can be a header type, a header stack, a header_union, or a struct
     containing fields with such types.

     void emit<T>(in T hdr); *)

  let emit (value_ctx : Value.t) (value_arch : Value.t) (pkt : t) :
      t * Value.t * Value.t * Value.t =
    (* Get "hdr" in context *)
    let value_hdr = Spec.Func.find_var_e_local value_ctx "hdr" in
    (* Get bits of "hdr" *)
    let bits =
      Spec.Func.write_bits_from_value value_hdr
      |> unwrap_list_v |> List.map unwrap_bool_v |> Array.of_list
    in
    let pkt = { bits = Array.append pkt.bits bits } in
    (* Create call result *)
    let value_callResult =
      let value_eps = wrap_opt_v "value" None in
      [ Term "RETURN"; NT value_eps ] #@ "returnResult"
    in
    (pkt, value_ctx, value_arch, value_callResult)
end

module Packet = struct
  let pp fmt ((pkt_in, pkt_out) : PacketIn.t * PacketOut.t) =
    let payload_bits =
      Array.sub pkt_in.bits pkt_in.idx (pkt_in.len - pkt_in.idx)
    in
    let packet = Array.append pkt_out.bits payload_bits in
    Format.fprintf fmt "%s" (bits_to_string packet)
end
