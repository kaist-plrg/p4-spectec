module Value = Runtime_dynamic.Value
open Interface.Wrap
open Interface.Unwrap

(* Bit manipulation *)

type bits = bool Array.t

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

(* Core extern objects *)

(* Input packet *)

module PacketIn = struct
  (* Type and initializer *)

  type t = { bits : bits; idx : int; len : int }

  let pp fmt (pkt : t) = Format.fprintf fmt "%s" (bits_to_string pkt.bits)

  let pp_payload fmt (pkt : t) =
    let bits = Array.sub pkt.bits pkt.idx (pkt.len - pkt.idx) in
    Format.fprintf fmt "%s" (bits_to_string bits)

  let init (pkt : string) =
    let bits = string_to_bits pkt in
    { bits; idx = 0; len = Array.length bits }

  (* Size *)

  (* Parser *)

  let parse (pkt : t) (size : int) =
    let bits = Array.sub pkt.bits pkt.idx size in
    let pkt = { pkt with idx = pkt.idx + size } in
    (pkt, bits)

  (* Read a header from the packet into a fixed-sized header @hdr and advance the cursor.
     May trigger error PacketTooShort or StackOutOfBounds.
     @T must be a fixed-size header type

     void extract<T>(out T hdr); *)
  let extract call_rel_one call_func (value_ctx : Value.t) (value_sto : Value.t)
      (pkt : t) : t * Value.t * Value.t * Value.t =
    (* Get "T" *)
    let value_cursor = [ Term "LOCAL" ] #@ "cursor" in
    let value_nameIR = wrap_text_v "T" in
    let value_typ =
      call_func "find_type_eval" [] [ value_cursor; value_ctx; value_nameIR ]
      |> unwrap_opt_v |> Option.get
    in
    (* Get size of "T" after canonicalization *)
    let value_typ_subst =
      call_func "subst_type_eval" [] [ value_cursor; value_ctx; value_typ ]
    in
    let size =
      call_func "sizeof_maxSizeInBits'" [] [ value_typ_subst ] |> unwrap_num_v
    in
    (* Parse from packet *)
    let pkt, bits = parse pkt (Bigint.to_int_exn size) in
    let value_prefixedNameIR =
      let value_nameIR = wrap_text_v "hdr" in
      [ Term "`"; NT value_nameIR ] #@ "prefixedNameIR"
    in
    let value_hdr =
      call_func "find_value_eval" []
        [ value_cursor; value_ctx; value_prefixedNameIR ]
    in
    let value_bits =
      Array.to_list bits |> List.map wrap_bool_v
      |> wrap_list_v_typed Il.Ast.BoolT
    in
    let value_hdr =
      call_func "write_value_from_bits" [] [ value_hdr; value_bits ]
    in
    (* Update "hdr" in context *)
    let value_ctx =
      call_rel_one "Lvalue_write"
        [ value_cursor; value_ctx; value_sto; value_prefixedNameIR; value_hdr ]
    in
    (* Create call result *)
    let value_callResult =
      let value_eps = wrap_opt_v "value" None in
      [ Term "RETURN"; NT value_eps ] #@ "returnResult"
    in
    (pkt, value_ctx, value_sto, value_callResult)

  (* Read bits from the packet into a variable-sized header @variableSizeHeader
     and advance the cursor.
     @T must be a header containing exactly 1 varbit field.
     May trigger errors PacketTooShort, StackOutOfBounds, or HeaderTooShort.

     void extract<T>(out T variableSizeHeader,
                      in bit<32> variableFieldSizeInBits); *)
  (* let extract_varsize (ctx : Ctx.t) pkt : Ctx.t * SSig.t * t = *)

  (* Read bits from the packet without advancing the cursor.
     @returns: the bits read from the packet.
     T may be an arbitrary fixed-size type.

     T lookahead<T>(); *)
  (* let lookahead (ctx : Ctx.t) pkt : SSig.t = *)

  (* Advance the packet cursor by the specified number of bits.

     void advance(in bit<32> sizeInBits); *)
  (* let advance (ctx : Ctx.t) pkt = *)

  (* @return packet length in bytes.  This method may be unavailable on
     some target architectures.

     bit<32> length(); *)
  (* let length pkt = *)
end

(* Output packet *)

module PacketOut = struct
  type t = { bits : bits }

  let pp fmt pkt = Format.fprintf fmt "%s" (bits_to_string pkt.bits)
  let init () = { bits = Array.make 0 false }

  (* Write @hdr into the output packet, advancing cursor.
     @T can be a header type, a header stack, a header_union, or a struct
     containing fields with such types.

     void emit<T>(in T hdr); *)
  let emit call_func (value_ctx : Value.t) (value_sto : Value.t) (pkt : t) :
      t * Value.t * Value.t * Value.t =
    (* Get "hdr" in context *)
    let value_cursor = [ Term "LOCAL" ] #@ "cursor" in
    let value_prefixedNameIR =
      let value_nameIR = wrap_text_v "hdr" in
      [ Term "`"; NT value_nameIR ] #@ "prefixedNameIR"
    in
    let value_hdr =
      call_func "find_value_eval" []
        [ value_cursor; value_ctx; value_prefixedNameIR ]
    in
    (* Get bits of "hdr" *)
    let value_bits = call_func "write_bits_from_value" [] [ value_hdr ] in
    let bits =
      unwrap_list_v value_bits |> List.map unwrap_bool_v |> Array.of_list
    in
    let pkt = { bits = Array.append pkt.bits bits } in
    (* Create call result *)
    let value_callResult =
      let value_eps = wrap_opt_v "value" None in
      [ Term "RETURN"; NT value_eps ] #@ "returnResult"
    in
    (pkt, value_ctx, value_sto, value_callResult)
end
