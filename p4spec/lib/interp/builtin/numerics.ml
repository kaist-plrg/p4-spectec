open Lang
open Xl
open Il
module Value = Runtime.Dynamic_Il.Value
open Error
open Util.Source

(* Maximum bit width *)

let max_bit_width = Bigint.of_int 2048

(* Conversion between meta-bits and OCaml bool array *)

let bits_of_value (value : value) : bool array =
  value |> Value.get_list |> List.map Value.get_bool |> Array.of_list

let value_of_bits (add : value -> unit) (bits : bool array) : value =
  let value =
    let typ = VarT ("bits" $ no_region, []) in
    let values_bit =
      Array.to_list bits |> List.map (fun b -> Value.make BoolT (BoolV b))
    in
    Value.make typ (ListV values_bit)
  in
  add value;
  value

(* Conversion between meta-numerics and OCaml numerics *)

let bigint_of_value (value : value) : Bigint.t =
  value |> Value.get_num |> Num.to_int

let value_of_bigint (add : value -> unit) (i : Bigint.t) : value =
  let value = Value.make (Il.NumT `IntT) (NumV (`Int i)) in
  add value;
  value

(* Built-in implementations *)

(* dec $shl(int, int) : int *)

let rec shl' (v : Bigint.t) (o : Bigint.t) : Bigint.t =
  if Bigint.(o > zero) then shl' Bigint.(v * (one + one)) Bigint.(o - one)
  else v

let shl (add : value -> unit) (at : region) (targs : targ list)
    (values_input : value list) : value =
  Extract.zero at targs;
  let value_base, value_offset = Extract.two at values_input in
  let base = bigint_of_value value_base in
  let offset = bigint_of_value value_offset in
  if Bigint.(offset > max_bit_width) then error at "shift amount too large";
  shl' base offset |> value_of_bigint add

(* dec $shr(int, int) : int *)

let rec shr' (v : Bigint.t) (o : Bigint.t) : Bigint.t =
  if Bigint.(o > zero) then
    let v_shifted = Bigint.(v / (one + one)) in
    shr' v_shifted Bigint.(o - one)
  else v

let shr (add : value -> unit) (at : region) (targs : targ list)
    (values_input : value list) : value =
  Extract.zero at targs;
  let value_base, value_offset = Extract.two at values_input in
  let base = bigint_of_value value_base in
  let offset = bigint_of_value value_offset in
  if Bigint.(offset > max_bit_width) then error at "shift amount too large";
  shr' base offset |> value_of_bigint add

(* dec $shr_arith(int, int, int) : int *)

let shr_arith' (v : Bigint.t) (o : Bigint.t) (m : Bigint.t) : Bigint.t =
  let rec shr_arith'' (v : Bigint.t) (o : Bigint.t) : Bigint.t =
    if Bigint.(o > zero) then
      let v_shifted = Bigint.((v / (one + one)) + m) in
      shr_arith'' v_shifted Bigint.(o - one)
    else v
  in
  shr_arith'' v o

let shr_arith (add : value -> unit) (at : region) (targs : targ list)
    (values_input : value list) : value =
  Extract.zero at targs;
  let value_base, value_offset, value_modulus = Extract.three at values_input in
  let base = bigint_of_value value_base in
  let offset = bigint_of_value value_offset in
  if Bigint.(offset > max_bit_width) then error at "shift amount too large";
  let modulus = bigint_of_value value_modulus in
  shr_arith' base offset modulus |> value_of_bigint add

(* dec $pow2(int) : int *)

let pow2' (w : Bigint.t) : Bigint.t = shl' Bigint.one w

let pow2 (add : value -> unit) (at : region) (targs : targ list)
    (values_input : value list) : value =
  Extract.zero at targs;
  let value_width = Extract.one at values_input in
  let width = bigint_of_value value_width in
  pow2' width |> value_of_bigint add

(* dec $bitstr_to_int(int, bitstr) : int *)

let rec bitstr_to_int' (w : Bigint.t) (n : Bigint.t) : Bigint.t =
  let two = Bigint.(one + one) in
  let w' = pow2' w in
  if Bigint.(n >= w' / two) then bitstr_to_int' w Bigint.(n - w')
  else if Bigint.(n < -(w' / two)) then bitstr_to_int' w Bigint.(n + w')
  else n

let bitstr_to_int (add : value -> unit) (at : region) (targs : targ list)
    (values_input : value list) : value =
  Extract.zero at targs;
  let value_width, value_bitstr = Extract.two at values_input in
  let width = bigint_of_value value_width in
  if Bigint.(width > max_bit_width) then error at "bitstr width too large";
  let bitstr = bigint_of_value value_bitstr in
  bitstr_to_int' width bitstr |> value_of_bigint add

(* dec $int_to_bitstr(int, int) : bitstr *)

let rec int_to_bitstr' (w : Bigint.t) (n : Bigint.t) : Bigint.t =
  let w' = pow2' w in
  if Bigint.(n >= w') then Bigint.(n % w')
  else if Bigint.(n < zero) then int_to_bitstr' w Bigint.(n + w')
  else n

let int_to_bitstr (add : value -> unit) (at : region) (targs : targ list)
    (values_input : value list) : value =
  Extract.zero at targs;
  let value_width, value_int = Extract.two at values_input in
  let width = bigint_of_value value_width in
  if Bigint.(width > max_bit_width) then error at "bitstr width too large";
  let rawint = bigint_of_value value_int in
  int_to_bitstr' width rawint |> value_of_bigint add

(* dec $bits_to_int_unsigned(bool* ) : int *)

let bits_to_int_unsigned' (bits : bool array) : Bigint.t =
  Array.fold_left
    (fun i bit -> Bigint.((i lsl 1) + if bit then one else zero))
    Bigint.zero bits

let bits_to_int_unsigned (add : value -> unit) (at : region) (targs : targ list)
    (values_input : value list) : value =
  Extract.zero at targs;
  let value_bits = Extract.one at values_input in
  let bits = bits_of_value value_bits in
  bits_to_int_unsigned' bits |> value_of_bigint add

(* dec $bits_to_int_signed(bool* ) : int *)

let bits_to_int_signed' (bits : bool array) : Bigint.t =
  if Array.length bits = 0 then error no_region "empty bit array";
  let sign = bits.(0) in
  let int_unsigned = bits_to_int_unsigned' bits in
  if sign then
    let int_max =
      let len = Array.length bits - 1 in
      Bigint.(one lsl len)
    in
    Bigint.(int_unsigned - (int_max * (one + one)))
  else int_unsigned

let bits_to_int_signed (add : value -> unit) (at : region) (targs : targ list)
    (values_input : value list) : value =
  Extract.zero at targs;
  let value_bits = Extract.one at values_input in
  let bits = bits_of_value value_bits in
  bits_to_int_signed' bits |> value_of_bigint add

(* dec $int_to_bits_unsigned(int) : bool* *)

let int_to_bits_unsigned' (value : Bigint.t) (width : int) : bool array =
  Array.init width (fun i -> Bigint.(value land (one lsl i) > zero))
  |> Array.to_list |> List.rev |> Array.of_list

let int_to_bits_unsigned (add : value -> unit) (at : region) (targs : targ list)
    (values_input : value list) : value =
  Extract.zero at targs;
  let value_width, value_int = Extract.two at values_input in
  let width = bigint_of_value value_width in
  if Bigint.(width > max_bit_width) then error at "bitstr width too large";
  let width = Bigint.to_int_exn width in
  let value = bigint_of_value value_int in
  int_to_bits_unsigned' value width |> value_of_bits add

(* dec $int_to_bits_signed(int) : bool* *)

let int_to_bits_signed (add : value -> unit) (at : region) (targs : targ list)
    (values_input : value list) : value =
  Extract.zero at targs;
  let value_width, value_int = Extract.two at values_input in
  let width = bigint_of_value value_width in
  if Bigint.(width > max_bit_width) then error at "bitstr width too large";
  let width = Bigint.to_int_exn width in
  let value = bigint_of_value value_int in
  let mask = Bigint.((one lsl width) - one) in
  let value = Bigint.(value land mask) in
  int_to_bits_unsigned' value width |> value_of_bits add

(* dec $bneg(int) : int *)

let bneg (add : value -> unit) (at : region) (targs : targ list)
    (values_input : value list) : value =
  Extract.zero at targs;
  let value = Extract.one at values_input in
  let rawint = bigint_of_value value in
  Bigint.bit_not rawint |> value_of_bigint add

(* dec $band(int, int) : int *)

let band (add : value -> unit) (at : region) (targs : targ list)
    (values_input : value list) : value =
  Extract.zero at targs;
  let value_l, value_r = Extract.two at values_input in
  let rawint_l = bigint_of_value value_l in
  let rawint_r = bigint_of_value value_r in
  Bigint.bit_and rawint_l rawint_r |> value_of_bigint add

(* dec $bxor(int, int) : int *)

let bxor (add : value -> unit) (at : region) (targs : targ list)
    (values_input : value list) : value =
  Extract.zero at targs;
  let value_l, value_r = Extract.two at values_input in
  let rawint_l = bigint_of_value value_l in
  let rawint_r = bigint_of_value value_r in
  Bigint.bit_xor rawint_l rawint_r |> value_of_bigint add

(* dec $bor(int, int) : int *)

let bor (add : value -> unit) (at : region) (targs : targ list)
    (values_input : value list) : value =
  Extract.zero at targs;
  let value_l, value_r = Extract.two at values_input in
  let rawint_l = bigint_of_value value_l in
  let rawint_r = bigint_of_value value_r in
  Bigint.bit_or rawint_l rawint_r |> value_of_bigint add

(* dec $bitacc(int, int, int) : int *)

let bitacc' (n : Bigint.t) (m : Bigint.t) (l : Bigint.t) : Bigint.t =
  let slice_width = Bigint.(m + one - l) in
  if Bigint.(l < zero) then
    raise (Invalid_argument "bitslice x[y:z] must have y > z > 0");
  let shifted = Bigint.(n asr to_int_exn l) in
  let mask = Bigint.(pow2' slice_width - one) in
  Bigint.bit_and shifted mask

let bitacc (add : value -> unit) (at : region) (targs : targ list)
    (values_input : value list) : value =
  Extract.zero at targs;
  let value_b, value_h, value_l = Extract.three at values_input in
  let rawint_b = bigint_of_value value_b in
  let rawint_h = bigint_of_value value_h in
  let rawint_l = bigint_of_value value_l in
  bitacc' rawint_b rawint_h rawint_l |> value_of_bigint add

(* dec $bitacc_replace(int, int, int, int) : int *)

let bitacc_replace' (b : Bigint.t) (m : Bigint.t) (l : Bigint.t) (r : Bigint.t)
    : Bigint.t =
  let r = Bigint.(r lsl to_int_exn l) in
  let mask_hi =
    let mask_hi = pow2' Bigint.(m + one) in
    Bigint.(mask_hi - one)
  in
  let mask_lo =
    let mask_lo = pow2' l in
    Bigint.(mask_lo - one)
  in
  let mask = Bigint.(lnot (mask_hi lxor mask_lo)) in
  Bigint.(b land mask lxor r)

let bitacc_replace (add : value -> unit) (at : region) (targs : targ list)
    (values_input : value list) : value =
  Extract.zero at targs;
  let value_b, value_h, value_l, value_rhs = Extract.four at values_input in
  let rawint_b = bigint_of_value value_b in
  let rawint_h = bigint_of_value value_h in
  let rawint_l = bigint_of_value value_l in
  let rawint_rhs = bigint_of_value value_rhs in
  bitacc_replace' rawint_b rawint_h rawint_l rawint_rhs |> value_of_bigint add
