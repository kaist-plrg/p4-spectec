(* Builtins for hooks the K spec delegates (spec-k/3.1-hook-builtin.watsup).
   Each follows the LLVM backend's runtime (llvm-backend f02284f,
   runtime/strings/strings.cpp, runtime/strings/bytes.cpp). Preconditions under
   which the backend reports an error are checked in the spec (3.4-hook.watsup),
   which fails instead of calling these. *)

module Value = Runtime.Value
module Typ = Runtime.Type.Typ
module Num = Lang.Xl.Num
module Extract = Builtin.Extract

type impl =
  (Value.t -> unit) ->
  Util.Source.region ->
  Typ.t list ->
  Value.t list ->
  Value.t

let get_int (v : Value.t) : Bigint.t =
  match Value.Get.num v with `Int i | `Nat i -> i

let ret add v =
  add v;
  v

let int add i = ret add (Value.Make.int i)
let text add s = ret add (Value.Make.text s)

(* dec $string_length(text) : int -- bytes, as hook_STRING_length *)
let string_length : impl =
 fun add at targs vs ->
  Extract.zero at targs;
  let s = Extract.one at vs |> Value.Get.text in
  int add (Bigint.of_int (String.length s))

(* dec $string_substr(text, int, int) : text -- bytes [start, end), as hook_BYTES_substr *)
let string_substr : impl =
 fun add at targs vs ->
  Extract.zero at targs;
  let s, a, b = Extract.three at vs in
  let s = Value.Get.text s
  and a = Bigint.to_int_exn (get_int a)
  and b = Bigint.to_int_exn (get_int b) in
  text add (String.sub s a (b - a))

(* the first index >= pos where test holds, or -1 when pos >= length *)
let search (s : string) (pos : int) (test : int -> bool) : int =
  let n = String.length s in
  let rec go i = if i >= n then -1 else if test i then i else go (i + 1) in
  if pos >= n then -1 else go pos

(* dec $string_find(text, text, int) : int -- as hook_STRING_find *)
let string_find : impl =
 fun add at targs vs ->
  Extract.zero at targs;
  let h, n, p = Extract.three at vs in
  let h = Value.Get.text h
  and n = Value.Get.text n
  and p = Bigint.to_int_exn (get_int p) in
  let ln = String.length n and lh = String.length h in
  int add
    (Bigint.of_int
       (search h p (fun i -> i + ln <= lh && String.sub h i ln = n)))

(* dec $string_find_char(text, text, int) : int -- as hook_STRING_findChar *)
let string_find_char : impl =
 fun add at targs vs ->
  Extract.zero at targs;
  let h, cs, p = Extract.three at vs in
  let h = Value.Get.text h
  and cs = Value.Get.text cs
  and p = Bigint.to_int_exn (get_int p) in
  int add (Bigint.of_int (search h p (fun i -> String.contains cs h.[i])))

(* dec $string_chr(int) : text -- one byte, as hook_STRING_chr *)
let string_chr : impl =
 fun add at targs vs ->
  Extract.zero at targs;
  let i = Extract.one at vs |> get_int |> Bigint.to_int_exn in
  text add (String.make 1 (Char.chr i))

(* dec $int_to_string(int) : text -- decimal, as hook_STRING_int2string *)
let int_to_string : impl =
 fun add at targs vs ->
  Extract.zero at targs;
  text add (Bigint.to_string (Extract.one at vs |> get_int))

(* Reading an integer as GMP's mpz_set_str does for bases 2 to 36: white space
   is ignored, a leading '-' negates, and digits are case-insensitive. A
   leading '+' is dropped first, as hook_STRING_string2base_long does. None
   when GMP rejects the string. *)
let parse_base (s : string) (base : int) : Bigint.t option =
  let s =
    if String.length s > 0 && s.[0] = '+' then
      String.sub s 1 (String.length s - 1)
    else s
  in
  let s =
    String.concat ""
      (List.filter (fun t -> t <> "") (String.split_on_char ' ' s))
    |> String.to_seq
    |> Seq.filter (fun c -> not (String.contains "\t\n\011\012\r" c))
    |> String.of_seq
  in
  let neg, body =
    if String.length s > 0 && s.[0] = '-' then
      (true, String.sub s 1 (String.length s - 1))
    else (false, s)
  in
  let digit c =
    match c with
    | '0' .. '9' -> Char.code c - Char.code '0'
    | 'a' .. 'z' -> Char.code c - Char.code 'a' + 10
    | 'A' .. 'Z' -> Char.code c - Char.code 'A' + 10
    | _ -> 99
  in
  if
    base < 2 || base > 36 || body = ""
    || not (String.for_all (fun c -> digit c < base) body)
  then None
  else
    let b = Bigint.of_int base in
    let v =
      String.fold_left
        (fun acc c -> Bigint.((acc * b) + of_int (digit c)))
        Bigint.zero body
    in
    Some (if neg then Bigint.neg v else v)

let opt_int add (v : Bigint.t option) =
  let v = Option.map Value.Make.int v in
  Option.iter add v;
  ret add (Value.Make.opt (Typ.Make.opt Typ.Make.int) v)

(* dec $string_to_int(text) : int? -- as hook_STRING_string2int, in base 10 *)
let string_to_int : impl =
 fun add at targs vs ->
  Extract.zero at targs;
  opt_int add (parse_base (Extract.one at vs |> Value.Get.text) 10)

(* dec $string_to_base(text, int) : int? -- as hook_STRING_string2base *)
let string_to_base : impl =
 fun add at targs vs ->
  Extract.zero at targs;
  let s, b = Extract.two at vs in
  let b = get_int b in
  let base =
    if Bigint.(b >= of_int 2 && b <= of_int 36) then Bigint.to_int_exn b else 0
  in
  opt_int add (parse_base (Value.Get.text s) base)

(* Positions where needle occurs in s, searching again one byte after each
   match (so matches may overlap), at most limit of them, as hook_STRING_replace
   and hook_STRING_countAllOccurrences *)
let occurrences (s : string) (needle : string) (limit : int) : int list =
  let n = String.length s and k = String.length needle in
  let matches_at p = p + k <= n && String.sub s p k = needle in
  let rec go p acc count =
    if count >= limit || p > n then List.rev acc
    else
      let rec next q =
        if q >= n then None else if matches_at q then Some q else next (q + 1)
      in
      match next p with
      | None -> List.rev acc
      | Some q -> go (q + 1) (q :: acc) (count + 1)
  in
  go 0 [] 0

(* dec $string_replace(text, text, text, nat) : text -- as hook_STRING_replace:
   the first count occurrences of needle are replaced *)
let string_replace : impl =
 fun add at targs vs ->
  Extract.zero at targs;
  match vs with
  | [ s; needle; replacer; count ] ->
      let s = Value.Get.text s
      and needle = Value.Get.text needle
      and replacer = Value.Get.text replacer in
      let ms = occurrences s needle (Bigint.to_int_exn (get_int count)) in
      let b = Buffer.create (String.length s) in
      let h =
        List.fold_left
          (fun h m ->
            Buffer.add_string b (String.sub s h (m - h));
            Buffer.add_string b replacer;
            m + String.length needle)
          0 ms
      in
      Buffer.add_string b (String.sub s h (String.length s - h));
      text add (Buffer.contents b)
  | _ -> Builtin.Error.error at "string_replace: expected four arguments"

(* dec $string_count(text, text) : nat -- as hook_STRING_countAllOccurrences *)
let string_count : impl =
 fun add at targs vs ->
  Extract.zero at targs;
  let s, needle = Extract.two at vs in
  let s = Value.Get.text s in
  ret add
    (Value.Make.nat
       (Bigint.of_int
          (List.length
             (occurrences s (Value.Get.text needle) (String.length s + 1)))))

(* Integers: GMP semantics through Zarith, as runtime/arithmetic/int.cpp *)

let z (v : Value.t) : Z.t = Bigint.to_zarith_bigint (get_int v)
let zint add (i : Z.t) = int add (Bigint.of_zarith_bigint i)

let int_binop (f : Z.t -> Z.t -> Z.t) : impl =
 fun add at targs vs ->
  Extract.zero at targs;
  let a, b = Extract.two at vs in
  zint add (f (z a) (z b))

(* dec $int_and(int, int) : int, $int_or, $int_xor -- mpz_and, mpz_ior, mpz_xor *)
let int_and = int_binop Z.logand
let int_or = int_binop Z.logor
let int_xor = int_binop Z.logxor

(* dec $int_not(int) : int -- mpz_com *)
let int_not : impl =
 fun add at targs vs ->
  Extract.zero at targs;
  zint add (Z.lognot (z (Extract.one at vs)))

(* dec $int_shl(int, nat) : int -- mpz_mul_2exp *)
let int_shl = int_binop (fun a b -> Z.shift_left a (Z.to_int b))

(* dec $int_shr(int, nat) : int -- mpz_fdiv_q_2exp, rounding down; a shift
   by at least the number of bits of a gives 0 or -1, as hook_INT_shr does
   also for amounts that do not fit in an unsigned long *)
let int_shr =
  int_binop (fun a b ->
      if Z.geq b (Z.of_int (Z.numbits a)) then
        if Z.sign a < 0 then Z.minus_one else Z.zero
      else Z.shift_right a (Z.to_int b))

(* dec $int_log2(int) : int -- mpz_sizeinbase(a, 2) - 1, for a > 0 *)
let int_log2 : impl =
 fun add at targs vs ->
  Extract.zero at targs;
  zint add (Z.of_int (Z.numbits (z (Extract.one at vs)) - 1))

(* Bytes are lists of nats *)

let bytes_of_value (v : Value.t) : string =
  Value.Get.list v
  |> List.map (fun b -> Char.chr (Bigint.to_int_exn (get_int b)))
  |> List.to_seq |> String.of_seq

let value_of_bytes add (s : string) : Value.t =
  let bytes =
    List.init (String.length s) (fun i ->
        Value.Make.nat (Bigint.of_int (Char.code s.[i])))
  in
  List.iter add bytes;
  ret add (Value.Make.list (Typ.Make.list Typ.Make.nat) bytes)

(* dec $int_to_bytes(nat, int, bool) : nat* -- as hook_BYTES_int2bytes: the
   len lowest bytes of the two's complement of i, big-endian if the flag *)
let int_to_bytes : impl =
 fun add at targs vs ->
  Extract.zero at targs;
  let len, i, big = Extract.three at vs in
  let len = Bigint.to_int_exn (get_int len) in
  let x = Z.extract (z i) 0 (8 * len) in
  let little =
    String.init len (fun k -> Char.chr (Z.to_int (Z.extract x (8 * k) 8)))
  in
  let s =
    if Value.Get.bool big then String.init len (fun k -> little.[len - 1 - k])
    else little
  in
  value_of_bytes add s

(* dec $bytes_to_int(nat*, bool, bool) : int -- as hook_BYTES_bytes2int: big-endian
   if the first flag, two's complement if the second *)
let bytes_to_int : impl =
 fun add at targs vs ->
  Extract.zero at targs;
  let b, big, signed = Extract.three at vs in
  let s = bytes_of_value b in
  let n = String.length s in
  let byte k =
    Char.code (if Value.Get.bool big then s.[k] else s.[n - 1 - k])
  in
  let x = ref Z.zero in
  for k = 0 to n - 1 do
    x := Z.(add (shift_left !x 8) (of_int (byte k)))
  done;
  let x =
    if Value.Get.bool signed && n > 0 && byte 0 >= 0x80 then
      Z.sub !x (Z.shift_left Z.one (8 * n))
    else !x
  in
  zint add x

(* dec $string_to_bytes(text) : nat*, $bytes_to_string(nat* ) : text -- the
   same bytes, as hook_BYTES_string2bytes and hook_BYTES_bytes2string *)
let string_to_bytes : impl =
 fun add at targs vs ->
  Extract.zero at targs;
  value_of_bytes add (Extract.one at vs |> Value.Get.text)

let bytes_to_string : impl =
 fun add at targs vs ->
  Extract.zero at targs;
  text add (bytes_of_value (Extract.one at vs))

(* Floats are text in the backend's output format; operations are in
   floats.ml *)

let float_of (v : Value.t) : Floats.t = Floats.of_literal (Value.Get.text v)
let float add (f : Floats.t) = text add (Floats.to_string f)
let bool add b = ret add (Value.Make.bool b)
let small (v : Value.t) : int = Bigint.to_int_exn (get_int v)

let float_unary (op : Floats.t -> Floats.t) : impl =
 fun add at targs vs ->
  Extract.zero at targs;
  float add (op (float_of (Extract.one at vs)))

let float_binary (op : Floats.t -> Floats.t -> Floats.t) : impl =
 fun add at targs vs ->
  Extract.zero at targs;
  let a, b = Extract.two at vs in
  float add (op (float_of a) (float_of b))

let float_test (op : Floats.t -> bool) : impl =
 fun add at targs vs ->
  Extract.zero at targs;
  bool add (op (float_of (Extract.one at vs)))

let float_compare (op : Floats.t -> Floats.t -> bool) : impl =
 fun add at targs vs ->
  Extract.zero at targs;
  let a, b = Extract.two at vs in
  bool add (op (float_of a) (float_of b))

(* dec $float_root(text, nat) : text? *)
let float_root : impl =
 fun add at targs vs ->
  Extract.zero at targs;
  let a, n = Extract.two at vs in
  let v =
    Option.map
      (fun f -> Value.Make.text (Floats.to_string f))
      (Floats.root (float_of a) (small n))
  in
  Option.iter add v;
  ret add (Value.Make.opt (Typ.Make.opt Typ.Make.text) v)

(* dec $float_round(text, nat, nat) : text *)
let float_round : impl =
 fun add at targs vs ->
  Extract.zero at targs;
  let a, p, e = Extract.three at vs in
  float add (Floats.round (float_of a) (small p) (small e))

(* dec $int_to_float(int, nat, nat) : text *)
let int_to_float : impl =
 fun add at targs vs ->
  Extract.zero at targs;
  let i, p, e = Extract.three at vs in
  float add (Floats.of_int (z i) (small p) (small e))

(* dec $float_to_int(text) : int? *)
let float_to_int : impl =
 fun add at targs vs ->
  Extract.zero at targs;
  opt_int add
    (Option.map Bigint.of_zarith_bigint
       (Floats.to_int (float_of (Extract.one at vs))))

(* dec $float_max_value(nat, nat) : text *)
let float_max_value : impl =
 fun add at targs vs ->
  Extract.zero at targs;
  let p, e = Extract.two at vs in
  float add (Floats.max_value (small p) (small e))

(* dec $float_precision(text) : nat, $float_exponent_bits(text) : nat *)
let float_precision : impl =
 fun add at targs vs ->
  Extract.zero at targs;
  ret add (Value.Make.nat (Bigint.of_int (float_of (Extract.one at vs)).prec))

let float_exponent_bits : impl =
 fun add at targs vs ->
  Extract.zero at targs;
  ret add (Value.Make.nat (Bigint.of_int (float_of (Extract.one at vs)).exp))

(* dec $int_bit_range(int, nat, nat) : int -- as hook_INT_bitRange: len bits
   of i from bit off, in two's complement *)
let int_bit_range : impl =
 fun add at targs vs ->
  Extract.zero at targs;
  let i, off, len = Extract.three at vs in
  let len = Z.to_int (z len) in
  zint add (if len = 0 then Z.zero else Z.extract (z i) (Z.to_int (z off)) len)

(* dec $int_powmod(int, int, int) : int? -- as hook_INT_powmod (mpz_powm): a
   negative exponent needs the inverse of the base; none for modulus 0, where
   GMP fails *)
let int_powmod : impl =
 fun add at targs vs ->
  Extract.zero at targs;
  let a, b, m = Extract.three at vs in
  let a = z a and b = z b and m = z m in
  opt_int add
    (if Z.sign m = 0 || (Z.sign b < 0 && not (Z.equal (Z.gcd a m) Z.one)) then
       None
     else Some (Bigint.of_zarith_bigint (Z.powm a b m)))

(* dec $string_compare(text, text) : int -- byte by byte, then by length, as
   hook_STRING_lt (memcmp) *)
let string_compare : impl =
 fun add at targs vs ->
  Extract.zero at targs;
  let a, b = Extract.two at vs in
  zint add
    (Z.of_int
       (compare (String.compare (Value.Get.text a) (Value.Get.text b)) 0))

(* KRYPTO digests, as plugin-c/crypto.cpp (Crypto++) of the blockchain plugin:
   dec $keccak256(nat* ) : nat*, $sha256, $ripemd160 *)
let digest (f : string -> string) : impl =
 fun add at targs vs ->
  Extract.zero at targs;
  value_of_bytes add (f (bytes_of_value (Extract.one at vs)))

let keccak256 =
  digest (fun s -> Digestif.KECCAK_256.(to_raw_string (digest_string s)))

let sha256 = digest (fun s -> Digestif.SHA256.(to_raw_string (digest_string s)))

let ripemd160 =
  digest (fun s -> Digestif.RMD160.(to_raw_string (digest_string s)))

external ecdsa_recover_c : string -> int -> string -> string -> string
  = "kore_ecdsa_recover"

(* dec $ecdsa_recover(nat*, int, nat*, nat* ) : nat* -- as hook_KRYPTO_ecdsaRecover:
   the public key of the signature (v, r, s) of a 32-byte hash, or no bytes *)
let ecdsa_recover : impl =
 fun add at targs vs ->
  Extract.zero at targs;
  match vs with
  | [ hash; v; r; s ] ->
      let hash = bytes_of_value hash
      and r = bytes_of_value r
      and s = bytes_of_value s
      and v = z v in
      value_of_bytes add
        (if
           String.length hash <> 32
           || String.length r <> 32
           || String.length s <> 32
           || Z.lt v (Z.of_int 27)
           || Z.gt v (Z.of_int 28)
         then ""
         else ecdsa_recover_c hash (Z.to_int v - 27) r s)
  | _ -> Builtin.Error.error at "ecdsa_recover: expected four arguments"

let entries : (string * impl) list =
  [
    ("int_bit_range", int_bit_range);
    ("int_powmod", int_powmod);
    ("string_compare", string_compare);
    ("keccak256", keccak256);
    ("sha256", sha256);
    ("ripemd160", ripemd160);
    ("ecdsa_recover", ecdsa_recover);
    ("string_length", string_length);
    ("string_substr", string_substr);
    ("string_find", string_find);
    ("string_find_char", string_find_char);
    ("string_chr", string_chr);
    ("int_to_string", int_to_string);
    ("string_to_int", string_to_int);
    ("string_to_base", string_to_base);
    ("string_replace", string_replace);
    ("string_count", string_count);
    ("int_and", int_and);
    ("int_or", int_or);
    ("int_xor", int_xor);
    ("int_not", int_not);
    ("int_shl", int_shl);
    ("int_shr", int_shr);
    ("int_log2", int_log2);
    ("int_to_bytes", int_to_bytes);
    ("bytes_to_int", bytes_to_int);
    ("string_to_bytes", string_to_bytes);
    ("bytes_to_string", bytes_to_string);
    ("float_add", float_binary Floats.add);
    ("float_sub", float_binary Floats.sub);
    ("float_mul", float_binary Floats.mul);
    ("float_div", float_binary Floats.div);
    ("float_min", float_binary Floats.min);
    ("float_max", float_binary Floats.max);
    ("float_neg", float_unary Floats.neg);
    ("float_abs", float_unary Floats.abs);
    ("float_ceil", float_unary Floats.ceil);
    ("float_floor", float_unary Floats.floor);
    ("float_trunc", float_unary Floats.trunc);
    ("float_root", float_root);
    ("float_round", float_round);
    ("int_to_float", int_to_float);
    ("float_to_int", float_to_int);
    ("float_max_value", float_max_value);
    ("float_precision", float_precision);
    ("float_exponent_bits", float_exponent_bits);
    ("float_sign", float_test Floats.sign);
    ("float_is_nan", float_test Floats.is_nan);
    ("float_eq", float_compare Floats.eq);
    ("float_lt", float_compare Floats.lt);
    ("float_le", float_compare Floats.le);
    ("float_gt", float_compare Floats.gt);
    ("float_ge", float_compare Floats.ge);
  ]
