(* K floats (FLOAT.Float), following the LLVM backend (llvm-backend f02284f).
   A K float is an MPFR number together with its precision and number of
   exponent bits. The spec keeps a float as text in the format the backend
   prints; this module reads and prints that text. *)

module M = Mlmpfr

type t = { prec : int; exp : int; value : M.mpfr_float }

exception Bad_float of string

(* Precision and exponent bits from the literal's suffix:
   f = (24, 8), none = (53, 11), pNxM = (N, M),
   as init_float2 in runtime/strings/strings.cpp *)
let format_of_literal (s : string) : int * int =
  let n = String.length s in
  if n > 0 && (s.[n - 1] = 'f' || s.[n - 1] = 'F') then (24, 8)
  else
    match (String.index_from_opt s 0 'p', String.index_from_opt s 0 'P') with
    | None, None -> (53, 11)
    | p, q ->
        let i =
          match (p, q) with
          | Some i, Some j -> min i j
          | Some i, None | None, Some i -> i
          | _ -> assert false
        in
        let j =
          match (String.index_opt s 'x', String.index_opt s 'X') with
          | Some a, Some b -> min a b
          | Some a, None | None, Some a -> a
          | None, None -> raise (Bad_float s)
        in
        ( int_of_string (String.sub s (i + 1) (j - i - 1)),
          int_of_string (String.sub s (j + 1) (n - j - 1)) )

(* The number part: everything before the last of "fFdDpP", unless the
   literal is exactly an infinity, as init_float2 *)
let number_of_literal (s : string) : string =
  if s = "+Infinity" || s = "-Infinity" || s = "Infinity" then s
  else
    let last = ref (-1) in
    String.iteri (fun i c -> if String.contains "fFdDpP" c then last := i) s;
    if !last < 0 then s else String.sub s 0 !last

(* Whether mpfr_set_str in base 10 accepts the whole string: a decimal number
   with an optional exponent, or an infinity or NaN. mlmpfr does not report
   invalid strings, while the backend fails on them. *)
let is_number (s : string) : bool =
  let n = String.length s in
  let digits i =
    let j = ref i in
    while !j < n && '0' <= s.[!j] && s.[!j] <= '9' do
      incr j
    done;
    !j
  in
  let i = if n > 0 && (s.[0] = '+' || s.[0] = '-') then 1 else 0 in
  match String.lowercase_ascii (String.sub s i (n - i)) with
  | "inf" | "infinity" | "nan" | "@inf@" | "@nan@" -> true
  | _ ->
      let j = digits i in
      let k = if j < n && s.[j] = '.' then digits (j + 1) else j in
      let mantissa = (k - i - if j < k then 1 else 0) > 0 in
      let l =
        if k < n && (s.[k] = 'e' || s.[k] = 'E') then
          let m =
            if k + 1 < n && (s.[k + 1] = '+' || s.[k + 1] = '-') then k + 2
            else k + 1
          in
          let e = digits m in
          if e > m then e else -1
        else k
      in
      mantissa && l = n

let of_literal (s : string) : t =
  let prec, exp = format_of_literal s in
  let number = number_of_literal s in
  if not (is_number number) then raise (Bad_float s);
  { prec; exp; value = M.make_from_str ~prec ~rnd:M.To_Nearest ~base:10 number }

(* As float_to_string in runtime/strings/numeric.cpp *)
let to_string (f : t) : string =
  let suffix =
    if f.prec = 53 && f.exp = 11 then ""
    else if f.prec = 24 && f.exp = 8 then "f"
    else Printf.sprintf "p%dx%d" f.prec f.exp
  in
  if M.nan_p f.value then "NaN" ^ suffix
  else if M.inf_p f.value then
    (if M.signbit f.value = M.Negative then "-Infinity" else "Infinity")
    ^ suffix
  else
    let digits, e = M.get_str ~rnd:M.To_Nearest ~base:10 ~size:0 f.value in
    let sign, digits =
      if String.length digits > 0 && digits.[0] = '-' then
        ("-", String.sub digits 1 (String.length digits - 1))
      else ("", digits)
    in
    sign ^ "0." ^ digits ^ "e" ^ e ^ suffix

(* A literal in the canonical form: equal floats get equal text *)
let normalize (s : string) : string = to_string (of_literal s)

(* Arithmetic, as runtime/arithmetic/float.cpp. An operation runs with MPFR's
   exponent range narrowed to the result's format (mpfr_enter), rounds to
   nearest, and then rounds into that range including subnormals before the
   range is restored (mpfr_leave). *)

let emin (e : int) : int = -(1 lsl (e - 1)) + 2
let emax (e : int) : int = (1 lsl (e - 1)) - 1

let in_format (prec : int) (exp : int) (op : unit -> M.mpfr_float) : t =
  let saved_min = M.get_emin () and saved_max = M.get_emax () in
  M.set_emin (emin exp - prec + 2);
  M.set_emax (emax exp + 1);
  Fun.protect
    ~finally:(fun () ->
      M.set_emin saved_min;
      M.set_emax saved_max)
    (fun () ->
      (* an exact result carries no ternary value; mpfr_leave(0, …) then passes 0 *)
      let x =
        match op () with v, None -> (v, Some M.Correct_Rounding) | x -> x
      in
      let x = M.check_range ~rnd:M.To_Nearest x in
      let x = M.subnormalize ~rnd:M.To_Nearest x in
      { prec; exp; value = x })

let rnd = M.To_Nearest

(* Unary and binary operations: the result has the format of the first argument *)
let unary (op : M.mpfr_float -> M.mpfr_float) (a : t) : t =
  in_format a.prec a.exp (fun () -> op a.value)

let binary (op : M.mpfr_float -> M.mpfr_float -> M.mpfr_float) (a : t) (b : t) :
    t =
  in_format a.prec a.exp (fun () -> op a.value b.value)

let add a b = binary (M.add ~rnd ~prec:a.prec) a b
let sub a b = binary (M.sub ~rnd ~prec:a.prec) a b
let mul a b = binary (M.mul ~rnd ~prec:a.prec) a b
let div a b = binary (M.div ~rnd ~prec:a.prec) a b
let min a b = binary (M.min ~rnd ~prec:a.prec) a b
let max a b = binary (M.max ~rnd ~prec:a.prec) a b
let neg a = unary (M.neg ~rnd ~prec:a.prec) a
let abs a = unary (M.abs ~rnd ~prec:a.prec) a
let ceil a = unary (M.ceil ~prec:a.prec) a
let floor a = unary (M.floor ~prec:a.prec) a
let trunc a = unary (M.trunc ~prec:a.prec) a

(* rootFloat(a, n): only square roots, with mpfr_sqrt, as mlmpfr has no
   mpfr_rootn_ui. The LLVM backend uses mpfr_rootn_ui, which gives +0.0 for
   -0.0 where mpfr_sqrt gives -0.0. *)
let root (a : t) (n : int) : t option =
  if n = 2 then Some (unary (M.sqrt ~rnd ~prec:a.prec) a) else None

(* roundFloat(a, prec, exp): mpfr_set into the given format *)
let round (a : t) (prec : int) (exp : int) : t =
  in_format prec exp (fun () -> M.make_from_mpfr ~prec ~rnd a.value)

(* Int2Float(i, prec, exp): mpfr_set_z into the given format. The integer is
   first made exactly, with as many bits as it has. *)
let of_int (i : Z.t) (prec : int) (exp : int) : t =
  let exact =
    M.make_from_str ~prec:(Stdlib.max 2 (Z.numbits i)) ~base:10 (Z.to_string i)
  in
  in_format prec exp (fun () -> M.make_from_mpfr ~prec ~rnd exact)

(* Float2Int(a): mpfr_get_z rounding to nearest; none when a is not finite.
   Rounding to an integer at a's own precision is exact, and the binary digits
   of the result give the integer. *)
let to_int (a : t) : Z.t option =
  if not (M.number_p a.value) then None
  else
    let r = M.rint ~rnd ~prec:a.prec a.value in
    if M.zero_p r then Some Z.zero
    else
      let digits, e = M.get_str ~rnd ~base:2 ~size:a.prec r in
      let neg = String.length digits > 0 && digits.[0] = '-' in
      let digits =
        if neg then String.sub digits 1 (String.length digits - 1) else digits
      in
      let m = Z.of_string_base 2 digits in
      let shift = int_of_string e - String.length digits in
      let v =
        if shift >= 0 then Z.shift_left m shift else Z.shift_right m (-shift)
      in
      Some (if neg then Z.neg v else v)

(* maxValueFloat(prec, exp): the largest finite number of the format *)
let max_value (prec : int) (exp : int) : t =
  in_format prec exp (fun () -> M.nextbelow (M.make_inf ~prec M.Positive))

let eq a b = M.equal_p a.value b.value
let lt a b = M.less_p a.value b.value
let le a b = M.lessequal_p a.value b.value
let gt a b = M.greater_p a.value b.value
let ge a b = M.greaterequal_p a.value b.value
let sign a = M.signbit a.value = M.Negative
let is_nan a = M.nan_p a.value
