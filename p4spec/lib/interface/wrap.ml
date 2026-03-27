open Domain
open Lang
open Il
module Value = Runtime.Value
open Util.Source

(* Mixop generator *)

let mixop_of (s : string) : Mixop.t = Pass.Frontend.Parse.parse_mixop s

(* Type generators *)

let wrap_var_t (s : string) : typ' = VarT (s $ no_region, [])
let wrap_iter_t (i : iter) (t : typ') : typ' = IterT (t $ no_region, i)

(* Value note generators *)

let with_typ (typ : typ') (v : value') : value = Value.make typ v

(* Value generators *)

type symbol = NT of value | Term of string

type symb =
  | Arg of value
  | Atom of string
  | Brack of string * symb * string
  | Infix of symb * string * symb
  | Seq of symb list

let wrap_bool_v (b : bool) : value = BoolV b |> with_typ BoolT

let wrap_num_v_nat (n : Bigint.t) : value =
  NumV (`Nat n) |> with_typ (NumT `NatT)

let wrap_num_v_int (i : Bigint.t) : value =
  NumV (`Int i) |> with_typ (NumT `IntT)

let wrap_text_v (s : string) : value = TextV s |> with_typ TextT

let wrap_case_v (s : string) (v : valuecase) : value =
  CaseV v |> with_typ (wrap_var_t s)

let wrap_tuple_v (s : string) (vs : value list) : value =
  TupleV vs |> with_typ (wrap_var_t s)

let wrap_opt_v_typed (t : typ') (v : value option) : value =
  OptV v |> with_typ (wrap_iter_t Opt t)

let wrap_opt_v (s : string) (v : value option) : value =
  OptV v |> with_typ (wrap_iter_t Opt (wrap_var_t s))

let wrap_list_v (s : string) (vs : value list) : value =
  ListV vs |> with_typ (wrap_iter_t List (wrap_var_t s))

let wrap_list_v_typed (t : typ') (vs : value list) : value =
  ListV vs |> with_typ (wrap_iter_t List t)

let wrap_extern_v (s : string) (json : Yojson.Safe.t) : value =
  ExternV json |> with_typ (wrap_var_t s)

let ( #@ ) (valuecase : valuecase) (s : string) : value =
  wrap_case_v s valuecase

let ( #@@ ) (v : value) (s : string) : value =
  { v with note = { v.note with typ = wrap_var_t s } }
