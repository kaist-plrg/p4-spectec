open Interface.Wrap
open Interface.Unwrap
module IO = Runtime_simulator.Io
module Value = Runtime_dynamic.Value

(* Helpers for invoking functions in the spec *)

type call_func = string -> Sl.Ast.typ list -> Value.t list -> Value.t

let call : call_func ref = ref (fun _ _ -> assert false)
let register f = call := f

(* write_value_from_bits *)

let write_value_from_bits (value_target : Value.t) (varsize : int)
    (bits : bool Array.t) : Value.t =
  let value_varsize = varsize |> Bigint.of_int |> wrap_num_v_nat in
  let value_bits =
    bits |> Array.to_list |> List.map wrap_bool_v
    |> wrap_list_v_typed Il.Ast.BoolT
  in
  !call "write_value_from_bits" [] [ value_target; value_varsize; value_bits ]

(* write_bits_from_value *)

let write_bits_from_value (value_source : Value.t) : Value.t =
  !call "write_bits_from_value" [] [ value_source ]

(* bitacc_op *)

let bitacc_op (value_base : Value.t) (value_hi : Value.t) (value_lo : Value.t) :
    Value.t =
  !call "bitacc_op" [] [ value_base; value_hi; value_lo ]

(* default *)

let default (value_typ : Value.t) : Value.t = !call "default" [] [ value_typ ]

(* cast_op *)

let cast_op (value_typ : Value.t) (value_value : Value.t) : Value.t =
  !call "cast_op" [] [ value_typ; value_value ]

(* sizeof_min/maxSizeInBits *)

let sizeof_minSizeInBits' (value_typ : Value.t) : Bigint.t =
  !call "sizeof_minSizeInBits'" [] [ value_typ ] |> unwrap_num_v

let sizeof_maxSizeInBits' (value_typ : Value.t) : Bigint.t =
  !call "sizeof_maxSizeInBits'" [] [ value_typ ] |> unwrap_num_v

(* find/update_store_externState *)

let find_store_externState (value_sto : Value.t) (value_oid : Value.t) : Value.t
    =
  !call "find_store_externState" [] [ value_sto; value_oid ]
  |> unwrap_opt_v |> Option.get

let update_store_externState (value_sto : Value.t) (value_oid : Value.t)
    (value_externState : Value.t) : Value.t =
  !call "update_store_externState" []
    [ value_sto; value_oid; value_externState ]
  |> unwrap_opt_v |> Option.get

(* find_type_e *)

let find_type_e (value_cursor : Value.t) (value_ctx : Value.t) (name : string) :
    Value.t =
  let value_nameIR = wrap_text_v name in
  !call "find_type_e" [] [ value_cursor; value_ctx; value_nameIR ]

let find_type_e_local (value_ctx : Value.t) (name : string) : Value.t =
  let value_cursor = [ Term "LOCAL" ] #@ "cursor" in
  find_type_e value_cursor value_ctx name |> unwrap_opt_v |> Option.get

(* find_var_e *)

let find_var_e (value_cursor : Value.t) (value_ctx : Value.t) (name : string) :
    Value.t =
  let value_prefixedNameIR =
    let value_nameIR = wrap_text_v name in
    [ Term "`"; NT value_nameIR ] #@ "prefixedNameIR"
  in
  !call "find_var_e" [] [ value_cursor; value_ctx; value_prefixedNameIR ]

let find_var_e_global (value_ctx : Value.t) (name : string) : Value.t =
  let value_cursor = [ Term "LOCAL" ] #@ "cursor" in
  find_var_e value_cursor value_ctx name

let find_var_e_local (value_ctx : Value.t) (name : string) : Value.t =
  let value_cursor = [ Term "LOCAL" ] #@ "cursor" in
  find_var_e value_cursor value_ctx name

(* subst_type_e *)

let subst_type_e (value_cursor : Value.t) (value_ctx : Value.t)
    (value_typ : Value.t) : Value.t =
  !call "subst_type_e" [] [ value_cursor; value_ctx; value_typ ]

let subst_type_e_local (value_ctx : Value.t) (value_typ : Value.t) : Value.t =
  let value_cursor = [ Term "LOCAL" ] #@ "cursor" in
  subst_type_e value_cursor value_ctx value_typ
