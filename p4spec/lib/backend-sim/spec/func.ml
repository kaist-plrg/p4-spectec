open Lang
open Interface.Wrap
open Interface.Unwrap
module Value = Runtime.Sim.Value
module IO = Runtime.Sim.Io

(* Helpers for invoking functions in the spec *)

type call_func = string -> Sl.typ list -> Value.t list -> Value.t

let call : call_func ref = ref (fun _ _ -> assert false)
let register f = call := f

(* write_value_from_bits *)

let write_value_from_bits (value_target : Value.t) (varsize : int)
    (bits : bool Array.t) : Value.t =
  let value_varsize = varsize |> Bigint.of_int |> wrap_num_v_nat in
  let value_bits =
    bits |> Array.to_list |> List.map wrap_bool_v |> wrap_list_v_typed Il.BoolT
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

(* tableObject_add_entry *)

let tableObject_add_entry (value_ctx : Value.t) (value_tableObject : Value.t)
    (value_tableEntryPriorityInterface : Value.t)
    (value_tableKeysetInterface : Value.t)
    (value_tableActionInterface : Value.t) : Value.t =
  !call "tableObject_add_entry" []
    [
      value_ctx;
      value_tableObject;
      value_tableEntryPriorityInterface;
      value_tableKeysetInterface;
      value_tableActionInterface;
    ]

(* tableObject_add_default_action *)

let tableObject_add_default_action (value_ctx : Value.t)
    (value_tableObject : Value.t) (value_tableActionInterface : Value.t) :
    Value.t =
  !call "tableObject_add_default_action" []
    [ value_ctx; value_tableObject; value_tableActionInterface ]

(* find/update_object_qualified_e/unqualified_e *)

let find_object_qualified_e (value_arch : Value.t) (value_objectId : Value.t) :
    Value.t option =
  !call "find_object_qualified_e" [] [ value_arch; value_objectId ]
  |> unwrap_opt_v

let find_object_unqualified_e (value_arch : Value.t) (value_id : Value.t) :
    Value.t option =
  !call "find_object_unqualified_e" [] [ value_arch; value_id ] |> unwrap_opt_v

let update_object_qualified_e (value_arch : Value.t) (value_objectId : Value.t)
    (value_object : Value.t) : Value.t =
  !call "update_object_qualified_e" []
    [ value_arch; value_objectId; value_object ]

let update_object_unqualified_e (value_arch : Value.t) (value_id : Value.t)
    (value_object : Value.t) : Value.t =
  !call "update_object_unqualified_e" [] [ value_arch; value_id; value_object ]

(* find/update_objectState_e *)

let find_objectState_e (value_arch : Value.t) (value_objectId : Value.t) :
    Value.t =
  !call "find_objectState_e" [] [ value_arch; value_objectId ]
  |> unwrap_opt_v |> Option.get

let update_objectState_e (value_arch : Value.t) (value_objectId : Value.t)
    (value_objectState : Value.t) : Value.t =
  !call "update_objectState_e" []
    [ value_arch; value_objectId; value_objectState ]
  |> unwrap_opt_v |> Option.get

let find_archState_e (value_arch : Value.t) : Value.t =
  !call "find_archState_e" [] [ value_arch ]

let update_archState_e (value_arch : Value.t) (value_archState : Value.t) :
    Value.t =
  !call "update_archState_e" [] [ value_arch; value_archState ]

(* find_type_e *)

let find_type_e (value_cursor : Value.t) (value_ctx : Value.t) (name : string) :
    Value.t =
  let value_nameIR = wrap_text_v name in
  !call "find_type_e" [] [ value_cursor; value_ctx; value_nameIR ]

let find_type_e_local (value_ctx : Value.t) (name : string) : Value.t =
  let value_cursor = [ Term "LOCAL" ] #@ "cursor" in
  find_type_e value_cursor value_ctx name |> unwrap_opt_v |> Option.get

(* find_var_value_t *)

let find_var_value_t (value_cursor : Value.t) (value_ctx : Value.t)
    (name : string) : Value.t =
  let value_prefixedNameIR =
    let value_nameIR = wrap_text_v name in
    [ Term "`"; NT value_nameIR ] #@ "prefixedNameIR"
  in
  !call "find_var_value_t" [] [ value_prefixedNameIR; value_cursor; value_ctx ]

let find_var_value_t_global (value_ctx : Value.t) (name : string) : Value.t =
  let value_cursor = [ Term "LOCAL" ] #@ "cursor" in
  find_var_value_t value_cursor value_ctx name

let find_var_value_t_local (value_ctx : Value.t) (name : string) : Value.t =
  let value_cursor = [ Term "LOCAL" ] #@ "cursor" in
  find_var_value_t value_cursor value_ctx name

(* find_var_e *)

let find_var_e (value_cursor : Value.t) (value_ctx : Value.t) (name : string) :
    Value.t =
  let value_prefixedNameIR =
    let value_nameIR = wrap_text_v name in
    [ Term "`"; NT value_nameIR ] #@ "prefixedNameIR"
  in
  !call "find_var_e" [] [ value_prefixedNameIR; value_cursor; value_ctx ]

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
