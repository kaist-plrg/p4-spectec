open Lang
module Typ = Runtime.Type.Typ
module Value = Runtime.Value
module IO = Runtime.Sim.Io
open Error
open Util.Source

(* Helpers for invoking functions in the spec *)

type call_func = string -> Sl.typ list -> Value.t list -> Value.t

let call : call_func ref = Runner.Spec.Func.call

(* write_value_from_bits *)

let write_value_from_bits (value_target : Value.t) (varsize : int)
    (bits : bool Array.t) : Value.t =
  let value_varsize = varsize |> Bigint.of_int |> Value.Make.nat in
  let typ_bits = Typ.Make.var ("bit" $ no_region) [] |> Typ.Make.list in
  let value_bits =
    bits |> Array.to_list |> List.map Value.Make.bool
    |> Value.Make.list typ_bits
  in
  !call "write_value_from_bits" [] [ value_target; value_varsize; value_bits ]

(* write_bits_from_value *)

let write_bits_from_value (value_source : Value.t) : Value.t =
  !call "write_bits_from_value" [] [ value_source ]

(* bitacc_range_op *)

let bitacc_range_op (value_base : Value.t) (value_hi : Value.t)
    (value_lo : Value.t) : Value.t =
  !call "bitacc_range_op" [] [ value_base; value_hi; value_lo ]

(* default *)

let default (value_typ : Value.t) : Value.t = !call "default" [] [ value_typ ]

(* cast_op *)

let cast_op (value_typ : Value.t) (value_value : Value.t) : Value.t =
  !call "cast_op" [] [ value_typ; value_value ]

(* sizeof_min/maxSizeInBits *)

let sizeof_minSizeInBits' (value_typ : Value.t) : Bigint.t =
  !call "sizeof_minSizeInBits'" [] [ value_typ ] |> Value.Get.num |> function
  | `Nat n -> n
  | `Int i -> i

let sizeof_maxSizeInBits' (value_typ : Value.t) : Bigint.t =
  !call "sizeof_maxSizeInBits'" [] [ value_typ ] |> Value.Get.num |> function
  | `Nat n -> n
  | `Int i -> i

(* key_interface_of_tableObject *)

let key_interface_of_tableObject (value_tableObject : Value.t) :
    (Value.t * Value.t * Value.t) list =
  !call "key_interface_of_tableObject" [] [ value_tableObject ]
  |> Value.Get.list
  |> List.map (fun value ->
         match Value.Get.tuple value with
         | [ value_a; value_b; value_c ] -> (value_a, value_b, value_c)
         | _ -> error no_region "expected a 3-tuple")

(* tableObject_add_entry *)

let tableObject_add_entry (value_ctx : Value.t) (value_tableObject : Value.t)
    (value_tableEntryPriorityInterface : Value.t)
    (value_tableKeysetInterface : Value.t)
    (value_tableActionInterface : Value.t) : Value.t option =
  !call "tableObject_add_entry" []
    [
      value_ctx;
      value_tableObject;
      value_tableEntryPriorityInterface;
      value_tableKeysetInterface;
      value_tableActionInterface;
    ]
  |> Value.Get.opt

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
  |> Value.Get.opt

let find_object_unqualified_e (value_arch : Value.t) (value_id : Value.t) :
    Value.t option =
  !call "find_object_unqualified_e" [] [ value_arch; value_id ] |> Value.Get.opt

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
  |> Value.Get.opt |> Option.get

let update_objectState_e (value_arch : Value.t) (value_objectId : Value.t)
    (value_objectState : Value.t) : Value.t =
  !call "update_objectState_e" []
    [ value_arch; value_objectId; value_objectState ]
  |> Value.Get.opt |> Option.get

let find_archState_e (value_arch : Value.t) : Value.t =
  !call "find_archState_e" [] [ value_arch ]

let update_archState_e (value_arch : Value.t) (value_archState : Value.t) :
    Value.t =
  !call "update_archState_e" [] [ value_arch; value_archState ]

(* find_type_e *)

let find_type_e (value_cursor : Value.t) (value_ctx : Value.t) (name : string) :
    Value.t =
  let value_nameIR = Value.Make.text name in
  !call "find_type_e" [] [ value_cursor; value_ctx; value_nameIR ]

let find_type_e_local (value_ctx : Value.t) (name : string) : Value.t =
  let value_cursor = Value.Make.("LOCAL" <| [] <<| "cursor") in
  find_type_e value_cursor value_ctx name |> Value.Get.opt |> Option.get

(* find_var_value_t *)

let find_var_value_t (value_cursor : Value.t) (value_ctx : Value.t)
    (name : string) : Value.t =
  let value_prefixedNameIR =
    let value_nameIR = Value.Make.text name in
    Value.Make.("`` nameIR" <| [ value_nameIR ] <<| "prefixedNameIR")
  in
  !call "find_var_value_t" [] [ value_prefixedNameIR; value_cursor; value_ctx ]

let find_var_value_t_global (value_ctx : Value.t) (name : string) : Value.t =
  let value_cursor = Value.Make.("GLOBAL" <| [] <<| "cursor") in
  find_var_value_t value_cursor value_ctx name

let find_var_value_t_local (value_ctx : Value.t) (name : string) : Value.t =
  let value_cursor = Value.Make.("LOCAL" <| [] <<| "cursor") in
  find_var_value_t value_cursor value_ctx name

(* find_var_e *)

let find_var_e (value_cursor : Value.t) (value_ctx : Value.t) (name : string) :
    Value.t =
  let value_prefixedNameIR =
    let value_nameIR = Value.Make.text name in
    Value.Make.("`` nameIR" <| [ value_nameIR ] <<| "prefixedNameIR")
  in
  !call "find_var_e" [] [ value_prefixedNameIR; value_cursor; value_ctx ]

let find_var_e_global (value_ctx : Value.t) (name : string) : Value.t =
  let value_cursor = Value.Make.("GLOBAL" <| [] <<| "cursor") in
  find_var_e value_cursor value_ctx name

let find_var_e_local (value_ctx : Value.t) (name : string) : Value.t =
  let value_cursor = Value.Make.("LOCAL" <| [] <<| "cursor") in
  find_var_e value_cursor value_ctx name

(* subst_type_e *)

let subst_type_e (value_cursor : Value.t) (value_ctx : Value.t)
    (value_typ : Value.t) : Value.t =
  !call "subst_type_e" [] [ value_cursor; value_ctx; value_typ ]

let subst_type_e_local (value_ctx : Value.t) (value_typ : Value.t) : Value.t =
  let value_cursor = Value.Make.("LOCAL" <| [] <<| "cursor") in
  subst_type_e value_cursor value_ctx value_typ
