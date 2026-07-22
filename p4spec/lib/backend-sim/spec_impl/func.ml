module Typ = Runtime.Type.Typ
module Value = Runtime.Value
module IO = Runtime.Sim.Io
open Error
open Util.Source

(* Helpers for invoking functions in the spec *)

module Make (V : Runtime.Valrep.SAFE) = struct
  type vt = V.t

  (* A trampoline for calling functions in the spec,
     which will be registered at initialization time. *)

  type call_func = string -> Typ.t list -> vt list -> vt

  let call : call_func ref = ref (fun _ _ _ -> assert false)
  let register f = call := f

  (* write_value_from_bits *)

  let write_value_from_bits (value_target : vt) (varsize : int)
      (bits : bool Array.t) : vt =
    let value_varsize = varsize |> Bigint.of_int |> V.Make.nat in
    let typ_bits = Typ.Make.var ("bit" $ no_region) [] |> Typ.Make.list in
    let value_bits =
      bits |> Array.to_list |> List.map V.Make.bool |> V.Make.list typ_bits
    in
    !call "write_value_from_bits" [] [ value_target; value_varsize; value_bits ]

  (* write_bits_from_value *)

  let write_bits_from_value (value_source : vt) : vt =
    !call "write_bits_from_value" [] [ value_source ]

  (* bitacc_range_op *)

  let bitacc_range_op (value_base : vt) (value_hi : vt) (value_lo : vt) : vt =
    !call "bitacc_range_op" [] [ value_base; value_hi; value_lo ]

  (* default *)

  let default (value_typ : vt) : vt = !call "default" [] [ value_typ ]

  (* cast_op *)

  let cast_op (value_typ : vt) (value_value : vt) : vt =
    !call "cast_op" [] [ value_typ; value_value ]

  (* sizeof_min/maxSizeInBits *)

  let sizeof_minSizeInBits' (value_typ : vt) : Bigint.t =
    !call "sizeof_minSizeInBits'" [] [ value_typ ] |> V.Get.num |> function
    | `Nat n -> n
    | `Int i -> i

  let sizeof_maxSizeInBits' (value_typ : vt) : Bigint.t =
    !call "sizeof_maxSizeInBits'" [] [ value_typ ] |> V.Get.num |> function
    | `Nat n -> n
    | `Int i -> i

  (* key_interface_of_tableObject *)

  let key_interface_of_tableObject (value_tableObject : vt) :
      (vt * vt * vt) list =
    !call "key_interface_of_tableObject" [] [ value_tableObject ]
    |> V.Get.list
    |> List.map (fun value ->
           match V.Get.tuple value with
           | [ value_a; value_b; value_c ] -> (value_a, value_b, value_c)
           | _ -> error no_region "expected a 3-tuple")

  (* tableObject_add_entry *)

  let tableObject_add_entry (value_ctx : vt) (value_tableObject : vt)
      (value_tableEntryPriorityInterface : vt) (value_tableKeysetInterface : vt)
      (value_tableActionInterface : vt) : vt option =
    !call "tableObject_add_entry" []
      [
        value_ctx;
        value_tableObject;
        value_tableEntryPriorityInterface;
        value_tableKeysetInterface;
        value_tableActionInterface;
      ]
    |> V.Get.opt

  (* tableObject_add_default_action *)

  let tableObject_add_default_action (value_ctx : vt) (value_tableObject : vt)
      (value_tableActionInterface : vt) : vt =
    !call "tableObject_add_default_action" []
      [ value_ctx; value_tableObject; value_tableActionInterface ]

  (* find/update_object_qualified_e/unqualified_e *)

  let find_object_qualified_e (value_arch : vt) (value_objectId : vt) :
      vt option =
    !call "find_object_qualified_e" [] [ value_arch; value_objectId ]
    |> V.Get.opt

  let find_object_unqualified_e (value_arch : vt) (value_id : vt) : vt option =
    !call "find_object_unqualified_e" [] [ value_arch; value_id ] |> V.Get.opt

  let update_object_qualified_e (value_arch : vt) (value_objectId : vt)
      (value_object : vt) : vt =
    !call "update_object_qualified_e" []
      [ value_arch; value_objectId; value_object ]

  let update_object_unqualified_e (value_arch : vt) (value_id : vt)
      (value_object : vt) : vt =
    !call "update_object_unqualified_e" []
      [ value_arch; value_id; value_object ]

  (* find/update_objectState_e *)

  let find_objectState_e (value_arch : vt) (value_objectId : vt) : vt =
    !call "find_objectState_e" [] [ value_arch; value_objectId ]
    |> V.Get.opt |> Option.get

  let update_objectState_e (value_arch : vt) (value_objectId : vt)
      (value_objectState : vt) : vt =
    !call "update_objectState_e" []
      [ value_arch; value_objectId; value_objectState ]
    |> V.Get.opt |> Option.get

  let find_archState_e (value_arch : vt) : vt =
    !call "find_archState_e" [] [ value_arch ]

  let update_archState_e (value_arch : vt) (value_archState : vt) : vt =
    !call "update_archState_e" [] [ value_arch; value_archState ]

  (* find_type_e *)

  let find_type_e (value_cursor : vt) (value_ctx : vt) (name : string) : vt =
    let value_nameIR = V.Make.text name in
    !call "find_type_e" [] [ value_cursor; value_ctx; value_nameIR ]

  let find_type_e_local (value_ctx : vt) (name : string) : vt =
    let value_cursor = V.Make.("LOCAL" <| [] <<| Typs.cursor) in
    find_type_e value_cursor value_ctx name |> V.Get.opt |> Option.get

  (* find_var_value_t *)

  let find_var_value_t (value_cursor : vt) (value_ctx : vt) (name : string) : vt
      =
    let value_prefixedNameIR =
      let value_nameIR = V.Make.text name in
      V.Make.("`` nameIR" <| [ value_nameIR ] <<| Typs.prefixed_name_ir)
    in
    !call "find_var_value_t" []
      [ value_prefixedNameIR; value_cursor; value_ctx ]

  let find_var_value_t_global (value_ctx : vt) (name : string) : vt =
    let value_cursor = V.Make.("GLOBAL" <| [] <<| Typs.cursor) in
    find_var_value_t value_cursor value_ctx name

  let find_var_value_t_local (value_ctx : vt) (name : string) : vt =
    let value_cursor = V.Make.("LOCAL" <| [] <<| Typs.cursor) in
    find_var_value_t value_cursor value_ctx name

  (* find_var_e *)

  let find_var_e (value_cursor : vt) (value_ctx : vt) (name : string) : vt =
    let value_prefixedNameIR =
      let value_nameIR = V.Make.text name in
      V.Make.("`` nameIR" <| [ value_nameIR ] <<| Typs.prefixed_name_ir)
    in
    !call "find_var_e" [] [ value_prefixedNameIR; value_cursor; value_ctx ]

  let find_var_e_global (value_ctx : vt) (name : string) : vt =
    let value_cursor = V.Make.("GLOBAL" <| [] <<| Typs.cursor) in
    find_var_e value_cursor value_ctx name

  let find_var_e_local (value_ctx : vt) (name : string) : vt =
    let value_cursor = V.Make.("LOCAL" <| [] <<| Typs.cursor) in
    find_var_e value_cursor value_ctx name

  (* subst_type_e *)

  let subst_type_e (value_cursor : vt) (value_ctx : vt) (value_typ : vt) : vt =
    !call "subst_type_e" [] [ value_cursor; value_ctx; value_typ ]

  let subst_type_e_local (value_ctx : vt) (value_typ : vt) : vt =
    let value_cursor = V.Make.("LOCAL" <| [] <<| Typs.cursor) in
    subst_type_e value_cursor value_ctx value_typ
end

module type S = sig
  type vt

  include module type of Make ((
    struct
      include Runtime.Valrep.V_value
    end :
      Runtime.Valrep.SAFE))
    with type vt := vt
end
