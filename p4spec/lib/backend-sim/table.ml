module Typ = Runtime.Type.Typ
module Value = Runtime.Value
open Util.Source

(* Match-action table interface *)

let find_table (value_arch : Value.t) (value_tableName : Value.t) : Value.t =
  let find_table_unqualified table_name_unqualified =
    let value_tableName_unqualified = Value.Make.text table_name_unqualified in
    Spec.Func.find_object_unqualified_e value_arch value_tableName_unqualified
    |> Option.get
  in
  let table_name = Value.Get.text value_tableName in
  match String.split_on_char '.' table_name with
  | [] -> assert false
  | [ table_name_unqualified ] -> find_table_unqualified table_name_unqualified
  | names -> (
      let typ_objectId =
        Typ.Make.var ("nameIR" $ no_region) [] |> Typ.Make.list
      in
      let values_name = List.map Value.Make.text names in
      let value_objectId = Value.Make.list typ_objectId values_name in
      match Spec.Func.find_object_qualified_e value_arch value_objectId with
      | Some value_table -> value_table
      | None ->
          let table_name_unqualified = names |> List.rev |> List.hd in
          find_table_unqualified table_name_unqualified)

let update_table (value_arch : Value.t) (value_tableName : Value.t)
    (value_tableObject : Value.t) : Value.t =
  let update_table_unqualified table_name_unqualified =
    let value_tableName_unqualified = Value.Make.text table_name_unqualified in
    Spec.Func.update_object_unqualified_e value_arch value_tableName_unqualified
      value_tableObject
  in
  let table_name = Value.Get.text value_tableName in
  match String.split_on_char '.' table_name with
  | [] -> assert false
  | [ table_name_unqualified ] ->
      update_table_unqualified table_name_unqualified
  | names ->
      let typ_objectId =
        Typ.Make.var ("nameIR" $ no_region) [] |> Typ.Make.list
      in
      let values_name = List.map Value.Make.text names in
      let value_objectId = Value.Make.list typ_objectId values_name in
      if
        Spec.Func.find_object_qualified_e value_arch value_objectId
        |> Option.is_some
      then
        Spec.Func.update_object_qualified_e value_arch value_objectId
          value_tableObject
      else
        let table_name_unqualified = names |> List.rev |> List.hd in
        update_table_unqualified table_name_unqualified

let add_entry (value_ctx : Value.t) (value_arch : Value.t)
    (value_tableName : Value.t) (value_tableEntryPriorityInterface : Value.t)
    (value_tableKeysetInterface : Value.t)
    (value_tableActionInterface : Value.t) : Value.t =
  (* Lookup table object *)
  let value_tableObject = find_table value_arch value_tableName in
  (* Add entry to table object *)
  let value_tableObject =
    match
      Spec.Func.tableObject_add_entry value_ctx value_tableObject
        value_tableEntryPriorityInterface value_tableKeysetInterface
        value_tableActionInterface
    with
    | Some value_tableObject -> value_tableObject
    | None ->
        (* Replace the key names of the keyset interface with those of the table,
           assuming key fields are given in order *)
        let values_nameIR_key =
          Spec.Func.key_interface_of_tableObject value_tableObject
          |> List.filter_map
               (fun (value_nameIR_key, value_nameIR_matchKind, _value_typeIR) ->
                 if Value.Get.text value_nameIR_matchKind = "selector" then None
                 else Some value_nameIR_key)
        in
        let values_tableKeyInterface =
          Value.Get.list value_tableKeysetInterface
        in
        let values_tableKeyValueInterface =
          values_tableKeyInterface |> List.map Value.Get.tuple
          |> List.map (fun values -> List.nth values 1)
        in
        let typ_tableKeyInterface =
          Typ.Make.var ("tableKeyInterface" $ no_region) []
        in
        let typ_tableKeyInterfaceList = Typ.Make.list typ_tableKeyInterface in
        let value_tableKeysetInterface =
          List.map2
            (fun value_nameIR_key value_tableKeyValueInterface ->
              [ value_nameIR_key; value_tableKeyValueInterface ])
            values_nameIR_key values_tableKeyValueInterface
          |> List.map (Value.Make.tuple typ_tableKeyInterface)
          |> Value.Make.list typ_tableKeyInterfaceList
        in
        Spec.Func.tableObject_add_entry value_ctx value_tableObject
          value_tableEntryPriorityInterface value_tableKeysetInterface
          value_tableActionInterface
        |> Option.get
  in
  (* Update arch with modified table object *)
  update_table value_arch value_tableName value_tableObject

let add_default_action (value_ctx : Value.t) (value_arch : Value.t)
    (value_tableName : Value.t) (value_tableActionInterface : Value.t) : Value.t
    =
  (* Lookup table object *)
  let value_tableObject = find_table value_arch value_tableName in
  (* Add entry to table object *)
  let value_tableObject =
    Spec.Func.tableObject_add_default_action value_ctx value_tableObject
      value_tableActionInterface
  in
  (* Update arch with modified table object *)
  update_table value_arch value_tableName value_tableObject
