open Interface.Wrap
open Interface.Unwrap
module Value = Runtime.Sim.Value

(* Match-action table interface *)

let find_table (value_arch : Value.t) (value_tableName : Value.t) : Value.t =
  let find_table_unqualified table_name_unqualified =
    let value_tableName_unqualified = wrap_text_v table_name_unqualified in
    Spec.Func.find_object_unqualified_e value_arch value_tableName_unqualified
    |> Option.get
  in
  let table_name = unwrap_text_v value_tableName in
  match String.split_on_char '.' table_name with
  | [] -> assert false
  | [ table_name_unqualified ] -> find_table_unqualified table_name_unqualified
  | names -> (
      let values_name = List.map wrap_text_v names in
      let value_objectId = wrap_list_v "nameIR" values_name in
      match Spec.Func.find_object_qualified_e value_arch value_objectId with
      | Some value_table -> value_table
      | None ->
          let table_name_unqualified = names |> List.rev |> List.hd in
          find_table_unqualified table_name_unqualified)

let update_table (value_arch : Value.t) (value_tableName : Value.t)
    (value_tableObject : Value.t) : Value.t =
  let update_table_unqualified table_name_unqualified =
    let value_tableName_unqualified = wrap_text_v table_name_unqualified in
    Spec.Func.update_object_unqualified_e value_arch value_tableName_unqualified
      value_tableObject
  in
  let table_name = unwrap_text_v value_tableName in
  match String.split_on_char '.' table_name with
  | [] -> assert false
  | [ table_name_unqualified ] ->
      update_table_unqualified table_name_unqualified
  | names ->
      let values_name = List.map wrap_text_v names in
      let value_objectId = wrap_list_v "nameIR" values_name in
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
    Spec.Func.tableObject_add_entry value_ctx value_tableObject
      value_tableEntryPriorityInterface value_tableKeysetInterface
      value_tableActionInterface
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
