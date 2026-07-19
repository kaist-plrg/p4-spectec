module Typ = Runtime.Type.Typ
module Value = Runtime.Value
open Util.Source

let var (name : string) : Typ.t = Typ.Make.var (name $ no_region) []
let call_result = var "callResult"
let cursor = var "cursor"
let error_value = var "errorValue"
let prefixed_name_ir = var "prefixedNameIR"
let reject_transition_result = var "rejectTransitionResult"
let return_result = var "returnResult"
let storage_reference = var "storageReference"
let table_key_value_interface = var "tableKeyValueInterface"
let value = var "value"
let transition_result = var "transitionResult"
let eval_context = var "evalContext"
let arch = var "arch"
let type_ir = var "typeIR"
let mo_reject_error_value = Value.Mixops.of_string "REJECT errorValue"
