(* Spec type (and mixop) references threaded to the [VAL] case ops (B2a).

   The compiled<->extern case bridge ([make_case_typed]/[case_of_typed]) and the
   [VAL] [Get.case]/[( <<| )]/[( |>>? )] surface take the value's spec type as a
   structured [Il.typ], not a bare string (which was fragile: a stale typename
   only failed at runtime — see API.md §9.3). These are the monomorphic spec
   types the externs construct/inspect; defined once here and referenced across
   backend-sim so each name lives in exactly one place. *)

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

(* State-persist types, threaded to [V.marshal]/[V.unmarshal] so the typed
   [Obj.t] becomes an honest [Value.t] before it is yojson-serialized (B5). *)
let eval_context = var "evalContext"
let type_ir = var "typeIR"

(* The lone mixop threaded structurally (the [( |>>? )] REJECT test). *)
let mo_reject_error_value = Value.Mixops.of_string "REJECT errorValue"
