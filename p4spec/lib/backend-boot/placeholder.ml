module Typ = Runtime.Type.Typ
module Value = Runtime.Value
module Run = Runtime.Dynamic_Runner.Signature

module Make : Run.EXTERN = struct
  let eval_extern_rel (_name : string) (_values_input : Value.t list) :
      Run.rel_result =
    failwith "TODO"

  let eval_extern_func (_name : string) (_typs : Typ.t list)
      (_values_input : Value.t list) : Run.func_result =
    failwith "TODO"
end
