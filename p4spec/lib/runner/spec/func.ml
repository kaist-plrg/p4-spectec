module Typ = Runtime.Type.Typ
module Value = Runtime.Value

(* Helpers for invoking functions in the spec *)

type call_func = string -> Typ.t list -> Value.t list -> Value.t

let call : call_func ref = ref (fun _ _ -> assert false)
let register f = call := f
