module Value = Runtime.Value

(* Helpers for invoking relations in the spec *)

type call_rel = string -> Value.t list -> Value.t list

let call : call_rel ref = ref (fun _ _ -> assert false)
let register f = call := f
