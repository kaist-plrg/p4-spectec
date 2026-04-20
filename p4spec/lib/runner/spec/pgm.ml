module Value = Runtime.Value
module IO = Runtime.Sim.Io

(* Helpers for invoking relations taking a program in the spec *)

type call_pgm = string -> string list -> string -> Value.t * Value.t

let call : call_pgm ref = ref (fun _ _ _ -> assert false)
let register f = call := f
