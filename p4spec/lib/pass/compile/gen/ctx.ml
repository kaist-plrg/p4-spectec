open Runtime.OCaml.Envs

(* Context *)

type t = { var : NEnv.t }

let init () : t = { var = NEnv.empty }
