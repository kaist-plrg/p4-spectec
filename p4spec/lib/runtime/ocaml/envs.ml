(* Environments *)

(* Name environment *)

module NEnv = Dynamic.Envs.MakeVarEnv (struct
  type t = string

  let to_string = Fun.id
end)
