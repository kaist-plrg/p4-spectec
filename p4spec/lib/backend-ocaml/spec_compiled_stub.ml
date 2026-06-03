(* Stub — overwritten by make gen-ocaml *)
open Util.Source
module Run = Runtime.Dynamic_Runner.Signature

module Make (_ : Run.INTERFACE) (_ : Run.EXTERN) () : Run.INTERP_ML = struct
  module Cache = struct
    let cache_on () = ()
    let cache_off () = ()
  end

  let init ~cache:_ ~det:_ ~guard:_ () = ()
  let clear () = ()

  let eval_func (_name : string) (_typs : Runtime.Type.Typ.t list)
      (_args : Runtime.Value.t list) : Run.func_result =
    Fail (no_region, "ML interpreter: run `make gen-ocaml` to generate it")

  let eval_rel (_name : string) (_args : Runtime.Value.t list) : Run.rel_result
      =
    Fail (no_region, "ML interpreter: run `make gen-ocaml` to generate it")

  let eval_program (_relname : string) (_includes : string list)
      (_path : string) : Run.program_result =
    Fail
      (`Runtime
        (no_region, "ML interpreter: run `make gen-ocaml` to generate it"))
end
