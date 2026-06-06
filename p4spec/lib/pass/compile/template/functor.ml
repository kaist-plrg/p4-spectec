(* Functor header and footer for generated OCaml *)

let header =
  {|
module Make
    (Interface : Run.INTERFACE)
    (Extern : Run.EXTERN)
    () : Run.INTERP_ML = struct

  module Cache = struct
    let cache_on () = ()
    let cache_off () = ()
  end

  let init ~cache:_ ~det:_ ~guard:_ () = ()
  let clear () = ()

|}

let footer =
  {|
  let eval_rel (_name : string) (_args : Value.t list) : Run.rel_result =
    Fail (no_region, "ML interpreter: eval_rel dispatch not yet wired (Phase 7)")

  let eval_program (_relname : string) (_includes : string list)
      (_path : string) : Run.program_result =
    Fail (`Runtime (no_region, "ML interpreter: eval_program dispatch not yet wired (Phase 7)"))
end
|}
