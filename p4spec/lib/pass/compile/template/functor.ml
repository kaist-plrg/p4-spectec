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
  let eval_program (relname__ : string) (includes__ : string list)
      (path__ : string) : Run.program_result =
    match Interface.parse_program includes__ [path__] with
    | Run.Pass value_program -> (
        match eval_rel relname__ [ value_program ] with
        | Run.Pass values_output -> Run.Pass values_output
        | Run.Fail (at, msg) -> Run.Fail (`Runtime (at, msg)))
    | Run.Fail (`Syntax (at, msg)) -> Run.Fail (`Syntax (at, msg))
end
|}
