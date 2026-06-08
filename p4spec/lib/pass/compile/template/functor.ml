(* Functor header and footer for generated OCaml *)

let header =
  {|
module Make
    (Interface : Run.INTERFACE)
    (Extern : Run.EXTERN)
    () : Run.INTERP_ML = struct

|}

let cache_section (cache_ids : string list) : string =
  let resets =
    String.concat "\n      "
      (List.map (fun id -> Printf.sprintf "Hashtbl.clear %s;" id) cache_ids)
  in
  Printf.sprintf
    {|
  let cache_enabled__ = ref false

  module Cache = struct
    let cache_on () = cache_enabled__ := true
    let cache_off () =
      cache_enabled__ := false;
      %s
  end

  let init ~cache ~det:_ ~guard:_ () =
    if cache then Cache.cache_on ()

  let clear () = Cache.cache_off ()

|}
    resets

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
