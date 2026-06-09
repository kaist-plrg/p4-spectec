(* Functor header and footer for generated OCaml *)

let header =
  {|
module Make
    (Interface : Run.INTERFACE)
    (Extern : Run.EXTERN)
    () : Run.INTERP_ML = struct

|}

let h_module : string =
  {|
  module H__ = struct
    type ('k, 'v) t = {
      data: ('k * 'v) list array;
      size: int;
    }
    let create n = { data = Array.make n []; size = n }
    let hash k = (Hashtbl.hash_param 100 1000 k) land max_int
    let find_opt h k =
      let b = (hash k) mod h.size in
      List.assoc_opt k h.data.(b)
    let replace h k v =
      let b = (hash k) mod h.size in
      h.data.(b) <- (k, v) :: List.filter (fun (k2,_) -> k2 <> k) h.data.(b)
    let clear h = Array.fill h.data 0 h.size []
  end

|}

let cache_section (cache_ids : string list) : string =
  let resets =
    String.concat "\n      "
      (List.map (fun id -> Printf.sprintf "H__.clear %s;" id) cache_ids)
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
