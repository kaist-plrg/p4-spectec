(* Thin functor shell + top-level program-dispatch entry for generated OCaml.

   The heavy logic lives at module top-level, reading the per-instance context
   [cur__]. [Make] builds its own [ctx__] from the [Interface]/[Extern] functor
   arguments and switches [cur__] to it across the three dispatch entry points
   via [with_ctx]. Towers nest correctly because control crosses levels only
   through [eval_rel] / [eval_func] / [eval_program]. *)

(* Top-level program dispatch — reads the current instance's parser. *)
let eval_program =
  {|
let eval_program (relname__ : string) (includes__ : string list)
    (path__ : string) : Run.program_result =
  match (!cur__).iface.parse_program includes__ [path__] with
  | Run.Pass value_program -> (
      match eval_rel relname__ [ value_program ] with
      | Run.Pass values_output -> Run.Pass values_output
      | Run.Fail (at, msg) -> Run.Fail (`Runtime (at, msg)))
  | Run.Fail (`Syntax (at, msg)) -> Run.Fail (`Syntax (at, msg))
|}

(* The public functor contract, preserved structurally. *)
let make =
  {|
module Make
    (Interface : Run.INTERFACE)
    (Extern : Run.EXTERN)
    () : Run.INTERP_ML = struct

  let my_ctx : ctx__ = {
    iface = {
      checkpoint = Interface.checkpoint;
      seff = Interface.seff;
      call_builtin = Interface.call_builtin;
      parse_program = Interface.parse_program;
    };
    extern = {
      checkpoint = Extern.checkpoint;
      seff = Extern.seff;
      eval_extern_rel = Extern.eval_extern_rel;
      eval_extern_func = Extern.eval_extern_func;
    };
    cache_enabled = false;
    caches = make_caches__ ();
  }

  module Cache = struct
    let cache_on () = my_ctx.cache_enabled <- true
    let cache_off () =
      my_ctx.cache_enabled <- false;
      clear_caches__ my_ctx.caches
  end

  let init ~cache ~det:_ ~guard:_ () =
    if cache then Cache.cache_on ()

  let clear () = Cache.cache_off ()

  let eval_func name__ typs__ args__ =
    with_ctx my_ctx (fun () -> eval_func name__ typs__ args__)
  let eval_rel name__ args__ =
    with_ctx my_ctx (fun () -> eval_rel name__ args__)
  let eval_program relname__ includes__ path__ =
    with_ctx my_ctx (fun () -> eval_program relname__ includes__ path__)
end
|}
