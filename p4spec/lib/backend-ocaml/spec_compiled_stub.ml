(* Stub thin shell — copied to spec_compiled.ml by `make restore-stub`,
   overwritten by `make gen-ocaml`. Identical in shape to the generated shell;
   it routes through the stub [spec_parts] (compiled_stub/), whose dispatch
   entry points fail with "run `make gen-ocaml`". *)
module Run = Runtime.Dynamic_Runner.Signature
open Spec_parts.Ctx

module Make (Interface : Run.INTERFACE) (Extern : Run.EXTERN) () :
  Run.INTERP_ML = struct
  let my_ctx : ctx__ =
    {
      iface =
        {
          parse_program = Interface.parse_program;
          call_builtin = Interface.call_builtin;
        };
      extern =
        {
          eval_extern_rel = Extern.eval_extern_rel;
          eval_extern_func = Extern.eval_extern_func;
        };
    }

  module Cache = struct
    let cache_on () = ()
    let cache_off () = ()
  end

  let init ~cache:_ ~det:_ ~guard:_ () = ()
  let clear () = ()

  let eval_func name__ typs__ args__ =
    with_ctx my_ctx (fun () ->
        Spec_parts.Dispatch.eval_func name__ typs__ args__)

  let eval_rel name__ args__ =
    with_ctx my_ctx (fun () -> Spec_parts.Dispatch.eval_rel name__ args__)

  let eval_program relname__ includes__ path__ =
    with_ctx my_ctx (fun () ->
        Spec_parts.Dispatch.eval_program relname__ includes__ path__)
end
