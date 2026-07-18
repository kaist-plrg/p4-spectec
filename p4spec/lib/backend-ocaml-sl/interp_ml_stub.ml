(* Stub thin shell — copied to interp_ml.ml by `make restore-stub`,
   overwritten by `make gen-ocaml-sl`. Identical in shape to the generated
   shell; it routes through the stub [spec_parts_sl] (compiled_stub/), whose
   dispatch entry points fail with "run `make gen-ocaml-sl`". *)
module Run = Runtime.Dynamic_Runner.Signature
open Spec_parts_sl.Trampoline

module Make (Interface : Run.INTERFACE) (Extern : Run.EXTERN) () :
  Run.INTERP_ML = struct
  let trampoline : trampoline__ =
    {
      interface =
        {
          call_builtin = Interface.call_builtin;
          parse_program = Interface.parse_program;
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
    with_trampoline trampoline (fun () ->
        Spec_parts_sl.Dispatch.eval_func name__ typs__ args__)

  let eval_rel name__ args__ =
    with_trampoline trampoline (fun () ->
        Spec_parts_sl.Dispatch.eval_rel name__ args__)

  let eval_program relname__ includes__ path__ =
    with_trampoline trampoline (fun () ->
        Spec_parts_sl.Dispatch.eval_program relname__ includes__ path__)
end
