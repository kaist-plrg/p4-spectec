open Runtime.Dynamic_Runner.Signature
open Error
open Util.Source

module Make () : RUNNER = struct
  module Spec_ = Backend_sim.Spec.Make (Valrep.V_value)
  module Placeholder = Backend_sim.Placeholder.Make (Spec_)

  module MakeExtern
      (Interp_IL : INTERP_IL)
      (Interp_SL : INTERP_SL)
      (Interp_ML : INTERP_ML) : EXTERN = struct
    let init_mode mode_ =
      let call_func name typs values =
        (match mode_ with
        | IL_mode -> Interp_IL.eval_func name typs values
        | SL_mode -> Interp_SL.eval_func name typs values
        | ML_mode -> Interp_ML.eval_func name typs values
        | Empty_mode -> assert false)
        |> function
        | Pass value -> value
        | Fail (at, msg) -> error at msg
      in
      let call_rel name values =
        (match mode_ with
        | IL_mode -> Interp_IL.eval_rel name values
        | SL_mode -> Interp_SL.eval_rel name values
        | ML_mode -> Interp_ML.eval_rel name values
        | Empty_mode -> assert false)
        |> function
        | Pass values -> values
        | Fail (at, msg) -> error at msg
      in
      let call_pgm relname includes filename =
        (match mode_ with
        | IL_mode -> Interp_IL.eval_program relname includes filename
        | SL_mode -> Interp_SL.eval_program relname includes filename
        | ML_mode -> Interp_ML.eval_program relname includes filename
        | Empty_mode -> assert false)
        |> function
        | Pass [ value_ctx; value_arch ] -> (value_ctx, value_arch)
        | Pass _ -> error no_region "unexpected number of return values"
        | Fail (`Syntax (at, msg) | `Runtime (at, msg)) -> error at msg
      in
      Spec_.Func.register call_func;
      Spec_.Rel.register call_rel;
      Spec_.Pgm.register call_pgm

    let checkpoint () : int = 0
    let seff (before : int) (after : int) : bool = before <> after
    let clear () = ()

    module Cache = struct
      let cache_on () = ()
      let cache_off () = ()
    end

    let eval_extern_rel = Placeholder.eval_extern_rel
    let eval_extern_func = Placeholder.eval_extern_func
  end

  include (
    Runner.Make.Make_rec (Interface.P4) (MakeExtern) (Interp_il.Interp.Make)
      (Interp_sl.Interp.Make)
      (Backend_ocaml_p4.Interp_ml.Make) :
        RUNNER)
end
