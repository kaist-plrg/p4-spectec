(* The interface and the runner the K spec runs with *)

module Run = Runtime.Dynamic_Runner.Signature

(* The SpecTec SL interface with the builtins the K spec delegates hooks to
   (spec-k/3.1-hook-builtin.watsup) *)
module Interface_K = struct
  include Interface.SpecTec_SL
  module Builtin_K = Builtin.Call.Make (Builtins) ()

  let call_builtin = Builtin_K.invoke
  let checkpoint = Builtin_K.checkpoint
  let seff = Builtin_K.seff
end

(* A runner without a tower, as Backend_boot.Build.build_null makes, with the
   interface above *)
let runner ?(cache = true) ?(det = false) (spec : Run.spec) :
    ((module Run.RUNNER), Run.error) result =
  let (module Runner) =
    (module Runner.Make.Make_rec
              (Interface_K)
              (Backend_boot.Spectec.Make_null (Interface_K))
              (Interp_al.Interp.Make)
              (Interp_sl.Interp.Make)
              (Interp_pl.Interp.Make) : Run.RUNNER)
  in
  Result.map
    (fun () -> (module Runner : Run.RUNNER))
    (Runner.init ~cache ~det ~guard:false spec)
