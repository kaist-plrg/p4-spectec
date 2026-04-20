module Run = Runtime.Dynamic_Runner.Signature

let gen_boot_placeholder () =
  (module Runner.Make.Make (Interface.SpecTec) (Extern.Make)
            (Interp_il.Interp.Make)
            (Interp_sl.Interp.Make) : Run.RUNNER)
