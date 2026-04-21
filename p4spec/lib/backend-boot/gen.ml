module Run = Runtime.Dynamic_Runner.Signature

let gen_boot_zero () =
  (module Runner.Make.Make (Interface.SpecTec) (Spectec.Make_zero)
            (Interp_il.Interp.Make)
            (Interp_sl.Interp.Make) : Run.RUNNER)

let get_p4 () =
  (module Runner.Make.Make (Interface.P4) (Backend_sim.Placeholder.Make)
            (Interp_il.Interp.Make)
            (Interp_sl.Interp.Make) : Run.RUNNER)

let gen_boot_one () =
  let (module Runner_P4) = get_p4 () in
  let booter =
    (module Runner.Make.Make (Interface.SpecTec) (Spectec.Make_one (Runner_P4))
              (Interp_il.Interp.Make)
              (Interp_sl.Interp.Make) : Run.RUNNER)
  in
  ((module Runner_P4 : Run.RUNNER), booter)
