open Runtime.Dynamic_Runner.Signature

let gen_boot_zero () =
  (module Runner.Make.Make (Interface.SpecTec) (Spectec.Make_zero)
            (Interp_il.Interp.Make)
            (Interp_sl.Interp.Make) : RUNNER)

let gen_boot_one () =
  let (module Runner_P4) = (module P4.Make () : RUNNER) in
  let booter =
    (module Runner.Make.Make (Interface.SpecTec) (Spectec.Make_one (Runner_P4))
              (Interp_il.Interp.Make)
              (Interp_sl.Interp.Make) : RUNNER)
  in
  ((module Runner_P4 : RUNNER), booter)
