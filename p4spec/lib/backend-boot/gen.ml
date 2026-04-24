open Runtime.Dynamic_Runner.Signature

let gen_boot_zero () =
  (module Runner.Make.Make (Interface.SpecTec) (Spectec.Make_zero)
            (Interp_il.Interp.Make)
            (Interp_sl.Interp.Make) : RUNNER)

let gen_boot_one () =
  let (module Runner_P4) = (module P4.Make () : RUNNER) in
  let top =
    (module Runner.Make.Make (Interface.SpecTec) (Spectec.Make_top (Runner_P4))
              (Interp_il.Interp.Make)
              (Interp_sl.Interp.Make) : RUNNER)
  in
  ((module Runner_P4 : RUNNER), top)

let gen_boot_two () =
  let (module Runner_SpecTec) = gen_boot_zero () in
  let interm =
    (module Runner.Make.Make
              (Interface.SpecTec)
              (Spectec.Make_interm (Runner_SpecTec))
              (Interp_il.Interp.Make)
              (Interp_sl.Interp.Make) : RUNNER)
  in
  let (module Runner_P4) = (module P4.Make () : RUNNER) in
  let top =
    (module Runner.Make.Make (Interface.SpecTec) (Spectec.Make_top (Runner_P4))
              (Interp_il.Interp.Make)
              (Interp_sl.Interp.Make) : RUNNER)
  in
  ((module Runner_SpecTec : RUNNER), interm, (module Runner_P4 : RUNNER), top)
