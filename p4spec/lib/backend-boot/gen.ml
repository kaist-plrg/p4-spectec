module Run = Runtime.Dynamic_Runner.Signature

let gen_boot_zero () =
  (module Runner.Make.Make (Interface.SpecTec) (Spectec.Make_zero)
            (Interp_il.Interp.Make)
            (Interp_sl.Interp.Make) : Run.RUNNER)

let get_p4 () =
  let module MakeExtern (_ : Run.INTERP_IL) (_ : Run.INTERP_SL) : Run.EXTERN =
  struct
    let init_mode _ = ()
    let eval_extern_rel = Backend_sim.Placeholder.Make.eval_extern_rel
    let eval_extern_func = Backend_sim.Placeholder.Make.eval_extern_func
  end in
  (module Runner.Make.Make (Interface.P4) (MakeExtern) (Interp_il.Interp.Make)
            (Interp_sl.Interp.Make) : Run.RUNNER)

let gen_boot_one () =
  let (module Runner_P4) = get_p4 () in
  let booter =
    (module Runner.Make.Make (Interface.SpecTec) (Spectec.Make_one (Runner_P4))
              (Interp_il.Interp.Make)
              (Interp_sl.Interp.Make) : Run.RUNNER)
  in
  ((module Runner_P4 : Run.RUNNER), booter)
