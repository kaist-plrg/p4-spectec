open Runtime.Dynamic_Runner.Signature

(* Single-runner baseline. No extern routing: any extern call errors.
 *   boot [Make_null] --extern--> (error)
 *)

let gen_zero_spectec () =
  let boot =
    (module Runner.Make.Make (Interface.SpecTec) (Spectec.Make_null)
              (Interp_il.Interp.Make)
              (Interp_sl.Interp.Make) : RUNNER)
  in
  boot

(* Two-layer stack: boot spec delegates externs to the P4 runner.
 *   boot [Make_parametric(Runner_P4)] --extern--> Runner_P4.eval_rel/func
 *)

let gen_square_p4 () =
  let (module Runner_P4) = (module P4.Make () : RUNNER) in
  let boot =
    (module Runner.Make.Make
              (Interface.SpecTec)
              (Spectec.Make_parametric (Runner_P4))
              (Interp_il.Interp.Make)
              (Interp_sl.Interp.Make) : RUNNER)
  in
  ((module Runner_P4 : RUNNER), boot)

(* Three-layer P4 stack: boot -> mid -> P4 runner.
 *   boot [Make_parametric(Runner_SpecTec_mid)] --extern--> Runner_SpecTec_mid.eval_rel/func
 *   Runner_SpecTec_mid [Make_parametric(Runner_P4)] --extern--> Runner_P4.eval_rel/func
 *)

let gen_cube_p4 () =
  let (module Runner_P4) = (module P4.Make () : RUNNER) in
  let (module Runner_SpecTec_mid) =
    (module Runner.Make.Make
              (Interface.SpecTec)
              (Spectec.Make_parametric (Runner_P4))
              (Interp_il.Interp.Make)
              (Interp_sl.Interp.Make) : RUNNER)
  in
  let boot =
    (module Runner.Make.Make
              (Interface.SpecTec)
              (Spectec.Make_parametric (Runner_SpecTec_mid))
              (Interp_il.Interp.Make)
              (Interp_sl.Interp.Make) : RUNNER)
  in
  ((module Runner_P4 : RUNNER), (module Runner_SpecTec_mid : RUNNER), boot)

(* Three-layer SpecTec stack giving each layer its own extern routing.
 *   boot [Make_parametric(Runner_SpecTec_mid)]
 *     --extern--> Runner_SpecTec_mid.eval_rel/func
 *   Runner_SpecTec_mid [Make_parametric(Runner_SpecTec_pgm)]
 *     --extern--> Runner_SpecTec_pgm.eval_rel/func
 *   Runner_SpecTec_pgm [Make_null]
 *     --extern--> (error)
 *)

let gen_cube_spectec () =
  let (module Runner_SpecTec_pgm) =
    (module Runner.Make.Make (Interface.SpecTec) (Spectec.Make_null)
              (Interp_il.Interp.Make)
              (Interp_sl.Interp.Make) : RUNNER)
  in
  let (module Runner_SpecTec_mid) =
    (module Runner.Make.Make
              (Interface.SpecTec)
              (Spectec.Make_parametric (Runner_SpecTec_pgm))
              (Interp_il.Interp.Make)
              (Interp_sl.Interp.Make) : RUNNER)
  in
  let boot =
    (module Runner.Make.Make
              (Interface.SpecTec)
              (Spectec.Make_parametric (Runner_SpecTec_mid))
              (Interp_il.Interp.Make)
              (Interp_sl.Interp.Make) : RUNNER)
  in
  ( (module Runner_SpecTec_pgm : RUNNER),
    (module Runner_SpecTec_mid : RUNNER),
    boot )
