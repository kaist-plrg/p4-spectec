open Runtime.Dynamic_Runner.Signature

(* Single-runner baseline. No extern routing: any extern call errors.
 *   boot [Make_null] --extern--> (error)
 *)

let gen_zero_spectec () =
  let boot =
    (module Runner.Make.Make_nonrec (Interface.SpecTec) (Spectec.Make_null)
              (Interp_il.Interp.Make)
              (Interp_sl.Interp.Make) : RUNNER)
  in
  boot

(* N-layer P4 stack: boot -> [ mid * N ] -> P4 runner.
 *   boot [Make_parametric(Runner_SpecTec_mid)]
 *     --extern--> Runner_SpecTec_mid.eval_rel/func
 *   [ Runner_SpecTec_mid [Make_parametric(Runner_P4)] * N ]
 *     --extern-->
 *   Runner_P4.eval_rel/func [P4.Make]
 *)

let gen_n_p4 ~(depth : int) =
  let (module Runner_P4) = (module P4.Make () : RUNNER) in
  let (module Runner_intermediate), runners_intermediate =
    List.fold_left
      (fun ((module Runner_before : RUNNER), runners) _ ->
        let runner =
          (module Runner.Make.Make_nonrec
                    (Interface.SpecTec)
                    (Spectec.Make_parametric (Runner_before))
                    (Interp_il.Interp.Make)
                    (Interp_sl.Interp.Make) : RUNNER)
        in
        (runner, runners @ [ runner ]))
      ((module Runner_P4 : RUNNER), [])
      (List.init depth Fun.id)
  in
  let boot =
    (module Runner.Make.Make_nonrec
              (Interface.SpecTec)
              (Spectec.Make_parametric (Runner_intermediate))
              (Interp_il.Interp.Make)
              (Interp_sl.Interp.Make) : RUNNER)
  in
  ((module Runner_P4 : RUNNER), runners_intermediate, boot)

(* N-layer SpecTec stack giving each layer its own extern routing.
 *   boot [Make_parametric(Runner_SpecTec_mid)]
 *     --extern--> Runner_SpecTec_mid.eval_rel/func
 *   [ Runner_SpecTec_mid [Make_parametric(Runner_SpecTec_pgm)]
 *     --extern--> Runner_SpecTec_pgm.eval_rel/func * N ]
 *   Runner_SpecTec_pgm [Make_null]
 *     --extern--> (error)
 *)

let gen_n_spectec ~(depth : int) =
  let (module Runner_SpecTec_pgm) =
    (module Runner.Make.Make_nonrec (Interface.SpecTec) (Spectec.Make_null)
              (Interp_il.Interp.Make)
              (Interp_sl.Interp.Make) : RUNNER)
  in
  let (module Runner_SpecTec_mid), runners_intermediate =
    List.fold_left
      (fun ((module Runner_before : RUNNER), runners) _ ->
        let runner =
          (module Runner.Make.Make_nonrec
                    (Interface.SpecTec)
                    (Spectec.Make_parametric (Runner_before))
                    (Interp_il.Interp.Make)
                    (Interp_sl.Interp.Make) : RUNNER)
        in
        (runner, runners @ [ runner ]))
      ((module Runner_SpecTec_pgm : RUNNER), [])
      (List.init depth Fun.id)
  in
  let boot =
    (module Runner.Make.Make_nonrec
              (Interface.SpecTec)
              (Spectec.Make_parametric (Runner_SpecTec_mid))
              (Interp_il.Interp.Make)
              (Interp_sl.Interp.Make) : RUNNER)
  in
  ((module Runner_SpecTec_mid : RUNNER), runners_intermediate, boot)
