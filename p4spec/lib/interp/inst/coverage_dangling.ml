open Domain.Lib
module DCov = Coverage.Dangling.Single
module Value = Runtime.Dynamic_Il.Value

let make () =
  (* Dangling coverage and a reader for it *)
  let coverage = ref DCov.empty in
  let coverage_backup = ref DCov.empty in
  let read () = !coverage in
  (* Instruction coverage measurement handler *)
  let module H : Handler.HANDLER = struct
    include Handler.Default

    let init_spec (spec : Handler.spec) : unit =
      match spec with SL spec_sl -> coverage := DCov.init spec_sl | _ -> ()

    let backup () : unit = coverage_backup := !coverage

    let restore () : unit =
      coverage := !coverage_backup;
      coverage_backup := DCov.empty

    let on_instr_dangling (hit : bool) (pid : PId.t) (value_cond : Value.t) :
        unit =
      if hit then coverage := DCov.hit !coverage pid
      else coverage := DCov.miss !coverage pid value_cond.note.vid
  end in
  (* Return the handler and the reader *)
  ((module H : Handler.HANDLER), read)
