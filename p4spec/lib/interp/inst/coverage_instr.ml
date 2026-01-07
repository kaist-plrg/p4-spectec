open Lang
open Sl
module ICov = Coverage.Instr.Single
module Value = Runtime.Dynamic_Il.Value

let make () =
  (* Instruction coverage and a reader for it *)
  let coverage = ref ICov.empty in
  let read () = !coverage in
  (* Instruction coverage measurement handler *)
  let module H : Handler.HANDLER = struct
    include Handler.Default

    let init_spec (spec : Handler.spec) : unit =
      match spec with SL spec_sl -> coverage := ICov.init spec_sl | _ -> ()

    let on_instr (instr : instr) : unit =
      coverage := ICov.hit !coverage instr.note.iid
  end in
  (* Return the handler and the reader *)
  ((module H : Handler.HANDLER), read)
