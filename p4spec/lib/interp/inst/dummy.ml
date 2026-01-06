open Domain.Lib
module Value = Runtime.Dynamic_Il.Value
open Handler

module Handler : HANDLER = struct
  include Default

  let on_rel_enter (rid : RId.t) (_values_input : Value.t list) : unit =
    Format.printf "Entering relation: %s\n" (RId.to_string rid)

  let on_rel_exit (rid : RId.t) : unit =
    Format.printf "Exiting relation: %s\n" (RId.to_string rid)

  let on_func_enter (fid : FId.t) (_values_input : Value.t list) : unit =
    Format.printf "Entering function: %s\n" (FId.to_string fid)

  let on_func_exit (fid : FId.t) : unit =
    Format.printf "Exiting function: %s\n" (FId.to_string fid)
end
