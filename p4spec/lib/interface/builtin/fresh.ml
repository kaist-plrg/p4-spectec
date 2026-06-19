open Lang
open Il

module Make (V : Valrep.SAFE) = struct
  open Util.Source

  (* dec $fresh_typeId() : typeId *)

  let fresh_typeId (ctr : int ref) (add : V.t -> unit) (at : region)
      (targs : targ list) (values_input : V.t list) : V.t =
    Extract.zero at targs;
    Extract.zero at values_input;
    let tid = "FRESH__" ^ string_of_int !ctr in
    ctr := !ctr + 1;
    let value = V.Make.text tid in
    add value;
    value
end
