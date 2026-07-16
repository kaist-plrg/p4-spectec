open Lang
open Domain.Lib

(* ===== Theta construction ===== *)

(* Build a substitution theta from tparams and concrete targs.
   tparams : Il.tparam list = (string phrase) list  (keys for TIdMap)
   targs   : Il.typ list = Il.targ list             (concrete types) *)
let build_theta (tparams : Il.tparam list) (targs : Il.typ list) : Subst.theta =
  TIdMap.of_lists tparams targs
