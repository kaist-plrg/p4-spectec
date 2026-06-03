open Lang
open Domain.Lib
open Util.Source

(* ===== Theta construction ===== *)

(* Build a substitution theta from tparams and concrete targs.
   tparams : Il.tparam list = (string phrase) list  (keys for TIdMap)
   targs   : Il.typ list = Il.targ list             (concrete types) *)
let build_theta (tparams : Il.tparam list) (targs : Il.typ list) : Subst.theta =
  TIdMap.of_lists tparams targs

(* ===== Specialization ===== *)

(* Clone a Sl.definedfunc with theta applied throughout.
   The resulting FuncDecD has tparams = [] and a fresh name. *)
let specialize (dfunc : Sl.definedfunc) (theta : Subst.theta)
    ~(new_name : string) : Sl.def =
  let id, _tparams, params, typ_ret, block, elseblock, hints = dfunc in
  Sl.FuncDecD
    ( { id with it = new_name },
      [],
      List.map (Subst.subst_sl_param theta) params,
      Subst.subst_typ theta typ_ret,
      Subst.subst_block theta block,
      Option.map (Subst.subst_block theta) elseblock,
      hints )
  $ id.at

(* Clone a Sl.builtinfunc with theta applied.
   The resulting BuiltinDecD has tparams = [] and a fresh name. *)
let specialize_builtin (bfunc : Sl.builtinfunc) (theta : Subst.theta)
    ~(new_name : string) : Sl.def =
  let id, _tparams, params, typ_ret, hints = bfunc in
  Sl.BuiltinDecD
    ( { id with it = new_name },
      [],
      List.map (Subst.subst_sl_param theta) params,
      Subst.subst_typ theta typ_ret,
      hints )
  $ id.at

(* Clone a Sl.externfunc with theta applied.
   The resulting ExternDecD has tparams = [] and a fresh name. *)
let specialize_extern (efunc : Sl.externfunc) (theta : Subst.theta)
    ~(new_name : string) : Sl.def =
  let id, _tparams, params, typ_ret, hints = efunc in
  Sl.ExternDecD
    ( { id with it = new_name },
      [],
      List.map (Subst.subst_sl_param theta) params,
      Subst.subst_typ theta typ_ret,
      hints )
  $ id.at
