module Mixop = Domain.Mixop
module Mixfix = Domain.Mixfix
module Il = Lang.Il
open Util.Source

(* [V]-generic replacement for [Runtime.Value.Get.mtch_dispatch]/
   [build_mtchtbl]/[( |>>! )]. Those are hardcoded to [Value.t]'s
   self-describing [.it] tag; this version threads the value's spec-level
   type explicitly ([V.Get.case] needs it under [V_native]) and is
   otherwise identical in shape. *)
module Make (V : Sig.SAFE) = struct
  module MixopHashed = struct
    type t = Mixop.t

    let equal = Mixop.eq

    let hash (m : Mixop.t) : int =
      Hashtbl.hash (Mixop.string_of_mixop m) land 0x7FFFFFFF
  end

  module MtchTbl = Hashtbl.Make (MixopHashed)

  type 'a mtch = region -> V.t list -> 'a
  type 'a mtchtbl = 'a mtch MtchTbl.t

  let build_mtchtbl (cases : (Mixop.t * (region -> V.t list -> 'a)) list) :
      'a mtchtbl =
    let tbl = MtchTbl.create (List.length cases) in
    List.iter (fun (mixop, f) -> MtchTbl.add tbl mixop f) cases;
    tbl

  let dispatch (value : V.t) (typ : Il.typ) (tbl : 'a mtchtbl)
      (case_default : 'a mtch) : 'a =
    let at = V.at value in
    let mixop, args = Mixfix.split (V.Get.case value typ) in
    match MtchTbl.find_opt tbl mixop with
    | Some f -> f at args
    | None -> case_default at args

  let case_exact (value : V.t) (mixop_expect : Mixop.t) (typ : Il.typ) :
      V.t list =
    match V.Get.(value |>>? (mixop_expect, typ)) with
    | Some args -> args
    | None ->
        Util.Error.error_unparse
          (Format.asprintf "expected case with %s"
             (Mixop.string_of_mixop mixop_expect))
end
