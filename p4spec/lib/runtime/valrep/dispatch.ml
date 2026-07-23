module Mixop = Domain.Mixop
module Mixfix = Domain.Mixfix
module Il = Lang.Il
open Util.Source

(* [V]-generic dispatch over spec variant values. Replaces the [Value.t]-
   specific helpers ([build_mtchtbl]/[mtch_dispatch]) used in the interpreter;
   threads the spec-level type so [V.Get.case] works under [V_native]. *)
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

  let build_mtchtbl (cases : (Mixop.t * 'a mtch) list) : 'a mtchtbl =
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

  (* Assert [value] has exactly the given mixop; return its args or error. *)
  let case_exact (value : V.t) (mixop_expect : Mixop.t) (typ : Il.typ) :
      V.t list =
    match V.Get.(value |>>? (mixop_expect, typ)) with
    | Some args -> args
    | None ->
        Util.Error.error_unparse
          (Format.asprintf "expected case with %s"
             (Mixop.string_of_mixop mixop_expect))
end
