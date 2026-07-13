module Il = Lang.Il
open Util.Source

let var (name : string) : Il.typ = Il.VarT (name $ no_region, []) $ no_region

let typ_param = var "param"
let typ_prem = var "prem"
let typ_defn = var "defn"

(* Constants used by ili/unboot.ml's dispatch-table call sites. *)

let typ_iterprem = var "iterprem"
let typ_rulmatch = var "rulmatch"
let typ_rulpath = var "rulpath"
let typ_rulgroup = var "rulgroup"
let typ_elsgroup = var "elsgroup"
let typ_clause = var "clause"
let typ_tblrow = var "tblrow"
