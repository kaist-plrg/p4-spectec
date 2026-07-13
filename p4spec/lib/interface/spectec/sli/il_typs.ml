module Il = Lang.Il
open Util.Source

let var (name : string) : Il.typ = Il.VarT (name $ no_region, []) $ no_region

let typ_param = var "param"
let typ_holdcase = var "holdcase"
let typ_guard = var "guard"
let typ_instr = var "instr"
let typ_defn = var "defn"

(* Constants used by sli/unboot.ml's dispatch-table call sites. *)

let typ_iterinstr = var "iterinstr"
let typ_case = var "case"
let typ_tblrow = var "tblrow"
