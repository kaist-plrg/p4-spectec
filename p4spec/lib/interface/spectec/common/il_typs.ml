module Il = Lang.Il
open Util.Source

(* Il.typ mirrors of Typs constants (same underlying type, different alias path) *)

let var (name : string) : Il.typ = Il.VarT (name $ no_region, []) $ no_region

let typ_val = var "val"
let typ_typ = var "typ"
let typ_deftyp = var "deftyp"
let typ_unop = var "unop"
let typ_binop = var "binop"
let typ_cmpop = var "cmpop"
let typ_arg = var "arg"
let typ_exp = var "exp"
let typ_path = var "path"
let typ_pattern = var "pattern"

(* Additional constants needed by common/boot.ml and common/unboot.ml's
   [<<|! typ_X] / [|>>! mop_X] call sites (missed by the initial Task 2
   inventory, which only covered the ten dispatch-table types). *)

let typ_iter = var "iter"
let typ_vari = var "vari"
let typ_optyp = var "optyp"
let typ_numtyp = var "numtyp"
let typ_typfield = var "typfield"
let typ_typcase = var "typcase"
let typ_num = var "num"
let typ_valfield = var "valfield"
let typ_valcase = var "valcase"
let typ_boolunop = var "boolunop"
let typ_numunop = var "numunop"
let typ_boolbinop = var "boolbinop"
let typ_numbinop = var "numbinop"
let typ_polycmpop = var "polycmpop"
let typ_numcmpop = var "numcmpop"
let typ_expcase = var "expcase"
let typ_expfield = var "expfield"
let typ_listpattern = var "listpattern"
let typ_optpattern = var "optpattern"
let typ_iterexp = var "iterexp"
