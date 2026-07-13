module Il = Lang.Il
open Util.Source

(* [Il.typ] mirrors of [Typs]'s [Runtime.Type.Typ.t] constants, for
   [V.Get.case]/[case_of_typed] dispatch (needs [Il.typ], not
   [Runtime.Type.Typ.t] — same underlying type, different alias path). *)

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
