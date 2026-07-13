module Il = Lang.Il
open Util.Source

let var (name : string) : Il.typ = Il.VarT (name $ no_region, []) $ no_region

let typ_param = var "param"
let typ_prem = var "prem"
let typ_defn = var "defn"
