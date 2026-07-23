module Typ = Runtime.Type.Typ
open Util.Source

(* Pre-computed type constants *)

let typ_param = Typ.Make.var ("param" $ no_region) []
let typ_iterprem = Typ.Make.var ("iterprem" $ no_region) []
let typ_prem = Typ.Make.var ("prem" $ no_region) []
let typ_rulmatch = Typ.Make.var ("rulmatch" $ no_region) []
let typ_rulpath = Typ.Make.var ("rulpath" $ no_region) []
let typ_rulgroup = Typ.Make.var ("rulgroup" $ no_region) []
let typ_elsgroup = Typ.Make.var ("elsgroup" $ no_region) []
let typ_clause = Typ.Make.var ("clause" $ no_region) []
let typ_tblrow = Typ.Make.var ("tblrow" $ no_region) []
let typ_defn = Typ.Make.var ("defn" $ no_region) []
