module Typ = Runtime.Type.Typ
open Util.Source

(* Pre-computed type constants *)

let typ_paramIL = Typ.Make.var ("paramIL" $ no_region) []
let typ_iterprem = Typ.Make.var ("iterprem" $ no_region) []
let typ_prem = Typ.Make.var ("prem" $ no_region) []
let typ_rulmatch = Typ.Make.var ("rulmatch" $ no_region) []
let typ_rulpath = Typ.Make.var ("rulpath" $ no_region) []
let typ_rulgroup = Typ.Make.var ("rulgroup" $ no_region) []
let typ_elsgroup = Typ.Make.var ("elsgroup" $ no_region) []
let typ_clause = Typ.Make.var ("clause" $ no_region) []
let typ_tblrowIL = Typ.Make.var ("tblrowIL" $ no_region) []
let typ_defnIL = Typ.Make.var ("defnIL" $ no_region) []
