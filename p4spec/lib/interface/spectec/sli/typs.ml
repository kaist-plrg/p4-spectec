module Typ = Runtime.Type.Typ
open Util.Source

(* Pre-computed type constants *)

let typ_paramSL = Typ.Make.var ("paramSL" $ no_region) []
let typ_iterinstr = Typ.Make.var ("iterinstr" $ no_region) []
let typ_instr = Typ.Make.var ("instr" $ no_region) []
let typ_block = Typ.Make.var ("block" $ no_region) []
let typ_holdcase = Typ.Make.var ("holdcase" $ no_region) []
let typ_guard = Typ.Make.var ("guard" $ no_region) []
let typ_case = Typ.Make.var ("case" $ no_region) []
let typ_tblrowSL = Typ.Make.var ("tblrowSL" $ no_region) []
let typ_defnSL = Typ.Make.var ("defnSL" $ no_region) []
