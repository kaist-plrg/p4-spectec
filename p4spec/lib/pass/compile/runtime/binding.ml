open Lang
open Runtime.Dynamic_Sl

(* [typ] is the SpecTec type of the bound variable, needed to re-wrap composite
   values whose OCaml representation carries a note *)
type t = Var.t * Ml.id * Sl.typ
