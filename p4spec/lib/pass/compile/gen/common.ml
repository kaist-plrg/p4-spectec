open Lang
open Util.Source

(* Helpers *)

(* Create a fake variable expression with a given type *)

let make_var_exp (var : string) (typ : Sl.typ) : Sl.exp =
  let id = var $ no_region in
  Il.VarE id $$ (no_region, typ.it)

(* Raise an Unmatch exception *)

let raise_unmatch (msg : string) : Ml.expr =
  Ml.AppE (Ml.VarE "raise", [ Ml.AppE (Ml.VarE "Unmatch", [ Ml.StrE msg ]) ])
