open Lang
open Il
open Il.Print
open Util.Source

(* Type *)

type t = typ

let to_string t = string_of_typ t

(* Constructor *)

let rec iterate (typ : t) (iters : iter list) : t =
  match iters with
  | [] -> typ
  | iter :: iters -> iterate (IterT (typ, iter) $ typ.at) iters
