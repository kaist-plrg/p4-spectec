open Il.Ast
open Util.Source

(* Variable with type and dimention *)

type t = id * typ * iter list

(* Conversion to expression *)

let as_exp ((id, typ, iters) : t) : exp =
  List.fold_left
    (fun exp iter ->
      let typ =
        let typ = exp.note $ exp.at in
        IterT (typ, iter)
      in
      IterE (exp, (iter, [])) $$ (exp.at, typ))
    (VarE id $$ (id.at, typ.it))
    iters
