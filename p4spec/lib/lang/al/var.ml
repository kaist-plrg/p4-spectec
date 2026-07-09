open Ast
open Util.Source

(* Variable with type and dimention *)

type t = id * typ * iter list

(* Conversion to expression *)

let as_exp ~(dim : bool) ((id, typ, iters) : t) : exp =
  let exp_base = Il.VarE id $$ (id.at, typ.it) in
  let exp, _ =
    List.fold_left
      (fun (exp, iters) iter ->
        let typ = Il.IterT (exp.note $ exp.at, iter) $ typ.at in
        let var = (id, typ, iters) in
        let iterexp = if dim then (iter, [ var ]) else (iter, []) in
        let exp = Il.IterE (exp, iterexp) $$ (exp.at, typ.it) in
        (exp, iters @ [ iter ]))
      (exp_base, []) iters
  in
  exp
