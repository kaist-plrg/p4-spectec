open Lang
module Collect = Collect
module Naming = Naming
module Constpool = Constpool
module Marshal = Marshal
module Unmarshal = Unmarshal
module Converter = Converter

(* Final assembly: marshal/unmarshal defs (one [Ml.LetRec] per SCC group) plus
   the converter lookup table. [groups] is [Scc.Converter.compute ctx typs],
   computed by the caller — [Scc.Converter] depends on [Naming], so calling
   it here (inside [Gen]) would make [Gen] and [Scc] depend on each other.
   See [codegen.ml]. *)
let compile (ctx : Ctx.t) (typs : Sl.typ list) (groups : Sl.typ list list) :
    Ml.toplevel list * Ml.funcdef list list * Ml.funcdef list list * Ml.toplevel
    =
  let pool, marshal_groups =
    List.fold_left_map
      (List.fold_left_map (fun pool typ -> Marshal.compile ctx pool typ))
      Constpool.empty groups
  in
  let unmarshal_groups = List.map (List.map (Unmarshal.compile ctx)) groups in
  let const_decls = List.rev_map (fun (n, e) -> Ml.Let (n, e)) pool.consts in
  let converter_table_ml = Converter.compile_converter_table typs in
  (const_decls, marshal_groups, unmarshal_groups, converter_table_ml)
