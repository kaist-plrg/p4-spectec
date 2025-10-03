(* Converts SL AST to PL AST *)

open Sl.Ast
open Util.Source

let prosify_instr ?(rel : Pl.Ast.rid option = None) instr = failwith "TODO"
let prosify_instrs ?(rel = None) instrs =
  List.map (prosify_instr ~rel) instrs

let prosify_def (def : def) : Pl.Ast.def option =
  match def.it with
  | TypD _ -> None
  | RelD (id, _, exps, instrs, _) ->
    let instrs = prosify_instrs ~rel:(Some id) instrs in
    let rel : Pl.Ast.rel = (id, exps, instrs) in
    Some (Pl.Ast.RelD rel $ def.at)
  | DecD (id, tparams, args, instrs, _) ->
    let instrs = prosify_instrs instrs in
    let func : Pl.Ast.func = (id, tparams, args, instrs) in
    Some (Pl.Ast.DecD func $ def.at)

let prosify_spec (spec : spec) : Pl.Ast.spec =
  List.filter_map prosify_def spec
