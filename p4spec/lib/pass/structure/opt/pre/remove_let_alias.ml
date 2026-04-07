open Lang
open Ol.Ast
open Util.Source

(* remove_block redundant, trivial let aliases from the code,

   let y = x; if (y == 0) then { let z = y + y; let y = 1; let k = y + y; ... }

   will be transformed into

   if (x == 0) then { let z = x + x; let y = 1; let k = y + y; ... }

   Notice the stop condition when we meet a shadowing let binding

   Other trivial binds include:
      - let y = x*
      - let y = x?
      - let y* = x*
      - let y? = x? *)

let rec remove_instr (instr : instr) : block =
  match instr.it with
  | LetI ({ it = VarE id_l; _ }, { it = VarE id_r; _ }, _, block) ->
      let renamer = Re.Renamer.singleton id_l id_r in
      block |> Re.Renamer.rename_instrs renamer |> remove_block
  | LetI
      ( { it = IterE ({ it = VarE id_l; _ }, (iter_l, _)); _ },
        { it = IterE ({ it = VarE id_r; _ }, (iter_r, _)); _ },
        _,
        block )
    when Il.Eq.eq_iter iter_l iter_r ->
      let renamer = Re.Renamer.singleton id_l id_r in
      block |> Re.Renamer.rename_instrs renamer |> remove_block
  | LetI
      ( { it = VarE id_l; _ },
        ({ it = IterE ({ it = VarE _; _ }, _); _ } as exp_r),
        _,
        block ) ->
      let replacer = Re.Replacer.singleton id_l exp_r in
      block |> Re.Replacer.replace_instrs replacer |> remove_block
  | IfI (exp_cond, iterexps, block_then) ->
      let block_then = remove_block block_then in
      [ IfI (exp_cond, iterexps, block_then) $ instr.at ]
  | HoldI (id, notexp, iterexps, block_hold, block_nothold) ->
      let block_hold = remove_block block_hold in
      let block_nothold = remove_block block_nothold in
      [ HoldI (id, notexp, iterexps, block_hold, block_nothold) $ instr.at ]
  | CaseI (exp, cases, total) ->
      let cases =
        let guards, blocks = List.split cases in
        let blocks = List.map remove_block blocks in
        List.combine guards blocks
      in
      [ CaseI (exp, cases, total) $ instr.at ]
  | GroupI (id_group, rel_signature, exps_group, block) ->
      let block = remove_block block in
      [ GroupI (id_group, rel_signature, exps_group, block) $ instr.at ]
  | LetI (exp_l, exp_r, iterinstrs, block) ->
      let block = remove_block block in
      [ LetI (exp_l, exp_r, iterinstrs, block) $ instr.at ]
  | RuleI (id, notexp, inputs, iterinstrs, block) ->
      let block = remove_block block in
      [ RuleI (id, notexp, inputs, iterinstrs, block) $ instr.at ]
  | _ -> [ instr ]

and remove_block (block : block) : block =
  match block with
  | [] -> []
  | instr_h :: block_t ->
      let block_h = remove_instr instr_h in
      let block_t = remove_block block_t in
      block_h @ block_t

let apply (block : block) : block = remove_block block
